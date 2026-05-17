#' @include operations.R utils.R
NULL

# Centralized per-op validator.
#
# Background: the dafr query DSL surfaced through many dispatchers - eltwise
# (`.apply_eltwise`), reduction-to-scalar (`.apply_reduction_to_scalar`),
# matrix-reduction fast paths (`.apply_reduction_fast`), grouped reductions
# (`.apply_reduction_grouped_*`). Each had its own per-op `type =` validator
# wired into the op functions (`.reject_non_float_type`, `.reject_unknown_param`,
# input-type guards). Adding a new op or a new dispatcher meant remembering
# to wire it through every fast/slow path. Round-5/6 fuzzing surfaced many
# cases where one path missed a guard the other had.
#
# This module replaces the scattered per-op guards with one front-door
# validator backed by a single metadata table (`.OP_META`). Every
# dispatcher calls `.validate_op_invocation()` BEFORE its fast/slow path
# dispatch; the op functions themselves only handle the happy path.
#
# Coverage rules (Julia parity, DataAxesFormats.jl/src/operations.jl):
#
# - Sum / Count           accept any Number type (parse_number_type_value)
# - Mean / Median / Var / Std / VarN / StdN / GeoMean / Quantile
#                         accept only Float types (parse_float_type_value)
# - Min / Max / Mode      do not accept a `type` parameter at all
# - Abs / Convert / Round / Clamp / Significant / Fraction
#                         accept Number / Float / none depending on op
# - String input is supported only by Mode / Count reductions (and not
#   by any eltwise except a future explicit String op)
#
# Beyond `type`, each op declares its valid named params; anything else
# triggers `the parameter: <X> does not exist for the operation: <Y>`.

# Op metadata table. Keys are dispatch names (matching .dafr_builtin
# attributes on the registered functions). Values describe what
# parameters each op accepts. NULL means "no declared meta" - custom
# ops registered via register_eltwise / register_reduction skip
# validation entirely.
.OP_META <- list(
    # ---- Reductions ------------------------------------------------------
    Sum     = list(kind = "reduction", type_param = "number",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Mean    = list(kind = "reduction", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Median  = list(kind = "reduction", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Min     = list(kind = "reduction", type_param = "none",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Max     = list(kind = "reduction", type_param = "none",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Mode    = list(kind = "reduction", type_param = "none",
                   accepts_string_input = TRUE,
                   other_params = character(0)),
    Count   = list(kind = "reduction", type_param = "number",
                   accepts_string_input = TRUE,
                   other_params = character(0)),
    Var     = list(kind = "reduction", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Std     = list(kind = "reduction", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    VarN    = list(kind = "reduction", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = c("eps")),
    StdN    = list(kind = "reduction", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = c("eps")),
    GeoMean = list(kind = "reduction", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = c("eps")),
    Quantile = list(kind = "reduction", type_param = "float",
                    accepts_string_input = FALSE,
                    other_params = c("p")),

    # ---- Eltwise --------------------------------------------------------
    Abs     = list(kind = "eltwise", type_param = "number",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Log     = list(kind = "eltwise", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = c("base", "eps")),
    Round   = list(kind = "eltwise", type_param = "number",
                   accepts_string_input = FALSE,
                   # Julia parity: Round does NOT take `digits`.
                   # dafr historically exposed `digits` via the direct
                   # R-API; tolerate it there but reject in the
                   # query-DSL path (handled by .validate_op_invocation).
                   other_params = character(0)),
    Clamp   = list(kind = "eltwise", type_param = "number",
                   accepts_string_input = FALSE,
                   # Julia parity (operations.jl Clamp uses `min` / `max`):
                   # dafr historically also accepted `low`/`high` as
                   # synonyms. Reject those in the query DSL so
                   # `% Clamp low N` raises "the parameter: low does not
                   # exist" identically to Julia. The R-API direct
                   # function call still accepts low/high since .op_clamp
                   # keeps the legacy signature.
                   other_params = c("min", "max")),
    Significant = list(kind = "eltwise", type_param = "none",
                       accepts_string_input = FALSE,
                       other_params = c("high", "low")),
    Convert = list(kind = "eltwise", type_param = "required:number",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Fraction = list(kind = "eltwise", type_param = "float",
                    accepts_string_input = FALSE,
                    other_params = character(0)),
    Exp     = list(kind = "eltwise", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = character(0)),
    Sqrt    = list(kind = "eltwise", type_param = "float",
                   accepts_string_input = FALSE,
                   other_params = character(0))
)


# Look up metadata for a builtin op. Returns NULL when the op is custom
# (registered via register_eltwise / register_reduction with no
# .dafr_builtin attribute) - we pass those through unvalidated.
.op_meta <- function(name) {
    if (is.null(name) || !is.character(name) || length(name) != 1L) return(NULL)
    .OP_META[[name]]
}


# Single front-door validator. Called from every dispatcher BEFORE
# fast/slow path selection. Validates:
#   1. Input element type vs `accepts_string_input`
#   2. `type` parameter vs the op's type-param policy
#   3. Unknown / undeclared named params
#   4. Op-specific param value constraints (range / not-negative / ...)
#
# `op_name` is the user-facing op name (the .dafr_builtin label or, for
# custom ops, the registered name). `op_kind` is "eltwise" or "reduction"
# for error-message wording (`for the eltwise operation: X` vs `for the
# reduction operation: X`). `input_value` is the in-scope vector / matrix
# / scalar being fed to the op (NULL for grouped reductions that need
# to validate without the input on hand). `params` is the coerced named
# param list.
.validate_op_invocation <- function(op_name, op_kind, input_value, params) {
    meta <- .op_meta(op_name)
    if (is.null(meta)) return(invisible())
    # Julia parity: order of validation matches parse_query's pipeline:
    # parse_*_type_value (type tag) FIRST, then input-type compatibility,
    # then declared-name checks. Lots of fuzzed both_error_mismatch
    # cases were just different orderings of the same error trigger.
    # ---- 1. `type` parameter -----------------------------------------
    type <- params[["type"]]
    if (identical(meta$type_param, "none")) {
        .reject_unknown_param(op_name, "type", type)
    } else if (identical(meta$type_param, "float")) {
        .reject_non_float_type(op_name, type)
    } else if (identical(meta$type_param, "number")) {
        .reject_non_number_type(op_name, type)
    } else if (identical(meta$type_param, "required:number")) {
        if (is.null(type)) {
            # Julia parity: parse_query reports `missing required
            # parameter: <name>` (one line).
            stop(sprintf("missing required parameter: type"),
                call. = FALSE)
        }
        .reject_non_number_type(op_name, type)
    }
    # ---- 2. Input element type ---------------------------------------
    if (!is.null(input_value) && !isTRUE(meta$accepts_string_input)) {
        v <- input_value
        if (methods::is(v, "Matrix")) v <- v@x
        if (is.character(v) || is.factor(v)) {
            kw <- if (identical(op_kind, "eltwise"))
                "the eltwise operation: %s"
            else
                "the reduction operation: %s"
            stop(sprintf(
                "unsupported input type: String\nfor %s",
                sprintf(kw, op_name)
            ), call. = FALSE)
        }
    }
    # ---- 3. Unknown named params -------------------------------------
    declared <- c("type", meta$other_params %||% character(0))
    supplied <- names(params)
    bad <- setdiff(supplied, declared)
    if (length(bad) > 0L) {
        # Julia parity: report the first unknown param (matches Julia
        # error wording exactly: `the parameter: <name>\ndoes not exist
        # for the operation: <op>`).
        stop(sprintf(
            "the parameter: %s\ndoes not exist for the operation: %s",
            bad[1L], op_name
        ), call. = FALSE)
    }
    invisible()
}


# Parse-time slice of .validate_op_invocation: runs only the checks
# that don't need the actual input value (type tag, declared-name).
# Called from .parse_eltwise / .parse_reduction so an invalid `type`
# tag like `>> Mean type NotAType` surfaces at parse, before any axis
# / property lookup at eval time. Julia parity: parse_query validates
# type tags eagerly.
.validate_op_invocation_at_parse <- function(op_name, op_kind, params) {
    meta <- .op_meta(op_name)
    if (is.null(meta)) return(invisible())
    type <- params[["type"]]
    if (identical(meta$type_param, "none")) {
        .reject_unknown_param(op_name, "type", type)
    } else if (identical(meta$type_param, "float")) {
        .reject_non_float_type(op_name, type)
    } else if (identical(meta$type_param, "number")) {
        .reject_non_number_type(op_name, type)
    } else if (identical(meta$type_param, "required:number") &&
               !is.null(type)) {
        # `Convert` requires `type` but it must be a number type. Empty
        # case is checked at eval-time (might be filled by op arg).
        .reject_non_number_type(op_name, type)
    }
    invisible()
}
