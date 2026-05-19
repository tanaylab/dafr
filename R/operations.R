#' @include utils.R
NULL

.ops_env <- new.env(parent = emptyenv())
.ops_env$reductions <- list()
.ops_env$eltwise <- list()
# Track the source location (file:line) where each op was first
# registered, so conflict errors mirror Julia's
# `conflicting registrations for the <kind> operation: <name>\n
#  first in: <file>:<line>\nsecond in: <file>:<line>` template.
.ops_env$sources_reductions <- list()
.ops_env$sources_eltwise    <- list()

# Best-effort source-location capture for the calling frame. Returns
# `file:line` if the call has a srcref, otherwise `"<unknown>:0"`.
.caller_source <- function(n = 2L) {
    sref <- tryCatch(attr(sys.call(-n), "srcref"), error = function(e) NULL)
    if (is.null(sref)) sref <- tryCatch(
        attr(sys.call(-(n + 1L)), "srcref"), error = function(e) NULL)
    if (is.null(sref)) return("<unknown>:0")
    file <- attr(sref, "srcfile")$filename %||% "<unknown>"
    line <- sref[[1L]]
    sprintf("%s:%d", file, line)
}

#' Register a reduction operation.
#'
#' Stores a user-defined reduction function under the given name so that it
#' can be retrieved by [get_reduction()] and invoked during query evaluation.
#'
#' @param name Op name (character scalar, matches token in query strings).
#' @param fn Function `function(x, ...)` where `x` is a numeric vector or
#'   matrix column and `...` collects named parameters.
#' @param overwrite Logical scalar; set to `TRUE` to replace an already-
#'   registered operation.
#' @param source Optional `"file:line"` string identifying where the
#'   registration happened. Used in the conflict error message;
#'   auto-captured from the caller's srcref if omitted.
#' @return Invisibly `NULL`.
#' @examples
#' register_reduction("Median_example", function(x, ...) median(x, ...), overwrite = TRUE)
#' registered_reductions()
#' @export
register_reduction <- function(name, fn, overwrite = FALSE, source = NULL) {
    .assert_name(name, "reduction name")
    if (!is.function(fn)) {
        stop(sprintf("`fn` must be a function (for reduction %s)", sQuote(name)),
            call. = FALSE
        )
    }
    if (is.null(source)) source <- .caller_source()
    if (!isTRUE(overwrite) && !is.null(.ops_env$reductions[[name]])) {
        stop(sprintf(
            "conflicting registrations for the reduction operation: %s\nfirst in: %s\nsecond in: %s",
            name, .ops_env$sources_reductions[[name]] %||% "<unknown>:0",
            source
        ), call. = FALSE)
    }
    .ops_env$reductions[[name]] <- fn
    .ops_env$sources_reductions[[name]] <- source
    invisible(NULL)
}

#' Register an eltwise operation.
#'
#' Stores a user-defined element-wise function under the given name so that it
#' can be retrieved by [get_eltwise()] and invoked during query evaluation.
#'
#' @param name Op name (character scalar, matches token in query strings).
#' @param fn Function `function(x, ...)` where `x` is a numeric vector or
#'   matrix (eltwise ops preserve shape) and `...` collects named parameters.
#' @param overwrite Logical scalar; set to `TRUE` to replace an already-
#'   registered operation.
#' @param source Optional `"file:line"` string identifying where the
#'   registration happened. Used in the conflict error message;
#'   auto-captured from the caller's srcref if omitted.
#' @return Invisibly `NULL`.
#' @examples
#' register_eltwise("Clamp01", function(x, ...) pmin(pmax(x, 0), 1),
#'                  overwrite = TRUE)
#' registered_eltwise()
#' @export
register_eltwise <- function(name, fn, overwrite = FALSE, source = NULL) {
    .assert_name(name, "eltwise name")
    if (!is.function(fn)) {
        stop(sprintf("`fn` must be a function (for eltwise %s)", sQuote(name)),
            call. = FALSE
        )
    }
    if (is.null(source)) source <- .caller_source()
    if (!isTRUE(overwrite) && !is.null(.ops_env$eltwise[[name]])) {
        stop(sprintf(
            "conflicting registrations for the eltwise operation: %s\nfirst in: %s\nsecond in: %s",
            name, .ops_env$sources_eltwise[[name]] %||% "<unknown>:0",
            source
        ), call. = FALSE)
    }
    .ops_env$eltwise[[name]] <- fn
    .ops_env$sources_eltwise[[name]] <- source
    invisible(NULL)
}

#' Register a query operation (eltwise or reduction).
#'
#' Single entry point for adding custom eltwise / reduction operations
#' to the dafr query DSL at runtime. Mirrors Julia's
#' `register_query_operation()` from `DataAxesFormats.Registry`.
#'
#' @param kind Either `"eltwise"` or `"reduction"`.
#' @param name Op name; must match `[A-Z][A-Za-z0-9_]*` to be a valid
#'   query token.
#' @param fn Function `function(x, ...)` where `x` is a numeric value
#'   (scalar / vector / matrix) and `...` collects named parameters.
#' @param overwrite Logical; replace an already-registered op of the
#'   same kind and name.
#' @param source Optional `"file:line"` string identifying where the
#'   registration happened. Used in the conflict error message;
#'   auto-captured from the caller's srcref if omitted.
#' @return Invisibly `NULL`.
#' @examples
#' register_query_operation("eltwise", "Clamp01",
#'   function(x, ...) pmin(pmax(x, 0), 1),
#'   overwrite = TRUE)
#' @export
register_query_operation <- function(kind, name, fn,
                                     overwrite = FALSE,
                                     source = NULL) {
    if (!is.character(kind) || length(kind) != 1L ||
        !(kind %in% c("eltwise", "reduction"))) {
        stop("kind must be either \"eltwise\" or \"reduction\"",
             call. = FALSE)
    }
    if (is.null(source)) source <- .caller_source()
    if (kind == "eltwise") {
        register_eltwise(name, fn, overwrite = overwrite, source = source)
    } else {
        register_reduction(name, fn, overwrite = overwrite, source = source)
    }
}

#' Retrieve a registered reduction operation by name.
#'
#' Looks up a reduction operation previously stored via
#' [register_reduction()]. Raises if the name is not registered.
#'
#' @param name Op name (character scalar).
#' @return The stored function.
#' @examples
#' registered_reductions()   # "Sum" is among the built-ins
#' fn <- get_reduction("Sum")
#' fn(c(1, 2, 3))
#' @export
get_reduction <- function(name) {
    fn <- .ops_env$reductions[[name]]
    if (is.null(fn)) {
        stop(sprintf("unknown reduction operation: %s", sQuote(name)), call. = FALSE)
    }
    fn
}

#' Retrieve a registered eltwise operation by name.
#'
#' Looks up an eltwise operation previously stored via
#' [register_eltwise()]. Raises if the name is not registered.
#'
#' @param name Op name (character scalar).
#' @return The stored function.
#' @examples
#' registered_eltwise()      # "Abs" is among the built-ins
#' fn <- get_eltwise("Abs")
#' fn(c(-1, 2, -3))
#' @export
get_eltwise <- function(name) {
    fn <- .ops_env$eltwise[[name]]
    if (is.null(fn)) {
        stop(sprintf("unknown eltwise operation: %s", sQuote(name)), call. = FALSE)
    }
    fn
}

#' List registered reduction operations.
#'
#' Returns the names of all currently registered reduction operations.
#'
#' @return Sorted character vector of registered reduction names.
#' @examples
#' head(registered_reductions())
#' register_reduction("Median_example", function(x, ...) median(x), overwrite = TRUE)
#' "Median_example" %in% registered_reductions()
#' @export
registered_reductions <- function() sort(names(.ops_env$reductions))

#' List registered eltwise operations.
#'
#' Returns the names of all currently registered eltwise operations.
#'
#' @return Sorted character vector of registered eltwise names.
#' @examples
#' head(registered_eltwise())
#' register_eltwise("Negate_example", function(x, ...) -x, overwrite = TRUE)
#' "Negate_example" %in% registered_eltwise()
#' @export
registered_eltwise <- function() sort(names(.ops_env$eltwise))

# Internal: valid named parameters accepted by an operation's query-DSL form.
# Derived from the function's formal arguments minus the input slot `x` and
# the variadic `...`. The `type` pseudo-parameter is universally accepted —
# it controls output-type conversion at the query layer (see Julia
# DataAxesFormats.jl operations.jl: every reduction/eltwise treats `type`
# the same way).
.op_valid_params <- function(fn) {
    formal_names <- names(formals(fn))
    params <- setdiff(formal_names, c("x", "..."))
    union(params, "type")
}

.op_sum <- function(x, ..., na_rm = FALSE, type = NULL) {
    .reject_non_number_type("Sum", type)
    .cast_to_type(sum(x, na.rm = na_rm), type)
}
.op_mean <- function(x, ..., na_rm = FALSE, type = NULL) {
    # Julia parity (operations.jl Mean): `type` must be a float type.
    .reject_non_float_type("Mean", type)
    .cast_to_type(mean(x, na.rm = na_rm), type)
}
.op_max <- function(x, ..., na_rm = FALSE, type = NULL) {
    .reject_unknown_param("Max", "type", type)
    max(x, na.rm = na_rm)
}
.op_min <- function(x, ..., na_rm = FALSE, type = NULL) {
    .reject_unknown_param("Min", "type", type)
    min(x, na.rm = na_rm)
}
.op_count <- function(x, ..., type = NULL) {
    .reject_non_number_type("Count", type)
    .cast_to_type(length(x), type)
}

.op_log <- function(x, ..., eps = 0, base = exp(1), type = NULL) {
    .reject_non_float_type("Log", type)
    .require_numeric_param("Log", "eps", eps)
    .require_numeric_param("Log", "base", base)
    eps  <- as.numeric(.resolve_named_numeric(eps))
    base <- as.numeric(.resolve_named_numeric(base))
    # Julia parity: NaN/Inf are valid float values that propagate to the
    # output; only strictly-negative or zero base is rejected. R's `<=`
    # returns NA on NaN, so wrap with isTRUE to avoid the "missing value
    # where TRUE/FALSE needed" branch error.
    if (isTRUE(base <= 0)) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: positive\nfor the parameter: base\nfor the operation: Log",
            format(base)
        ), call. = FALSE)
    }
    if (isTRUE(eps < 0)) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: not negative\nfor the parameter: eps\nfor the operation: Log",
            format(eps)
        ), call. = FALSE)
    }
    # Julia parity: log(0) returns -Inf without error; log of a negative
    # value raises DomainError. Reject only strictly-negative `x + eps`.
    # Sparse Matrices keep zeros implicit, so only inspect explicit cells.
    .x <- if (methods::is(x, "Matrix")) x@x else as.numeric(x)
    if (length(.x) > 0L && any(.x + eps < 0, na.rm = TRUE)) {
        stop(sprintf(
            "DomainError with %s:\nlog was called with a negative real argument",
            format(min(.x[!is.na(.x)]))
        ), call. = FALSE)
    }
    .cast_to_type(log(x + eps, base = base), type)
}
.op_abs <- function(x, ..., type = NULL) {
    .reject_non_number_type("Abs", type)
    .cast_to_type(abs(x), type)
}
.op_exp <- function(x, ..., type = NULL) {
    .reject_non_number_type("Exp", type)
    .cast_to_type(exp(x), type)
}
.op_sqrt <- function(x, ..., type = NULL) {
    .reject_non_number_type("Sqrt", type)
    .cast_to_type(sqrt(x), type)
}
.op_round <- function(x, ..., digits = 0, type = NULL) {
    .reject_non_number_type("Round", type)
    # Julia parity: when no `type` is given, % Round defaults to Int64
    # (queries.jl uses int_type_for(eltype(x))). Casts trigger the same
    # InexactError check on NaN / Inf / non-integer-after-round (which
    # can't happen for digits == 0 but matters for non-zero digits and
    # for NaN propagation). Integer / integer64 inputs short-circuit
    # since round on an integer is the identity.
    if (is.null(type) && (is.numeric(x) || methods::is(x, "Matrix"))
        && !inherits(x, "integer64")
        && !is.integer(if (methods::is(x, "Matrix")) x@x else x)) {
        type <- "Int64"
    }
    .cast_to_type(round(x, digits = digits), type)
}

.op_clamp <- function(x, ..., min = NULL, max = NULL, type = NULL) {
    .reject_non_number_type("Clamp", type)
    .require_numeric_param("Clamp", "min", min)
    .require_numeric_param("Clamp", "max", max)
    min <- if (is.null(min)) -Inf else as.numeric(min)
    max <- if (is.null(max)) Inf else as.numeric(max)
    if (min >= max) {
        stop(sprintf("Clamp: min (%g) must be strictly less than max (%g)", min, max),
            call. = FALSE
        )
    }
    if (methods::is(x, "dgCMatrix")) {
        if (min <= 0 && 0 <= max) {
            out <- x
            out@x <- pmin(pmax(out@x, min), max)
            return(.cast_to_type(out, type))
        }
        x <- as.matrix(x)
    }
    .cast_to_type(pmin(pmax(x, min), max), type)
}

.op_convert <- function(x, ..., type) {
    if (missing(type)) {
        # Julia parity: error wording is "missing required parameter:
        # type" - matches the format used by Significant / Quantile.
        stop("missing required parameter: type", call. = FALSE)
    }
    # Julia parity (operations.jl Convert dispatch): non-numeric input
    # types (String / Symbol) are explicitly rejected at the eltwise
    # entry. Without this guard, `as.<T>(character_vec)` silently yields
    # a vector of NAs with a coercion warning.
    if (is.character(x) || is.factor(x)) {
        stop(
            "unsupported input type: String\n",
            "for the eltwise operation: Convert",
            call. = FALSE
        )
    }
    # Normalize Julia type names to R-native canonical form. Int*/UInt*
    # narrower than 64 bits land on R's integer (32-bit); Int64/UInt64
    # land on bit64::integer64. Range / inexact checking happens after
    # type resolution so the error message reports the Julia type name.
    # Julia parity (operations.jl DTYPE_BY_NAME): accept both cased and
    # all-lowercase forms ("Int8" / "int8", "Float64" / "float64", etc.).
    julia_int_to_r <- c(
        Int    = "integer64",
        Int8   = "integer",  Int16  = "integer",  Int32  = "integer",
        UInt8  = "integer",  UInt16 = "integer",  UInt32 = "integer",
        Int64  = "integer64", UInt64 = "integer64"
    )
    julia_aliases <- c(
        Float32 = "double",
        Float64 = "double",
        julia_int_to_r,
        Bool = "logical"
    )
    # Synonym for each cased alias under its all-lowercase form.
    lower_aliases <- julia_aliases
    names(lower_aliases) <- tolower(names(lower_aliases))
    julia_aliases <- c(julia_aliases, lower_aliases)
    julia_type <- NULL
    if (is.character(type) && length(type) == 1L && type %in% names(julia_aliases)) {
        # Normalize to canonical cased form so downstream error/check uses
        # the Julia-style name (InexactError: UInt8(...)).
        canonical <- c(
            float32 = "Float32", float64 = "Float64",
            int = "Int",
            int8 = "Int8", int16 = "Int16", int32 = "Int32", int64 = "Int64",
            uint8 = "UInt8", uint16 = "UInt16", uint32 = "UInt32", uint64 = "UInt64",
            bool = "Bool"
        )
        julia_type <- if (type %in% names(canonical)) unname(canonical[type]) else type
        type <- unname(julia_aliases[type])
    }
    valid_types <- c("double", "integer", "logical", "integer64")
    if (!is.character(type) || length(type) != 1L || !type %in% valid_types) {
        stop(sprintf(
            "Convert: 'type' must be one of 'double', 'integer', 'logical', 'integer64' (or Julia aliases 'Float32'/'Float64'/'Int*'/'UInt*'/'Bool'); got %s",
            sQuote(as.character(type)[1L])
        ), call. = FALSE)
    }
    # Julia InexactError: integer targets reject fractional / out-of-range
    # values. Apply the check using the Julia type name when one was given
    # so the error message reads `InexactError: UInt8(-1)`, otherwise fall
    # back to the canonical R integer range when type == "integer".
    if (type %in% c("integer", "integer64")) {
        check_name <- julia_type %||% if (type == "integer64") "Int64" else "integer"
        if (!is.null(.INT_TYPE_RANGES[[check_name]])) {
            .check_inexact_int(x, check_name)
        }
    }
    if (type == "logical") {
        .check_inexact_bool(x, julia_type %||% "Bool")
    }
    # integer64 path: densify sparse input (no sparse integer64 class exists).
    # Preserve dim/dimnames -- bit64::as.integer64 strips them on matrix input.
    if (type == "integer64") {
        if (methods::is(x, "dgCMatrix")) {
            x <- as.matrix(x)
        }
        out <- bit64::as.integer64(x)
        if (!is.null(dim(x))) {
            dim(out) <- dim(x)
            dimnames(out) <- dimnames(x)
        }
        return(out)
    }
    # Sparse preservation for dgCMatrix
    if (methods::is(x, "dgCMatrix")) {
        if (type == "double") return(x)
        if (type == "integer") {
            x@x <- as.double(as.integer(x@x))
            return(x)
        }
        if (type == "logical") {
            x@x <- as.double(x@x != 0)
            return(Matrix::drop0(x))
        }
    }
    # Dense or vector: existing behaviour via storage.mode
    storage.mode(x) <- type
    x
}

## ---- Param-validation helpers (Julia parity) ------------------------------
#
# These mirror the Julia `parse_*_type_value` / `parse_number_value`
# helpers in DataAxesFormats.jl/src/operations.jl. Error wording follows
# the Julia template:
#   invalid value: "<val>"
#   value must be: <constraint>
#   for the parameter: <name>
#   for the operation: <op>

.NUMBER_TYPES <- c(
    "Bool",
    "Int", "Int8", "Int16", "Int32", "Int64",
    "UInt8", "UInt16", "UInt32", "UInt64",
    "Float32", "Float64",
    "double", "integer", "logical", "integer64",
    # Julia parity (operations.jl DTYPE_BY_NAME): lowercase aliases.
    "bool", "int", "int8", "int16", "int32", "int64",
    "uint8", "uint16", "uint32", "uint64",
    "float32", "float64"
)
# Legacy alias - retained for any callers that still reference it. Same
# membership as .NUMBER_TYPES since String never belonged on the numeric
# allow-list (Julia parses number types via parse_number_type_value, which
# does not accept "String").
.NUMBER_TYPES_FILTERED_NUMERIC <- .NUMBER_TYPES

# Canonicalize lowercase Julia type aliases to cased form so downstream
# range / inexact / storage-mode logic only deals with the cased names.
.JULIA_TYPE_CANONICAL <- c(
    bool = "Bool",
    int = "Int", int8 = "Int8", int16 = "Int16", int32 = "Int32", int64 = "Int64",
    uint8 = "UInt8", uint16 = "UInt16", uint32 = "UInt32", uint64 = "UInt64",
    float32 = "Float32", float64 = "Float64"
)
.canonicalize_julia_type <- function(type) {
    if (is.null(type)) return(type)
    s <- as.character(type)[[1L]]
    if (!is.na(s) && s %in% names(.JULIA_TYPE_CANONICAL)) {
        return(unname(.JULIA_TYPE_CANONICAL[[s]]))
    }
    type
}

# Julia InexactError ranges for integer-typed targets. Values outside
# the range, or values with a non-zero fractional part, must raise.
# Int64 / UInt64 limits are stored as doubles (precision loss past 2^53)
# but the check is conservative: any finite value past these bounds is
# clearly out of range.
.INT_TYPE_RANGES <- list(
    Int8   = c(-128, 127),
    Int16  = c(-32768, 32767),
    Int32  = c(-2147483648, 2147483647),
    Int64  = c(-2^63, 2^63 - 1),
    UInt8  = c(0, 255),
    UInt16 = c(0, 65535),
    UInt32 = c(0, 4294967295),
    UInt64 = c(0, 2^64 - 1),
    integer = c(-2147483648, 2147483647)
)

.check_inexact_bool <- function(value, type_name) {
    if (methods::is(value, "Matrix")) value <- value@x
    if (length(value) == 0L) return(invisible())
    if (is.logical(value)) return(invisible())
    v <- suppressWarnings(as.double(value))
    # Julia parity: Bool(Inf) / Bool(NaN) raises InexactError, same
    # as Bool(0.5). Treat non-finite values (excluding genuine NA) as
    # bad.
    bad <- (is.finite(v) & v != 0 & v != 1) |
        (is.nan(v) | (is.infinite(v)))
    if (any(bad, na.rm = TRUE)) {
        which_bad <- which(bad)[1L]
        stop(sprintf("InexactError: %s(%s)",
            type_name, format(v[which_bad])
        ), call. = FALSE)
    }
    invisible()
}

# Julia parity: raise InexactError for any value that can't be losslessly
# represented in the target integer type. Mimics InexactError(T(v)).
#
# Float32 source parity: when a value's Float64 representation is
# slightly fractional but its Float32 round-trip lands on an exact
# integer, Julia would have accumulated the upstream computation in
# Float32 and cast cleanly. R has no Float32 storage and accumulates
# all Float64, so a Float32-source `frac >> Sum type Int32` produces
# 6.000000044703484 that fails the strict integer check despite Julia
# returning a clean Int32(6). Round-trip through Float32 (writeBin /
# readBin size = 4) before the inexact check to recover that parity.
.check_inexact_int <- function(value, type_name) {
    bounds <- .INT_TYPE_RANGES[[type_name]]
    if (is.null(bounds)) return(invisible())
    if (methods::is(value, "Matrix")) value <- value@x
    if (length(value) == 0L) return(invisible())
    v <- suppressWarnings(as.double(value))
    strict_bad <- !is.finite(v) | (v < bounds[1L]) | (v > bounds[2L]) |
        (v != floor(v))
    if (!any(strict_bad, na.rm = TRUE)) return(invisible())
    # Some entries failed strict check; retry with the Float32 round-trip
    # so Float32-source results that happen to land on a clean integer
    # (modulo Float32 ulp noise reintroduced by Float64 accumulation)
    # match Julia.
    v_f32 <- readBin(writeBin(v, raw(), size = 4L), "double",
                     n = length(v), size = 4L)
    bad <- !is.finite(v_f32) | (v_f32 < bounds[1L]) | (v_f32 > bounds[2L]) |
        (v_f32 != floor(v_f32))
    if (any(bad, na.rm = TRUE)) {
        which_bad <- which(bad)[1L]
        stop(sprintf("InexactError: %s(%s)",
            type_name, format(v[which_bad])
        ), call. = FALSE)
    }
    invisible()
}

# CO5 parity: honor `type =` on numeric eltwise ops by coercing the
# result. Matches Julia DAF where `% Op type Float32` returns a
# Float32-storage result.
.cast_to_type <- function(value, type) {
    if (is.null(type) || is.null(value)) return(value)
    if (methods::is(value, "Matrix")) return(value)  # leave Matrix alone
    type_name <- .canonicalize_julia_type(as.character(type)[[1L]])
    target <- switch(type_name,
        integer = , Int = , Int8 = , Int16 = , Int32 = , Int64 = ,
        UInt8 = , UInt16 = , UInt32 = , UInt64 = "integer",
        numeric = , double = , Float32 = , Float64 = "double",
        logical = , Bool = "logical",
        character = , String = "character",
        NULL
    )
    if (is.null(target)) return(value)
    # Range / inexact check for any integer target (Julia parity:
    # % Round type Int8 / type UInt32 etc. raises InexactError when a
    # value is fractional, infinite, or outside the target's range).
    if (target == "integer" && !is.null(.INT_TYPE_RANGES[[type_name]])) {
        .check_inexact_int(value, type_name)
    }
    # Bool target: only 0 / 1 / NA accepted (Julia parity:
    # Bool(0.5) -> InexactError).
    if (target == "logical" && (type_name %in% c("Bool", "logical"))) {
        .check_inexact_bool(value, type_name)
    }
    if (is.matrix(value)) {
        dn <- dimnames(value)
        storage.mode(value) <- target
        dimnames(value) <- dn
        return(value)
    }
    nm <- names(value)
    out <- as.vector(value, mode = target)
    names(out) <- nm
    out
}

.reject_non_float_type <- function(op_name, type) {
    if (is.null(type)) return(invisible())
    raw <- as.character(type)[[1L]]
    canon <- .canonicalize_julia_type(raw)
    float_types <- c("Float32", "Float64", "double")
    if (!canon %in% float_types) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: a float type\nfor the parameter: type\nfor the operation: %s",
            raw, op_name
        ), call. = FALSE)
    }
    invisible()
}

# Julia parity: ops that do not declare a given parameter raise
# "the parameter: <name>\ndoes not exist for the operation: <op>" when
# the query supplies one. Multi-line format matches Julia's wording
# byte for byte; the newline avoids triage tools confusing this with
# a different (but related) Julia error message ("the parameter: X is
# required" vs "the parameter: X does not exist").
.reject_unknown_param <- function(op_name, param_name, value) {
    if (is.null(value)) return(invisible())
    stop(sprintf(
        "the parameter: %s\ndoes not exist for the operation: %s",
        param_name, op_name
    ), call. = FALSE)
}

.reject_non_number_type <- function(op_name, type) {
    if (is.null(type)) return(invisible())
    raw <- as.character(type)[[1L]]
    if (!raw %in% .NUMBER_TYPES) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: a number type\nfor the parameter: type\nfor the operation: %s",
            raw, op_name
        ), call. = FALSE)
    }
    invisible()
}

# Julia DAF.jl accepts named constants in numeric operation parameters
# (`Log base e`, `Log base pi`, etc.). Map them to their R numeric value
# so downstream as.numeric() / arithmetic sees a real number.
.NAMED_NUMERIC_CONSTANTS <- list(e = exp(1), pi = pi)

.resolve_named_numeric <- function(value) {
    if (is.character(value) && length(value) == 1L) {
        v <- .NAMED_NUMERIC_CONSTANTS[[value]]
        if (!is.null(v)) return(v)
    }
    value
}

.require_numeric_param <- function(op_name, param_name, value) {
    if (is.null(value)) return(invisible())
    if (is.numeric(value) && length(value) == 1L) return(invisible())
    # Julia parity: accept named constants like "e", "pi" as numeric.
    if (is.character(value) && length(value) == 1L &&
        !is.null(.NAMED_NUMERIC_CONSTANTS[[value]])) {
        return(invisible())
    }
    # Julia parity (parse_number_value via parse(Float64, s)): accept
    # NaN/Inf/-Inf (case-insensitive) and let the kernel propagate them
    # to the output. Without this guard, `% Log base NaN` errors here
    # while Julia returns an all-NaN vector.
    if (is.character(value) && length(value) == 1L) {
        lo <- tolower(value)
        if (lo %in% c("nan", "inf", "-inf", "+inf",
                      "infinity", "-infinity", "+infinity")) {
            return(invisible())
        }
    }
    n <- suppressWarnings(as.numeric(value))
    # is.na(NaN) is TRUE in R but a parsed NaN literal is valid input —
    # distinguish via is.nan to avoid rejecting it.
    if (length(value) == 1L && (!is.na(n) || is.nan(n))) return(invisible())
    stop(sprintf(
        "invalid value: \"%s\"\nvalue must be: a number\nfor the parameter: %s\nfor the operation: %s",
        as.character(value)[[1L]], param_name, op_name
    ), call. = FALSE)
}

.op_fraction <- function(x, ..., type = NULL) {
    # Julia parity: only float types are accepted via the `type`
    # parameter (DataAxesFormats.jl/src/operations.jl Fraction
    # constructor uses parse_float_type_value). Delegate to the
    # central .reject_non_float_type helper so lowercase aliases
    # (`float32` / `float64`) work the same as cased forms.
    .reject_non_float_type("Fraction", type)
    if (is.null(dim(x)) && length(x) == 1L) {
        stop("applying Fraction eltwise operation to a scalar",
            call. = FALSE)
    }
    # Julia parity: NaN inputs propagate through the division (Inf or NaN
    # outputs). Zero-sum columns / vectors return zeros. NA never appears
    # in Julia storage, so an NaN total goes through unchanged division.
    if (methods::is(x, "dgCMatrix")) {
        out <- x
        col_sums <- Matrix::colSums(out)
        for (j in seq_along(col_sums)) {
            start <- out@p[j] + 1L
            end <- out@p[j + 1L]
            if (start <= end) {
                cs <- col_sums[j]
                if (is.na(cs)) {
                    out@x[start:end] <- NaN
                } else if (cs != 0) {
                    out@x[start:end] <- out@x[start:end] / cs
                } else {
                    out@x[start:end] <- 0
                }
            }
        }
        return(out)
    }
    if (is.matrix(x)) {
        col_sums <- colSums(x)
        out <- x
        storage.mode(out) <- "double"
        for (j in seq_len(ncol(out))) {
            cs <- col_sums[j]
            out[, j] <- if (is.na(cs)) out[, j] / cs
                        else if (cs == 0) 0
                        else out[, j] / cs
        }
        return(out)
    }
    total <- sum(x)
    if (!is.na(total) && total == 0) return(rep(0, length(x)))
    x / total
}

.significant_vec <- function(v, high, low) {
    # Julia parity: a NaN entry propagates through Significant. R's
    # max() on a vector containing NaN returns NaN; comparing that to
    # `high` yields NA which then trips `if (...)` with the "missing
    # value where TRUE/FALSE needed" branch error. Skip the all-zeros
    # fast path when v isn't strictly comparable.
    abs_v <- abs(v)
    if (!any(is.nan(v) | is.nan(high) | is.nan(low)) &&
        isTRUE(max(abs_v) < high)) {
        return(rep(0, length(v)))
    }
    keep <- abs_v >= low
    keep[is.na(keep)] <- TRUE   # NaN entries fall through to output
    v[!keep] <- 0
    v
}

.op_significant <- function(x, ..., high, low = high) {
    if (missing(high)) {
        # Julia parity: error wording is "missing required parameter:
        # high" - matches the format used by Convert / Quantile etc.
        stop("missing required parameter: high", call. = FALSE)
    }
    # Julia parity: named constants (`e`, `pi`) accepted as numeric
    # params on every op. Without this, `% Significant high e` errors
    # while `% Log base e` works (Log already routes through
    # .require_numeric_param). Resolve here for symmetry.
    .require_numeric_param("Significant", "high", high)
    .require_numeric_param("Significant", "low", low)
    high <- as.numeric(.resolve_named_numeric(high))
    low  <- as.numeric(.resolve_named_numeric(low))
    # Julia parity (DataAxesFormats.jl operations.jl Significant):
    # parameter validation messages use the
    #   invalid value: "<val>"
    #   value must be: <constraint>
    #   for the parameter: <name>
    #   for the operation: Significant
    # template.
    # Julia parity: NaN/Inf in `high` / `low` aren't rejected here -
    # Julia parses them via parse(Float64, ...), they pass through, and
    # the per-element comparison naturally yields NaN-tainted output.
    # R's `<=` / `<` return NA on NaN; isTRUE pins the test to a real
    # logical so the "missing value where TRUE/FALSE needed" branch
    # error doesn't fire on `% Significant high NaN`. Only reject when
    # high is a real number that's <= 0.
    if (!is.numeric(high) || length(high) != 1L ||
        (!is.nan(high) && isTRUE(high <= 0))) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: positive\nfor the parameter: high\nfor the operation: Significant",
            as.character(high)[1L]
        ), call. = FALSE)
    }
    if (!is.numeric(low) || length(low) != 1L) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: not negative\nfor the parameter: low\nfor the operation: Significant",
            as.character(low)[1L]
        ), call. = FALSE)
    }
    if (isTRUE(low < 0)) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: not negative\nfor the parameter: low\nfor the operation: Significant",
            as.character(low)[1L]
        ), call. = FALSE)
    }
    if (isTRUE(low > high)) {
        stop(sprintf(
            "invalid value: \"%s\"\nvalue must be: at most high (%s)\nfor the parameter: low\nfor the operation: Significant",
            as.character(low)[1L], format(as.numeric(high))
        ), call. = FALSE)
    }
    if (is.null(dim(x)) && length(x) == 1L) {
        stop("applying Significant eltwise operation to a scalar",
            call. = FALSE)
    }
    # Julia parity: Significant on a Bool input uses Bool(high) / Bool(low)
    # internally - a fractional `high` like 0.5 cannot round-trip and
    # raises InexactError. R promotes Bool to numeric silently, which
    # would otherwise mask the type-mismatch.
    if (is.logical(x)) {
        for (nm in list(list("high", high), list("low", low))) {
            v <- nm[[2L]]
            if (is.numeric(v) && length(v) == 1L && !is.na(v) &&
                v != 0 && v != 1) {
                stop(sprintf("InexactError: Bool(%s)", format(v)),
                    call. = FALSE)
            }
        }
    }
    # Julia parity: Significant on integer input casts high/low to the
    # input's int type. Inf / NaN / non-integer high or low can't
    # round-trip and raise InexactError. R stores integers as double or
    # integer64 internally, so we have to detect this manually.
    is_int_input <- is.integer(x) || inherits(x, "integer64") ||
        (methods::is(x, "Matrix") && is.integer(x@x)) ||
        (is.matrix(x) && is.integer(x))
    if (!is_int_input && (is.numeric(x) || methods::is(x, "Matrix"))) {
        is_int_input <- isTRUE(attr(x, "dafr_int_storage"))
    }
    if (is_int_input) {
        int_name <- attr(x, "dafr_julia_int_type") %||%
            (if (inherits(x, "integer64")) "Int64" else "Int32")
        bounds <- .INT_TYPE_RANGES[[int_name]]
        for (nm in list(list("high", high), list("low", low))) {
            v <- nm[[2L]]
            if (is.numeric(v) && length(v) == 1L) {
                bad <- FALSE
                if (is.nan(v)) {
                    bad <- TRUE
                } else if (!is.na(v)) {
                    if (!is.finite(v) || v != trunc(v)) {
                        bad <- TRUE
                    } else if (!is.null(bounds) &&
                               (v < bounds[1L] || v > bounds[2L])) {
                        bad <- TRUE
                    }
                }
                if (bad) {
                    stop(sprintf("InexactError: %s(%s)",
                        int_name, format(v)
                    ), call. = FALSE)
                }
            }
        }
    }
    if (methods::is(x, "dgCMatrix")) {
        out <- x
        for (j in seq_len(ncol(out))) {
            start <- out@p[j] + 1L
            end <- out@p[j + 1L]
            if (start <= end) {
                out@x[start:end] <- .significant_vec(out@x[start:end], high, low)
            }
        }
        return(Matrix::drop0(out))
    }
    if (is.matrix(x)) {
        out <- x
        storage.mode(out) <- "double"
        for (j in seq_len(ncol(out))) {
            out[, j] <- .significant_vec(out[, j], high, low)
        }
        return(out)
    }
    .significant_vec(x, high, low)
}

.var_uncorrected <- function(x, na_rm) {
    if (na_rm) x <- x[!is.na(x)]
    n <- length(x)
    if (n == 0L) return(NA_real_)
    # Julia parity: NaN in input -> NaN result (not NA). Real NA (a thing R has
    # but Julia DAF does not) still yields NA. is.na() is true for both, so we
    # only differentiate when something actually NaN is present.
    if (anyNA(x)) {
        if (any(is.nan(x))) return(NaN)
        return(NA_real_)
    }
    mu <- sum(x) / n
    sum((x - mu)^2) / n
}

.op_var <- function(x, ..., na_rm = FALSE, type = NULL) {
    .reject_non_float_type("Var", type)
    .cast_to_type(.var_uncorrected(as.numeric(x), isTRUE(na_rm)), type)
}

.op_std <- function(x, ..., na_rm = FALSE, type = NULL) {
    .reject_non_float_type("Std", type)
    v <- .var_uncorrected(as.numeric(x), isTRUE(na_rm))
    .cast_to_type(if (is.na(v)) v else sqrt(v), type)
}

.assert_non_negative_eps <- function(eps, op_name = NULL) {
    # Julia parity: named constants (`e`, `pi`) and NaN are valid as
    # eps params via parse_number_value. Route through the same
    # validators used by Log so VarN/StdN/GeoMean don't reject them
    # while Log accepts.
    if (!is.null(op_name)) {
        .require_numeric_param(op_name, "eps", eps)
    }
    eps <- as.numeric(.resolve_named_numeric(eps))
    if (!is.numeric(eps) || length(eps) != 1L ||
        (!is.nan(eps) && (is.na(eps) || isTRUE(eps < 0)))) {
        if (!is.null(op_name)) {
            # Julia parity error template.
            stop(sprintf(
                "invalid value: \"%s\"\nvalue must be: not negative\nfor the parameter: eps\nfor the operation: %s",
                as.character(eps)[1L], op_name
            ), call. = FALSE)
        }
        stop(sprintf("'eps' must be a non-negative number (got %s)",
            as.character(eps)[1L]
        ), call. = FALSE)
    }
    eps
}

.op_varn <- function(x, ..., na_rm = FALSE, eps = 0, type = NULL) {
    .reject_non_float_type("VarN", type)
    eps <- .assert_non_negative_eps(eps, "VarN")
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    v <- .var_uncorrected(x, na_rm = FALSE)
    mu <- if (length(x) == 0L) NA_real_ else sum(x) / length(x)
    .cast_to_type(v / (mu + eps), type)
}

.op_stdn <- function(x, ..., na_rm = FALSE, eps = 0, type = NULL) {
    .reject_non_float_type("StdN", type)
    eps <- .assert_non_negative_eps(eps, "StdN")
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    v <- .var_uncorrected(x, na_rm = FALSE)
    mu <- if (length(x) == 0L) NA_real_ else sum(x) / length(x)
    .cast_to_type(sqrt(v) / (mu + eps), type)
}

.op_median <- function(x, ..., na_rm = FALSE, type = NULL) {
    .reject_non_float_type("Median", type)
    v <- as.numeric(x)
    # Julia DAF.jl median([..., NaN, ...]) returns NaN. R's stats::median
    # coerces NaN to NA and returns NA. Preserve NaN by short-circuiting
    # before stats::median (only when na_rm is FALSE - na_rm = TRUE drops
    # both NA and NaN, matching Julia's no-NaN behaviour).
    if (!isTRUE(na_rm) && length(v) > 0L && anyNA(v) && any(is.nan(v))) {
        return(.cast_to_type(NaN, type))
    }
    .cast_to_type(stats::median(v, na.rm = isTRUE(na_rm)), type)
}

.op_quantile <- function(x, ..., p, na_rm = FALSE, type = NULL) {
    .reject_non_float_type("Quantile", type)
    if (missing(p)) {
        # Julia parity: error wording is "missing required parameter:
        # p".
        stop("missing required parameter: p", call. = FALSE)
    }
    .require_numeric_param("Quantile", "p", p)
    p <- as.numeric(.resolve_named_numeric(p))
    if (!is.numeric(p) || length(p) != 1L || is.na(p) || p < 0 || p > 1) {
        stop(sprintf("Quantile: 'p' must be in [0, 1] (got %s)",
            as.character(p)[1L]
        ), call. = FALSE)
    }
    .cast_to_type(
        unname(stats::quantile(as.numeric(x), probs = p, na.rm = isTRUE(na_rm))),
        type
    )
}

.op_geomean <- function(x, ..., eps = 0, na_rm = FALSE, type = NULL) {
    .reject_non_float_type("GeoMean", type)
    eps <- .assert_non_negative_eps(eps, "GeoMean")
    x <- as.numeric(x)
    if (isTRUE(na_rm)) x <- x[!is.na(x)]
    # Julia parity: DomainError if log() sees a non-positive value.
    # NaN inputs pass through silently (R's log(NaN) = NaN, like Julia).
    if (length(x) > 0L && any(x + eps < 0, na.rm = TRUE)) {
        stop(sprintf(
            "DomainError with %s:\nlog was called with a negative real argument",
            format(min(x[!is.na(x)]))
        ), call. = FALSE)
    }
    if (eps == 0) {
        exp(mean(log(x)))
    } else {
        exp(mean(log(x + eps))) - eps
    }
}

.op_mode <- function(x, ..., type = NULL) {
    .reject_unknown_param("Mode", "type", type)
    # Julia's Mode supports strings explicitly (operations.jl:1058-1066,
    # `supports_strings(::Mode) = true`). Mirror that, normalizing factor
    # to character at the boundary as Julia does for CategoricalVector
    # (anndata_format.jl:403).
    if (is.factor(x)) x <- as.character(x)
    if (!is.numeric(x) && !is.logical(x) && !is.character(x)) {
        stop("Mode: only numeric, logical, and character input are supported; got ",
            sQuote(typeof(x)), call. = FALSE
        )
    }
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

attr(.op_sum, ".dafr_builtin") <- "Sum"
attr(.op_mean, ".dafr_builtin") <- "Mean"
attr(.op_max, ".dafr_builtin") <- "Max"
attr(.op_min, ".dafr_builtin") <- "Min"
attr(.op_count, ".dafr_builtin") <- "Count"
attr(.op_log, ".dafr_builtin") <- "Log"
attr(.op_abs, ".dafr_builtin") <- "Abs"
attr(.op_exp, ".dafr_builtin") <- "Exp"
attr(.op_sqrt, ".dafr_builtin") <- "Sqrt"
attr(.op_round, ".dafr_builtin") <- "Round"
attr(.op_clamp, ".dafr_builtin") <- "Clamp"
attr(.op_convert, ".dafr_builtin") <- "Convert"
attr(.op_fraction, ".dafr_builtin") <- "Fraction"
attr(.op_significant, ".dafr_builtin") <- "Significant"
attr(.op_var, ".dafr_builtin") <- "Var"
attr(.op_std, ".dafr_builtin") <- "Std"
attr(.op_varn, ".dafr_builtin") <- "VarN"
attr(.op_stdn, ".dafr_builtin") <- "StdN"
attr(.op_median, ".dafr_builtin") <- "Median"
attr(.op_quantile, ".dafr_builtin") <- "Quantile"
attr(.op_geomean, ".dafr_builtin") <- "GeoMean"
attr(.op_mode, ".dafr_builtin") <- "Mode"

.register_default_ops <- function() {
    register_reduction("Sum", .op_sum, overwrite = TRUE)
    register_reduction("Mean", .op_mean, overwrite = TRUE)
    register_reduction("Max", .op_max, overwrite = TRUE)
    register_reduction("Min", .op_min, overwrite = TRUE)
    register_reduction("Count", .op_count, overwrite = TRUE)
    register_reduction("Var", .op_var, overwrite = TRUE)
    register_reduction("Std", .op_std, overwrite = TRUE)
    register_reduction("VarN", .op_varn, overwrite = TRUE)
    register_reduction("StdN", .op_stdn, overwrite = TRUE)
    register_reduction("Median", .op_median, overwrite = TRUE)
    register_reduction("Quantile", .op_quantile, overwrite = TRUE)
    register_reduction("GeoMean", .op_geomean, overwrite = TRUE)
    register_reduction("Mode", .op_mode, overwrite = TRUE)

    register_eltwise("Log", .op_log, overwrite = TRUE)
    register_eltwise("Abs", .op_abs, overwrite = TRUE)
    register_eltwise("Exp", .op_exp, overwrite = TRUE)
    register_eltwise("Sqrt", .op_sqrt, overwrite = TRUE)
    register_eltwise("Round", .op_round, overwrite = TRUE)
    register_eltwise("Clamp", .op_clamp, overwrite = TRUE)
    register_eltwise("Convert", .op_convert, overwrite = TRUE)
    register_eltwise("Fraction", .op_fraction, overwrite = TRUE)
    register_eltwise("Significant", .op_significant, overwrite = TRUE)

    invisible(NULL)
}
