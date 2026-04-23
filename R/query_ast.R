#' @include query_tokens.R utils.R
NULL

# AST node constructors + canonical-string serialiser.
# Reference: DataAxesFormats.jl queries.jl export list lines 5-53.

# Canonicalise a scalar value for use in a query string.
#
# Forces `scipen = 0` (R's built-in default) so the output is deterministic
# regardless of the user's global `options(scipen = ...)` setting. At scipen=0
# R emits the shorter of fixed/scientific notations, e.g. `1e-06`, `1e+06`,
# `0.5`, `12345`. This matches Julia's default `Float64` print behaviour on
# the small/large values that show up in query canonical strings
# (e.g. `< 1e-6`), while keeping human-readable values (e.g. `< 0.5`, `= 42`)
# in their natural form. Without this override a user with `scipen = 3`
# would produce `"0.000001"` instead of `"1e-06"` and canonical strings would
# drift from Julia's.
.fmt_value <- function(v) {
    if (is.character(v)) return(v)
    old <- options(scipen = 0L)
    on.exit(options(old), add = TRUE)
    format(v)
}

.qop <- function(op, ...) {
    structure(list(op = op, ...),
        class = c(paste0("qop_", op), "qop")
    )
}

.qop_names <- function() .qop("Names")
.qop_axis <- function(axis_name) .qop("Axis", axis_name = axis_name)
.qop_as_axis <- function(axis_name) .qop("AsAxis", axis_name = axis_name)
.qop_if_missing <- function(default) {
    # Coerce to character so the stored AST matches what parse_query() produces
    # (the parser always emits character-typed literals). Without this, numeric
    # defaults such as IfMissing(42) produce AST $default = 42 (numeric) while
    # parse_query("|| 42") produces "42" (character); the canonical strings
    # are equal but the ASTs are not. Mirrors .qop_eltwise_typed.
    default <- .fmt_value(default)
    .qop("IfMissing", default = default)
}
.qop_if_not <- function(value = NULL) .qop("IfNot", value = value)
.qop_lookup_scalar <- function(name = NULL) .qop("LookupScalar", name = name)
.qop_lookup_vector <- function(name = NULL) .qop("LookupVector", name = name)
.qop_lookup_matrix <- function(name = NULL) .qop("LookupMatrix", name = name)

.canonicalise_ast <- function(ast) {
    parts <- vapply(ast, .canonicalise_node, character(1))
    paste(parts, collapse = " ")
}

.canonicalise_node <- function(n) {
    switch(n$op,
        Names = "?",
        Axis = paste0("@ ", .escape_value(n$axis_name)),
        AsAxis = if (is.null(n$axis_name)) "=@" else paste0("=@ ", .escape_value(n$axis_name)),
        IfMissing = paste0("|| ", .escape_value(.fmt_value(n$default))),
        IfNot = if (is.null(n$value)) "??" else paste0("?? ", .escape_value(.fmt_value(n$value))),
        LookupScalar = if (is.null(n$name)) "." else paste0(". ", .escape_value(n$name)),
        LookupVector = if (is.null(n$name)) ":" else paste0(": ", .escape_value(n$name)),
        LookupMatrix = if (is.null(n$name)) "::" else paste0(":: ", .escape_value(n$name)),
        BeginMask = paste0("[ ", .escape_value(n$property)),
        BeginNegatedMask = paste0("[ ! ", .escape_value(n$property)),
        EndMask = "]",
        IsLess = paste0("< ", .escape_value(.fmt_value(n$value))),
        IsLessEqual = paste0("<= ", .escape_value(.fmt_value(n$value))),
        IsEqual = paste0("= ", .escape_value(.fmt_value(n$value))),
        IsNotEqual = paste0("!= ", .escape_value(.fmt_value(n$value))),
        IsGreater = paste0("> ", .escape_value(.fmt_value(n$value))),
        IsGreaterEqual = paste0(">= ", .escape_value(.fmt_value(n$value))),
        IsMatch = paste0("~ ", n$pattern),
        IsNotMatch = paste0("!~ ", n$pattern),
        AndMask = paste0("& ", .escape_value(n$property)),
        AndNegatedMask = paste0("& ! ", .escape_value(n$property)),
        OrMask = paste0("| ", .escape_value(n$property)),
        OrNegatedMask = paste0("| ! ", .escape_value(n$property)),
        XorMask = paste0("^ ", .escape_value(n$property)),
        XorNegatedMask = paste0("^ ! ", .escape_value(n$property)),
        SquareRowIs = paste0("@- ", .escape_value(.fmt_value(n$value))),
        SquareColumnIs = paste0("@| ", .escape_value(.fmt_value(n$value))),
        GroupBy = paste0("/ ", .escape_value(n$property)),
        GroupRowsBy = paste0("-/ ", .escape_value(n$property)),
        GroupColumnsBy = paste0("|/ ", .escape_value(n$property)),
        CountBy = paste0("* ", .escape_value(n$property)),
        ReduceToColumn = .canonicalise_reduction(">|", n$reduction, n$params),
        ReduceToRow = .canonicalise_reduction(">-", n$reduction, n$params),
        Eltwise = .canonicalise_eltwise(n$name, n$params),
        stop(sprintf("no canonicaliser for %s", n$op), call. = FALSE)
    )
}

#' Escape a value for use as a query literal.
#'
#' If `s` contains any of the query metacharacters (whitespace, `!`,
#' `&`, `*`, `%`, `.`, `/`, `:`, `<`, `=`, `>`, `?`, `@`, `[`, `]`,
#' `^`, `|`, `~`, `"`), the result is double-quoted and any backslash
#' or double-quote inside is backslash-escaped. Otherwise `s` is
#' returned unchanged.
#'
#' @param s Character scalar.
#' @return Character scalar suitable for concatenation into a query
#'   string.
#' @examples
#' escape_value("plain")
#' escape_value("has space")
#' unescape_value(escape_value("has \"quotes\""))
#' @seealso [unescape_value()], [canonical_query()]
#' @export
escape_value <- function(s) {
    if (grepl("[\\s\\\\!&*%./:<=>?@\\[\\]^\\|~\"]", s, perl = TRUE)) {
        paste0("\"", gsub("([\\\\\"])", "\\\\\\1", s, perl = TRUE), "\"")
    } else {
        s
    }
}

# Kept as a private alias because many internals and tests call
# `.escape_value` directly; do not remove without a separate sweep.
.escape_value <- escape_value

#' Inverse of [escape_value()].
#'
#' Strips an outer pair of double quotes (if present) and unescapes
#' `\\` and `\"` sequences. Leaves already-bare strings unchanged.
#'
#' @param s Character scalar (an escaped query literal).
#' @return Character scalar (the original value).
#' @examples
#' unescape_value("\"a b\"")
#' unescape_value("plain")
#' stopifnot(identical(unescape_value(escape_value("a b")), "a b"))
#' @seealso [escape_value()]
#' @export
unescape_value <- function(s) {
    stopifnot(is.character(s), length(s) == 1L)
    if (!startsWith(s, "\"") || !endsWith(s, "\"") || nchar(s) < 2L) {
        return(s)
    }
    inner <- substr(s, 2L, nchar(s) - 1L)
    # Replace \" and \\ sequences left-to-right in a single pass so
    # that "\\\"" decodes to "\"" rather than the escape of a literal
    # quote following a backslash. Use a regex with capture group.
    gsub("\\\\([\\\\\"])", "\\1", inner, perl = TRUE)
}

.qop_begin_mask <- function(property, negated = FALSE) {
    if (negated) {
        .qop("BeginNegatedMask", property = property)
    } else {
        .qop("BeginMask", property = property)
    }
}
.qop_end_mask <- function() .qop("EndMask")

.qop_is_less <- function(value) .qop("IsLess", value = value)
.qop_is_less_equal <- function(value) .qop("IsLessEqual", value = value)
.qop_is_equal <- function(value) .qop("IsEqual", value = value)
.qop_is_not_equal <- function(value) .qop("IsNotEqual", value = value)
.qop_is_greater <- function(value) .qop("IsGreater", value = value)
.qop_is_greater_equal <- function(value) .qop("IsGreaterEqual", value = value)
.qop_is_match <- function(pattern) .qop("IsMatch", pattern = pattern)
.qop_is_not_match <- function(pattern) .qop("IsNotMatch", pattern = pattern)

.qop_and_mask <- function(property, negated = FALSE) {
    if (negated) {
        .qop("AndNegatedMask", property = property)
    } else {
        .qop("AndMask", property = property)
    }
}
.qop_or_mask <- function(property, negated = FALSE) {
    if (negated) {
        .qop("OrNegatedMask", property = property)
    } else {
        .qop("OrMask", property = property)
    }
}
.qop_xor_mask <- function(property, negated = FALSE) {
    if (negated) {
        .qop("XorNegatedMask", property = property)
    } else {
        .qop("XorMask", property = property)
    }
}

.qop_square_row_is <- function(value) {
    # Coerce to character so the stored AST matches parse_query output.
    # See .qop_if_missing for the full rationale.
    value <- .fmt_value(value)
    .qop("SquareRowIs", value = value)
}
.qop_square_column_is <- function(value) {
    value <- .fmt_value(value)
    .qop("SquareColumnIs", value = value)
}

.qop_group_by <- function(property) .qop("GroupBy", property = property)
.qop_group_rows_by <- function(property) .qop("GroupRowsBy", property = property)
.qop_group_columns_by <- function(property) .qop("GroupColumnsBy", property = property)
.qop_count_by <- function(property) .qop("CountBy", property = property)

.qop_reduce_to_column <- function(reduction, params = list()) {
    .qop("ReduceToColumn", reduction = reduction, params = params)
}
.qop_reduce_to_row <- function(reduction, params = list()) {
    .qop("ReduceToRow", reduction = reduction, params = params)
}

.qop_eltwise <- function(name, params = list()) {
    .qop("Eltwise", name = name, params = params)
}

# Helper for .make_typed_reduction-backed eltwise builders. Factory
# passes `(type, params)` where `type` is either NULL or a character
# scalar (e.g. the `Int64` in `% Convert type Int64`). Fold `type`
# into the params dict as a first key when non-NULL. All param values
# are coerced to character scalars so AST round-trips through the
# canonical string (which the parser produces as strings).
.qop_eltwise_typed <- function(name, type, params) {
    if (!is.null(type)) {
        params <- c(list(type = type), as.list(params))
    }
    if (length(params)) {
        params <- lapply(params, .fmt_value)
    }
    .qop_eltwise(name, params = params)
}

# Thin wrappers for each element-wise op. Keep the `.qop_<name>`
# pattern used by the rest of the builder surface.
.qop_abs <- function() .qop_eltwise("Abs")
.qop_round <- function() .qop_eltwise("Round")
.qop_significant <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Significant", type, params)
}
.qop_clamp <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Clamp", type, params)
}
.qop_convert <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Convert", type, params)
}
.qop_fraction <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Fraction", type, params)
}
.qop_log <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Log", type, params)
}

# Reduction op builders. Although these canonicalise as `% <Name>` (i.e. the
# parser produces `qop_Eltwise` nodes), they are semantically reductions —
# consuming a matrix/vector and producing a vector/scalar. The surface
# distinction is only in the query-evaluator.
.qop_max <- function() .qop_eltwise("Max")
.qop_min <- function() .qop_eltwise("Min")
.qop_mode <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Mode", type, params)
}
.qop_sum <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Sum", type, params)
}
.qop_mean <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Mean", type, params)
}
.qop_median <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Median", type, params)
}
.qop_count <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Count", type, params)
}
.qop_geomean <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("GeoMean", type, params)
}
.qop_quantile <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Quantile", type, params)
}
.qop_std <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Std", type, params)
}
.qop_std_n <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("StdN", type, params)
}
.qop_var <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("Var", type, params)
}
.qop_var_n <- function(type = NULL, params = list()) {
    .qop_eltwise_typed("VarN", type, params)
}

.canonicalise_reduction <- function(tok, reduction, params) {
    head <- paste0(tok, " ", .escape_value(reduction))
    if (length(params) == 0L) {
        return(head)
    }
    tail <- paste(vapply(
        names(params), function(k) {
            paste0(
                .escape_value(k), ": ",
                .escape_value(.fmt_value(params[[k]]))
            )
        },
        character(1)
    ), collapse = " ")
    paste0(head, " ", tail)
}

.canonicalise_eltwise <- function(name, params) {
    head <- paste0("% ", .escape_value(name))
    if (length(params) == 0L) {
        return(head)
    }
    tail <- paste(vapply(
        names(params), function(k) {
            paste0(
                .escape_value(k), ": ",
                .escape_value(.fmt_value(params[[k]]))
            )
        },
        character(1)
    ), collapse = " ")
    paste0(head, " ", tail)
}
