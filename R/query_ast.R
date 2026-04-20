#' @include query_tokens.R utils.R
NULL

# AST node constructors + canonical-string serialiser.
# Reference: DataAxesFormats.jl queries.jl export list lines 5-53.

.qop <- function(op, ...) {
  structure(list(op = op, ...),
            class = c(paste0("qop_", op), "qop"))
}

.qop_names         <- function() .qop("Names")
.qop_axis          <- function(axis_name) .qop("Axis", axis_name = axis_name)
.qop_as_axis       <- function(axis_name) .qop("AsAxis", axis_name = axis_name)
.qop_if_missing    <- function(default)   .qop("IfMissing", default = default)
.qop_if_not        <- function(value = NULL) .qop("IfNot", value = value)
.qop_lookup_scalar <- function(name = NULL) .qop("LookupScalar", name = name)
.qop_lookup_vector <- function(name = NULL) .qop("LookupVector", name = name)
.qop_lookup_matrix <- function(name = NULL) .qop("LookupMatrix", name = name)

.canonicalise_ast <- function(ast) {
  parts <- vapply(ast, .canonicalise_node, character(1))
  paste(parts, collapse = " ")
}

.canonicalise_node <- function(n) {
  switch(n$op,
    Names        = "?",
    Axis         = paste0("@ ", .escape_value(n$axis_name)),
    AsAxis       = paste0("=@ ", .escape_value(n$axis_name)),
    IfMissing    = paste0("|| ", .escape_value(format(n$default))),
    IfNot        = if (is.null(n$value)) "??" else paste0("?? ", .escape_value(format(n$value))),
    LookupScalar = if (is.null(n$name)) "." else paste0(". ", .escape_value(n$name)),
    LookupVector = if (is.null(n$name)) ":" else paste0(": ", .escape_value(n$name)),
    LookupMatrix = if (is.null(n$name)) "::" else paste0(":: ", .escape_value(n$name)),
    BeginMask         = paste0("[ ",     .escape_value(n$property)),
    BeginNegatedMask  = paste0("[ ! ",   .escape_value(n$property)),
    EndMask           = "]",
    IsLess            = paste0("< ",     .escape_value(format(n$value))),
    IsLessEqual       = paste0("<= ",    .escape_value(format(n$value))),
    IsEqual           = paste0("= ",     .escape_value(format(n$value))),
    IsNotEqual        = paste0("!= ",    .escape_value(format(n$value))),
    IsGreater         = paste0("> ",     .escape_value(format(n$value))),
    IsGreaterEqual    = paste0(">= ",    .escape_value(format(n$value))),
    IsMatch           = paste0("~ ",     .escape_value(n$pattern)),
    IsNotMatch        = paste0("!~ ",    .escape_value(n$pattern)),
    AndMask           = paste0("& ",     .escape_value(n$property)),
    AndNegatedMask    = paste0("& ! ",   .escape_value(n$property)),
    OrMask            = paste0("| ",     .escape_value(n$property)),
    OrNegatedMask     = paste0("| ! ",   .escape_value(n$property)),
    XorMask           = paste0("^ ",     .escape_value(n$property)),
    XorNegatedMask    = paste0("^ ! ",   .escape_value(n$property)),
    SquareRowIs       = paste0("@- ",    .escape_value(format(n$value))),
    SquareColumnIs    = paste0("@| ",    .escape_value(format(n$value))),
    GroupBy           = paste0("/ ",     .escape_value(n$property)),
    GroupRowsBy       = paste0("-/ ",    .escape_value(n$property)),
    GroupColumnsBy    = paste0("|/ ",    .escape_value(n$property)),
    CountBy           = paste0("* ",     .escape_value(n$property)),
    ReduceToColumn    = .canonicalise_reduction(">|", n$reduction, n$params),
    ReduceToRow       = .canonicalise_reduction(">-", n$reduction, n$params),
    Eltwise           = .canonicalise_eltwise(n$name, n$params),
    stop(sprintf("no canonicaliser for %s", n$op), call. = FALSE))
}

.escape_value <- function(s) {
  if (grepl("[\\s!&*%./:<=>?@\\[\\]^\\|~\"]", s, perl = TRUE)) {
    paste0("\"", gsub("[\\\\\"]", "\\\\\\0", s, perl = TRUE), "\"")
  } else {
    s
  }
}

.qop_begin_mask <- function(property, negated = FALSE) {
  if (negated) .qop("BeginNegatedMask", property = property)
  else         .qop("BeginMask",        property = property)
}
.qop_end_mask <- function() .qop("EndMask")

.qop_is_less          <- function(value) .qop("IsLess",          value = value)
.qop_is_less_equal    <- function(value) .qop("IsLessEqual",     value = value)
.qop_is_equal         <- function(value) .qop("IsEqual",         value = value)
.qop_is_not_equal     <- function(value) .qop("IsNotEqual",      value = value)
.qop_is_greater       <- function(value) .qop("IsGreater",       value = value)
.qop_is_greater_equal <- function(value) .qop("IsGreaterEqual",  value = value)
.qop_is_match         <- function(pattern) .qop("IsMatch",    pattern = pattern)
.qop_is_not_match     <- function(pattern) .qop("IsNotMatch", pattern = pattern)

.qop_and_mask <- function(property, negated = FALSE) {
  if (negated) .qop("AndNegatedMask", property = property)
  else         .qop("AndMask",        property = property)
}
.qop_or_mask <- function(property, negated = FALSE) {
  if (negated) .qop("OrNegatedMask", property = property)
  else         .qop("OrMask",        property = property)
}
.qop_xor_mask <- function(property, negated = FALSE) {
  if (negated) .qop("XorNegatedMask", property = property)
  else         .qop("XorMask",        property = property)
}

.qop_square_row_is    <- function(value) .qop("SquareRowIs",    value = value)
.qop_square_column_is <- function(value) .qop("SquareColumnIs", value = value)

.qop_group_by         <- function(property) .qop("GroupBy",         property = property)
.qop_group_rows_by    <- function(property) .qop("GroupRowsBy",    property = property)
.qop_group_columns_by <- function(property) .qop("GroupColumnsBy", property = property)
.qop_count_by         <- function(property) .qop("CountBy",         property = property)

.qop_reduce_to_column <- function(reduction, params = list()) {
  .qop("ReduceToColumn", reduction = reduction, params = params)
}
.qop_reduce_to_row <- function(reduction, params = list()) {
  .qop("ReduceToRow",    reduction = reduction, params = params)
}

.qop_eltwise <- function(name, params = list()) {
  .qop("Eltwise", name = name, params = params)
}

.canonicalise_reduction <- function(tok, reduction, params) {
  head <- paste0(tok, " ", .escape_value(reduction))
  if (length(params) == 0L) return(head)
  tail <- paste(vapply(names(params), function(k)
                        paste0(.escape_value(k), ": ",
                               .escape_value(format(params[[k]]))),
                      character(1)), collapse = " ")
  paste0(head, " ", tail)
}

.canonicalise_eltwise <- function(name, params) {
  head <- paste0("% ", .escape_value(name))
  if (length(params) == 0L) return(head)
  tail <- paste(vapply(names(params), function(k)
                        paste0(.escape_value(k), ": ",
                               .escape_value(format(params[[k]]))),
                      character(1)), collapse = " ")
  paste0(head, "(", tail, ")")
}
