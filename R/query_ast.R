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
    stop(sprintf("no canonicaliser for %s", n$op), call. = FALSE))
}

.escape_value <- function(s) {
  if (grepl("[\\s!&*%./:<=>?@\\[\\]^\\|~\"]", s, perl = TRUE)) {
    paste0("\"", gsub("[\\\\\"]", "\\\\\\0", s, perl = TRUE), "\"")
  } else {
    s
  }
}
