#' @include query_ast.R operations.R format_api.R classes.R
NULL

# Evaluator: QueryState stack machine over DafReader.
# Reference: DataAxesFormats.jl queries.jl:1501 (QueryState), 1765
# (get_query_final_state).

#' Evaluate a parsed AST against a DafReader.
#' @keywords internal
#' @noRd
.eval_query <- function(daf, ast) {
  state <- list(kind = "init", value = NULL, if_missing = NULL)
  i <- 1L
  n <- length(ast)
  while (i <= n) {
    node <- ast[[i]]
    # Lookahead: if next node is IfMissing, hoist its default forward
    if (i < n && identical(ast[[i + 1L]]$op, "IfMissing")) {
      state$if_missing <- ast[[i + 1L]]$default
      state <- .apply_node(node, state, daf)
      state$if_missing <- NULL  # consume; don't carry forward past this lookup
      i <- i + 2L
      next
    }
    state <- .apply_node(node, state, daf)
    i <- i + 1L
  }
  state$value
}

.apply_node <- function(node, state, daf) {
  dispatch <- switch(node$op,
    Names        = .apply_names,
    Axis         = .apply_axis,
    LookupScalar = .apply_lookup_scalar,
    LookupVector = .apply_lookup_vector,
    LookupMatrix = .apply_lookup_matrix,
    IfMissing    = .apply_if_missing,
    IfNot        = .apply_if_not,
    AsAxis       = .apply_as_axis,
    BeginMask    = .apply_begin_mask,
    BeginNegatedMask = .apply_begin_mask,
    EndMask      = .apply_end_mask,
    AndMask = , AndNegatedMask = , OrMask = , OrNegatedMask = ,
    XorMask = , XorNegatedMask = .apply_logical_mask,
    IsLess = , IsLessEqual = , IsEqual = , IsNotEqual = ,
    IsGreater = , IsGreaterEqual = , IsMatch = , IsNotMatch = .apply_comparator,
    SquareRowIs = , SquareColumnIs = .apply_square_slice,
    ReduceToColumn = , ReduceToRow = .apply_reduction,
    Eltwise = .apply_eltwise,
    GroupBy = , GroupRowsBy = , GroupColumnsBy = .apply_groupby,
    CountBy = .apply_countby,
    stop(sprintf("eval: no handler for %s", node$op), call. = FALSE))
  dispatch(node, state, daf)
}

# --- lookups (this task) -----------------------------------------------

.apply_axis <- function(node, state, daf) {
  if (!format_has_axis(daf, node$axis_name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "vector", value = state$if_missing,
                  axis = node$axis_name))
    }
    stop(sprintf("no axis %s in daf %s",
                 sQuote(node$axis_name),
                 sQuote(S7::prop(daf, "name"))), call. = FALSE)
  }
  state$value <- format_axis_array(daf, node$axis_name)
  state$axis  <- node$axis_name
  state$kind  <- "axis"
  state
}

.apply_lookup_scalar <- function(node, state, daf) {
  if (is.null(node$name)) {
    # bare '.' -> ready to list scalar names (used with '?' follow-up)
    state$kind <- "scalar_names_ready"
    return(state)
  }
  if (!format_has_scalar(daf, node$name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "scalar", value = state$if_missing))
    }
    stop(sprintf("no scalar %s in daf %s",
                 sQuote(node$name),
                 sQuote(S7::prop(daf, "name"))), call. = FALSE)
  }
  state$value <- format_get_scalar(daf, node$name)
  state$kind  <- "scalar"
  state
}

.apply_names <- function(node, state, daf) {
  if (identical(state$kind, "scalar_names_ready")) {
    return(list(kind = "names", value = format_scalars_set(daf)))
  }
  if (identical(state$kind, "axis")) {
    # '@ axis : ?' case handled in Q8; for '@ ?' we return axes_set
    return(list(kind = "names", value = format_axes_set(daf)))
  }
  # bare `@ ?` -> axes_set (state$kind is "init" at this point because
  # the parser emits Names as its own node when encountered)
  list(kind = "names", value = format_axes_set(daf))
}

.apply_if_missing <- function(node, state, daf) state  # consumed via lookahead

# Stubs for nodes covered by Q8-Q13: raise descriptive errors so dispatcher
# can route but callers get clear feedback on what's not yet implemented.

.apply_lookup_vector <- function(node, state, daf) {
  stop("not yet implemented: LookupVector", call. = FALSE)
}
.apply_lookup_matrix <- function(node, state, daf) {
  stop("not yet implemented: LookupMatrix", call. = FALSE)
}
.apply_if_not <- function(node, state, daf) {
  stop("not yet implemented: IfNot", call. = FALSE)
}
.apply_as_axis <- function(node, state, daf) {
  stop("not yet implemented: AsAxis", call. = FALSE)
}
.apply_begin_mask <- function(node, state, daf) {
  stop("not yet implemented: mask", call. = FALSE)
}
.apply_end_mask <- function(node, state, daf) {
  stop("not yet implemented: mask", call. = FALSE)
}
.apply_logical_mask <- function(node, state, daf) {
  stop("not yet implemented: logical mask", call. = FALSE)
}
.apply_comparator <- function(node, state, daf) {
  stop("not yet implemented: comparator", call. = FALSE)
}
.apply_square_slice <- function(node, state, daf) {
  stop("not yet implemented: square slice", call. = FALSE)
}
.apply_reduction <- function(node, state, daf) {
  stop("not yet implemented: reduction", call. = FALSE)
}
.apply_eltwise <- function(node, state, daf) {
  stop("not yet implemented: eltwise", call. = FALSE)
}
.apply_groupby <- function(node, state, daf) {
  stop("not yet implemented: groupby", call. = FALSE)
}
.apply_countby <- function(node, state, daf) {
  stop("not yet implemented: countby", call. = FALSE)
}
