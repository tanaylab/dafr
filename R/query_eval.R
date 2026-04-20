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
  if (identical(state$kind, "axis")) {
    # second axis -> matrix dimension in scope
    state$kind <- "two_axes"
    state$rows_axis <- state$axis
    state$cols_axis <- node$axis_name
    state$value <- NULL
    state$axis  <- NULL
    return(state)
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
  if (identical(state$kind, "vector_names_ready")) {
    return(list(kind = "names", value = format_vectors_set(daf, state$axis)))
  }
  if (identical(state$kind, "matrix_names_ready")) {
    return(list(kind = "names",
                value = format_matrices_set(daf, state$rows_axis, state$cols_axis)))
  }
  if (identical(state$kind, "axis")) {
    return(list(kind = "names", value = format_vectors_set(daf, state$axis)))
  }
  if (identical(state$kind, "two_axes")) {
    return(list(kind = "names",
                value = format_matrices_set(daf, state$rows_axis, state$cols_axis)))
  }
  list(kind = "names", value = format_axes_set(daf))
}

.apply_if_missing <- function(node, state, daf) state  # consumed via lookahead

# Stubs for nodes covered by Q8-Q13: raise descriptive errors so dispatcher
# can route but callers get clear feedback on what's not yet implemented.

.apply_lookup_vector <- function(node, state, daf) {
  if (!identical(state$kind, "axis")) {
    stop(sprintf("':' requires an axis in scope (got %s)", state$kind),
         call. = FALSE)
  }
  axis <- state$axis
  if (is.null(node$name)) {
    # '@ axis : ?' -> set ready state; Names node will complete the listing
    state$kind <- "vector_names_ready"
    return(state)
  }
  if (!format_has_vector(daf, axis, node$name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "vector",
                  value = rep(state$if_missing,
                              format_axis_length(daf, axis)),
                  axis = axis))
    }
    stop(sprintf("no vector %s on axis %s",
                 sQuote(node$name), sQuote(axis)), call. = FALSE)
  }
  list(kind = "vector",
       value = format_get_vector(daf, axis, node$name),
       axis  = axis)
}

.apply_lookup_matrix <- function(node, state, daf) {
  if (!identical(state$kind, "two_axes")) {
    stop(sprintf("'::' requires two axes in scope (got %s)", state$kind),
         call. = FALSE)
  }
  rows <- state$rows_axis; cols <- state$cols_axis
  if (is.null(node$name)) {
    # '@ rows @ cols :: ?' -> set ready state; Names node will complete
    state$kind <- "matrix_names_ready"
    return(state)
  }
  if (!format_has_matrix(daf, rows, cols, node$name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "matrix",
                  value = matrix(state$if_missing,
                                 format_axis_length(daf, rows),
                                 format_axis_length(daf, cols)),
                  rows_axis = rows, cols_axis = cols))
    }
    stop(sprintf("no matrix %s [%s, %s]",
                 sQuote(node$name), sQuote(rows), sQuote(cols)),
         call. = FALSE)
  }
  list(kind = "matrix",
       value = format_get_matrix(daf, rows, cols, node$name),
       rows_axis = rows, cols_axis = cols)
}
.apply_if_not <- function(node, state, daf) {
  stop("not yet implemented: IfNot", call. = FALSE)
}
.apply_as_axis <- function(node, state, daf) {
  stop("not yet implemented: AsAxis", call. = FALSE)
}
.apply_begin_mask <- function(node, state, daf) {
  if (!identical(state$kind, "axis")) {
    stop("'[' mask requires an axis in scope", call. = FALSE)
  }
  vec <- format_get_vector(daf, state$axis, node$property)
  mask <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
  if (identical(node$op, "BeginNegatedMask")) mask <- !mask
  state$pending_mask <- mask
  state$pending_property <- node$property
  state$pending_vec <- vec
  state$kind <- "mask"
  state
}

.apply_end_mask <- function(node, state, daf) {
  axis <- state$axis
  entries <- format_axis_array(daf, axis)
  list(kind = "axis", axis = axis, value = entries[state$pending_mask])
}

.apply_logical_mask <- function(node, state, daf) {
  if (!identical(state$kind, "mask")) {
    stop("logical mask combinator outside of mask", call. = FALSE)
  }
  vec <- format_get_vector(daf, state$axis, node$property)
  m   <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
  negated <- grepl("NegatedMask$", node$op)
  if (negated) m <- !m
  op <- if (startsWith(node$op, "And")) "And" else
        if (startsWith(node$op, "Or"))  "Or"  else "Xor"
  combined <- switch(op,
    And = state$pending_mask & m,
    Or  = state$pending_mask | m,
    Xor = xor(state$pending_mask, m))
  # Save the pre-combinator mask and combinator op so a trailing
  # comparator on this property can refine rather than overwrite.
  state$pending_combinator_mask <- state$pending_mask
  state$pending_combinator_op   <- op
  state$pending_combinator_neg  <- negated
  state$pending_mask <- combined
  state$pending_vec  <- vec   # allow a trailing comparator on this property
  state
}

.apply_comparator <- function(node, state, daf) {
  if (!identical(state$kind, "mask")) {
    stop("comparator outside of mask", call. = FALSE)
  }
  vec <- state$pending_vec
  test <- switch(node$op,
    IsLess         = vec <  .coerce_cmp(node$value, vec),
    IsLessEqual    = vec <= .coerce_cmp(node$value, vec),
    IsEqual        = vec == .coerce_cmp(node$value, vec),
    IsNotEqual     = vec != .coerce_cmp(node$value, vec),
    IsGreater      = vec >  .coerce_cmp(node$value, vec),
    IsGreaterEqual = vec >= .coerce_cmp(node$value, vec),
    IsMatch        = grepl(node$pattern, as.character(vec), perl = TRUE),
    IsNotMatch     = !grepl(node$pattern, as.character(vec), perl = TRUE))
  if (!is.null(state$pending_combinator_mask)) {
    # A trailing comparator after a logical combinator: apply the combinator
    # operation between the pre-combinator mask and this comparison result.
    m <- if (isTRUE(state$pending_combinator_neg)) !test else test
    test <- switch(state$pending_combinator_op,
      And = state$pending_combinator_mask & m,
      Or  = state$pending_combinator_mask | m,
      Xor = xor(state$pending_combinator_mask, m))
    state$pending_combinator_mask <- NULL
    state$pending_combinator_op   <- NULL
    state$pending_combinator_neg  <- NULL
  }
  state$pending_mask <- test
  state
}

.coerce_cmp <- function(value_string, ref_vec) {
  if (is.numeric(ref_vec)) as.numeric(value_string)
  else if (is.logical(ref_vec)) as.logical(value_string)
  else as.character(value_string)
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
