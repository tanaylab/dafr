#' @include query_ast.R operations.R format_api.R classes.R
NULL

# Evaluator: QueryState stack machine over DafReader.
# Reference: DataAxesFormats.jl queries.jl:1501 (QueryState), 1765
# (get_query_final_state).

#' Evaluate a parsed AST against a DafReader.
#' @keywords internal
#' @noRd
.eval_query <- function(daf, ast) {
    state <- list(kind = "init", value = NULL, if_missing = NULL,
                  if_missing_type = NULL)
    i <- 1L
    n <- length(ast)
    while (i <= n) {
        node <- ast[[i]]
        # Lookahead: if next node is IfMissing, hoist its default forward
        if (i < n && identical(ast[[i + 1L]]$op, "IfMissing")) {
            state$if_missing <- ast[[i + 1L]]$default
            state$if_missing_type <- ast[[i + 1L]]$type
            state <- .apply_node(node, state, daf)
            state$if_missing <- NULL # consume; don't carry forward past this lookup
            state$if_missing_type <- NULL
            i <- i + 2L
            next
        }
        # Lookahead 1: fused Log + Sum/Mean reduction.
        if (isTRUE(dafr_opt("dafr.perf.fast_paths")) &&
            i < n &&
            identical(node$op, "Eltwise") &&
            identical(node$name, "Log") &&
            ast[[i + 1L]]$op %in% c("ReduceToColumn", "ReduceToRow") &&
            ast[[i + 1L]]$reduction %in% c("Sum", "Mean") &&
            length(ast[[i + 1L]]$params) == 0L &&
            identical(state$kind, "matrix")) {
            fused <- .try_fused_log_reduce(node, ast[[i + 1L]], state, daf)
            if (!is.null(fused)) {
                state <- fused
                i <- i + 2L
                next
            }
        }
        state <- .apply_node(node, state, daf)
        i <- i + 1L
    }
    if (identical(state$kind, "pending_count")) {
        state <- .finalize_pending_count(state, daf)
    }
    state$value
}

.try_fused_log_reduce <- function(log_node, red_node, state, daf) {
    fn <- get_eltwise(log_node$name)
    if (!identical(attr(fn, ".dafr_builtin"), "Log")) return(NULL)
    params <- .coerce_params(log_node$params)
    eps <- params$eps %||% 0
    base <- params$base %||% exp(1)
    threshold <- as.integer(dafr_opt("dafr.omp_threshold"))
    axis <- if (identical(red_node$op, "ReduceToColumn")) 0L else 1L
    reducer <- red_node$reduction
    m <- state$value

    if (methods::is(m, "dgCMatrix")) {
        # CSC: implicit zeros are NOT in @x. The kernel accounts for them
        # by adding (n_zeros * log(eps)/log(base)) per row/column. With eps
        # <= 0 that contribution is -Inf or NaN and silently poisons every
        # row/col that has any implicit zero (essentially all of them for
        # typical UMI matrices). Bail so the unfused path runs and the user
        # sees the -Inf/NaN they asked for.
        if (eps <= 0) return(NULL)
        out <- kernel_log_reduce_csc_cpp(
            m@x, m@i, m@p,
            nrow(m), ncol(m),
            eps, base, axis, reducer, threshold
        )
        target_axis <- if (axis == 0L) state$rows_axis else state$cols_axis
        names(out) <- format_axis_array(daf, target_axis)
        return(list(kind = "vector", axis = target_axis, value = out))
    }
    if (is.matrix(m)) {
        # Dense: every cell is materialised, so eps == 0 on actual zero entries
        # produces -Inf in those cells and the per-row sum becomes -Inf. The
        # user can see this. Only bail if eps < 0 (always nonsense) OR if
        # eps == 0 and there's a zero in the matrix.
        if (eps < 0) return(NULL)
        if (eps == 0 && any(m == 0, na.rm = TRUE)) return(NULL)
        if (!is.double(m)) storage.mode(m) <- "double"
        out <- kernel_log_reduce_dense_cpp(
            m, eps, base, axis, reducer, threshold
        )
        target_axis <- if (axis == 0L) state$rows_axis else state$cols_axis
        names(out) <- format_axis_array(daf, target_axis)
        return(list(kind = "vector", axis = target_axis, value = out))
    }
    NULL
}

.apply_node <- function(node, state, daf) {
    dispatch <- switch(node$op,
        Names = .apply_names,
        Axis = .apply_axis,
        LookupScalar = .apply_lookup_scalar,
        LookupVector = .apply_lookup_vector,
        LookupMatrix = .apply_lookup_matrix,
        IfMissing = .apply_if_missing,
        IfNot = .apply_if_not,
        AsAxis = .apply_as_axis,
        BeginMask = .apply_begin_mask,
        BeginNegatedMask = .apply_begin_mask,
        EndMask = .apply_end_mask,
        AndMask = ,
        AndNegatedMask = ,
        OrMask = ,
        OrNegatedMask = ,
        XorMask = ,
        XorNegatedMask = .apply_logical_mask,
        IsLess = ,
        IsLessEqual = ,
        IsEqual = ,
        IsNotEqual = ,
        IsGreater = ,
        IsGreaterEqual = ,
        IsMatch = ,
        IsNotMatch = .apply_comparator,
        SquareRowIs = ,
        SquareColumnIs = .apply_square_slice,
        ReduceToColumn = ,
        ReduceToRow = ,
        ReduceToScalar = .apply_reduction,
        Eltwise = .apply_eltwise,
        GroupBy = ,
        GroupRowsBy = ,
        GroupColumnsBy = .apply_groupby,
        CountBy = .apply_countby,
        stop(sprintf("eval: no handler for %s", node$op), call. = FALSE)
    )
    dispatch(node, state, daf)
}

# --- lookups (this task) -----------------------------------------------

.apply_axis <- function(node, state, daf) {
    if (!format_has_axis(daf, node$axis_name)) {
        if (!is.null(state$if_missing)) {
            return(list(
                kind = "vector", value = state$if_missing,
                axis = node$axis_name
            ))
        }
        stop(sprintf(
            "no axis %s in daf %s",
            sQuote(node$axis_name),
            sQuote(S7::prop(daf, "name"))
        ), call. = FALSE)
    }
    # Entry-pick transitions for the Julia SCALAR_QUERY phrase
    # `: vec @ axis = entry` / `:: m @ rows-axis = R @ cols-axis = C`.
    if (identical(state$kind, "vector")) {
        if (!identical(node$axis_name, state$axis)) {
            stop(sprintf(
                "entry-pick axis %s does not match the vector's axis %s",
                sQuote(node$axis_name), sQuote(state$axis)
            ), call. = FALSE)
        }
        if (!is.null(state$indices)) {
            stop("entry-pick is not supported on a masked vector",
                call. = FALSE)
        }
        state$kind <- "entry_pick_vector"
        state$pick_axis <- node$axis_name
        return(state)
    }
    if (identical(state$kind, "matrix")) {
        if (identical(node$axis_name, state$rows_axis)) {
            pick_dim <- "row"
        } else if (identical(node$axis_name, state$cols_axis)) {
            pick_dim <- "col"
        } else {
            stop(sprintf(
                "entry-pick axis %s does not match the matrix axes (%s, %s)",
                sQuote(node$axis_name),
                sQuote(state$rows_axis), sQuote(state$cols_axis)
            ), call. = FALSE)
        }
        state$kind <- "entry_pick_matrix"
        state$pick_axis <- node$axis_name
        state$pick_dim <- pick_dim
        return(state)
    }
    if (identical(state$kind, "axis_pending_matrix")) {
        # `@ axis :: matrix-prop @ other-axis = entry`: the second axis names
        # the column dimension of the matrix; the slice is finalised by the
        # IsEqual that should follow.
        state$cols_axis <- node$axis_name
        state$kind <- "axis_pending_matrix_pick"
        return(state)
    }
    if (identical(state$kind, "vector_entry_pending_axis")) {
        # `: vec @ axis = entry` — record the axis; IsEqual completes it.
        state$axis <- node$axis_name
        state$kind <- "vector_entry_pending_pick"
        return(state)
    }
    if (identical(state$kind, "matrix_entry_pending_first_axis")) {
        state$rows_axis <- node$axis_name
        state$kind <- "matrix_entry_pending_first_pick"
        return(state)
    }
    if (identical(state$kind, "matrix_entry_pending_second_axis")) {
        state$cols_axis <- node$axis_name
        state$kind <- "matrix_entry_pending_second_pick"
        return(state)
    }
    if (identical(state$kind, "mask_matrix_pending_axis")) {
        # `[ matrix-prop @ cols-axis = entry ...`: capture cols_axis; the
        # comparator (IsEqual) finalises the column slice and switches to
        # the regular `mask` state.
        state$matrix_cols_axis <- node$axis_name
        state$kind <- "mask_matrix_pending_pick"
        return(state)
    }
    if (identical(state$kind, "mask_matrix_pending_combinator")) {
        state$matrix_cols_axis <- node$axis_name
        state$kind <- "mask_matrix_pending_combinator_pick"
        return(state)
    }
    if (identical(state$kind, "groupby_vector_matrix_pending_axis")) {
        state$matrix_cols_axis <- node$axis_name
        state$kind <- "groupby_vector_matrix_pending_pick"
        return(state)
    }
    if (identical(state$kind, "groupby_matrix_rows_pending_axis")) {
        state$matrix_cols_axis <- node$axis_name
        state$kind <- "groupby_matrix_rows_pending_pick"
        return(state)
    }
    if (identical(state$kind, "groupby_matrix_cols_pending_axis")) {
        state$matrix_cols_axis <- node$axis_name
        state$kind <- "groupby_matrix_cols_pending_pick"
        return(state)
    }
    if (identical(state$kind, "countby_matrix_pending_axis")) {
        state$matrix_cols_axis <- node$axis_name
        state$kind <- "countby_matrix_pending_pick"
        return(state)
    }
    if (identical(state$kind, "matrix_chain_pending_axis")) {
        state$matrix2_cols_axis <- node$axis_name
        state$kind <- "matrix_chain_pending_pick"
        return(state)
    }
    if (identical(state$kind, "axis")) {
        # second axis -> matrix dimension in scope.
        # If the first axis was mask-filtered, carry its surviving-entry
        # indices forward as row_indices so a subsequent LookupMatrix
        # returns only the filtered rows.
        state$kind <- "two_axes"
        state$rows_axis <- state$axis
        state$cols_axis <- node$axis_name
        state$row_indices <- state$indices
        state$indices <- NULL
        state$value <- NULL
        state$axis <- NULL
        return(state)
    }
    state$value <- format_axis_array(daf, node$axis_name)
    state$axis <- node$axis_name
    state$kind <- "axis"
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
            default <- .coerce_if_missing_default(
                state$if_missing, state$if_missing_type
            )
            return(list(kind = "scalar", value = default))
        }
        stop(sprintf(
            "no scalar %s in daf %s",
            sQuote(node$name),
            sQuote(S7::prop(daf, "name"))
        ), call. = FALSE)
    }
    state$value <- format_get_scalar(daf, node$name)
    state$kind <- "scalar"
    state
}

# Coerce an IfMissing default to the requested Julia-style dtype, or leave
# it as the parser-emitted character if no type was given.
.coerce_if_missing_default <- function(value, type) {
    if (is.null(type)) return(value)
    switch(type,
        String  = as.character(value),
        Bool    = {
            v <- as.character(value)
            if (v %in% c("0", "false", "FALSE")) FALSE
            else if (v %in% c("1", "true", "TRUE")) TRUE
            else as.logical(value)
        },
        Int8 = , Int16 = , Int32 = ,
        UInt8 = , UInt16 = , UInt32 = as.integer(value),
        Int64 = , UInt64 = bit64::as.integer64(value),
        Float32 = , Float64 = as.double(value),
        stop(sprintf(
            "IfMissing: unknown type %s (expected one of Bool, Int8/16/32/64, UInt8/16/32/64, Float32/64, String)",
            sQuote(type)
        ), call. = FALSE)
    )
}

.apply_names <- function(node, state, daf) {
    if (identical(state$kind, "scalar_names_ready")) {
        return(list(kind = "names", value = format_scalars_set(daf)))
    }
    if (identical(state$kind, "vector_names_ready")) {
        return(list(kind = "names", value = format_vectors_set(daf, state$axis)))
    }
    if (identical(state$kind, "matrix_names_ready")) {
        return(list(
            kind = "names",
            value = format_matrices_set(daf, state$rows_axis, state$cols_axis)
        ))
    }
    if (identical(state$kind, "axis")) {
        return(list(kind = "names", value = format_vectors_set(daf, state$axis)))
    }
    if (identical(state$kind, "two_axes")) {
        return(list(
            kind = "names",
            value = format_matrices_set(daf, state$rows_axis, state$cols_axis)
        ))
    }
    list(kind = "names", value = format_axes_set(daf))
}

.apply_if_missing <- function(node, state, daf) state # consumed via lookahead

# Stubs for nodes covered by Q8-Q13: raise descriptive errors so dispatcher
# can route but callers get clear feedback on what's not yet implemented.

.apply_lookup_vector <- function(node, state, daf) {
    if (identical(state$kind, "vector_axis")) {
        return(.apply_chained_lookup_vector(node, state, daf))
    }
    if (identical(state$kind, "vector")) {
        # `: prop1 : prop2` - implicit AsAxis. Behaves like `: prop1 =@ : prop2`:
        # the prior property name names the target axis whose entries are the
        # pivot values, and the new property is looked up on that axis.
        # (Julia: lookup_vector_by_vector via ensure_vector_is_axis.)
        state$chain_target_axis <- NULL
        state$kind <- "vector_axis"
        return(.apply_chained_lookup_vector(node, state, daf))
    }
    if (identical(state$kind, "grouped_vector")) {
        return(.apply_chained_lookup_grouped(node, state, daf))
    }
    if (identical(state$kind, "pending_count")) {
        return(.apply_chained_lookup_count(node, state, daf))
    }
    if (identical(state$kind, "mask")) {
        return(.apply_chained_lookup_mask(node, state, daf))
    }
    if (identical(state$kind, "matrix")) {
        return(.apply_chained_lookup_matrix(node, state, daf))
    }
    if (identical(state$kind, "init")) {
        # Top-level `: vec @ axis = entry` — Julia lookup_vector_entry. The
        # axis (and the entry to pick) arrives in the following nodes, so
        # stash the property name on a pending state.
        if (is.null(node$name)) {
            stop("':' requires an axis in scope to list vector names",
                call. = FALSE)
        }
        return(list(
            kind = "vector_entry_pending_axis",
            property = node$name,
            if_missing = state$if_missing,
            if_missing_type = state$if_missing_type
        ))
    }
    if (!identical(state$kind, "axis")) {
        stop(sprintf("':' requires an axis in scope (got %s)", state$kind),
            call. = FALSE
        )
    }
    axis <- state$axis
    if (is.null(node$name)) {
        # '@ axis : ?' -> set ready state; Names node will complete the listing
        state$kind <- "vector_names_ready"
        return(state)
    }
    indices <- state$indices
    if (!format_has_vector(daf, axis, node$name)) {
        if (!is.null(state$if_missing)) {
            out_len <- if (is.null(indices)) {
                format_axis_length(daf, axis)
            } else {
                length(indices)
            }
            return(list(
                kind = "vector",
                value = rep(state$if_missing, out_len),
                axis = axis,
                property = node$name
            ))
        }
        stop(sprintf(
            "no vector %s on axis %s",
            sQuote(node$name), sQuote(axis)
        ), call. = FALSE)
    }
    value <- format_get_vector(daf, axis, node$name)
    if (!is.null(indices)) {
        value <- value[indices]
        # Tag the surviving-axis-entry names so a subsequent chain
        # (`.apply_chained_lookup_vector`) sees the masked subset rather
        # than falling back to the full axis array.
        names(value) <- format_axis_array(daf, axis)[indices]
    }
    list(
        kind = "vector",
        value = value,
        axis = axis,
        indices = indices,
        property = node$name
    )
}

.apply_lookup_matrix <- function(node, state, daf) {
    if (identical(state$kind, "matrix")) {
        # `:: matrix-of-axis-entries :: matrix2 @ axis = entry` — for each
        # cell of the in-scope matrix (whose values are entries on some
        # axis named after the prior matrix property), look up matrix2
        # indexed by (entry, axis-entry-from-IsEqual). The result has the
        # same shape as the in-scope matrix. Julia phrase
        # lookup_matrix_column_by_matrix and the square_matrix_*_by_matrix
        # cousins. The actual slice is finalised once Axis+IsEqual (or the
        # @|/@- square slice) arrives.
        if (is.null(node$name)) {
            stop("'::' on a matrix requires a matrix property name",
                call. = FALSE)
        }
        target_axis <- state$matrix_property
        if (is.null(target_axis) || !format_has_axis(daf, target_axis)) {
            stop("matrix-of-axis-entries chain requires the prior matrix property name to match an axis",
                call. = FALSE)
        }
        return(list(
            kind = "matrix_chain_pending_axis",
            value = state$value,
            rows_axis = state$rows_axis,
            cols_axis = state$cols_axis,
            chain_target_axis = target_axis,
            matrix2_property = node$name,
            if_missing = state$if_missing,
            if_missing_type = state$if_missing_type
        ))
    }
    if (identical(state$kind, "init")) {
        # Top-level `:: m @ rows = R @ cols = C` — Julia lookup_matrix_entry.
        # Stash the property; subsequent Axis/IsEqual nodes complete it.
        if (is.null(node$name)) {
            stop("'::' requires two axes in scope to list matrix names",
                call. = FALSE)
        }
        return(list(
            kind = "matrix_entry_pending_first_axis",
            matrix_property = node$name,
            if_missing = state$if_missing,
            if_missing_type = state$if_missing_type
        ))
    }
    if (identical(state$kind, "axis")) {
        # `@ axis :: matrix-prop ...`: the matrix lookup is completed once a
        # second axis (or a square slice) arrives. Stash the property name
        # and any IfMissing default; preserve a row-mask filter from the
        # first axis so the eventual slice respects it.
        if (is.null(node$name)) {
            stop("'::' requires two axes in scope (got axis)",
                call. = FALSE)
        }
        return(list(
            kind = "axis_pending_matrix",
            rows_axis = state$axis,
            row_indices = state$indices,
            matrix_property = node$name,
            matrix_if_missing = state$if_missing,
            matrix_if_missing_type = state$if_missing_type
        ))
    }
    if (!identical(state$kind, "two_axes")) {
        stop(sprintf("'::' requires two axes in scope (got %s)", state$kind),
            call. = FALSE
        )
    }
    rows <- state$rows_axis
    cols <- state$cols_axis
    if (is.null(node$name)) {
        # '@ rows @ cols :: ?' -> set ready state; Names node will complete
        state$kind <- "matrix_names_ready"
        return(state)
    }
    row_indices <- state$row_indices
    transposed <- FALSE
    if (!format_has_matrix(daf, rows, cols, node$name)) {
        if (format_has_matrix(daf, cols, rows, node$name)) {
            # Stored on the transposed orientation. Julia auto-relayouts
            # via get_matrix(...; relayout = true); we just transpose the
            # fetched matrix back into the queried (rows, cols) shape.
            transposed <- TRUE
        } else {
            if (!is.null(state$if_missing)) {
                nrow_out <- if (is.null(row_indices)) {
                    format_axis_length(daf, rows)
                } else {
                    length(row_indices)
                }
                return(list(
                    kind = "matrix",
                    value = matrix(
                        state$if_missing,
                        nrow_out,
                        format_axis_length(daf, cols)
                    ),
                    rows_axis = rows, cols_axis = cols
                ))
            }
            stop(
                sprintf(
                    "no matrix %s [%s, %s]",
                    sQuote(node$name), sQuote(rows), sQuote(cols)
                ),
                call. = FALSE
            )
        }
    }
    m <- if (transposed) {
        m_stored <- format_get_matrix(daf, cols, rows, node$name)
        if (methods::is(m_stored, "Matrix")) {
            Matrix::t(m_stored)
        } else {
            t(m_stored)
        }
    } else {
        format_get_matrix(daf, rows, cols, node$name)
    }
    if (!is.null(row_indices)) {
        m <- m[row_indices, , drop = FALSE]
    }
    list(
        kind = "matrix",
        value = m,
        rows_axis = rows, cols_axis = cols,
        matrix_property = node$name
    )
}
.apply_if_not <- function(node, state, daf) {
    # Record the chain-final sentinel for consumption by AsAxis-driven
    # chained lookup. An absent value (node$value == NULL) means drop
    # empty entries; a present value is substituted for empty entries.
    state$if_not_present <- TRUE
    state$if_not_value <- node$value
    state
}
.apply_as_axis <- function(node, state, daf) {
    if (identical(state$kind, "grouped_matrix_rows")) {
        # `-/ prop =@`: marks the row-group labels as entries on the prop's
        # axis. We only need to record this so a subsequent reduction labels
        # the result rows by axis name; for now, treat as a no-op pass.
        state$grouped_rows_as_axis <- node$axis_name %||% TRUE
        return(state)
    }
    if (identical(state$kind, "grouped_matrix_cols")) {
        state$grouped_cols_as_axis <- node$axis_name %||% TRUE
        return(state)
    }
    if (identical(state$kind, "grouped_vector")) {
        # `: prop =@` after a GroupBy: marks the chained group labels as
        # entries on `prop`'s axis. Treated as a no-op annotation for now.
        state$grouped_as_axis <- node$axis_name %||% TRUE
        return(state)
    }
    if (identical(state$kind, "pending_count")) {
        # `* prop =@` (and `* prop =@ axis_name`) marks the count's
        # b-side as entries on its (or `axis_name`'s) axis. CountBy
        # already pivots through `b_pivot_axis`, so `=@` is an explicit
        # annotation that doesn't change the result; tracked so a
        # future Julia-strict mode can verify the axis name matches.
        if (is.character(node$axis_name)) {
            state$b_pivot_axis <- node$axis_name
        }
        state$count_as_axis <- node$axis_name %||% TRUE
        return(state)
    }
    if (!identical(state$kind, "vector")) {
        stop("'=@' requires a vector in scope", call. = FALSE)
    }
    target_axis <- node$axis_name # NULL for bare, character scalar for explicit
    state$chain_target_axis <- target_axis
    state$kind <- "vector_axis"
    state
}

.apply_chained_lookup_vector <- function(node, state, daf) {
    # state$value: vector of length = axis_length(base_axis); entries are
    # names of chain_target_axis. Look up node$name on chain_target_axis,
    # index by pivot_values. Missing / empty pivot values either drop
    # rows (bare IfNot) or substitute the IfNot sentinel.
    #
    # Once an entry has been "finalised" by a `??` sentinel, the sentinel
    # is the entry's terminal value: subsequent chained lookups must NOT
    # try to look it up against the next axis (which would either fail or
    # silently substitute a later `??` sentinel). We track this through
    # `state$pending_final_mask`, mirroring Julia's pending_final_values.
    base_axis <- state$axis
    pivot_values <- state$value
    target_axis <- state$chain_target_axis
    if (is.null(target_axis)) {
        # Bare '=@': infer target axis from pivot vector's property name.
        target_axis <- state$property
        if (is.null(target_axis)) {
            stop("bare '=@' requires a named vector lookup before it",
                call. = FALSE
            )
        }
    }
    if (!format_has_axis(daf, target_axis)) {
        stop(sprintf(
            "AsAxis target axis %s does not exist",
            sQuote(target_axis)
        ), call. = FALSE)
    }

    final_mask <- state$pending_final_mask
    if (is.null(final_mask)) {
        final_mask <- rep(FALSE, length(pivot_values))
    }

    lookup_vec <- format_get_vector(daf, target_axis, node$name)
    target_entries <- format_axis_array(daf, target_axis)
    indices <- match(pivot_values, target_entries)

    empty_mask <- is.na(indices) |
        (is.character(pivot_values) & !nzchar(pivot_values))
    out <- rep(NA, length(pivot_values))
    mode(out) <- mode(lookup_vec)
    do_lookup <- !empty_mask & !final_mask
    out[do_lookup] <- lookup_vec[indices[do_lookup]]
    # Already-final entries keep the value the prior `??` sentinel wrote.
    if (any(final_mask)) {
        prior_finals <- methods::as(
            pivot_values[final_mask], class(lookup_vec)[[1L]]
        )
        out[final_mask] <- prior_finals
    }

    # Post-first-hop, pivot_values carries surviving axis names set by a prior
    # .apply_chained_lookup_vector call; use them to preserve any '??' row drop.
    # First-hop pivot_values comes from format_get_vector (unnamed on all current
    # backends), so we seed from the full axis instead.
    base_entries <- if (!is.null(names(pivot_values))) {
        names(pivot_values)
    } else {
        format_axis_array(daf, base_axis)
    }
    if (isTRUE(state$if_not_present)) {
        sentinel <- state$if_not_value
        # `??` only applies to entries whose pivot is empty AND that aren't
        # already finalised by a prior `??`.
        new_empty <- empty_mask & !final_mask
        if (is.null(sentinel)) {
            keep <- !new_empty
            out <- out[keep]
            base_entries <- base_entries[keep]
            final_mask <- final_mask[keep]
        } else {
            sentinel_typed <- methods::as(sentinel, class(lookup_vec)[[1L]])
            out[new_empty] <- sentinel_typed
            final_mask[new_empty] <- TRUE
        }
    } else {
        unhandled_empty <- empty_mask & !final_mask
        if (any(unhandled_empty)) {
            stop(
                sprintf(
                    "chain lookup on axis %s has %d empty pivot values and no '??' sentinel",
                    sQuote(base_axis), sum(unhandled_empty)
                ),
                call. = FALSE
            )
        }
    }
    names(out) <- base_entries
    # Carry forward any prior mask's surviving-entry indices. After a `??`
    # bare-drop the surviving set may have shrunk further; recompute by
    # mapping the (post-drop) base_entries back to the full axis.
    out_indices <- if (!is.null(state$indices) ||
                       length(base_entries) != format_axis_length(daf, base_axis)) {
        all_entries <- format_axis_array(daf, base_axis)
        match(base_entries, all_entries)
    } else {
        NULL
    }
    list(
        kind     = "vector",
        value    = out,
        axis     = base_axis,
        indices  = out_indices,
        property = node$name,
        pending_final_mask = final_mask
    )
}
.apply_chained_lookup_grouped <- function(node, state, daf) {
    # `/ axis ?? : prop` - chain on the GroupBy labels. The pending_groups
    # vector holds entries on `pending_groups_axis`; replace it with the
    # value of `prop` on that axis, applying any pending IfNot sentinel.
    target_axis <- state$pending_groups_axis
    if (is.null(target_axis)) {
        stop("internal: grouped_vector missing pending_groups_axis",
            call. = FALSE)
    }
    if (!format_has_axis(daf, target_axis)) {
        stop(sprintf(
            "chain target axis %s does not exist", sQuote(target_axis)
        ), call. = FALSE)
    }
    if (!format_has_vector(daf, target_axis, node$name)) {
        stop(sprintf(
            "no vector %s on axis %s",
            sQuote(node$name), sQuote(target_axis)
        ), call. = FALSE)
    }
    pivot <- state$pending_groups
    target_entries <- format_axis_array(daf, target_axis)
    idx <- match(pivot, target_entries)
    empty_mask <- is.na(idx) |
        (is.character(pivot) & !nzchar(pivot))

    lookup_vec <- format_get_vector(daf, target_axis, node$name)
    new_groups <- rep(NA, length(pivot))
    mode(new_groups) <- mode(lookup_vec)
    new_groups[!empty_mask] <- lookup_vec[idx[!empty_mask]]

    if (isTRUE(state$if_not_present)) {
        sentinel <- state$if_not_value
        if (is.null(sentinel)) {
            keep <- !empty_mask
            state$value <- state$value[keep]
            new_groups <- new_groups[keep]
        } else {
            sentinel_typed <- methods::as(
                sentinel, class(lookup_vec)[[1L]]
            )
            new_groups[empty_mask] <- sentinel_typed
        }
    } else if (any(empty_mask)) {
        stop(sprintf(
            "chain on grouped_vector via axis %s has %d empty pivot values and no '??' sentinel",
            sQuote(target_axis), sum(empty_mask)
        ), call. = FALSE)
    }

    state$pending_groups <- new_groups
    state$pending_groups_axis <- node$name
    state$if_not_present <- NULL
    state$if_not_value <- NULL
    state
}

# Chained `: prop` inside a mask predicate: pivot the current pending_vec
# through `pending_property`'s axis, replacing pending_vec with prop's
# value for each cell. Empty pivot values become NA (so a downstream
# comparator's NA → false propagation drops them from the mask) when an
# IfNot was supplied; otherwise the absence of pivots is an error.
.apply_chained_lookup_mask <- function(node, state, daf) {
    target_axis <- state$pending_property
    if (is.null(target_axis)) {
        stop("internal: mask state missing pending_property", call. = FALSE)
    }
    if (!format_has_axis(daf, target_axis)) {
        stop(sprintf(
            "chain target axis %s does not exist", sQuote(target_axis)
        ), call. = FALSE)
    }
    if (!format_has_vector(daf, target_axis, node$name)) {
        stop(sprintf(
            "no vector %s on axis %s",
            sQuote(node$name), sQuote(target_axis)
        ), call. = FALSE)
    }
    pivot <- state$pending_vec
    target_entries <- format_axis_array(daf, target_axis)
    idx <- match(pivot, target_entries)
    empty_mask <- is.na(idx) |
        (is.character(pivot) & !nzchar(pivot))
    lookup_vec <- format_get_vector(daf, target_axis, node$name)
    new_vec <- rep(NA, length(pivot))
    mode(new_vec) <- mode(lookup_vec)
    new_vec[!empty_mask] <- lookup_vec[idx[!empty_mask]]
    if (!isTRUE(state$if_not_present) && any(empty_mask)) {
        stop(sprintf(
            "chain inside mask via axis %s has %d empty pivot values and no '??' sentinel",
            sQuote(target_axis), sum(empty_mask)
        ), call. = FALSE)
    }
    if (isTRUE(state$if_not_present) && !is.null(state$if_not_value)) {
        sentinel_typed <- methods::as(
            state$if_not_value, class(lookup_vec)[[1L]]
        )
        new_vec[empty_mask] <- sentinel_typed
    }
    state$pending_vec <- new_vec
    state$pending_property <- node$name
    state$if_not_present <- NULL
    state$if_not_value <- NULL
    state
}

# `:: m : prop` — the matrix values are axis-of-prop entries on some axis
# (taken from the matrix property name unless `=@` overrode it). Replace
# every cell of the matrix with the looked-up `prop` value, preserving
# shape and dimnames. (Julia lookup_vector_by_matrix.)
.apply_chained_lookup_matrix <- function(node, state, daf) {
    target_axis <- state$matrix_chain_target_axis
    if (is.null(target_axis)) {
        # axis_of_property: when the matrix property name happens to also
        # be the name of an axis, that axis is the implicit chain target.
        guess <- attr(state, "matrix_property") %||% state$matrix_property
        if (is.character(guess) && format_has_axis(daf, guess)) {
            target_axis <- guess
        } else {
            stop("matrix chain requires AsAxis or a matrix property whose name matches an axis",
                call. = FALSE)
        }
    }
    if (!format_has_vector(daf, target_axis, node$name)) {
        stop(sprintf(
            "no vector %s on axis %s",
            sQuote(node$name), sQuote(target_axis)
        ), call. = FALSE)
    }
    target_entries <- format_axis_array(daf, target_axis)
    lookup_vec <- format_get_vector(daf, target_axis, node$name)
    m <- state$value
    rn <- rownames(m)
    if (is.null(rn) && !is.null(state$rows_axis)) {
        rn <- format_axis_array(daf, state$rows_axis)
        if (!is.null(state$row_indices)) rn <- rn[state$row_indices]
    }
    cn <- colnames(m)
    if (is.null(cn) && !is.null(state$cols_axis)) {
        cn <- format_axis_array(daf, state$cols_axis)
    }
    flat <- as.character(as.vector(m))
    idx <- match(flat, target_entries)
    out <- rep(NA, length(flat))
    mode(out) <- mode(lookup_vec)
    ok <- !is.na(idx) & nzchar(flat)
    out[ok] <- lookup_vec[idx[ok]]
    if (isTRUE(state$if_not_present)) {
        sentinel <- state$if_not_value
        if (!is.null(sentinel)) {
            sentinel_typed <- methods::as(sentinel, class(lookup_vec)[[1L]])
            out[!ok] <- sentinel_typed
        }
        # bare `??` for matrix doesn't have a row/col to drop; leave NA.
        state$if_not_present <- NULL
        state$if_not_value <- NULL
    }
    new_m <- matrix(out, nrow = nrow(m), ncol = ncol(m),
        dimnames = list(rn, cn))
    state$value <- new_m
    state$matrix_property <- node$name
    state$matrix_chain_target_axis <- NULL
    state
}

# Resolve `[ matrix-prop @ cols-axis = entry ...`: the column slice is
# fetched and reduced to a per-rows-axis vector; downstream comparators
# work on that vector exactly like the vector-mask path.
.apply_mask_matrix_axis_entry <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix mask on axis %s expects '@ %s = <entry>', got %s",
            sQuote(state$matrix_cols_axis),
            sQuote(state$matrix_cols_axis),
            sQuote(node$op)
        ), call. = FALSE)
    }
    rows <- state$axis
    cols <- state$matrix_cols_axis
    prop <- state$matrix_property
    cols_arr <- format_axis_array(daf, cols)
    col_idx <- match(as.character(node$value), cols_arr)
    if (is.na(col_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(cols)
        ), call. = FALSE)
    }
    vec <- if (format_has_matrix(daf, rows, cols, prop)) {
        m <- format_get_matrix(daf, rows, cols, prop)
        m[, col_idx, drop = TRUE]
    } else if (format_has_matrix(daf, cols, rows, prop)) {
        m <- format_get_matrix(daf, cols, rows, prop)
        m[col_idx, , drop = TRUE]
    } else {
        stop(sprintf(
            "no matrix %s on axes %s, %s",
            sQuote(prop), sQuote(rows), sQuote(cols)
        ), call. = FALSE)
    }
    if (methods::is(vec, "sparseVector") || methods::is(vec, "Matrix")) {
        vec <- as.numeric(vec)
    }
    mask <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
    state$kind <- "mask"
    state$pending_mask <- mask
    state$pending_property <- prop
    state$pending_vec <- vec
    state$matrix_property <- NULL
    state$matrix_cols_axis <- NULL
    state
}

# Combinator counterpart of `.apply_mask_matrix_axis_entry`: invoked
# after `& matrix-prop @ cols = entry` (or `|`/`^`, optionally negated).
# Resolves the column slice into pending_vec, combines it with the prior
# pending_mask via the combinator op, and leaves the state ready for an
# optional trailing comparator (which uses pending_combinator_* to refine
# the combined mask, mirroring the vector-property combinator path).
.apply_mask_matrix_axis_entry_combinator <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix mask combinator on axis %s expects '@ %s = <entry>', got %s",
            sQuote(state$matrix_cols_axis),
            sQuote(state$matrix_cols_axis),
            sQuote(node$op)
        ), call. = FALSE)
    }
    rows <- state$axis
    cols <- state$matrix_cols_axis
    prop <- state$matrix_property
    cols_arr <- format_axis_array(daf, cols)
    col_idx <- match(as.character(node$value), cols_arr)
    if (is.na(col_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(cols)
        ), call. = FALSE)
    }
    vec <- if (format_has_matrix(daf, rows, cols, prop)) {
        format_get_matrix(daf, rows, cols, prop)[, col_idx, drop = TRUE]
    } else if (format_has_matrix(daf, cols, rows, prop)) {
        format_get_matrix(daf, cols, rows, prop)[col_idx, , drop = TRUE]
    } else {
        stop(sprintf(
            "no matrix %s on axes %s, %s",
            sQuote(prop), sQuote(rows), sQuote(cols)
        ), call. = FALSE)
    }
    if (methods::is(vec, "sparseVector") || methods::is(vec, "Matrix")) {
        vec <- as.numeric(vec)
    }
    .combine_mask_with_vec(state, vec)
}

# Square-matrix counterpart of the combinator resolver.
.apply_mask_matrix_square_slice_combinator <- function(node, state, daf) {
    rows <- state$axis
    prop <- state$matrix_property
    if (!format_has_matrix(daf, rows, rows, prop)) {
        stop(sprintf(
            "no square matrix %s on axis %s",
            sQuote(prop), sQuote(rows)
        ), call. = FALSE)
    }
    rows_arr <- format_axis_array(daf, rows)
    idx <- match(as.character(node$value), rows_arr)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(rows)
        ), call. = FALSE)
    }
    m <- format_get_matrix(daf, rows, rows, prop)
    vec <- if (identical(node$op, "SquareRowIs")) {
        m[idx, , drop = TRUE]
    } else {
        m[, idx, drop = TRUE]
    }
    if (methods::is(vec, "sparseVector") || methods::is(vec, "Matrix")) {
        vec <- as.numeric(vec)
    }
    .combine_mask_with_vec(state, vec)
}

# Shared finaliser for the combinator-with-matrix-slice paths above. Folds
# `vec` (the resolved column slice) into the prior pending mask via the
# stashed combinator op, sets pending_combinator_* so a trailing comparator
# can replace `vec`'s truthy default with a comparison result, and returns
# to the regular `mask` state.
.combine_mask_with_vec <- function(state, vec) {
    m <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
    if (isTRUE(state$matrix_negated)) m <- !m
    op <- state$matrix_combinator_op
    prior <- state$matrix_combinator_prior
    combined <- switch(op,
        And = prior & m,
        Or  = prior | m,
        Xor = xor(prior, m)
    )
    list(
        kind = "mask",
        axis = state$axis,
        pending_mask = combined,
        pending_property = state$matrix_property,
        pending_vec = vec,
        pending_combinator_mask = prior,
        pending_combinator_op = op,
        pending_combinator_neg = isTRUE(state$matrix_negated),
        pending_mask_negated = state$pending_mask_negated
    )
}

# `[ square-matrix @| entry ]` / `[ ... @- entry ]` — column or row slice
# of a SQUARE matrix indexed by the in-scope axis.
.apply_mask_matrix_square_slice <- function(node, state, daf) {
    rows <- state$axis
    prop <- state$matrix_property
    if (!format_has_matrix(daf, rows, rows, prop)) {
        stop(sprintf(
            "no square matrix %s on axis %s",
            sQuote(prop), sQuote(rows)
        ), call. = FALSE)
    }
    rows_arr <- format_axis_array(daf, rows)
    idx <- match(as.character(node$value), rows_arr)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(rows)
        ), call. = FALSE)
    }
    m <- format_get_matrix(daf, rows, rows, prop)
    vec <- if (identical(node$op, "SquareRowIs")) {
        m[idx, , drop = TRUE]
    } else {
        m[, idx, drop = TRUE]
    }
    if (methods::is(vec, "sparseVector") || methods::is(vec, "Matrix")) {
        vec <- as.numeric(vec)
    }
    mask <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
    state$kind <- "mask"
    state$pending_mask <- mask
    state$pending_property <- prop
    state$pending_vec <- vec
    state$matrix_property <- NULL
    state
}

.apply_begin_mask <- function(node, state, daf) {
    if (!identical(state$kind, "axis")) {
        stop("'[' mask requires an axis in scope", call. = FALSE)
    }
    negated <- identical(node$op, "BeginNegatedMask")
    if (format_has_vector(daf, state$axis, node$property)) {
        vec <- format_get_vector(daf, state$axis, node$property)
        mask <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
        state$pending_mask <- mask
        state$pending_property <- node$property
        state$pending_vec <- vec
        # The Julia BeginNegatedMask flag is applied at apply_mask (=
        # EndMask) time over the FINAL accumulated mask, so a trailing
        # comparator's result is negated correctly. (Pre-negating here
        # would be discarded the moment a comparator overwrites
        # pending_mask.)
        state$pending_mask_negated <- negated
        state$kind <- "mask"
        return(state)
    }
    # Vector lookup failed: the property is (presumably) a matrix and the
    # column axis arrives next via `@ axis = entry` or `@| / @-`. Defer to
    # the matrix-mask resolver. (Julia lookup_matrix_column_mask /
    # lookup_square_matrix_*_mask phrases.)
    state$kind <- "mask_matrix_pending_axis"
    state$matrix_property <- node$property
    state$pending_mask_negated <- negated
    state
}

.apply_end_mask <- function(node, state, daf) {
    axis <- state$axis
    entries <- format_axis_array(daf, axis)
    mask <- state$pending_mask
    if (isTRUE(state$pending_mask_negated)) {
        # Outer-bracket negation (`[ ! ... ]`) is applied to the final mask
        # so a `!` propagates over a trailing comparator (`[ ! age > 60 ]`
        # means NOT(age > 60), not (NOT-age) > 60).
        mask <- !mask
    }
    keep <- !is.na(mask) & mask
    # Carry the surviving-entry indices forward so that a subsequent
    # LookupVector / LookupMatrix subsets by the mask rather than returning
    # the full axis-length vector. indices=NULL (i.e. all entries pass) is
    # treated as "no filter" by downstream consumers.
    list(
        kind = "axis", axis = axis, value = entries[keep],
        indices = if (all(keep)) NULL else which(keep)
    )
}

.apply_logical_mask <- function(node, state, daf) {
    if (!identical(state$kind, "mask")) {
        stop("logical mask combinator outside of mask", call. = FALSE)
    }
    negated <- grepl("NegatedMask$", node$op)
    op <- if (startsWith(node$op, "And")) "And"
          else if (startsWith(node$op, "Or")) "Or"
          else "Xor"
    if (!format_has_vector(daf, state$axis, node$property)) {
        # Property isn't a vector on the in-scope axis - defer to
        # `mask_matrix_pending_combinator`. Subsequent `@ axis = entry`
        # (or `@| / @-`) supplies the column slice, then a comparator
        # combines the resulting per-axis vector with the prior mask via
        # `op` (negated by `negated`).
        state$kind <- "mask_matrix_pending_combinator"
        state$matrix_property <- node$property
        state$matrix_negated <- negated
        state$matrix_combinator_op <- op
        state$matrix_combinator_prior <- state$pending_mask
        return(state)
    }
    vec <- format_get_vector(daf, state$axis, node$property)
    m <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
    if (negated) m <- !m
    combined <- switch(op,
        And = state$pending_mask & m,
        Or  = state$pending_mask | m,
        Xor = xor(state$pending_mask, m)
    )
    # Save the pre-combinator mask and combinator op so a trailing
    # comparator on this property can refine rather than overwrite.
    state$pending_combinator_mask <- state$pending_mask
    state$pending_combinator_op <- op
    state$pending_combinator_neg <- negated
    state$pending_mask <- combined
    state$pending_vec <- vec # allow a trailing comparator on this property
    state
}

.apply_comparator <- function(node, state, daf) {
    # Entry-pick resolves `= entry` after `@ axis` when a vector or matrix
    # is in scope (Julia SCALAR_QUERY phrases).
    if (identical(state$kind, "entry_pick_vector")) {
        return(.apply_entry_pick_vector(node, state, daf))
    }
    if (identical(state$kind, "entry_pick_matrix")) {
        return(.apply_entry_pick_matrix(node, state, daf))
    }
    if (identical(state$kind, "axis_pending_matrix_pick")) {
        return(.apply_matrix_column_by_axis(node, state, daf))
    }
    if (identical(state$kind, "vector_entry_pending_pick")) {
        return(.apply_top_level_vector_entry(node, state, daf))
    }
    if (identical(state$kind, "matrix_entry_pending_first_pick")) {
        if (!identical(node$op, "IsEqual")) {
            stop(sprintf(
                "matrix entry-pick on axis %s expects '= <entry>', got %s",
                sQuote(state$rows_axis), sQuote(node$op)
            ), call. = FALSE)
        }
        state$row_value <- as.character(node$value)
        state$kind <- "matrix_entry_pending_second_axis"
        return(state)
    }
    if (identical(state$kind, "matrix_entry_pending_second_pick")) {
        return(.apply_top_level_matrix_entry(node, state, daf))
    }
    if (identical(state$kind, "mask_matrix_pending_pick")) {
        return(.apply_mask_matrix_axis_entry(node, state, daf))
    }
    if (identical(state$kind, "mask_matrix_pending_combinator_pick")) {
        return(.apply_mask_matrix_axis_entry_combinator(node, state, daf))
    }
    if (identical(state$kind, "groupby_vector_matrix_pending_pick")) {
        return(.apply_groupby_vector_matrix_entry(node, state, daf))
    }
    if (identical(state$kind, "groupby_matrix_rows_pending_pick")) {
        return(.apply_groupby_matrix_rows_matrix_entry(node, state, daf))
    }
    if (identical(state$kind, "groupby_matrix_cols_pending_pick")) {
        return(.apply_groupby_matrix_cols_matrix_entry(node, state, daf))
    }
    if (identical(state$kind, "countby_matrix_pending_pick")) {
        return(.apply_countby_matrix_entry(node, state, daf))
    }
    if (identical(state$kind, "matrix_chain_pending_pick")) {
        return(.apply_matrix_chain_matrix_entry(node, state, daf))
    }
    if (identical(state$kind, "vector")) {
        # `: vec > x` etc. - element-wise comparator over a vector returns a
        # bool vector along the same axis. (Julia compare_vector phrase.)
        vec <- state$value
        test <- switch(node$op,
            IsLess         = vec <  .coerce_cmp(node$value, vec),
            IsLessEqual    = vec <= .coerce_cmp(node$value, vec),
            IsEqual        = vec == .coerce_cmp(node$value, vec),
            IsNotEqual     = vec != .coerce_cmp(node$value, vec),
            IsGreater      = vec >  .coerce_cmp(node$value, vec),
            IsGreaterEqual = vec >= .coerce_cmp(node$value, vec),
            IsMatch        = grepl(node$pattern, as.character(vec), perl = TRUE),
            IsNotMatch     = !grepl(node$pattern, as.character(vec), perl = TRUE)
        )
        state$value <- test
        return(state)
    }
    if (identical(state$kind, "axis")) {
        # `@ axis != value`: compare each axis entry name against `value`,
        # returning a bool vector along the axis. Mirrors Julia's compare on
        # the axis-name vector.
        vec <- state$value
        test <- switch(node$op,
            IsLess         = vec <  .coerce_cmp(node$value, vec),
            IsLessEqual    = vec <= .coerce_cmp(node$value, vec),
            IsEqual        = vec == .coerce_cmp(node$value, vec),
            IsNotEqual     = vec != .coerce_cmp(node$value, vec),
            IsGreater      = vec >  .coerce_cmp(node$value, vec),
            IsGreaterEqual = vec >= .coerce_cmp(node$value, vec),
            IsMatch        = grepl(node$pattern, as.character(vec), perl = TRUE),
            IsNotMatch     = !grepl(node$pattern, as.character(vec), perl = TRUE)
        )
        return(list(
            kind = "vector", axis = state$axis, value = test,
            indices = state$indices
        ))
    }
    if (identical(state$kind, "matrix")) {
        # `:: m > x` etc. - element-wise comparator over a matrix returns a
        # bool matrix along the same two axes. (Julia compare_matrix phrase.)
        m <- state$value
        rhs <- .coerce_cmp(node$value, m)
        test <- switch(node$op,
            IsLess         = m <  rhs,
            IsLessEqual    = m <= rhs,
            IsEqual        = m == rhs,
            IsNotEqual     = m != rhs,
            IsGreater      = m >  rhs,
            IsGreaterEqual = m >= rhs,
            IsMatch        = matrix(
                grepl(node$pattern, as.character(m), perl = TRUE),
                nrow = nrow(m), ncol = ncol(m),
                dimnames = dimnames(m)
            ),
            IsNotMatch     = matrix(
                !grepl(node$pattern, as.character(m), perl = TRUE),
                nrow = nrow(m), ncol = ncol(m),
                dimnames = dimnames(m)
            )
        )
        state$value <- test
        return(state)
    }
    if (identical(state$kind, "scalar")) {
        # `. v > x` - comparator on a scalar lookup returns a scalar bool.
        v <- state$value
        rhs <- .coerce_cmp(node$value, v)
        test <- switch(node$op,
            IsLess         = v <  rhs,
            IsLessEqual    = v <= rhs,
            IsEqual        = v == rhs,
            IsNotEqual     = v != rhs,
            IsGreater      = v >  rhs,
            IsGreaterEqual = v >= rhs,
            IsMatch        = grepl(node$pattern, as.character(v), perl = TRUE),
            IsNotMatch     = !grepl(node$pattern, as.character(v), perl = TRUE)
        )
        state$value <- test
        return(state)
    }
    if (!identical(state$kind, "mask")) {
        stop("comparator outside of mask", call. = FALSE)
    }
    vec <- state$pending_vec
    test <- switch(node$op,
        IsLess         = vec < .coerce_cmp(node$value, vec),
        IsLessEqual    = vec <= .coerce_cmp(node$value, vec),
        IsEqual        = vec == .coerce_cmp(node$value, vec),
        IsNotEqual     = vec != .coerce_cmp(node$value, vec),
        IsGreater      = vec > .coerce_cmp(node$value, vec),
        IsGreaterEqual = vec >= .coerce_cmp(node$value, vec),
        IsMatch        = grepl(node$pattern, as.character(vec), perl = TRUE),
        IsNotMatch     = !grepl(node$pattern, as.character(vec), perl = TRUE)
    )
    if (!is.null(state$pending_combinator_mask)) {
        # A trailing comparator after a logical combinator: apply the combinator
        # operation between the pre-combinator mask and this comparison result.
        m <- if (isTRUE(state$pending_combinator_neg)) !test else test
        test <- switch(state$pending_combinator_op,
            And = state$pending_combinator_mask & m,
            Or  = state$pending_combinator_mask | m,
            Xor = xor(state$pending_combinator_mask, m)
        )
        state$pending_combinator_mask <- NULL
        state$pending_combinator_op <- NULL
        state$pending_combinator_neg <- NULL
    }
    state$pending_mask <- test
    state
}

.coerce_cmp <- function(value_string, ref_vec) {
    if (is.numeric(ref_vec)) {
        as.numeric(value_string)
    } else if (is.logical(ref_vec)) {
        as.logical(value_string)
    } else {
        as.character(value_string)
    }
}

# Entry-pick from a vector: `@ axis = entry` after `: vector-property`.
# Consumes the in-scope vector and returns the scalar at the named entry.
.apply_entry_pick_vector <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "entry-pick on axis %s expects '= <entry>', got %s",
            sQuote(state$pick_axis), sQuote(node$op)
        ), call. = FALSE)
    }
    entry <- as.character(node$value)
    axis_entries <- format_axis_array(daf, state$pick_axis)
    idx <- match(entry, axis_entries)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(entry), sQuote(state$pick_axis)
        ), call. = FALSE)
    }
    val <- state$value[[idx]]
    # Strip any name attribute so the scalar is a bare length-1 value.
    names(val) <- NULL
    list(kind = "scalar", value = val)
}

# Entry-pick from a matrix: `@ axis = entry` after `:: matrix-property`.
# Picks one row (or column) and leaves a vector along the remaining axis.
# Two consecutive entry-picks reduce the matrix all the way to a scalar via
# the entry_pick_vector path.
.apply_entry_pick_matrix <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "entry-pick on axis %s expects '= <entry>', got %s",
            sQuote(state$pick_axis), sQuote(node$op)
        ), call. = FALSE)
    }
    entry <- as.character(node$value)
    axis_entries <- format_axis_array(daf, state$pick_axis)
    idx <- match(entry, axis_entries)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(entry), sQuote(state$pick_axis)
        ), call. = FALSE)
    }
    m <- state$value
    if (identical(state$pick_dim, "row")) {
        vec <- m[idx, , drop = TRUE]
        remaining_axis <- state$cols_axis
    } else {
        vec <- m[, idx, drop = TRUE]
        remaining_axis <- state$rows_axis
    }
    if (methods::is(vec, "sparseVector") || methods::is(vec, "Matrix")) {
        vec <- as.numeric(vec)
        names(vec) <- format_axis_array(daf, remaining_axis)
    }
    list(kind = "vector", value = vec, axis = remaining_axis)
}

# Resolve top-level `: vec @ axis = entry` (Julia lookup_vector_entry).
.apply_top_level_vector_entry <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "vector entry-pick on axis %s expects '= <entry>', got %s",
            sQuote(state$axis), sQuote(node$op)
        ), call. = FALSE)
    }
    axis <- state$axis
    prop <- state$property
    if (!format_has_vector(daf, axis, prop)) {
        if (!is.null(state$if_missing)) {
            default <- .coerce_if_missing_default(
                state$if_missing, state$if_missing_type
            )
            return(list(kind = "scalar", value = default))
        }
        stop(sprintf(
            "no vector %s on axis %s", sQuote(prop), sQuote(axis)
        ), call. = FALSE)
    }
    vec <- format_get_vector(daf, axis, prop)
    entry <- as.character(node$value)
    axis_entries <- format_axis_array(daf, axis)
    idx <- match(entry, axis_entries)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s", sQuote(entry), sQuote(axis)
        ), call. = FALSE)
    }
    val <- vec[[idx]]
    names(val) <- NULL
    list(kind = "scalar", value = val)
}

# Resolve top-level `:: m @ rows = R @ cols = C` (Julia lookup_matrix_entry).
.apply_top_level_matrix_entry <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix entry-pick on axis %s expects '= <entry>', got %s",
            sQuote(state$cols_axis), sQuote(node$op)
        ), call. = FALSE)
    }
    rows <- state$rows_axis
    cols <- state$cols_axis
    prop <- state$matrix_property
    transposed <- FALSE
    if (!format_has_matrix(daf, rows, cols, prop)) {
        if (format_has_matrix(daf, cols, rows, prop)) {
            transposed <- TRUE
        } else {
            if (!is.null(state$if_missing)) {
                default <- .coerce_if_missing_default(
                    state$if_missing, state$if_missing_type
                )
                return(list(kind = "scalar", value = default))
            }
            stop(sprintf(
                "no matrix %s [%s, %s]",
                sQuote(prop), sQuote(rows), sQuote(cols)
            ), call. = FALSE)
        }
    }
    rows_array <- format_axis_array(daf, rows)
    cols_array <- format_axis_array(daf, cols)
    row_idx <- match(state$row_value, rows_array)
    col_idx <- match(as.character(node$value), cols_array)
    if (is.na(row_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(state$row_value), sQuote(rows)
        ), call. = FALSE)
    }
    if (is.na(col_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(cols)
        ), call. = FALSE)
    }
    m <- if (transposed) {
        m_stored <- format_get_matrix(daf, cols, rows, prop)
        m_stored[col_idx, row_idx]
    } else {
        m_stored <- format_get_matrix(daf, rows, cols, prop)
        m_stored[row_idx, col_idx]
    }
    val <- as.numeric(m)[[1L]]
    list(kind = "scalar", value = val)
}

# Resolve `@ rows-axis :: matrix-prop @ cols-axis = entry` once the IsEqual
# arrives. Fetches the matrix, slices the named column, and returns a vector
# along the in-scope rows axis. Honours any IfMissing default that was
# attached to the LookupMatrix node, and any row-mask filter carried over
# from a `[ ... ]` predicate on the rows axis.
.apply_matrix_column_by_axis <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix-column slice on axis %s expects '= <entry>', got %s",
            sQuote(state$cols_axis), sQuote(node$op)
        ), call. = FALSE)
    }
    rows <- state$rows_axis
    cols <- state$cols_axis
    prop <- state$matrix_property
    rows_array <- format_axis_array(daf, rows)
    row_names <- if (is.null(state$row_indices)) {
        rows_array
    } else {
        rows_array[state$row_indices]
    }
    n_rows_out <- length(row_names)

    if (!format_has_matrix(daf, rows, cols, prop)) {
        if (!is.null(state$matrix_if_missing)) {
            default <- .coerce_if_missing_default(
                state$matrix_if_missing, state$matrix_if_missing_type
            )
            return(list(
                kind = "vector",
                axis = rows,
                value = setNames(rep(default, n_rows_out), row_names)
            ))
        }
        stop(sprintf(
            "no matrix %s [%s, %s]",
            sQuote(prop), sQuote(rows), sQuote(cols)
        ), call. = FALSE)
    }

    cols_array <- format_axis_array(daf, cols)
    col_idx <- match(as.character(node$value), cols_array)
    if (is.na(col_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(cols)
        ), call. = FALSE)
    }

    m <- format_get_matrix(daf, rows, cols, prop)
    vec <- m[, col_idx, drop = TRUE]
    if (!is.null(state$row_indices)) {
        vec <- vec[state$row_indices]
    }
    if (methods::is(vec, "sparseVector") || methods::is(vec, "Matrix")) {
        vec <- as.numeric(vec)
    }
    names(vec) <- row_names
    list(kind = "vector", value = vec, axis = rows)
}

# Resolve `@ axis :: matrix-prop @| entry` (column slice) and
# `@ axis :: matrix-prop @- entry` (row slice) for square matrices, where
# both dimensions of the matrix share the in-scope axis.
.apply_square_slice_axis <- function(node, state, daf) {
    rows <- state$rows_axis
    prop <- state$matrix_property
    rows_array <- format_axis_array(daf, rows)
    row_names <- if (is.null(state$row_indices)) {
        rows_array
    } else {
        rows_array[state$row_indices]
    }
    n_rows_out <- length(row_names)

    if (!format_has_matrix(daf, rows, rows, prop)) {
        if (!is.null(state$matrix_if_missing)) {
            default <- .coerce_if_missing_default(
                state$matrix_if_missing, state$matrix_if_missing_type
            )
            return(list(
                kind = "vector",
                axis = rows,
                value = setNames(rep(default, n_rows_out), row_names)
            ))
        }
        stop(sprintf(
            "no matrix %s [%s, %s]",
            sQuote(prop), sQuote(rows), sQuote(rows)
        ), call. = FALSE)
    }

    idx <- match(as.character(node$value), rows_array)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(rows)
        ), call. = FALSE)
    }

    m <- format_get_matrix(daf, rows, rows, prop)
    vec <- if (identical(node$op, "SquareRowIs")) {
        m[idx, , drop = TRUE]
    } else {
        # SquareColumnIs
        m[, idx, drop = TRUE]
    }
    if (!is.null(state$row_indices)) {
        vec <- vec[state$row_indices]
    }
    if (methods::is(vec, "sparseVector") || methods::is(vec, "Matrix")) {
        vec <- as.numeric(vec)
    }
    names(vec) <- row_names
    list(kind = "vector", value = vec, axis = rows)
}

.apply_square_slice <- function(node, state, daf) {
    if (identical(state$kind, "axis_pending_matrix")) {
        return(.apply_square_slice_axis(node, state, daf))
    }
    if (identical(state$kind, "mask_matrix_pending_axis")) {
        return(.apply_mask_matrix_square_slice(node, state, daf))
    }
    if (identical(state$kind, "mask_matrix_pending_combinator")) {
        return(.apply_mask_matrix_square_slice_combinator(node, state, daf))
    }
    if (identical(state$kind, "groupby_vector_matrix_pending_axis")) {
        return(.apply_groupby_vector_matrix_square_slice(node, state, daf))
    }
    if (identical(state$kind, "groupby_matrix_rows_pending_axis")) {
        return(.apply_groupby_matrix_rows_matrix_square_slice(node, state, daf))
    }
    if (identical(state$kind, "groupby_matrix_cols_pending_axis")) {
        return(.apply_groupby_matrix_cols_matrix_square_slice(node, state, daf))
    }
    if (identical(state$kind, "countby_matrix_pending_axis")) {
        return(.apply_countby_matrix_square_slice(node, state, daf))
    }
    if (identical(state$kind, "matrix_chain_pending_axis")) {
        return(.apply_matrix_chain_matrix_square_slice(node, state, daf))
    }
    if (!identical(state$kind, "matrix")) {
        stop("square slice requires a matrix in scope", call. = FALSE)
    }
    m <- state$value
    if (identical(node$op, "SquareRowIs")) {
        rows <- format_axis_array(daf, state$rows_axis)
        idx <- match(node$value, rows)
        if (is.na(idx)) stop(sprintf("no row %s", sQuote(node$value)), call. = FALSE)
        cols <- format_axis_array(daf, state$cols_axis)
        return(list(
            kind = "vector",
            axis = state$cols_axis,
            value = setNames(as.numeric(m[idx, ]), cols)
        ))
    }
    # SquareColumnIs
    cols <- format_axis_array(daf, state$cols_axis)
    idx <- match(node$value, cols)
    if (is.na(idx)) stop(sprintf("no column %s", sQuote(node$value)), call. = FALSE)
    rows <- format_axis_array(daf, state$rows_axis)
    list(
        kind = "vector",
        axis = state$rows_axis,
        value = setNames(as.numeric(m[, idx]), rows)
    )
}
.apply_eltwise <- function(node, state, daf) {
    if (identical(state$kind, "scalar")) {
        # `. v % Abs` etc. - eltwise on a scalar is a 1-element apply.
        fn <- get_eltwise(node$name)
        params <- .coerce_params(node$params)
        state$value <- do.call(fn, c(list(state$value), params))
        return(state)
    }
    if (!state$kind %in% c("vector", "matrix")) {
        stop("'%' eltwise requires scalar, vector, or matrix in scope",
            call. = FALSE)
    }
    fn <- get_eltwise(node$name)
    params <- .coerce_params(node$params)
    builtin <- attr(fn, ".dafr_builtin")

    if (isTRUE(dafr_opt("dafr.perf.fast_paths")) && identical(builtin, "Log")) {
        eps <- as.numeric(params$eps %||% 0)
        base <- as.numeric(params$base %||% exp(1))

        # Fast path 1: sparsity-preserving Log on dgCMatrix. log1p(0) == 0,
        # so eps == 1 with default base (e) is the only Log parameterisation
        # that preserves sparsity; apply log1p in place to @x and keep @i /
        # @p.
        if (methods::is(state$value, "dgCMatrix") &&
            isTRUE(all.equal(eps, 1)) &&
            isTRUE(all.equal(base, exp(1)))) {
            out <- state$value
            out@x <- log1p(out@x)
            state$value <- out
            return(state)
        }

        # Fast path 2: OpenMP-parallel log(x + eps)/log(base) on dense
        # matrices and numeric vectors. Covers the common log2(x + 1e-5)
        # case (MCView's lfp transform, etc.) which previously fell
        # through to R's single-threaded log() + scalar coercion chain.
        threshold <- as.integer(dafr_opt("dafr.kernel_threshold"))
        if (is.matrix(state$value) && is.double(state$value)) {
            state$value <- kernel_log_dense_mat_cpp(
                state$value, eps = eps, base = base, threshold = threshold
            )
            return(state)
        }
        if (is.numeric(state$value) && is.null(dim(state$value))) {
            nm <- names(state$value)
            out <- kernel_log_dense_vec_cpp(
                as.double(state$value), eps = eps, base = base, threshold = threshold
            )
            names(out) <- nm
            state$value <- out
            return(state)
        }
    }

    state$value <- do.call(fn, c(list(state$value), params))
    state
}

.apply_reduction <- function(node, state, daf) {
    if (identical(state$kind, "grouped_vector")) {
        return(.apply_reduction_grouped_vector(node, state, daf))
    }
    if (identical(state$kind, "grouped_matrix_rows")) {
        return(.apply_reduction_grouped_matrix(node, state, daf, by = "rows"))
    }
    if (identical(state$kind, "grouped_matrix_cols")) {
        return(.apply_reduction_grouped_matrix(node, state, daf, by = "cols"))
    }
    # ReduceToScalar accepts plain vector or matrix inputs (Julia parity).
    if (identical(node$op, "ReduceToScalar") &&
        state$kind %in% c("vector", "matrix")) {
        return(.apply_reduction_to_scalar(node, state, daf))
    }
    if (!identical(state$kind, "matrix")) {
        stop(sprintf("%s requires a matrix or grouped scope", node$op),
            call. = FALSE
        )
    }
    fn <- get_reduction(node$reduction)
    params <- .coerce_params(node$params)

    # Fast path: built-in reduction -> vectorised primitive. Falls through to
    # NULL for unhandled built-ins (e.g. Count) or non-sparse/dense matrices.
    if (isTRUE(dafr_opt("dafr.perf.fast_paths"))) {
        fast <- .apply_reduction_fast(node, state, fn, params, daf)
        if (!is.null(fast)) return(fast)
    }

    .apply_reduction_slow(node, state, fn, params, daf)
}

# `>>` ReductionOperation reducing a plain vector or matrix to one scalar.
# Julia parity — `DataAxesFormats.jl` queries.jl ReductionOperation.
# For a matrix we materialise the data as a dense flat vector so that
# reductions whose result is non-associative (Var, Mode, Median, Quantile, ...)
# produce the same answer as in Julia.
.apply_reduction_to_scalar <- function(node, state, daf) {
    fn <- get_reduction(node$reduction)
    params <- .coerce_params(node$params)
    v <- switch(state$kind,
        vector = state$value,
        matrix = {
            m <- state$value
            if (methods::is(m, "Matrix")) {
                as.vector(as.matrix(m))
            } else {
                as.vector(m)
            }
        },
        stop(sprintf(
            "'>>' requires a vector or matrix in scope (got %s)",
            state$kind
        ), call. = FALSE)
    )
    out <- do.call(fn, c(list(v), params))
    if (length(out) == 1L) names(out) <- NULL
    list(kind = "scalar", value = out)
}

.dafr_kernel_threshold <- function() dafr_opt("dafr.kernel_threshold")

# Extract eps parameter for VarN/StdN reductions. Defaults to 0 when omitted,
# matching the slow-path default in .op_varn / .op_stdn.
.param_eps <- function(params) {
    as.numeric(params[["eps"]] %||% 0)
}

# Extract 'p' parameter for Quantile reductions. Validates range [0, 1].
.param_quantile_q <- function(params) {
    v <- params[["p"]]
    if (is.null(v)) stop("Quantile: missing 'p' parameter", call. = FALSE)
    q <- as.numeric(v)
    if (is.na(q) || q < 0 || q > 1) {
        stop(sprintf("Quantile: p must be in [0, 1] (got %g)", q), call. = FALSE)
    }
    q
}

.apply_reduction_fast <- function(node, state, fn, params, daf) {
    builtin <- attr(fn, ".dafr_builtin")
    if (is.null(builtin)) return(NULL)
    m <- state$value
    # Only dgCMatrix has a numeric @x that kernel_*_csc_cpp functions accept.
    # lgCMatrix has a logical @x and must fall through to the dense slow path.
    is_dg <- methods::is(m, "dgCMatrix")
    is_sparse <- is_dg || methods::is(m, "lgCMatrix")
    is_dense <- is.matrix(m)
    if (!is_sparse && !is_dense) return(NULL)

    # ReduceToColumn (axis=0): per-row reduction — one value per row, indexed by rows_axis.
    # ReduceToRow (axis=1): per-col reduction — one value per col, indexed by cols_axis.
    # Both branches share the same 12-case switch with row*/col* helpers swapped.
    # Deduplication considered and rejected: the two branches diverge in state key
    # access (rows_axis vs cols_axis, row_names vs col_names) and in helper names
    # (rowSums/colSums, rowMeans/colMeans, rowMaxs/colMaxs, etc.) such that a
    # single dispatcher would require a lookup table or function-factory of 10+
    # helper pairs — more infrastructure than the duplication it removes.
    if (identical(node$op, "ReduceToColumn")) {
        row_names <- if (is_dense) rownames(m) else m@Dimnames[[1L]]
        if (is.null(row_names)) row_names <- format_axis_array(daf, state$rows_axis)
        vals <- switch(builtin,
            Sum   = if (is_sparse) Matrix::rowSums(m) else rowSums(m),
            Mean  = if (is_sparse) Matrix::rowMeans(m) else rowMeans(m),
            Max   = if (is_dg)
                        kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                              axis = 0L, variant = "Max",
                                              threshold = .dafr_kernel_threshold())
                    else if (is_dense) matrixStats::rowMaxs(m)
                    else return(NULL),
            Min   = if (is_dg)
                        kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                              axis = 0L, variant = "Min",
                                              threshold = .dafr_kernel_threshold())
                    else if (is_dense) matrixStats::rowMins(m)
                    else return(NULL),
            Var   = if (is_dg)
                        kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                           axis = 0L, variant = "Var", eps = 0,
                                           threshold = .dafr_kernel_threshold())
                    else if (is_dense) {
                        nc <- ncol(m)
                        matrixStats::rowVars(m) * ((nc - 1L) / nc)
                    } else return(NULL),
            Std   = if (is_dg)
                        kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                           axis = 0L, variant = "Std", eps = 0,
                                           threshold = .dafr_kernel_threshold())
                    else if (is_dense) {
                        nc <- ncol(m)
                        sqrt(matrixStats::rowVars(m) * ((nc - 1L) / nc))
                    } else return(NULL),
            VarN  = {
                eps <- .param_eps(params)
                if (is_dg)
                    kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                       axis = 0L, variant = "VarN", eps = eps,
                                       threshold = .dafr_kernel_threshold())
                else if (is_dense) {
                    nc <- ncol(m)
                    v  <- matrixStats::rowVars(m) * ((nc - 1L) / nc)
                    mu <- rowMeans(m)
                    v / (mu + eps)
                } else return(NULL)
            },
            StdN  = {
                eps <- .param_eps(params)
                if (is_dg)
                    kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                       axis = 0L, variant = "StdN", eps = eps,
                                       threshold = .dafr_kernel_threshold())
                else if (is_dense) {
                    nc <- ncol(m)
                    mu <- rowMeans(m)
                    s  <- sqrt(matrixStats::rowVars(m) * ((nc - 1L) / nc))
                    s / (mu + eps)
                } else return(NULL)
            },
            GeoMean = {
                eps <- .param_eps(params)
                if (is_dg)
                    kernel_geomean_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                           axis = 0L, eps = eps,
                                           threshold = .dafr_kernel_threshold())
                else if (is_dense) {
                    if (eps == 0) {
                        exp(rowMeans(log(m)))
                    } else {
                        exp(rowMeans(log(m + eps))) - eps
                    }
                } else return(NULL)
            },
            Median = if (is_dg)
                kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                    axis = 0L, q = 0.5, threshold = .dafr_kernel_threshold())
            else if (is_dense) matrixStats::rowMedians(m)
            else return(NULL),
            Quantile = {
                q <- .param_quantile_q(params)
                if (is_dg)
                    kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                        axis = 0L, q = q, threshold = .dafr_kernel_threshold())
                # is.numeric(): fast path only for INTSXP/REALSXP; character/logical
                # dense matrices fall through to return(NULL) -> slow path.
                else if (is_dense && is.numeric(m))
                    kernel_quantile_dense_cpp(m, axis = 0L, q = q,
                        threshold = .dafr_kernel_threshold())
                else return(NULL)
            },
            Mode = if (is_dg && is.numeric(m@x))
                kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                    axis = 0L, threshold = .dafr_kernel_threshold())
            else if (is_dense && is.numeric(m))
                kernel_mode_dense_cpp(m, axis = 0L,
                    threshold = .dafr_kernel_threshold())
            else return(NULL),
            return(NULL)
        )
        return(list(
            kind = "vector", axis = state$rows_axis,
            value = setNames(as.numeric(vals), row_names)
        ))
    }
    # ReduceToRow: column-wise reduction
    col_names <- if (is_dense) colnames(m) else m@Dimnames[[2L]]
    if (is.null(col_names)) col_names <- format_axis_array(daf, state$cols_axis)
    vals <- switch(builtin,
        Sum   = if (is_sparse) Matrix::colSums(m) else colSums(m),
        Mean  = if (is_sparse) Matrix::colMeans(m) else colMeans(m),
        Max   = if (is_dg)
                    kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                          axis = 1L, variant = "Max",
                                          threshold = .dafr_kernel_threshold())
                else if (is_dense) matrixStats::colMaxs(m)
                else return(NULL),
        Min   = if (is_dg)
                    kernel_minmax_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                          axis = 1L, variant = "Min",
                                          threshold = .dafr_kernel_threshold())
                else if (is_dense) matrixStats::colMins(m)
                else return(NULL),
        Var   = if (is_dg)
                    kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                       axis = 1L, variant = "Var", eps = 0,
                                       threshold = .dafr_kernel_threshold())
                else if (is_dense) {
                    nr <- nrow(m)
                    matrixStats::colVars(m) * ((nr - 1L) / nr)
                } else return(NULL),
        Std   = if (is_dg)
                    kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                       axis = 1L, variant = "Std", eps = 0,
                                       threshold = .dafr_kernel_threshold())
                else if (is_dense) {
                    nr <- nrow(m)
                    sqrt(matrixStats::colVars(m) * ((nr - 1L) / nr))
                } else return(NULL),
        VarN  = {
            eps <- .param_eps(params)
            if (is_dg)
                kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                   axis = 1L, variant = "VarN", eps = eps,
                                   threshold = .dafr_kernel_threshold())
            else if (is_dense) {
                nr <- nrow(m)
                v  <- matrixStats::colVars(m) * ((nr - 1L) / nr)
                mu <- colMeans(m)
                v / (mu + eps)
            } else return(NULL)
        },
        StdN  = {
            eps <- .param_eps(params)
            if (is_dg)
                kernel_var_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                   axis = 1L, variant = "StdN", eps = eps,
                                   threshold = .dafr_kernel_threshold())
            else if (is_dense) {
                nr <- nrow(m)
                mu <- colMeans(m)
                s  <- sqrt(matrixStats::colVars(m) * ((nr - 1L) / nr))
                s / (mu + eps)
            } else return(NULL)
        },
        GeoMean = {
            eps <- .param_eps(params)
            if (is_dg)
                kernel_geomean_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                                       axis = 1L, eps = eps,
                                       threshold = .dafr_kernel_threshold())
            else if (is_dense) {
                if (eps == 0) {
                    exp(colMeans(log(m)))
                } else {
                    exp(colMeans(log(m + eps))) - eps
                }
            } else return(NULL)
        },
        Median = if (is_dg)
            kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                axis = 1L, q = 0.5, threshold = .dafr_kernel_threshold())
        else if (is_dense) matrixStats::colMedians(m)
        else return(NULL),
        Quantile = {
            q <- .param_quantile_q(params)
            if (is_dg)
                kernel_quantile_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                    axis = 1L, q = q, threshold = .dafr_kernel_threshold())
            # is.numeric(): fast path only for INTSXP/REALSXP; character/logical
            # dense matrices fall through to return(NULL) -> slow path.
            else if (is_dense && is.numeric(m))
                kernel_quantile_dense_cpp(m, axis = 1L, q = q,
                    threshold = .dafr_kernel_threshold())
            else return(NULL)
        },
        Mode = if (is_dg && is.numeric(m@x))
            kernel_mode_csc_cpp(m@x, m@i, m@p, nrow(m), ncol(m),
                axis = 1L, threshold = .dafr_kernel_threshold())
        else if (is_dense && is.numeric(m))
            kernel_mode_dense_cpp(m, axis = 1L,
                threshold = .dafr_kernel_threshold())
        else return(NULL),
        return(NULL)
    )
    list(
        kind = "vector", axis = state$cols_axis,
        value = setNames(as.numeric(vals), col_names)
    )
}

.apply_reduction_slow <- function(node, state, fn, params, daf) {
    m <- state$value
    if (identical(node$op, "ReduceToColumn")) {
        # ReduceToColumn: collapse across columns within each row -> one value per
        # row, result indexed by rows_axis.  margin = 1L (apply over rows).
        row_names <- rownames(m)
        if (is.null(row_names)) row_names <- format_axis_array(daf, state$rows_axis)
        vals <- apply(m, 1L, function(row) do.call(fn, c(list(row), params)))
        return(list(
            kind = "vector", axis = state$rows_axis,
            value = setNames(vals, row_names)
        ))
    }
    # ReduceToRow: collapse across rows within each column -> one value per
    # column, result indexed by cols_axis.  margin = 2L (apply over columns).
    col_names <- colnames(m)
    if (is.null(col_names)) col_names <- format_axis_array(daf, state$cols_axis)
    vals <- apply(m, 2L, function(col) do.call(fn, c(list(col), params)))
    list(
        kind = "vector", axis = state$cols_axis,
        value = setNames(vals, col_names)
    )
}

# --- Grouped-reduction helpers ---------------------------------------------
#
# Shared between .apply_reduction_grouped_vector / _matrix for builtin-fast-path
# dispatch and type-sniffing fallback.

# Returns the builtin label iff fn is one of the known grouped-capable ops
# (Sum/Mean/Min/Max/Var/Std/VarN/StdN/GeoMean/Median/Quantile/Mode); else NA.
.reduction_builtin_label <- function(fn) {
    lbl <- attr(fn, ".dafr_builtin")
    if (is.null(lbl)) return(NA_character_)
    if (lbl %in% c("Sum", "Mean", "Min", "Max", "Var", "Std", "VarN", "StdN",
                   "Median", "Quantile", "GeoMean", "Mode")) lbl else NA_character_
}

# Probe the output storage mode of a custom reduction by calling it once on a
# sample slice. Used by the fallback path to size the result vector/matrix.
.grouped_proto_storage <- function(fn, sample_arg, params) {
    out <- do.call(fn, c(list(sample_arg), params))
    if (length(out) != 1L) {
        stop("grouped reduction function must return a scalar",
            call. = FALSE)
    }
    storage.mode(out)
}

.grouped_allocate_output <- function(mode, n) {
    switch(mode,
        double    = numeric(n),
        integer   = integer(n),
        logical   = logical(n),
        character = character(n),
        stop(sprintf("unsupported grouped reduction output storage mode: %s",
            mode), call. = FALSE)
    )
}

# Mode on character vector: first-encountered tiebreak (matches .op_mode).
.grouped_mode_character <- function(x, group, ngroups) {
    out <- character(ngroups)
    for (g_i in seq_len(ngroups)) {
        vals <- x[group == g_i]
        if (length(vals) == 0L) {
            out[g_i] <- NA_character_
            next
        }
        ux <- unique(vals)
        out[g_i] <- ux[which.max(tabulate(match(vals, ux)))]
    }
    out
}

# G1 builtin fast-path: compute per-group reduction for a numeric vector.
.grouped_vector_builtin <- function(x, group, ngroups, label, params) {
    n_in_group <- as.integer(tabulate(group, ngroups))
    if (label %in% c("Sum", "Mean", "Min", "Max", "Var", "Std",
                     "VarN", "StdN", "GeoMean")) {
        eps <- if (label %in% c("VarN", "StdN", "GeoMean"))
            .param_eps(params) else 0
        sum_x  <- as.numeric(rowsum(x, group))
        means  <- sum_x / n_in_group
        switch(label,
            Sum  = sum_x,
            Mean = means,
            Min  = as.numeric(vapply(split(x, group), min, numeric(1))),
            Max  = as.numeric(vapply(split(x, group), max, numeric(1))),
            Var  = {
                sum_x2 <- as.numeric(rowsum(x * x, group))
                pmax(sum_x2 / n_in_group - means^2, 0)
            },
            Std = {
                sum_x2 <- as.numeric(rowsum(x * x, group))
                sqrt(pmax(sum_x2 / n_in_group - means^2, 0))
            },
            VarN = {
                sum_x2 <- as.numeric(rowsum(x * x, group))
                v <- pmax(sum_x2 / n_in_group - means^2, 0)
                v / (means + eps)
            },
            StdN = {
                sum_x2 <- as.numeric(rowsum(x * x, group))
                v <- pmax(sum_x2 / n_in_group - means^2, 0)
                sqrt(v) / (means + eps)
            },
            GeoMean = {
                if (eps == 0) {
                    log_sum <- as.numeric(rowsum(log(x), group))
                    exp(log_sum / n_in_group)
                } else {
                    log_sum <- as.numeric(rowsum(log(x + eps), group))
                    exp(log_sum / n_in_group) - eps
                }
            })
    } else if (label == "Median" || label == "Quantile") {
        q <- if (label == "Median") 0.5 else .param_quantile_q(params)
        vapply(split(x, group), function(v)
            stats::quantile(v, q, type = 7L, names = FALSE),
            numeric(1))
    } else if (label == "Mode") {
        vapply(split(x, group),
            function(v) as.numeric(.op_mode(v)), numeric(1))
    } else {
        stop("unreachable", call. = FALSE)
    }
}

.apply_reduction_grouped_vector <- function(node, state, daf) {
    fn <- get_reduction(node$reduction)
    params <- .coerce_params(node$params)
    x <- state$value
    g <- state$pending_groups
    # If `=@` was applied to the chained group labels (grouped_as_axis set
    # to a real axis name), the result should expose every entry of the
    # target axis - including groups with no members, which the IfMissing
    # default fills in. We still reduce over the SEEN groups for the fast
    # path, then expand the result vector at the end.
    axis_levels <- NULL
    if (!is.null(state$grouped_as_axis)) {
        target_axis <- state$pending_groups_axis
        if (is.character(state$grouped_as_axis)) {
            target_axis <- state$grouped_as_axis
        }
        if (!is.null(target_axis) && format_has_axis(daf, target_axis)) {
            axis_levels <- format_axis_array(daf, target_axis)
        }
    }
    # Preserve group first-appearance order via factor levels.
    gfac <- factor(g, levels = unique(g))
    gi <- as.integer(gfac)
    ngroups <- nlevels(gfac)
    lvls <- levels(gfac)

    label <- .reduction_builtin_label(fn)

    # Helper: take a length-ngroups result vector, expand to the full target
    # axis (when grouped_as_axis was set), and substitute the IfMissing
    # default for any entries that are missing/NA/NaN.
    finalize_vals <- function(vals, names_seen) {
        if (!is.null(axis_levels)) {
            full <- rep(NA, length(axis_levels))
            mode(full) <- mode(vals)
            idx_in_full <- match(names_seen, axis_levels)
            full[idx_in_full] <- vals
            vals <- full
            names_seen <- axis_levels
        }
        if (!is.null(state$if_missing)) {
            nan_mask <- is.na(vals) | (is.numeric(vals) & is.nan(vals))
            if (any(nan_mask)) {
                if (is.numeric(vals)) {
                    vals[nan_mask] <- as.numeric(state$if_missing)
                } else if (is.logical(vals)) {
                    vals[nan_mask] <- as.logical(state$if_missing)
                } else if (is.character(vals)) {
                    vals[nan_mask] <- as.character(state$if_missing)
                }
            }
        }
        list(value = vals, names = names_seen)
    }

    # Mode-on-character dedicated R helper (no C kernel; char Mode is rare).
    if (!is.na(label) && label == "Mode" && is.character(x)) {
        vals <- .grouped_mode_character(x, gi, ngroups)
        fin <- finalize_vals(vals, lvls)
        return(list(kind = "vector", axis = NULL,
            value = stats::setNames(fin$value, fin$names)))
    }

    # Numeric builtin fast path.
    if (!is.na(label) && is.numeric(x)) {
        vals <- .grouped_vector_builtin(x, gi, ngroups, label, params)
        fin <- finalize_vals(vals, lvls)
        return(list(kind = "vector", axis = NULL,
            value = stats::setNames(fin$value, fin$names)))
    }

    # Fallback: type-sniffed split+loop (replaces the old vapply(..., numeric(1))).
    idx <- split(seq_along(x), gi)
    # idx is named by levels; reorder to match seq_len(ngroups).
    idx <- idx[as.character(seq_len(ngroups))]
    valid <- which(lengths(idx) > 0L)
    if (length(valid) == 0L) {
        return(list(kind = "vector", axis = NULL,
            value = stats::setNames(numeric(ngroups), lvls)))
    }
    mode <- .grouped_proto_storage(fn, x[idx[[valid[1L]]]], params)
    out <- .grouped_allocate_output(mode, ngroups)
    for (g_i in seq_len(ngroups)) {
        if (length(idx[[g_i]]) == 0L) {
            out[g_i] <- switch(mode,
                double = NA_real_, integer = NA_integer_,
                logical = NA, character = NA_character_)
            next
        }
        out[g_i] <- do.call(fn, c(list(x[idx[[g_i]]]), params))
    }
    fin <- finalize_vals(out, lvls)
    list(kind = "vector", axis = NULL,
        value = stats::setNames(fin$value, fin$names))
}

# G2/G3 builtin fast-path: dispatch to appropriate C++ kernel or dense fallback.
.grouped_matrix_builtin <- function(m, gi, ngroups, n_in_group,
                                    axis, label, params) {
    is_dg <- methods::is(m, "dgCMatrix")

    # Median/Quantile: sparse kernel, dense via matrixStats
    if (label == "Median" || label == "Quantile") {
        q <- if (label == "Median") 0.5 else .param_quantile_q(params)
        if (is_dg) {
            return(kernel_grouped_quantile_csc_cpp(
                m@x, m@i, m@p, nrow(m), ncol(m),
                group = gi, ngroups = ngroups,
                n_in_group = n_in_group, axis = axis, q = q,
                threshold = .dafr_kernel_threshold()))
        }
        idx <- split(seq_along(gi), gi)
        idx <- idx[as.character(seq_len(ngroups))]
        if (axis == 2L) {
            out <- matrix(0, ngroups, ncol(m))
            for (g in seq_len(ngroups)) {
                if (length(idx[[g]]) == 0L) next
                sub <- m[idx[[g]], , drop = FALSE]
                out[g, ] <- matrixStats::colQuantiles(sub, probs = q,
                    type = 7L, useNames = FALSE)
            }
            return(out)
        }
        out <- matrix(0, nrow(m), ngroups)
        for (g in seq_len(ngroups)) {
            if (length(idx[[g]]) == 0L) next
            sub <- m[, idx[[g]], drop = FALSE]
            out[, g] <- matrixStats::rowQuantiles(sub, probs = q,
                type = 7L, useNames = FALSE)
        }
        return(out)
    }

    if (label == "Mode") {
        if (is_dg) {
            return(kernel_grouped_mode_csc_cpp(
                m@x, m@i, m@p, nrow(m), ncol(m),
                group = gi, ngroups = ngroups,
                n_in_group = n_in_group, axis = axis,
                threshold = .dafr_kernel_threshold()))
        }
        idx <- split(seq_along(gi), gi)
        idx <- idx[as.character(seq_len(ngroups))]
        if (axis == 2L) {
            out <- matrix(0, ngroups, ncol(m))
            for (g in seq_len(ngroups)) {
                if (length(idx[[g]]) == 0L) next
                sub <- m[idx[[g]], , drop = FALSE]
                out[g, ] <- apply(sub, 2L,
                    function(v) .op_mode(v))
            }
            return(out)
        }
        out <- matrix(0, nrow(m), ngroups)
        for (g in seq_len(ngroups)) {
            if (length(idx[[g]]) == 0L) next
            sub <- m[, idx[[g]], drop = FALSE]
            out[, g] <- apply(sub, 1L,
                function(v) .op_mode(v))
        }
        return(out)
    }

    # Shared ops: Sum/Mean/Min/Max/Var/Std/VarN/StdN/GeoMean.
    eps <- if (label %in% c("VarN", "StdN", "GeoMean"))
        .param_eps(params) else 0
    if (is_dg) {
        kernel_grouped_reduce_csc_cpp(
            m@x, m@i, m@p, nrow(m), ncol(m),
            group = gi, ngroups = ngroups,
            n_in_group = n_in_group, axis = axis,
            op = label, eps = eps,
            threshold = .dafr_kernel_threshold())
    } else {
        # Dense fast path: C++ grouped-rowsum kernel for all ops except GeoMean.
        # axis == 2 (G2, row-grouped): kernel output -> ngroups x ncol
        # axis == 3 (G3, col-grouped): kernel output -> nrow x ngroups
        # The kernel accepts INTSXP or REALSXP directly (no storage.mode copy).
        .grouped_dense_rowsum(m, gi, ngroups, n_in_group, axis, label, eps)
    }
}

# Scrub +/-Inf sentinels from empty groups in kernel_grouped_minmax_dense output.
# axis = 2L: output is ngroups x ncol -> zero-count groups = NA rows.
# axis = 3L: output is nrow x ngroups -> zero-count groups = NA cols.
.minmax_empty_to_na <- function(out, gi, ngroups, axis) {
    empty <- tabulate(gi, ngroups) == 0L
    if (!any(empty)) return(out)
    if (axis == 2L) {
        out[empty, ] <- NA_real_
    } else {
        out[, empty] <- NA_real_
    }
    out
}

# Dense grouped reduction via rowsum() / matrixStats.
# Called by .grouped_matrix_builtin for non-sparse, non-GeoMean ops.
.grouped_dense_rowsum <- function(m, gi, ngroups, n_in_group, axis, label, eps) {
    # n_in_group: integer vector length ngroups, count of obs per group.
    # axis 2 (G2): rows of m are grouped -> output ngroups x ncol(m)
    # axis 3 (G3): cols of m are grouped -> output nrow(m) x ngroups
    if (label == "GeoMean") {
        # GeoMean requires log; delegate to C++ dense kernel.
        return(kernel_grouped_reduce_dense_cpp(
            m, group = gi, ngroups = ngroups,
            n_in_group = n_in_group, axis = axis,
            op = label, eps = eps,
            threshold = .dafr_kernel_threshold()))
    }
    # need_sq: whether Var/Std/VarN/StdN need sum-of-squares.
    need_sq <- label %in% c("Var", "Std", "VarN", "StdN")
    # For Sum/Mean/Var/Std/VarN/StdN use the new Int-aware C++ kernel.
    use_kernel <- label %in% c("Sum", "Mean", "Var", "Std", "VarN", "StdN")

    if (axis == 2L) {
        # G2: groups along rows.  Output: ngroups x ncol.
        if (use_kernel) {
            res <- kernel_grouped_rowsum_dense_cpp(m, gi, ngroups, need_sq, axis,
                threshold = .dafr_kernel_threshold())
            rs  <- res$sum
            return(switch(label,
                Sum  = rs,
                Mean = rs / n_in_group,
                {
                    n   <- n_in_group
                    rs2 <- res$sq
                    mu  <- rs / n
                    v   <- pmax(rs2 / n - mu * mu, 0)
                    switch(label,
                        Var  = v,
                        Std  = sqrt(v),
                        VarN = v / (mu + eps),
                        StdN = sqrt(v) / (mu + eps)
                    )
                }
            ))
        }
        switch(label,
            Max = .minmax_empty_to_na(
                kernel_grouped_minmax_dense_cpp(m, groups = gi, ngroups = ngroups,
                    axis = 2L, variant = 1L,
                    threshold = .dafr_kernel_threshold()),
                gi, ngroups, axis = 2L),
            Min = .minmax_empty_to_na(
                kernel_grouped_minmax_dense_cpp(m, groups = gi, ngroups = ngroups,
                    axis = 2L, variant = 0L,
                    threshold = .dafr_kernel_threshold()),
                gi, ngroups, axis = 2L),
            # Fallback for unknown ops
            kernel_grouped_reduce_dense_cpp(
                m, group = gi, ngroups = ngroups,
                n_in_group = n_in_group, axis = axis,
                op = label, eps = eps,
                threshold = .dafr_kernel_threshold())
        )
    } else {
        # G3 (axis == 3): groups along cols.  Output: nrow(m) x ngroups.
        if (use_kernel) {
            res <- kernel_grouped_rowsum_dense_cpp(m, gi, ngroups, need_sq, axis,
                threshold = .dafr_kernel_threshold())
            rs  <- res$sum
            return(switch(label,
                Sum  = rs,
                Mean = t(t(rs) / n_in_group),
                {
                    n   <- n_in_group
                    rs2 <- res$sq
                    mu  <- t(t(rs) / n)
                    v   <- pmax(t(t(rs2) / n) - mu * mu, 0)
                    switch(label,
                        Var  = v,
                        Std  = sqrt(v),
                        VarN = v / (mu + eps),
                        StdN = sqrt(v) / (mu + eps)
                    )
                }
            ))
        }
        switch(label,
            Max = .minmax_empty_to_na(
                kernel_grouped_minmax_dense_cpp(m, groups = gi, ngroups = ngroups,
                    axis = 3L, variant = 1L,
                    threshold = .dafr_kernel_threshold()),
                gi, ngroups, axis = 3L),
            Min = .minmax_empty_to_na(
                kernel_grouped_minmax_dense_cpp(m, groups = gi, ngroups = ngroups,
                    axis = 3L, variant = 0L,
                    threshold = .dafr_kernel_threshold()),
                gi, ngroups, axis = 3L),
            # Fallback for unknown ops
            kernel_grouped_reduce_dense_cpp(
                m, group = gi, ngroups = ngroups,
                n_in_group = n_in_group, axis = axis,
                op = label, eps = eps,
                threshold = .dafr_kernel_threshold())
        )
    }
}

# Build a 0/1 indicator matrix (ncols x ngroups) for the G3 BLAS path.
# Entry [j, g] = 1 if gi[j] == g (gi is 1-based group assignment of length ncols).
.g3_indicator <- function(gi, ncols, ngroups) {
    ind <- matrix(0, ncols, ngroups)
    ind[cbind(seq_len(ncols), gi)] <- 1.0
    ind
}

# G2/G3 fallback: type-sniffed split+apply (replaces old sapply+apply path
# which silently produced wrong shapes on unequal group sizes).
.apply_reduction_grouped_matrix_fallback <- function(m, gi, ngroups,
                                                     node, fn, params,
                                                     by, state, daf) {
    idx <- split(seq_along(gi), gi)
    idx <- idx[as.character(seq_len(ngroups))]
    valid <- which(lengths(idx) > 0L)
    if (length(valid) == 0L) {
        stop("no valid groups in grouped-matrix fallback path",
            call. = FALSE)
    }

    if (identical(by, "rows") && identical(node$op, "ReduceToRow")) {
        # G2 fallback: by="rows", op="ReduceToRow" -> ngroups x ncol.
        sub0 <- m[idx[[valid[1L]]], , drop = FALSE]
        sample_out <- apply(sub0, 2L,
            function(col) do.call(fn, c(list(col), params)))
        mode <- storage.mode(sample_out)
        out <- matrix(.grouped_allocate_output(mode, 1L),
            nrow = ngroups, ncol = ncol(m))
        storage.mode(out) <- mode
        for (g in seq_len(ngroups)) {
            if (length(idx[[g]]) == 0L) next
            sub <- m[idx[[g]], , drop = FALSE]
            out[g, ] <- apply(sub, 2L,
                function(col) do.call(fn, c(list(col), params)))
        }
        return(out)
    }

    # G3 fallback: by="cols", op="ReduceToColumn" -> nrow x ngroups.
    sub0 <- m[, idx[[valid[1L]]], drop = FALSE]
    sample_out <- apply(sub0, 1L,
        function(row) do.call(fn, c(list(row), params)))
    mode <- storage.mode(sample_out)
    out <- matrix(.grouped_allocate_output(mode, 1L),
        nrow = nrow(m), ncol = ngroups)
    storage.mode(out) <- mode
    for (g in seq_len(ngroups)) {
        if (length(idx[[g]]) == 0L) next
        sub <- m[, idx[[g]], drop = FALSE]
        out[, g] <- apply(sub, 1L,
            function(row) do.call(fn, c(list(row), params)))
    }
    out
}

.apply_reduction_grouped_matrix <- function(node, state, daf, by) {
    fn <- get_reduction(node$reduction)
    params <- .coerce_params(node$params)
    m <- state$value
    label <- .reduction_builtin_label(fn)

    is_g2 <- identical(by, "rows") && identical(node$op, "ReduceToRow")
    is_g3 <- identical(by, "cols") && identical(node$op, "ReduceToColumn")
    is_g4a <- identical(by, "rows") && identical(node$op, "ReduceToColumn")
    is_g4b <- identical(by, "cols") && identical(node$op, "ReduceToRow")

    if (is_g2 || is_g3) {
        g <- if (is_g2) state$pending_row_groups else state$pending_col_groups
        gfac <- factor(g, levels = unique(g))
        gi <- as.integer(gfac)
        ngroups <- nlevels(gfac)
        lvls <- levels(gfac)
        n_in_group <- as.integer(tabulate(gi, ngroups))

        # lgCMatrix (and other non-dg sparse) cannot feed the numeric kernels;
        # route those through the fallback.
        is_dg <- methods::is(m, "dgCMatrix")
        is_dense <- is.matrix(m)
        kernel_ok <- is_dg || is_dense
        if (!is.na(label) && kernel_ok) {
            axis <- if (is_g2) 2L else 3L
            out <- .grouped_matrix_builtin(m, gi, ngroups, n_in_group,
                axis, label, params)
        } else {
            out <- .apply_reduction_grouped_matrix_fallback(
                m, gi, ngroups, node, fn, params, by, state, daf)
        }

        # Assign dimnames: group axis gets level labels, other axis inherits
        # from m if named, else from the daf axis array.
        if (is_g2) {
            rn <- lvls
            cn <- if (!is.null(colnames(m))) colnames(m)
                  else format_axis_array(daf, state$cols_axis)
            dimnames(out) <- list(rn, cn)
            return(list(kind = "matrix", value = out,
                rows_axis = NULL, cols_axis = state$cols_axis))
        }
        # G3
        rn <- if (!is.null(rownames(m))) rownames(m)
              else format_axis_array(daf, state$rows_axis)
        cn <- lvls
        dimnames(out) <- list(rn, cn)
        return(list(kind = "matrix", value = out,
            rows_axis = state$rows_axis, cols_axis = NULL))
    }

    if (is_g4a || is_g4b) {
        # G4: decompose as (inner G2 or G3 producing a matrix) + per-group
        # vector reduction across the ungrouped axis. Equivalent to the old
        # double-apply logic but sidesteps DSL recursion issues.
        if (is_g4a) {
            # Row-grouped + ReduceToColumn: G2 gives ngroups x ncol, then reduce
            # each row (across cols) to a scalar per group.
            inner_by <- "rows"
            inner_op <- "ReduceToRow"
            inner_state <- state
            inner_node <- list(op = inner_op,
                reduction = node$reduction, params = node$params)
            inner <- .apply_reduction_grouped_matrix(inner_node, inner_state,
                daf, by = inner_by)
            # inner$value is ngroups x ncol with rownames = group levels.
            mat <- inner$value
            g <- state$pending_row_groups
            gfac <- factor(g, levels = unique(g))
            lvls <- levels(gfac)
            ngroups <- nlevels(gfac)
            # Type-sniff on the first group's row so character/logical/integer
            # user reductions are preserved (vapply(numeric(1)) would coerce).
            sample_row <- mat[1L, , drop = TRUE]
            mode <- .grouped_proto_storage(fn, sample_row, params)
            out <- .grouped_allocate_output(mode, ngroups)
            for (g_i in seq_len(ngroups)) {
                out[g_i] <- do.call(fn,
                    c(list(mat[g_i, , drop = TRUE]), params))
            }
            names(out) <- lvls
            return(list(kind = "vector", axis = NULL, value = out))
        }
        # G4b: col-grouped + ReduceToRow: G3 gives nrow x ngroups, then
        # reduce each column (across rows) to a scalar per group.
        inner_node <- list(op = "ReduceToColumn",
            reduction = node$reduction, params = node$params)
        inner <- .apply_reduction_grouped_matrix(inner_node, state, daf,
            by = "cols")
        mat <- inner$value
        g <- state$pending_col_groups
        gfac <- factor(g, levels = unique(g))
        lvls <- levels(gfac)
        ngroups <- nlevels(gfac)
        # Type-sniff on the first group's column so character/logical/integer
        # user reductions are preserved (vapply(numeric(1)) would coerce).
        sample_col <- mat[, 1L, drop = TRUE]
        mode <- .grouped_proto_storage(fn, sample_col, params)
        out <- .grouped_allocate_output(mode, ngroups)
        for (g_i in seq_len(ngroups)) {
            out[g_i] <- do.call(fn,
                c(list(mat[, g_i, drop = TRUE]), params))
        }
        names(out) <- lvls
        return(list(kind = "vector", axis = NULL, value = out))
    }

    stop(sprintf("unsupported grouped matrix pattern: op=%s by=%s",
        node$op, by), call. = FALSE)
}

.coerce_params <- function(params) {
    # try numeric coercion for each value; fall back to string
    lapply(params, function(v) {
        n <- suppressWarnings(as.numeric(v))
        if (!is.na(n)) n else v
    })
}
.apply_groupby <- function(node, state, daf) {
    switch(node$op,
        GroupBy = .apply_groupby_vector(node, state, daf),
        GroupRowsBy = .apply_groupby_rows(node, state, daf),
        GroupColumnsBy = .apply_groupby_columns(node, state, daf),
        stop(sprintf("unknown grouping op: %s", node$op), call. = FALSE)
    )
}

# Resolve `vec / matrix-prop @ cols-axis = entry` into a group-label
# vector and switch to "grouped_vector". The column slice's per-row
# values become the group labels for the in-scope vector. Subsequent
# `?? : prop` chains and `>> Op` reductions then work as for the
# vector-property GroupBy path.
.apply_groupby_vector_matrix_entry <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix-column GroupBy on axis %s expects '@ %s = <entry>', got %s",
            sQuote(state$matrix_cols_axis),
            sQuote(state$matrix_cols_axis),
            sQuote(node$op)
        ), call. = FALSE)
    }
    rows <- state$axis
    cols <- state$matrix_cols_axis
    prop <- state$matrix_property
    cols_arr <- format_axis_array(daf, cols)
    col_idx <- match(as.character(node$value), cols_arr)
    if (is.na(col_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(cols)
        ), call. = FALSE)
    }
    grp <- if (format_has_matrix(daf, rows, cols, prop)) {
        format_get_matrix(daf, rows, cols, prop)[, col_idx, drop = TRUE]
    } else if (format_has_matrix(daf, cols, rows, prop)) {
        format_get_matrix(daf, cols, rows, prop)[col_idx, , drop = TRUE]
    } else {
        stop(sprintf(
            "no matrix %s on axes %s, %s",
            sQuote(prop), sQuote(rows), sQuote(cols)
        ), call. = FALSE)
    }
    if (methods::is(grp, "sparseVector") || methods::is(grp, "Matrix")) {
        grp <- as.numeric(grp)
    }
    if (!is.null(state$indices)) {
        grp <- grp[state$indices]
    }
    state$pending_groups <- grp
    state$pending_groups_axis <- prop
    state$matrix_property <- NULL
    state$matrix_cols_axis <- NULL
    state$kind <- "grouped_vector"
    state
}

# `vec / square-matrix @| entry` and `vec / ... @- entry`.
.apply_groupby_vector_matrix_square_slice <- function(node, state, daf) {
    rows <- state$axis
    prop <- state$matrix_property
    if (!format_has_matrix(daf, rows, rows, prop)) {
        stop(sprintf(
            "no square matrix %s on axis %s",
            sQuote(prop), sQuote(rows)
        ), call. = FALSE)
    }
    rows_arr <- format_axis_array(daf, rows)
    idx <- match(as.character(node$value), rows_arr)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(rows)
        ), call. = FALSE)
    }
    m <- format_get_matrix(daf, rows, rows, prop)
    grp <- if (identical(node$op, "SquareRowIs")) {
        m[idx, , drop = TRUE]
    } else {
        m[, idx, drop = TRUE]
    }
    if (methods::is(grp, "sparseVector") || methods::is(grp, "Matrix")) {
        grp <- as.numeric(grp)
    }
    if (!is.null(state$indices)) {
        grp <- grp[state$indices]
    }
    state$pending_groups <- grp
    state$pending_groups_axis <- prop
    state$matrix_property <- NULL
    state$kind <- "grouped_vector"
    state
}

# Helpers for `:: m -/ matrix-prop @ axis = entry` and `... |/ ...`. The
# row (or column) groups come from a matrix-column slice instead of a
# vector property. Both shapes share the same lookup logic; the only
# difference is which axis is the group axis (rows_axis vs cols_axis)
# and which slot the group labels land in.
.matrix_slice_for_group <- function(state, node, daf, group_axis_name) {
    cols <- state$matrix_cols_axis
    prop <- state$matrix_property
    rows <- group_axis_name
    cols_arr <- format_axis_array(daf, cols)
    col_idx <- match(as.character(node$value), cols_arr)
    if (is.na(col_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(cols)
        ), call. = FALSE)
    }
    grp <- if (format_has_matrix(daf, rows, cols, prop)) {
        format_get_matrix(daf, rows, cols, prop)[, col_idx, drop = TRUE]
    } else if (format_has_matrix(daf, cols, rows, prop)) {
        format_get_matrix(daf, cols, rows, prop)[col_idx, , drop = TRUE]
    } else {
        stop(sprintf(
            "no matrix %s on axes %s, %s",
            sQuote(prop), sQuote(rows), sQuote(cols)
        ), call. = FALSE)
    }
    if (methods::is(grp, "sparseVector") || methods::is(grp, "Matrix")) {
        grp <- as.numeric(grp)
    }
    grp
}

.matrix_square_slice_for_group <- function(state, node, daf, group_axis_name) {
    rows <- group_axis_name
    prop <- state$matrix_property
    if (!format_has_matrix(daf, rows, rows, prop)) {
        stop(sprintf(
            "no square matrix %s on axis %s",
            sQuote(prop), sQuote(rows)
        ), call. = FALSE)
    }
    rows_arr <- format_axis_array(daf, rows)
    idx <- match(as.character(node$value), rows_arr)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(rows)
        ), call. = FALSE)
    }
    m <- format_get_matrix(daf, rows, rows, prop)
    grp <- if (identical(node$op, "SquareRowIs")) {
        m[idx, , drop = TRUE]
    } else {
        m[, idx, drop = TRUE]
    }
    if (methods::is(grp, "sparseVector") || methods::is(grp, "Matrix")) {
        grp <- as.numeric(grp)
    }
    grp
}

.apply_groupby_matrix_rows_matrix_entry <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix-column GroupRowsBy on axis %s expects '@ %s = <entry>', got %s",
            sQuote(state$matrix_cols_axis),
            sQuote(state$matrix_cols_axis),
            sQuote(node$op)
        ), call. = FALSE)
    }
    grp <- .matrix_slice_for_group(state, node, daf, state$rows_axis)
    state$pending_row_groups <- grp
    state$matrix_property <- NULL
    state$matrix_cols_axis <- NULL
    state$kind <- "grouped_matrix_rows"
    state
}

.apply_groupby_matrix_rows_matrix_square_slice <- function(node, state, daf) {
    grp <- .matrix_square_slice_for_group(state, node, daf, state$rows_axis)
    state$pending_row_groups <- grp
    state$matrix_property <- NULL
    state$kind <- "grouped_matrix_rows"
    state
}

.apply_groupby_matrix_cols_matrix_entry <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix-column GroupColumnsBy on axis %s expects '@ %s = <entry>', got %s",
            sQuote(state$matrix_cols_axis),
            sQuote(state$matrix_cols_axis),
            sQuote(node$op)
        ), call. = FALSE)
    }
    grp <- .matrix_slice_for_group(state, node, daf, state$cols_axis)
    state$pending_col_groups <- grp
    state$matrix_property <- NULL
    state$matrix_cols_axis <- NULL
    state$kind <- "grouped_matrix_cols"
    state
}

.apply_groupby_matrix_cols_matrix_square_slice <- function(node, state, daf) {
    grp <- .matrix_square_slice_for_group(state, node, daf, state$cols_axis)
    state$pending_col_groups <- grp
    state$matrix_property <- NULL
    state$kind <- "grouped_matrix_cols"
    state
}

# Resolve `:: m1 :: m2 @ cols-axis = entry` (Julia
# lookup_matrix_column_by_matrix). For every cell `v` of m1, fetch
# m2[v, entry]. Result keeps m1's shape but holds m2-property values.
.apply_matrix_chain_matrix_entry <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix-chain matrix slice on axis %s expects '@ %s = <entry>', got %s",
            sQuote(state$matrix2_cols_axis),
            sQuote(state$matrix2_cols_axis),
            sQuote(node$op)
        ), call. = FALSE)
    }
    cols2_axis <- state$matrix2_cols_axis
    cols2_arr <- format_axis_array(daf, cols2_axis)
    col_idx <- match(as.character(node$value), cols2_arr)
    if (is.na(col_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(cols2_axis)
        ), call. = FALSE)
    }
    target_axis <- state$chain_target_axis
    prop2 <- state$matrix2_property
    # The matrix2 column slice is a vector along target_axis: vec[v] = m2[v, entry].
    slice <- if (format_has_matrix(daf, target_axis, cols2_axis, prop2)) {
        format_get_matrix(daf, target_axis, cols2_axis, prop2)[, col_idx, drop = TRUE]
    } else if (format_has_matrix(daf, cols2_axis, target_axis, prop2)) {
        format_get_matrix(daf, cols2_axis, target_axis, prop2)[col_idx, , drop = TRUE]
    } else {
        stop(sprintf(
            "no matrix %s on axes %s, %s",
            sQuote(prop2), sQuote(target_axis), sQuote(cols2_axis)
        ), call. = FALSE)
    }
    .matrix_chain_apply_slice(state, slice, daf)
}

# Square-matrix variants: `:: m1 :: m2 @| entry` / `... @- entry`.
.apply_matrix_chain_matrix_square_slice <- function(node, state, daf) {
    target_axis <- state$chain_target_axis
    prop2 <- state$matrix2_property
    if (!format_has_matrix(daf, target_axis, target_axis, prop2)) {
        stop(sprintf(
            "no square matrix %s on axis %s",
            sQuote(prop2), sQuote(target_axis)
        ), call. = FALSE)
    }
    target_entries <- format_axis_array(daf, target_axis)
    idx <- match(as.character(node$value), target_entries)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(target_axis)
        ), call. = FALSE)
    }
    m2 <- format_get_matrix(daf, target_axis, target_axis, prop2)
    slice <- if (identical(node$op, "SquareRowIs")) {
        m2[idx, , drop = TRUE]
    } else {
        m2[, idx, drop = TRUE]
    }
    .matrix_chain_apply_slice(state, slice, daf)
}

# Shared finaliser: `slice` is a vector indexed by `chain_target_axis`.
# For every cell of the in-scope matrix (which holds entries of that
# axis), look up `slice[entry]` and put the result in the corresponding
# cell of a same-shape output matrix.
.matrix_chain_apply_slice <- function(state, slice, daf) {
    if (methods::is(slice, "sparseVector") || methods::is(slice, "Matrix")) {
        slice <- as.numeric(slice)
    }
    target_axis <- state$chain_target_axis
    target_entries <- format_axis_array(daf, target_axis)
    m1 <- state$value
    rn <- rownames(m1)
    if (is.null(rn) && !is.null(state$rows_axis)) {
        rn <- format_axis_array(daf, state$rows_axis)
    }
    cn <- colnames(m1)
    if (is.null(cn) && !is.null(state$cols_axis)) {
        cn <- format_axis_array(daf, state$cols_axis)
    }
    flat <- as.character(as.vector(m1))
    idx <- match(flat, target_entries)
    out <- rep(NA, length(flat))
    mode(out) <- mode(slice)
    ok <- !is.na(idx) & nzchar(flat)
    out[ok] <- slice[idx[ok]]
    new_m <- matrix(out, nrow = nrow(m1), ncol = ncol(m1),
        dimnames = list(rn, cn))
    list(
        kind = "matrix",
        value = new_m,
        rows_axis = state$rows_axis,
        cols_axis = state$cols_axis,
        matrix_property = state$matrix2_property
    )
}

.apply_groupby_vector <- function(node, state, daf) {
    if (!identical(state$kind, "vector")) {
        stop("GroupBy requires a vector in scope", call. = FALSE)
    }
    if (!format_has_vector(daf, state$axis, node$property)) {
        # `/ matrix-prop @ axis = entry` (or `@| / @-`) - the group labels
        # come from a matrix-column slice, not a vector. Defer until the
        # column axis arrives. (Julia lookup_vector_group_by_*_matrix_*.)
        state$kind <- "groupby_vector_matrix_pending_axis"
        state$matrix_property <- node$property
        return(state)
    }
    grp <- format_get_vector(daf, state$axis, node$property)
    # If a prior mask narrowed the axis (state$indices set), restrict the
    # group labels to the surviving cells so they line up with state$value.
    if (!is.null(state$indices)) {
        grp <- grp[state$indices]
    }
    state$pending_groups <- grp
    # The property name doubles as the target-axis name when a chained
    # lookup follows (Julia axis_of_property). Stored so a later `: prop`
    # can pivot the group labels through this axis.
    state$pending_groups_axis <- node$property
    state$kind <- "grouped_vector"
    state
}

.apply_groupby_rows <- function(node, state, daf) {
    if (!identical(state$kind, "matrix")) {
        stop("GroupRowsBy requires a matrix in scope", call. = FALSE)
    }
    if (!format_has_vector(daf, state$rows_axis, node$property)) {
        # `:: m -/ matrix-prop @ axis = entry`: row groups come from a
        # matrix-column slice. Defer until the column axis arrives.
        state$kind <- "groupby_matrix_rows_pending_axis"
        state$matrix_property <- node$property
        return(state)
    }
    grp <- format_get_vector(daf, state$rows_axis, node$property)
    state$pending_row_groups <- grp
    state$kind <- "grouped_matrix_rows"
    state
}

.apply_groupby_columns <- function(node, state, daf) {
    if (!identical(state$kind, "matrix")) {
        stop("GroupColumnsBy requires a matrix in scope", call. = FALSE)
    }
    if (!format_has_vector(daf, state$cols_axis, node$property)) {
        # `:: m |/ matrix-prop @ axis = entry`: column groups come from a
        # matrix-column slice on the cols axis. Defer.
        state$kind <- "groupby_matrix_cols_pending_axis"
        state$matrix_property <- node$property
        return(state)
    }
    grp <- format_get_vector(daf, state$cols_axis, node$property)
    state$pending_col_groups <- grp
    state$kind <- "grouped_matrix_cols"
    state
}

# Resolve `: vec * matrix-prop @ cols-axis = entry` into a deferred
# count-matrix state: the b-side per-cell vector is the matrix-column
# slice. Subsequent `: prop` chains pivot it the same way as the plain
# count-by, and end-of-evaluation materialises the count matrix.
.apply_countby_matrix_entry <- function(node, state, daf) {
    if (!identical(node$op, "IsEqual")) {
        stop(sprintf(
            "matrix-column CountBy on axis %s expects '@ %s = <entry>', got %s",
            sQuote(state$matrix_cols_axis),
            sQuote(state$matrix_cols_axis),
            sQuote(node$op)
        ), call. = FALSE)
    }
    rows <- state$axis
    cols <- state$matrix_cols_axis
    prop <- state$matrix_property
    cols_arr <- format_axis_array(daf, cols)
    col_idx <- match(as.character(node$value), cols_arr)
    if (is.na(col_idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(cols)
        ), call. = FALSE)
    }
    b <- if (format_has_matrix(daf, rows, cols, prop)) {
        format_get_matrix(daf, rows, cols, prop)[, col_idx, drop = TRUE]
    } else if (format_has_matrix(daf, cols, rows, prop)) {
        format_get_matrix(daf, cols, rows, prop)[col_idx, , drop = TRUE]
    } else {
        stop(sprintf(
            "no matrix %s on axes %s, %s",
            sQuote(prop), sQuote(rows), sQuote(cols)
        ), call. = FALSE)
    }
    if (methods::is(b, "sparseVector") || methods::is(b, "Matrix")) {
        b <- as.numeric(b)
    }
    list(
        kind = "pending_count",
        a_per_cell = state$value,
        b_per_cell = b,
        a_axis_label = state$property %||% state$axis,
        b_axis_label = prop,
        b_pivot_axis = prop
    )
}

# `: vec * square-matrix @| entry` and `... @- entry`.
.apply_countby_matrix_square_slice <- function(node, state, daf) {
    rows <- state$axis
    prop <- state$matrix_property
    if (!format_has_matrix(daf, rows, rows, prop)) {
        stop(sprintf(
            "no square matrix %s on axis %s",
            sQuote(prop), sQuote(rows)
        ), call. = FALSE)
    }
    rows_arr <- format_axis_array(daf, rows)
    idx <- match(as.character(node$value), rows_arr)
    if (is.na(idx)) {
        stop(sprintf(
            "no entry %s on axis %s",
            sQuote(node$value), sQuote(rows)
        ), call. = FALSE)
    }
    m <- format_get_matrix(daf, rows, rows, prop)
    b <- if (identical(node$op, "SquareRowIs")) {
        m[idx, , drop = TRUE]
    } else {
        m[, idx, drop = TRUE]
    }
    if (methods::is(b, "sparseVector") || methods::is(b, "Matrix")) {
        b <- as.numeric(b)
    }
    list(
        kind = "pending_count",
        a_per_cell = state$value,
        b_per_cell = b,
        a_axis_label = state$property %||% state$axis,
        b_axis_label = prop,
        b_pivot_axis = prop
    )
}

.apply_countby <- function(node, state, daf) {
    if (identical(state$kind, "vector_axis")) {
        # `=@` immediately before `*` is a no-op for count-by - the property
        # axis is already implicit. Reset to plain vector so the count-by
        # path picks it up.
        state$kind <- "vector"
    }
    if (!identical(state$kind, "vector")) {
        stop("* CountBy requires a vector in scope", call. = FALSE)
    }
    if (!format_has_vector(daf, state$axis, node$property)) {
        # `: vec * matrix-prop @ axis = entry` (or `@| / @-`): the b-side
        # of the count comes from a matrix-column slice. Defer until the
        # column axis arrives. (Julia lookup_matrix_column_count and the
        # square_matrix_*_count phrases.)
        state$kind <- "countby_matrix_pending_axis"
        state$matrix_property <- node$property
        return(state)
    }
    a <- state$value
    b <- format_get_vector(daf, state$axis, node$property)
    # Defer matrix materialisation so a subsequent `: prop` can pivot the
    # b column through its property's axis (matching Julia's
    # lookup_vector_by_vector + compute_count_matrix sequence).
    list(
        kind = "pending_count",
        a_per_cell = a,
        b_per_cell = b,
        a_axis_label = state$property %||% state$axis,
        b_axis_label = node$property,
        b_pivot_axis = node$property
    )
}

# Chain a `: prop` on the b-column of a pending_count: replace b_per_cell
# with the property's value for each cell, pivoted through b_pivot_axis.
.apply_chained_lookup_count <- function(node, state, daf) {
    target_axis <- state$b_pivot_axis
    if (is.null(target_axis)) {
        stop("internal: pending_count missing b_pivot_axis", call. = FALSE)
    }
    if (!format_has_axis(daf, target_axis)) {
        stop(sprintf(
            "chain target axis %s does not exist", sQuote(target_axis)
        ), call. = FALSE)
    }
    if (!format_has_vector(daf, target_axis, node$name)) {
        stop(sprintf(
            "no vector %s on axis %s",
            sQuote(node$name), sQuote(target_axis)
        ), call. = FALSE)
    }
    pivot <- state$b_per_cell
    target_entries <- format_axis_array(daf, target_axis)
    idx <- match(pivot, target_entries)
    lookup_vec <- format_get_vector(daf, target_axis, node$name)
    new_b <- rep(NA, length(pivot))
    mode(new_b) <- mode(lookup_vec)
    ok <- !is.na(idx)
    new_b[ok] <- lookup_vec[idx[ok]]
    state$b_per_cell <- new_b
    state$b_axis_label <- node$name
    state$b_pivot_axis <- node$name
    state
}

# Build a count matrix from the pending pair of per-cell vectors. Used at
# end of evaluation if the count-by wasn't followed by something that
# materialises it (e.g. a reduction on the resulting matrix).
.finalize_pending_count <- function(state, daf) {
    a <- state$a_per_cell
    b <- state$b_per_cell
    t <- table(a, b)
    m <- as.matrix(t)
    list(
        kind = "matrix",
        value = m,
        rows_axis = state$a_axis_label,
        cols_axis = state$b_axis_label
    )
}
