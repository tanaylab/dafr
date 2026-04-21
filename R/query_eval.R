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
            state$if_missing <- NULL # consume; don't carry forward past this lookup
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
        ReduceToRow = .apply_reduction,
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
    if (identical(state$kind, "axis")) {
        # second axis -> matrix dimension in scope
        state$kind <- "two_axes"
        state$rows_axis <- state$axis
        state$cols_axis <- node$axis_name
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
            return(list(kind = "scalar", value = state$if_missing))
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
    if (!format_has_vector(daf, axis, node$name)) {
        if (!is.null(state$if_missing)) {
            return(list(
                kind = "vector",
                value = rep(
                    state$if_missing,
                    format_axis_length(daf, axis)
                ),
                axis = axis
            ))
        }
        stop(sprintf(
            "no vector %s on axis %s",
            sQuote(node$name), sQuote(axis)
        ), call. = FALSE)
    }
    list(
        kind = "vector",
        value = format_get_vector(daf, axis, node$name),
        axis = axis
    )
}

.apply_lookup_matrix <- function(node, state, daf) {
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
    if (!format_has_matrix(daf, rows, cols, node$name)) {
        if (!is.null(state$if_missing)) {
            return(list(
                kind = "matrix",
                value = matrix(
                    state$if_missing,
                    format_axis_length(daf, rows),
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
    list(
        kind = "matrix",
        value = format_get_matrix(daf, rows, cols, node$name),
        rows_axis = rows, cols_axis = cols
    )
}
.apply_if_not <- function(node, state, daf) {
    # V1: no-op. Slice 4 will implement empty-to-sentinel substitution.
    state
}
.apply_as_axis <- function(node, state, daf) {
    # V1: identity. Slice 4 will implement vector-value-as-axis-entries
    # resolution.
    state
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
    mask <- state$pending_mask
    keep <- !is.na(mask) & mask
    list(kind = "axis", axis = axis, value = entries[keep])
}

.apply_logical_mask <- function(node, state, daf) {
    if (!identical(state$kind, "mask")) {
        stop("logical mask combinator outside of mask", call. = FALSE)
    }
    vec <- format_get_vector(daf, state$axis, node$property)
    m <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
    negated <- grepl("NegatedMask$", node$op)
    if (negated) m <- !m
    op <- if (startsWith(node$op, "And")) "And" else if (startsWith(node$op, "Or")) "Or" else "Xor"
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
.apply_square_slice <- function(node, state, daf) {
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
    if (!state$kind %in% c("vector", "matrix")) {
        stop("'%' eltwise requires vector or matrix in scope", call. = FALSE)
    }
    fn <- get_eltwise(node$name)
    params <- .coerce_params(node$params)

    # Fast path: sparsity-preserving Log on dgCMatrix. log1p(0) == 0, so
    # eps == 1 with default base (e) is the only Log parameterisation that
    # preserves sparsity; apply log1p in place to @x and keep @i / @p.
    builtin <- attr(fn, ".dafr_builtin")
    if (isTRUE(dafr_opt("dafr.perf.fast_paths")) &&
        identical(builtin, "Log") &&
        methods::is(state$value, "dgCMatrix") &&
        isTRUE(all.equal(params$eps %||% 0, 1)) &&
        (is.null(params$base) ||
            isTRUE(all.equal(params$base, exp(1))))) {
        out <- state$value
        out@x <- log1p(out@x)
        state$value <- out
        return(state)
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
    if (!identical(state$kind, "matrix")) {
        stop(sprintf("%s requires a matrix or grouped scope", node$op),
            call. = FALSE
        )
    }
    fn <- get_reduction(node$reduction)
    params <- .coerce_params(node$params)

    # Fast path: bare default reduction (no params, built-in fn) -> vectorised
    # primitive. Falls through to NULL for unhandled built-ins (e.g. Count).
    if (isTRUE(dafr_opt("dafr.perf.fast_paths")) && length(params) == 0L) {
        fast <- .apply_reduction_fast(node, state, fn, daf)
        if (!is.null(fast)) return(fast)
    }

    .apply_reduction_slow(node, state, fn, params, daf)
}

.apply_reduction_fast <- function(node, state, fn, daf) {
    builtin <- attr(fn, ".dafr_builtin")
    if (is.null(builtin)) return(NULL)
    m <- state$value
    is_sparse <- methods::is(m, "dgCMatrix") || methods::is(m, "lgCMatrix")
    is_dense <- is.matrix(m)
    if (!is_sparse && !is_dense) return(NULL)

    if (identical(node$op, "ReduceToColumn")) {
        row_names <- if (is_dense) rownames(m) else m@Dimnames[[1L]]
        if (is.null(row_names)) row_names <- format_axis_array(daf, state$rows_axis)
        vals <- switch(builtin,
            Sum   = if (is_sparse) Matrix::rowSums(m) else rowSums(m),
            Mean  = if (is_sparse) Matrix::rowMeans(m) else rowMeans(m),
            Max   = if (is_sparse) matrixStats::rowMaxs(as.matrix(m)) else matrixStats::rowMaxs(m),
            Min   = if (is_sparse) matrixStats::rowMins(as.matrix(m)) else matrixStats::rowMins(m),
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
        Max   = if (is_sparse) matrixStats::colMaxs(as.matrix(m)) else matrixStats::colMaxs(m),
        Min   = if (is_sparse) matrixStats::colMins(as.matrix(m)) else matrixStats::colMins(m),
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

.apply_reduction_grouped_vector <- function(node, state, daf) {
    fn <- get_reduction(node$reduction)
    params <- .coerce_params(node$params)
    splitted <- split(state$value, state$pending_groups)
    vals <- vapply(
        splitted, function(x) do.call(fn, c(list(x), params)),
        numeric(1)
    )
    list(kind = "vector", axis = NULL, value = vals)
}

.apply_reduction_grouped_matrix <- function(node, state, daf, by) {
    fn <- get_reduction(node$reduction)
    params <- .coerce_params(node$params)
    m <- state$value
    if (identical(by, "rows")) {
        # Rows grouped: split row indices by group label.
        # ReduceToColumn: for each row-group, reduce across columns within each
        # surviving row (margin 1), producing a sub-matrix.  Group becomes the
        # new row dimension; columns are preserved.
        idx <- split(seq_len(nrow(m)), state$pending_row_groups)
        if (identical(node$op, "ReduceToColumn")) {
            out <- sapply(idx, function(i) {
                sub <- m[i, , drop = FALSE]
                apply(sub, 1L, function(row) do.call(fn, c(list(row), params)))
            })
            return(list(
                kind = "matrix", value = out,
                rows_axis = NULL, cols_axis = state$cols_axis
            ))
        }
        # ReduceToRow on grouped rows: reduce within each group across rows
        # (margin 2 = collapse rows within group to per-column value, then
        # reduce again across columns to get one scalar per group).
        out <- vapply(idx, function(i) {
            sub <- m[i, , drop = FALSE]
            reduced <- apply(sub, 2L, function(col) do.call(fn, c(list(col), params)))
            do.call(fn, c(list(reduced), params))
        }, numeric(1))
        return(list(kind = "vector", axis = NULL, value = out))
    }
    # by cols: split column indices by group label.
    # ReduceToRow: for each col-group, reduce across rows within each
    # surviving column (margin 2), producing a sub-matrix.  Group becomes
    # the new column dimension; rows are preserved.
    idx <- split(seq_len(ncol(m)), state$pending_col_groups)
    if (identical(node$op, "ReduceToRow")) {
        out <- sapply(idx, function(j) {
            sub <- m[, j, drop = FALSE]
            apply(sub, 2L, function(col) do.call(fn, c(list(col), params)))
        })
        return(list(
            kind = "matrix", value = out,
            rows_axis = state$rows_axis, cols_axis = NULL
        ))
    }
    # ReduceToColumn on grouped cols: reduce within each group across columns
    # (margin 1 = collapse columns within group to per-row value, then
    # reduce again across rows to get one scalar per group).
    out <- vapply(idx, function(j) {
        sub <- m[, j, drop = FALSE]
        reduced <- apply(sub, 1L, function(row) do.call(fn, c(list(row), params)))
        do.call(fn, c(list(reduced), params))
    }, numeric(1))
    list(kind = "vector", axis = NULL, value = out)
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

.apply_groupby_vector <- function(node, state, daf) {
    if (!identical(state$kind, "vector")) {
        stop("GroupBy requires a vector in scope", call. = FALSE)
    }
    grp <- format_get_vector(daf, state$axis, node$property)
    state$pending_groups <- grp
    state$kind <- "grouped_vector"
    state
}

.apply_groupby_rows <- function(node, state, daf) {
    if (!identical(state$kind, "matrix")) {
        stop("GroupRowsBy requires a matrix in scope", call. = FALSE)
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
    grp <- format_get_vector(daf, state$cols_axis, node$property)
    state$pending_col_groups <- grp
    state$kind <- "grouped_matrix_cols"
    state
}

.apply_countby <- function(node, state, daf) {
    if (!identical(state$kind, "vector")) {
        stop("* CountBy requires a vector in scope", call. = FALSE)
    }
    a <- state$value
    b <- format_get_vector(daf, state$axis, node$property)
    t <- table(a, b)
    m <- as.matrix(t)
    list(kind = "matrix", value = m, rows_axis = NULL, cols_axis = NULL)
}
