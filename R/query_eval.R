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
    if (identical(state$kind, "vector_axis")) {
        return(.apply_chained_lookup_vector(node, state, daf))
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
    }
    list(
        kind = "vector",
        value = value,
        axis = axis,
        property = node$name
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
    row_indices <- state$row_indices
    if (!format_has_matrix(daf, rows, cols, node$name)) {
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
    m <- format_get_matrix(daf, rows, cols, node$name)
    if (!is.null(row_indices)) {
        m <- m[row_indices, , drop = FALSE]
    }
    list(
        kind = "matrix",
        value = m,
        rows_axis = rows, cols_axis = cols
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
    lookup_vec <- format_get_vector(daf, target_axis, node$name)
    target_entries <- format_axis_array(daf, target_axis)
    indices <- match(pivot_values, target_entries)

    empty_mask <- is.na(indices) |
        (is.character(pivot_values) & !nzchar(pivot_values))
    out <- rep(NA, length(pivot_values))
    mode(out) <- mode(lookup_vec)
    out[!empty_mask] <- lookup_vec[indices[!empty_mask]]

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
        if (is.null(sentinel)) {
            keep <- !empty_mask
            out <- out[keep]
            base_entries <- base_entries[keep]
        } else {
            sentinel_typed <- methods::as(sentinel, class(lookup_vec)[[1L]])
            out[empty_mask] <- sentinel_typed
        }
    } else {
        if (any(empty_mask)) {
            stop(
                sprintf(
                    "chain lookup on axis %s has %d empty pivot values and no '??' sentinel",
                    sQuote(base_axis), sum(empty_mask)
                ),
                call. = FALSE
            )
        }
    }
    names(out) <- base_entries
    list(
        kind     = "vector",
        value    = out,
        axis     = base_axis,
        property = node$name
    )
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

    # Fast path: built-in reduction -> vectorised primitive. Falls through to
    # NULL for unhandled built-ins (e.g. Count) or non-sparse/dense matrices.
    if (isTRUE(dafr_opt("dafr.perf.fast_paths"))) {
        fast <- .apply_reduction_fast(node, state, fn, params, daf)
        if (!is.null(fast)) return(fast)
    }

    .apply_reduction_slow(node, state, fn, params, daf)
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
    # Deduplication was evaluated for slice-8-task-7 and deferred: the two branches
    # diverge in state key access (rows_axis vs cols_axis, row_names vs col_names) and
    # in helper names (rowSums/colSums, rowMeans/colMeans, rowMaxs/colMaxs, etc.) such
    # that a single dispatcher would require a lookup table or function-factory of 10+
    # helper pairs — more infrastructure than the duplication it removes. Left as-is.
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

# --- Grouped-reduction helpers (Slice 8 Task 10) --------------------------
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
    # Preserve group first-appearance order via factor levels.
    gfac <- factor(g, levels = unique(g))
    gi <- as.integer(gfac)
    ngroups <- nlevels(gfac)
    lvls <- levels(gfac)

    label <- .reduction_builtin_label(fn)

    # Mode-on-character dedicated R helper (no C kernel; char Mode is rare).
    if (!is.na(label) && label == "Mode" && is.character(x)) {
        vals <- .grouped_mode_character(x, gi, ngroups)
        return(list(kind = "vector", axis = NULL,
            value = stats::setNames(vals, lvls)))
    }

    # Numeric builtin fast path.
    if (!is.na(label) && is.numeric(x)) {
        vals <- .grouped_vector_builtin(x, gi, ngroups, label, params)
        return(list(kind = "vector", axis = NULL,
            value = stats::setNames(vals, lvls)))
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
    list(kind = "vector", axis = NULL,
        value = stats::setNames(out, lvls))
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
