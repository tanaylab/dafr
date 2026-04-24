#' Per-column bounded top-K with OpenMP.
#'
#' Returns the `k` largest (or largest-by-magnitude if `use_abs = TRUE`)
#' values of each column together with their row indices. Input may be a
#' dense numeric matrix or a `Matrix::dgCMatrix`; the sparse path scans
#' only the nonzero entries per column so it scales with `nnz`, not
#' `nrow * ncol`.
#'
#' Rows missing a valid comparison score (`NA` / `NaN` in dense input;
#' absent from the column's nonzero entries in the CSC input) are
#' excluded from consideration. When fewer than `k` entries qualify, the
#' tail of each output column is NA-padded.
#'
#' @param x Numeric dense matrix, or `Matrix::dgCMatrix`.
#' @param k Positive integer - number of entries to return per column.
#' @param use_abs Rank by `abs(value)` when `TRUE`, else by raw value.
#'   Values returned are always signed.
#' @param threshold Parallel-for threshold: enable OpenMP across columns
#'   only when `ncol(x) >= threshold`. Defaults to
#'   `dafr_opt("dafr.kernel_threshold")`.
#'
#' @return A list with two `k`-by-`ncol` matrices:
#' \describe{
#'   \item{`indices`}{Integer matrix of row indices (1-based). `NA_integer_`
#'     in padding slots.}
#'   \item{`values`}{Double matrix of signed values. `NA_real_` in padding
#'     slots.}
#' }
#' Column names of `x` are preserved on the output columns; row-name
#' lookup is the caller's responsibility (use `rownames(x)[indices]`).
#'
#' @examples
#' m <- matrix(c(1, -5, 3, 2, 4, -2, 7, -1, 6, 5, 8, -3), nrow = 4, ncol = 3)
#' top_k_per_col(m, k = 2)
#' top_k_per_col(m, k = 2, use_abs = TRUE)
#'
#' sm <- Matrix::sparseMatrix(
#'     i = c(1, 3, 2, 4, 1, 3),
#'     j = c(1, 1, 2, 2, 3, 3),
#'     x = c(5, 2, 7, 1, 4, 8),
#'     dims = c(4, 3)
#' )
#' top_k_per_col(sm, k = 2)
#'
#' @export
top_k_per_col <- function(x, k,
                          use_abs = FALSE,
                          threshold = dafr_opt("dafr.kernel_threshold")) {
    k <- as.integer(k)
    if (length(k) != 1L || is.na(k) || k <= 0L) {
        stop("`k` must be a positive integer scalar", call. = FALSE)
    }
    use_abs <- isTRUE(use_abs)
    threshold <- as.integer(threshold)
    if (length(threshold) != 1L || is.na(threshold) || threshold < 0L) {
        threshold <- 0L
    }

    if (methods::is(x, "dgCMatrix")) {
        result <- kernel_top_k_per_col_csc_cpp(
            x = x@x, i = x@i, p = x@p,
            nrow = nrow(x), ncol = ncol(x),
            k = k, use_abs = use_abs, threshold = threshold
        )
    } else if (is.matrix(x) && is.double(x)) {
        result <- kernel_top_k_per_col_dense_cpp(
            m = x, k = k, use_abs = use_abs, threshold = threshold
        )
    } else if (is.matrix(x) && is.numeric(x)) {
        storage.mode(x) <- "double"
        result <- kernel_top_k_per_col_dense_cpp(
            m = x, k = k, use_abs = use_abs, threshold = threshold
        )
    } else {
        stop(
            "`x` must be a dense numeric matrix or a Matrix::dgCMatrix",
            call. = FALSE
        )
    }

    if (!is.null(colnames(x))) {
        colnames(result$indices) <- colnames(x)
        colnames(result$values)  <- colnames(x)
    }

    result
}
