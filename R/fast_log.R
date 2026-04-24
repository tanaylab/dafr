#' Parallel `log(x + eps) / log(base)` on dense numeric input.
#'
#' Mirrors the fast-path that `% Log` takes inside the query evaluator,
#' but callable directly on an in-memory numeric matrix or vector without
#' wrapping the input in a [memory_daf()]. Intended for downstream
#' packages whose hot paths already have the matrix materialised in R
#' (e.g. a session-level cache) and want to avoid the single-threaded
#' `log(x + eps, base)` the query fallback would otherwise incur.
#'
#' `base` values of 2, 10, and `exp(1)` dispatch to `std::log2`,
#' `std::log10`, and `std::log` respectively — matching R's preference
#' for the more accurate form over `log(x) / log(base)`. Result is
#' equal to `log(x + eps, base)` to machine precision (verified by the
#' query-julia-compat parity suite).
#'
#' @param x Dense numeric matrix or numeric vector. Integers are
#'   silently promoted to double.
#' @param eps Non-negative scalar added inside the log. Default 0.
#' @param base Positive scalar. Default `exp(1)`.
#' @param threshold Parallel-for threshold: enable OpenMP only when
#'   `length(x) >= threshold`. Defaults to
#'   `dafr_opt("dafr.kernel_threshold")`.
#'
#' @return A numeric matrix or vector matching the shape of `x`. Names
#'   and dimnames are preserved.
#'
#' @examples
#' v <- c(1, 10, 100, 1000)
#' fast_log(v, base = 10)
#'
#' m <- matrix(runif(12, 0.1, 5), nrow = 3, ncol = 4)
#' fast_log(m, eps = 1e-5, base = 2)
#'
#' @seealso [top_k_per_col()] for a similarly scoped OpenMP primitive.
#'
#' @export
fast_log <- function(x, eps = 0, base = exp(1),
                     threshold = dafr_opt("dafr.kernel_threshold")) {
    if (length(eps) != 1L || !is.numeric(eps) || is.na(eps) || eps < 0) {
        stop("`eps` must be a single non-negative number", call. = FALSE)
    }
    if (length(base) != 1L || !is.numeric(base) || is.na(base) || base <= 0) {
        stop("`base` must be a single positive number", call. = FALSE)
    }
    threshold <- as.integer(threshold)
    if (length(threshold) != 1L || is.na(threshold) || threshold < 0L) {
        threshold <- 0L
    }

    if (is.matrix(x)) {
        if (!is.double(x)) {
            if (!is.numeric(x)) {
                stop("`x` must be a numeric matrix or vector", call. = FALSE)
            }
            storage.mode(x) <- "double"
        }
        out <- kernel_log_dense_mat_cpp(
            x, eps = as.double(eps), base = as.double(base),
            threshold = threshold
        )
        # kernel_log_dense_mat_cpp preserves shape but not dimnames.
        dimnames(out) <- dimnames(x)
        return(out)
    }

    if (is.numeric(x) && is.null(dim(x))) {
        nm <- names(x)
        out <- kernel_log_dense_vec_cpp(
            as.double(x), eps = as.double(eps), base = as.double(base),
            threshold = threshold
        )
        if (!is.null(nm)) names(out) <- nm
        return(out)
    }

    stop(
        "`x` must be a dense numeric matrix or numeric vector (got ",
        paste(class(x), collapse = "/"), ")",
        call. = FALSE
    )
}
