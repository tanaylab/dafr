#' Control OpenMP thread count for dafr kernels.
#'
#' `set_num_threads(n)` caps the number of OpenMP threads used by
#' every subsequent kernel dispatch. `get_num_threads()` reports the
#' current cap.
#'
#' dafr's kernels default to the system's OpenMP maximum (typically
#' `OMP_NUM_THREADS` or the machine's CPU count). CRAN policy allows
#' at most two cores for package checks, and `.onLoad()` detects
#' CRAN-like environments (via `_R_CHECK_LIMIT_CORES_` or an
#' `OMP_THREAD_LIMIT` ≤ 2) and caps to 2 automatically. Interactive
#' users can raise or lower the cap at any time.
#'
#' @param n Positive integer; the maximum number of OpenMP threads
#'   to use. Values < 1 are clamped to 1.
#' @return `set_num_threads` invisibly returns `n`. `get_num_threads`
#'   returns the current OpenMP max-threads value.
#' @examples
#' old <- get_num_threads()
#' set_num_threads(1L)   # single-thread mode
#' get_num_threads()     # 1
#' set_num_threads(old)  # restore
#' @export
set_num_threads <- function(n) {
    if (!is.numeric(n) || length(n) != 1L || is.na(n)) {
        stop("`n` must be a single non-NA integer", call. = FALSE)
    }
    n <- as.integer(n)
    if (n < 1L) n <- 1L
    dafr_set_num_threads(n)
    options(dafr.num_threads = n)
    invisible(n)
}

#' @rdname set_num_threads
#' @export
get_num_threads <- function() {
    dafr_get_max_threads()
}

# Called from .onLoad. Inspects the process environment and caps
# threads when running under a CRAN-style check harness.
.dafr_apply_cran_thread_cap <- function() {
    check_env <- nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = ""))
    thread_limit <- suppressWarnings(
        as.integer(Sys.getenv("OMP_THREAD_LIMIT", unset = NA_character_))
    )
    user_requested <- getOption("dafr.num_threads", default = NULL)

    if (!is.null(user_requested)) {
        set_num_threads(user_requested)
        return(invisible(user_requested))
    }

    if (isTRUE(check_env) ||
        (!is.na(thread_limit) && thread_limit > 0L && thread_limit <= 2L)) {
        set_num_threads(2L)
        return(invisible(2L))
    }

    invisible(NULL)
}
