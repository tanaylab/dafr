.dafr_default_options <- list(
  dafr.cache.memory_mb = 1024L,
  dafr.cache.disable   = FALSE,
  dafr.cache.stats     = FALSE,
  dafr.mmap            = TRUE,
  dafr.omp_threshold   = 10000L,
  dafr.inefficient     = "warn",  # one of "ignore", "warn", "error"
  dafr.verbose         = FALSE
)

set_default_options <- function() {
  current <- options()
  to_set  <- .dafr_default_options[setdiff(names(.dafr_default_options), names(current))]
  if (length(to_set)) options(to_set)
  invisible()
}

#' Get a dafr option with a typed default.
#' @noRd
dafr_opt <- function(name) {
  stopifnot(name %in% names(.dafr_default_options))
  getOption(name, .dafr_default_options[[name]])
}
