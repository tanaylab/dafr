#' Base abstract reader class.
#'
#' Abstract S7 class. Concrete subclasses (`MemoryDaf`, `FilesDaf`,
#' ...) inherit from `DafReader` or one of its abstract descendants
#' `DafReadOnly` / `DafWriter`.
#'
#' @param name Human-readable identifier for the `Daf` store.
#' @param internal Internal per-store environment used by format
#'   backends to stash backend-specific state; reserved for package use.
#' @param cache Three-tier cache environment (mapped / memory / query).
#'   See `new_cache_env()`.
#' @param axis_version_counter Environment tracking per-axis mutation
#'   counters; invalidates cached reads when an axis is modified.
#' @param vector_version_counter Environment tracking per-vector
#'   mutation counters.
#' @param matrix_version_counter Environment tracking per-matrix
#'   mutation counters.
#' @export
DafReader <- S7::new_class(
    name = "DafReader",
    package = "dafr",
    abstract = TRUE,
    properties = list(
        name                   = S7::class_character,
        internal               = S7::class_environment,
        cache                  = S7::class_environment,
        axis_version_counter   = S7::class_environment,
        vector_version_counter = S7::class_environment,
        matrix_version_counter = S7::class_environment
    )
)

#' Abstract read-only reader class.
#'
#' @inheritParams DafReader
#' @export
DafReadOnly <- S7::new_class(
    name = "DafReadOnly",
    package = "dafr",
    abstract = TRUE,
    parent = DafReader
)

#' Abstract writer class.
#'
#' @inheritParams DafReader
#' @export
DafWriter <- S7::new_class(
    name = "DafWriter",
    package = "dafr",
    abstract = TRUE,
    parent = DafReader
)

new_internal_env <- function() {
    e <- new.env(parent = emptyenv())
    e$closed <- FALSE
    e
}

new_cache_env <- function() {
    e <- new.env(parent = emptyenv())
    e$mapped <- new.env(parent = emptyenv())
    e$memory <- new.env(parent = emptyenv())
    e$query <- new.env(parent = emptyenv())
    e$lru <- character(0L) # entries "tier:key", MRU at tail
    e$bytes <- 0 # bytes used across memory + query
    e$cap <- .cache_default_cap()
    e
}

new_counter_env <- function() new.env(parent = emptyenv())
