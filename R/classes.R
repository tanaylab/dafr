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

# ---- Class-surface sugar ---------------------------------------------------

#' Test whether an object is a `DafReader`.
#'
#' Non-throwing predicate for any of the S7 class descendants
#' (`MemoryDaf`, `FilesDaf`, `ReadOnlyChainDaf`, `WriteChainDaf`,
#' `ViewDaf`, `ContractDaf`, ...).
#'
#' @param x Any R object.
#' @return `TRUE` if `x` inherits from [DafReader], else `FALSE`.
#' @examples
#' is_daf(memory_daf())
#' is_daf(NULL)
#' @export
is_daf <- function(x) S7::S7_inherits(x, DafReader)

#' Return the name of a `DafReader`.
#'
#' Asserts that `x` is a [DafReader] and returns its `name` property
#' (the string passed to the constructor).
#'
#' @param x A [DafReader].
#' @return Character scalar.
#' @examples
#' daf_name(memory_daf(name = "hello"))
#' @seealso [is_daf()], [read_only()]
#' @export
daf_name <- function(x) {
    if (!is_daf(x)) {
        stop("`x` must be a DafReader", call. = FALSE)
    }
    S7::prop(x, "name")
}
