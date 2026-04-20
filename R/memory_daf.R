#' In-memory Daf store.
#'
#' A concrete `DafWriter` backed entirely by R environments — no disk,
#' no mmap. Scalars, axes, vectors, and matrices live in nested
#' environments (hash tables) under the `internal` property:
#'
#' - `internal$scalars`     : `env(name -> value)`
#' - `internal$axes`        : `env(axis -> list(entries = character, dict = env))`
#' - `internal$vectors`     : `env(axis -> env(name -> vector))`
#' - `internal$matrices`    : `env(rows_axis -> env(cols_axis -> env(name -> matrix)))`
#'
#' @param name Human-readable identifier. Defaults to `"memory"`.
#' @return A `MemoryDaf` instance.
#' @export
#' @examples
#' d <- memory_daf(name = "scratch")
#' add_axis(d, "cell", c("A", "B", "C"))
#' set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
memory_daf <- function(name = "memory") {
  stopifnot(is.character(name), length(name) == 1L, !is.na(name))
  internal <- new_internal_env()
  internal$scalars  <- new.env(parent = emptyenv())
  internal$axes     <- new.env(parent = emptyenv())
  internal$vectors  <- new.env(parent = emptyenv())
  internal$matrices <- new.env(parent = emptyenv())
  MemoryDaf(
    name                   = name,
    internal               = internal,
    cache                  = new_cache_env(),
    axis_version_counter   = new_counter_env(),
    vector_version_counter = new_counter_env(),
    matrix_version_counter = new_counter_env()
  )
}

#' Concrete `DafWriter` backed by R environments (no I/O).
#'
#' Use `memory_daf()` to construct instances — the S7 constructor is
#' exported only for `isVirtualClass`-style checks.
#'
#' @inheritParams DafReader
#' @export
MemoryDaf <- S7::new_class(
  name    = "MemoryDaf",
  package = "dafr",
  parent  = DafWriter
)
