#' Base abstract reader class.
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
#' @export
DafReadOnly <- S7::new_class(
  name = "DafReadOnly",
  package = "dafr",
  abstract = TRUE,
  parent = DafReader
)

#' Abstract writer class.
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
  e$query  <- new.env(parent = emptyenv())
  e
}

new_counter_env <- function() new.env(parent = emptyenv())
