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

# ---- Scalars: query ---------------------------------------------------------

S7::method(format_has_scalar,
           list(MemoryDaf, S7::class_character)) <- function(daf, name) {
  exists(name, envir = S7::prop(daf, "internal")$scalars, inherits = FALSE)
}

S7::method(format_get_scalar,
           list(MemoryDaf, S7::class_character)) <- function(daf, name) {
  scalars <- S7::prop(daf, "internal")$scalars
  if (!exists(name, envir = scalars, inherits = FALSE)) {
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
  }
  get(name, envir = scalars, inherits = FALSE)
}

S7::method(format_scalars_set, MemoryDaf) <- function(daf) {
  sort(ls(S7::prop(daf, "internal")$scalars, all.names = TRUE),
       method = "radix")
}

# ---- Scalars: mutation ------------------------------------------------------

.assert_scalar_value <- function(name, value) {
  if (is.null(value)) {
    stop(sprintf("scalar %s value may not be NULL", sQuote(name)), call. = FALSE)
  }
  if (!is.atomic(value)) {
    stop(sprintf("scalar %s value must be an atomic scalar", sQuote(name)), call. = FALSE)
  }
  if (length(value) != 1L) {
    stop(sprintf("scalar %s value must have length 1 (got %d)", sQuote(name), length(value)), call. = FALSE)
  }
  if (is.na(value)) {
    stop(sprintf("scalar %s value may not be NA", sQuote(name)), call. = FALSE)
  }
  invisible()
}

S7::method(format_set_scalar,
           list(MemoryDaf, S7::class_character, S7::class_any, S7::class_logical)) <- function(daf, name, value, overwrite) {
  .assert_scalar_value(name, value)
  scalars <- S7::prop(daf, "internal")$scalars
  if (exists(name, envir = scalars, inherits = FALSE) && !overwrite) {
    stop(sprintf("scalar %s already exists; use overwrite = TRUE", sQuote(name)), call. = FALSE)
  }
  assign(name, value, envir = scalars)
  invisible()
}

S7::method(format_delete_scalar,
           list(MemoryDaf, S7::class_character, S7::class_logical)) <- function(daf, name, must_exist) {
  scalars <- S7::prop(daf, "internal")$scalars
  if (!exists(name, envir = scalars, inherits = FALSE)) {
    if (must_exist) {
      stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
    }
    return(invisible())
  }
  rm(list = name, envir = scalars)
  invisible()
}

# ---- Axes: query methods ----------------------------------------------------

S7::method(format_has_axis, list(MemoryDaf, S7::class_character)) <- function(daf, axis) {
  exists(axis, envir = S7::prop(daf, "internal")$axes, inherits = FALSE)
}

S7::method(format_axes_set, MemoryDaf) <- function(daf) {
  nms <- ls(S7::prop(daf, "internal")$axes, all.names = TRUE)
  sort(nms)
}

.memory_axis <- function(daf, axis) {
  axes <- S7::prop(daf, "internal")$axes
  if (!exists(axis, envir = axes, inherits = FALSE)) {
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  get(axis, envir = axes, inherits = FALSE)
}

S7::method(format_axis_length, list(MemoryDaf, S7::class_character)) <- function(daf, axis) {
  length(.memory_axis(daf, axis)$entries)
}

S7::method(format_axis_array, list(MemoryDaf, S7::class_character)) <- function(daf, axis) {
  .memory_axis(daf, axis)$entries
}

S7::method(format_axis_dict, list(MemoryDaf, S7::class_character)) <- function(daf, axis) {
  .memory_axis(daf, axis)$dict
}

# ---- Axes: mutation ---------------------------------------------------------

S7::method(format_add_axis,
           list(MemoryDaf, S7::class_character, S7::class_character)) <- function(daf, axis, entries) {
  if (!is.character(entries)) {
    stop(sprintf("axis %s entries must be a character vector", sQuote(axis)), call. = FALSE)
  }
  if (anyNA(entries)) {
    stop(sprintf("axis %s entries contain NA", sQuote(axis)), call. = FALSE)
  }
  if (any(!nzchar(entries))) {
    stop(sprintf("axis %s entries contain empty strings", sQuote(axis)), call. = FALSE)
  }
  if (anyDuplicated(entries)) {
    dup <- entries[duplicated(entries)][1L]
    stop(sprintf("axis %s has duplicate entry %s", sQuote(axis), sQuote(dup)), call. = FALSE)
  }
  axes <- S7::prop(daf, "internal")$axes
  if (exists(axis, envir = axes, inherits = FALSE)) {
    stop(sprintf("axis %s already exists", sQuote(axis)), call. = FALSE)
  }
  dict <- new.env(parent = emptyenv(), size = length(entries))
  for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
  assign(axis, list(entries = entries, dict = dict), envir = axes)
  bump_axis_counter(daf, axis)
  invisible()
}

S7::method(format_delete_axis,
           list(MemoryDaf, S7::class_character, S7::class_logical)) <- function(daf, axis, must_exist) {
  internal <- S7::prop(daf, "internal")
  if (!exists(axis, envir = internal$axes, inherits = FALSE)) {
    if (must_exist) {
      stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
    }
    return(invisible())
  }
  # Drop axis + its dependent vectors + matrix rows + matrix cols
  rm(list = axis, envir = internal$axes)
  if (exists(axis, envir = internal$vectors, inherits = FALSE)) {
    rm(list = axis, envir = internal$vectors)
  }
  if (exists(axis, envir = internal$matrices, inherits = FALSE)) {
    rm(list = axis, envir = internal$matrices)
  }
  for (rows in ls(internal$matrices, all.names = TRUE)) {
    cols_env <- get(rows, envir = internal$matrices, inherits = FALSE)
    if (exists(axis, envir = cols_env, inherits = FALSE)) {
      rm(list = axis, envir = cols_env)
    }
  }
  bump_axis_counter(daf, axis)
  invisible()
}
