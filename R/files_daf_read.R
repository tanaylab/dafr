#' @include files_daf.R files_io.R format_api.R
NULL

.files_root <- function(daf) S7::prop(daf, "internal")$path

# ---- scalars: query ----
S7::method(format_has_scalar,
           list(FilesDaf, S7::class_character)) <- function(daf, name) {
  file.exists(.path_scalar(.files_root(daf), name))
}
S7::method(format_has_scalar,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, name) {
  file.exists(.path_scalar(.files_root(daf), name))
}

.files_get_scalar <- function(daf, name) {
  p <- .path_scalar(.files_root(daf), name)
  if (!file.exists(p)) {
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
  }
  .read_scalar_json(p)
}
S7::method(format_get_scalar,
           list(FilesDaf, S7::class_character)) <- function(daf, name) {
  .files_get_scalar(daf, name)
}
S7::method(format_get_scalar,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, name) {
  .files_get_scalar(daf, name)
}

.files_scalars_set <- function(daf) {
  dir <- file.path(.files_root(daf), "scalars")
  files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
  sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(format_scalars_set, FilesDaf)         <- function(daf) {
  .files_scalars_set(daf)
}
S7::method(format_scalars_set, FilesDafReadOnly) <- function(daf) {
  .files_scalars_set(daf)
}

# ---- axes: query ----

.files_axis_parsed <- function(daf, axis) {
  cache <- S7::prop(daf, "internal")$axes
  if (exists(axis, envir = cache, inherits = FALSE)) {
    return(get(axis, envir = cache, inherits = FALSE))
  }
  p <- .path_axis(.files_root(daf), axis)
  if (!file.exists(p)) return(NULL)
  entries <- readLines(p, encoding = "UTF-8", warn = FALSE)
  if (anyNA(entries) || any(!nzchar(entries))) {
    stop(sprintf("files_daf: axis %s contains empty entries", sQuote(axis)),
         call. = FALSE)
  }
  if (anyDuplicated(entries)) {
    dup <- entries[duplicated(entries)][1L]
    stop(sprintf("files_daf: axis %s has duplicate entry %s",
                 sQuote(axis), sQuote(dup)), call. = FALSE)
  }
  dict <- new.env(parent = emptyenv(), size = length(entries))
  for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
  parsed <- list(entries = entries, dict = dict)
  assign(axis, parsed, envir = cache)
  parsed
}

.files_has_axis <- function(daf, axis) {
  file.exists(.path_axis(.files_root(daf), axis))
}

S7::method(format_has_axis,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  .files_has_axis(daf, axis)
}
S7::method(format_has_axis,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  .files_has_axis(daf, axis)
}

.files_axes_set <- function(daf) {
  dir <- file.path(.files_root(daf), "axes")
  files <- list.files(dir, pattern = "\\.txt$", full.names = FALSE)
  sort(sub("\\.txt$", "", files), method = "radix")
}
S7::method(format_axes_set, FilesDaf)         <- function(daf) .files_axes_set(daf)
S7::method(format_axes_set, FilesDafReadOnly) <- function(daf) .files_axes_set(daf)

.files_axis_require <- function(daf, axis) {
  parsed <- .files_axis_parsed(daf, axis)
  if (is.null(parsed)) {
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  parsed
}

S7::method(format_axis_length,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  length(.files_axis_require(daf, axis)$entries)
}
S7::method(format_axis_length,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  length(.files_axis_require(daf, axis)$entries)
}

S7::method(format_axis_array,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  .files_axis_require(daf, axis)$entries
}
S7::method(format_axis_array,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  .files_axis_require(daf, axis)$entries
}

S7::method(format_axis_dict,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  .files_axis_require(daf, axis)$dict
}
S7::method(format_axis_dict,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  .files_axis_require(daf, axis)$dict
}
