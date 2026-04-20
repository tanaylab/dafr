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
