#' Open a binary file as a read-only mmap-backed numeric vector.
#'
#' Length is the number of `double` elements; the file must contain at least
#' `length * 8` bytes.
#' @export
mmap_real <- function(path, length) {
  stopifnot(file.exists(path), is.numeric(length), length >= 0)
  mmap_real_altrep_cpp(path.expand(path), as.double(length))
}

#' @rdname mmap_real
#' @export
mmap_int <- function(path, length) {
  stopifnot(file.exists(path), is.numeric(length), length >= 0)
  mmap_int_altrep_cpp(path.expand(path), as.double(length))
}

#' @rdname mmap_real
#' @export
mmap_lgl <- function(path, length) {
  stopifnot(file.exists(path), is.numeric(length), length >= 0)
  mmap_lgl_altrep_cpp(path.expand(path), as.double(length))
}
