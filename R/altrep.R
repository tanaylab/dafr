#' Open a binary file as a read-only mmap-backed vector.
#'
#' `mmap_real` opens a file of IEEE 754 doubles, `mmap_int` opens int32,
#' `mmap_lgl` opens int32 interpreted as R logicals. `length` is the
#' element count; the file must contain at least `length * sizeof(element)`
#' bytes.
#'
#' Length is passed through as `double` to accommodate `R_xlen_t`; values
#' above 2^53 lose precision. Pass only integer-valued doubles.
#'
#' @param path Path to the binary file.
#' @param length Number of elements to map.
#' @return An ALTREP-backed R vector sharing the file's memory.
#' @examples
#' f <- tempfile(fileext = ".bin")
#' writeBin(c(1.5, 2.5, 3.5), f)
#' v <- mmap_real(f, 3L)
#' v[]
#' unlink(f)
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
