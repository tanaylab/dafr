#' Construct a `dgCMatrix` whose `x`, `i`, `p` slots are ALTREP mmap views.
#'
#' The three files must be the CSC component files of a sparse matrix:
#' `x_path` stores `nnz` doubles, `i_path` stores `nnz` int32 row indices
#' (0-based, matching `dgCMatrix@i`), `p_path` stores `ncol+1` int32
#' column pointers (0-based, matching `dgCMatrix@p`).
#'
#' @param x_path,i_path,p_path Paths to the `.bin` files.
#' @param nrow,ncol,nnz Matrix shape + number of non-zeros.
#' @param dimnames Optional `list(rowname_character, colname_character)`.
#' @export
mmap_dgCMatrix <- function(x_path, i_path, p_path, nrow, ncol, nnz,
                           dimnames = NULL) {
  stopifnot(file.exists(x_path), file.exists(i_path), file.exists(p_path))
  stopifnot(is.numeric(nrow), is.numeric(ncol), is.numeric(nnz))

  x_slot <- mmap_real(x_path, nnz)
  i_slot <- mmap_int(i_path,  nnz)
  p_slot <- mmap_int(p_path,  as.integer(ncol) + 1L)

  m <- methods::new("dgCMatrix",
    x        = x_slot,
    i        = i_slot,
    p        = p_slot,
    Dim      = c(as.integer(nrow), as.integer(ncol)),
    Dimnames = if (is.null(dimnames)) list(NULL, NULL) else dimnames)
  m
}
