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
#' @importClassesFrom Matrix dgCMatrix
#' @importFrom Matrix sparseMatrix
#' @examples
#' \donttest{
#' # Build a tiny CSC matrix: 3x2, 3 non-zeros
#' x_f <- tempfile(fileext = ".bin"); writeBin(c(1.0, 2.0, 3.0), x_f)
#' i_f <- tempfile(fileext = ".bin"); writeBin(c(0L, 1L, 2L), i_f)
#' p_f <- tempfile(fileext = ".bin"); writeBin(c(0L, 2L, 3L), p_f)
#' m <- mmap_dgCMatrix(x_f, i_f, p_f, nrow = 3L, ncol = 2L, nnz = 3L)
#' dim(m)
#' unlink(c(x_f, i_f, p_f))
#' }
#' @export
mmap_dgCMatrix <- function(x_path, i_path, p_path, nrow, ncol, nnz,
                           dimnames = NULL) {
    stopifnot(file.exists(x_path), file.exists(i_path), file.exists(p_path))
    stopifnot(is.numeric(nrow), is.numeric(ncol), is.numeric(nnz))

    if (!is.null(dimnames)) {
        stopifnot(is.list(dimnames), length(dimnames) == 2L)
    }

    x_slot <- mmap_real(x_path, nnz)
    i_slot <- mmap_int(i_path, nnz)
    p_slot <- mmap_int(p_path, as.integer(ncol) + 1L)

    # Cheap CSC invariant check: p[ncol+1] must equal nnz.
    stopifnot(p_slot[as.integer(ncol) + 1L] == nnz)

    m <- methods::new("dgCMatrix",
        x        = x_slot,
        i        = i_slot,
        p        = p_slot,
        Dim      = c(as.integer(nrow), as.integer(ncol)),
        Dimnames = if (is.null(dimnames)) list(NULL, NULL) else dimnames
    )
    m
}
