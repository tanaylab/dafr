# Construct a `dgCMatrix` whose `x`, `i`, `p` slots are ALTREP mmap views.

The three files must be the CSC component files of a sparse matrix:
`x_path` stores `nnz` doubles, `i_path` stores `nnz` int32 row indices
(0-based, matching `dgCMatrix@i`), `p_path` stores `ncol+1` int32 column
pointers (0-based, matching `dgCMatrix@p`).

## Usage

``` r
mmap_dgCMatrix(x_path, i_path, p_path, nrow, ncol, nnz, dimnames = NULL)
```

## Arguments

- x_path, i_path, p_path:

  Paths to the `.bin` files.

- nrow, ncol, nnz:

  Matrix shape + number of non-zeros.

- dimnames:

  Optional `list(rowname_character, colname_character)`.

## Examples

``` r
# \donttest{
# Build a tiny CSC matrix: 3x2, 3 non-zeros
x_f <- tempfile(fileext = ".bin"); writeBin(c(1.0, 2.0, 3.0), x_f)
i_f <- tempfile(fileext = ".bin"); writeBin(c(0L, 1L, 2L), i_f)
p_f <- tempfile(fileext = ".bin"); writeBin(c(0L, 2L, 3L), p_f)
m <- mmap_dgCMatrix(x_f, i_f, p_f, nrow = 3L, ncol = 2L, nnz = 3L)
dim(m)
#> [1] 3 2
unlink(c(x_f, i_f, p_f))
# }
```
