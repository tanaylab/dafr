#!/usr/bin/env Rscript
# Generates inst/extdata/small_test.h5ad — 50 obs x 20 var minimal fixture.
# Written in the CANONICAL AnnData layout (verified against Python anndata):
#   - /X is (n_obs, n_var) C-order. hdf5r presents an R matrix to h5py as its
#     transpose, so the dense /X is written as t(X) and h5py/anndata then see
#     the correct (n_obs, n_var) shape.
#   - /obs and /var carry the `dataframe` encoding (_index + column-order) so a
#     reader recognises the row names instead of falling back to 0..n-1.
# Run once from the package root; commit the output.

suppressPackageStartupMessages(library(hdf5r))

set.seed(42)
out_path <- "inst/extdata/small_test.h5ad"
unlink(out_path)
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

h5 <- H5File$new(out_path, mode = "w")
scalar_attr <- function(obj, name, value) {
    obj$create_attr(name, robj = value, space = H5S$new("scalar"))
}
array_encoding <- function(dset) {
    scalar_attr(dset, "encoding-type", "array")
    scalar_attr(dset, "encoding-version", "0.2.0")
}
dataframe_encoding <- function(grp, columns) {
    scalar_attr(grp, "encoding-type", "dataframe")
    scalar_attr(grp, "encoding-version", "0.2.0")
    scalar_attr(grp, "_index", "_index")
    if (length(columns) > 0L) {
        grp$create_attr("column-order", robj = as.character(columns))
    } else {
        grp$create_attr("column-order", dtype = h5types$H5T_NATIVE_DOUBLE,
            space = H5S$new("simple", dims = 0L, maxdims = 0L))
    }
}

# Root attributes
scalar_attr(h5, "encoding-type", "anndata")
scalar_attr(h5, "encoding-version", "0.1.0")

n_obs <- 50L
n_var <- 20L

# X: dense double matrix (obs x var). Write t(X) so the on-disk /X is canonical
# (n_obs, n_var) when read by h5py / anndata.
X <- matrix(rpois(n_obs * n_var, lambda = 3), n_obs, n_var)
storage.mode(X) <- "double"
array_encoding(h5$create_dataset("X", robj = t(X)))

# /obs group (dataframe: _index + donor + age)
obs <- h5$create_group("obs")
obs$create_dataset("_index", robj = sprintf("cell_%03d", seq_len(n_obs)))
obs$create_dataset("donor", robj = sample(c("D1", "D2", "D3"), n_obs, replace = TRUE))
obs$create_dataset("age", robj = sample.int(100L, n_obs, replace = TRUE))
dataframe_encoding(obs, c("donor", "age"))

# /var group (dataframe: _index + chrom)
var <- h5$create_group("var")
var$create_dataset("_index", robj = sprintf("gene_%03d", seq_len(n_var)))
var$create_dataset("chrom", robj = sample(as.character(1:22), n_var, replace = TRUE))
dataframe_encoding(var, "chrom")

# /uns group
uns <- h5$create_group("uns")
uns$create_dataset("organism", robj = "human")

# /layers group (empty)
h5$create_group("layers")

h5$close_all()
cat("Wrote ", out_path, "\n", sep = "")
