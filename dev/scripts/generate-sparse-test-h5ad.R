#!/usr/bin/env Rscript
# Generates inst/extdata/sparse_test.h5ad — sparse-X fixture for slice-13.
#
# Contents (mirrors layout expected by h5ad_as_daf):
#  - /X: sparse CSR (50 obs x 20 var, ~20% density)
#  - /obs: _index, donor (dense string), celltype (CATEGORICAL, 3 levels)
#  - /var: _index, chrom (dense string)
#  - /uns: nested — uns/params/seed = 42L, uns/params/normalization = "log1p",
#          plus flat uns/organism = "human"
#
# Run once from the package root; commit the output.

suppressPackageStartupMessages(library(hdf5r))
suppressPackageStartupMessages(library(Matrix))

set.seed(4242)
out_path <- "inst/extdata/sparse_test.h5ad"
unlink(out_path)
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

h5 <- H5File$new(out_path, mode = "w")

# Root attributes
h5$create_attr("encoding-type", robj = "anndata", space = H5S$new("scalar"))
h5$create_attr("encoding-version", robj = "0.1.0", space = H5S$new("scalar"))

n_obs <- 50L
n_var <- 20L

# Build a dense draw with ~20% density then construct the sparse view.
density <- 0.20
total <- n_obs * n_var
nnz <- round(total * density)
flat_idx <- sample.int(total, nnz)
vals <- as.numeric(sample.int(5L, nnz, replace = TRUE))
X_dense <- matrix(0, n_obs, n_var)
X_dense[flat_idx] <- vals
X_csc <- as(X_dense, "CsparseMatrix")    # canonical CSC (dgCMatrix)
X_csr <- as(X_csc, "RsparseMatrix")      # convert to CSR (dgRMatrix) for
                                         # exercise of the CSR read path

# /X as CSR group
X_grp <- h5$create_group("X")
X_grp$create_attr("encoding-type", robj = "csr_matrix", space = H5S$new("scalar"))
X_grp$create_attr("encoding-version", robj = "0.1.0", space = H5S$new("scalar"))
X_grp$create_attr("shape", robj = as.integer(c(n_obs, n_var)))
X_grp$create_dataset("data", robj = as.numeric(X_csr@x))
X_grp$create_dataset("indices", robj = as.integer(X_csr@j))    # 0-based col idx
X_grp$create_dataset("indptr", robj = as.integer(X_csr@p))     # row pointers

# /obs group
obs <- h5$create_group("obs")
obs_names <- sprintf("cell_%03d", seq_len(n_obs))
obs$create_dataset("_index", robj = obs_names)
obs$create_dataset("donor", robj = sample(c("D1", "D2", "D3"), n_obs, replace = TRUE))

# celltype: categorical group
celltype_levels <- c("T", "B", "NK")
celltype_codes <- sample.int(length(celltype_levels), n_obs, replace = TRUE) - 1L  # 0-based
ct_grp <- obs$create_group("celltype")
ct_grp$create_attr("encoding-type", robj = "categorical", space = H5S$new("scalar"))
ct_grp$create_attr("encoding-version", robj = "0.2.0", space = H5S$new("scalar"))
ct_grp$create_attr("ordered", robj = FALSE)
ct_grp$create_dataset("codes", robj = as.integer(celltype_codes))
ct_grp$create_dataset("categories", robj = celltype_levels)

# /var group
var <- h5$create_group("var")
var_names <- sprintf("gene_%03d", seq_len(n_var))
var$create_dataset("_index", robj = var_names)
var$create_dataset("chrom", robj = sample(as.character(1:22), n_var, replace = TRUE))

# /uns group — mix of flat + nested.
uns <- h5$create_group("uns")
uns$create_dataset("organism", robj = "human")
params <- uns$create_group("params")
params$create_dataset("seed", robj = 42L)
params$create_dataset("normalization", robj = "log1p")

# /layers group (empty)
h5$create_group("layers")

# /obsm — per-obs embeddings. X_umap: (n_obs x 2) dense double.
obsm <- h5$create_group("obsm")
X_umap <- matrix(rnorm(n_obs * 2L), nrow = n_obs, ncol = 2L)
obsm$create_dataset("X_umap", robj = X_umap)

# /varm — per-var loadings. PCs: (n_var x 3) dense double.
varm <- h5$create_group("varm")
PCs <- matrix(rnorm(n_var * 3L), nrow = n_var, ncol = 3L)
varm$create_dataset("PCs", robj = PCs)

h5$close_all()
cat("Wrote ", out_path, " (nnz=", nnz, ")\n", sep = "")
