#!/usr/bin/env Rscript
# Generates inst/extdata/small_test.h5ad — 50 obs x 20 var minimal fixture.
# Run once from the package root; commit the output.

suppressPackageStartupMessages(library(hdf5r))

set.seed(42)
out_path <- "inst/extdata/small_test.h5ad"
unlink(out_path)
dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)

h5 <- H5File$new(out_path, mode = "w")

# Root attributes
h5$create_attr("encoding-type", robj = "anndata", space = H5S$new("scalar"))
h5$create_attr("encoding-version", robj = "0.1.0", space = H5S$new("scalar"))

n_obs <- 50L
n_var <- 20L

# X: dense double matrix (obs x var)
X <- matrix(rpois(n_obs * n_var, lambda = 3), n_obs, n_var)
storage.mode(X) <- "double"
h5$create_dataset("X", robj = X)

# /obs group
obs <- h5$create_group("obs")
obs_names <- sprintf("cell_%03d", seq_len(n_obs))
obs$create_dataset("_index", robj = obs_names)
obs$create_dataset("donor", robj = sample(c("D1", "D2", "D3"), n_obs, replace = TRUE))
obs$create_dataset("age", robj = sample.int(100L, n_obs, replace = TRUE))

# /var group
var <- h5$create_group("var")
var_names <- sprintf("gene_%03d", seq_len(n_var))
var$create_dataset("_index", robj = var_names)
var$create_dataset("chrom", robj = sample(as.character(1:22), n_var, replace = TRUE))

# /uns group
uns <- h5$create_group("uns")
uns$create_dataset("organism", robj = "human")

# /layers group (empty)
h5$create_group("layers")

h5$close_all()
cat("Wrote ", out_path, "\n", sep = "")
