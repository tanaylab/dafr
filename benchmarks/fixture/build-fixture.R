#!/usr/bin/env Rscript
# Build the shared FilesDaf fixture corpus for the bake-off harness.
# Idempotent: existing fixtures are removed and rebuilt.

options(error = NULL)   # prevent interactive recover() in batch mode

suppressPackageStartupMessages({
    library(dafr)
    library(Matrix)
})

.script_dir <- function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", args, value = TRUE)
    if (length(file_arg) == 0L) stop("cannot determine script path")
    normalizePath(dirname(sub("--file=", "", file_arg)), winslash = "/")
}

fixture_root <- file.path(.script_dir(), "data")
if (dir.exists(fixture_root)) unlink(fixture_root, recursive = TRUE)
dir.create(fixture_root, recursive = TRUE)

.sha256_dir <- function(path) {
    files <- sort(list.files(path, recursive = TRUE, full.names = TRUE))
    hashes <- vapply(files, function(f) {
        digest::digest(file = f, algo = "sha256")
    }, character(1L))
    digest::digest(paste(basename(files), hashes, collapse = "\n"),
                   algo = "sha256")
}

.log <- function(name, path) {
    cat(sprintf("  built %-20s sha256=%s\n  at %s\n",
                name, substr(.sha256_dir(path), 1, 16), path))
}

# ---------- fixture 1: cells_daf ----------
cells_daf_dir <- file.path(fixture_root, "cells_daf")
src <- example_cells_daf()
dst <- files_daf(cells_daf_dir, name = "cells", mode = "w+")
copy_all(source = src, destination = dst, overwrite = TRUE)
rm(dst); invisible(gc())
.log("cells_daf", cells_daf_dir)

# ---------- fixture 3: chain_triple ----------
chain_root <- file.path(fixture_root, "chain_triple")
dir.create(chain_root)
base_dir <- file.path(chain_root, "base")
mid_dir  <- file.path(chain_root, "mid")
leaf_dir <- file.path(chain_root, "leaf")
base <- files_daf(base_dir, name = "base", mode = "w+")
copy_all(source = example_cells_daf(), destination = base, overwrite = TRUE)
mid  <- files_daf(mid_dir,  name = "mid",  mode = "w+")
invisible(complete_chain(base_daf = base, new_daf = mid, absolute = TRUE))
leaf <- files_daf(leaf_dir, name = "leaf", mode = "w+")
invisible(complete_chain(base_daf = mid,  new_daf = leaf, absolute = TRUE))
.log("chain_triple", chain_root)

# ---------- fixture 4: view_renamed ----------
view_root <- file.path(fixture_root, "view_renamed")
dir.create(view_root)
vr_base_dir <- file.path(view_root, "base")
vr_leaf_dir <- file.path(view_root, "leaf")
vr_base <- files_daf(vr_base_dir, name = "base", mode = "w+")
copy_all(source = example_cells_daf(), destination = vr_base, overwrite = TRUE)
vr_leaf <- files_daf(vr_leaf_dir, name = "leaf", mode = "w+")
invisible(complete_chain(
    base_daf = vr_base, new_daf = vr_leaf,
    axes = list(list("renamed_cell", "@ cell")),
    absolute = TRUE
))
.log("view_renamed", view_root)

# ---------- fixture 5: mmap_reopen ----------
# Same bytes as cells_daf, isolated directory so reopen benchmarks don't
# race against in-memory handles from the other fixtures.
mmap_dir <- file.path(fixture_root, "mmap_reopen")
dst2 <- files_daf(mmap_dir, name = "mmap", mode = "w+")
copy_all(source = example_cells_daf(), destination = dst2, overwrite = TRUE)
rm(dst2); invisible(gc())
.log("mmap_reopen", mmap_dir)

cat("\nNext: run benchmarks/fixture/build-big-sparse.R\n")
