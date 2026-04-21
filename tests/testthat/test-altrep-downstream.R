# These tests detect whether downstream code bypasses ALTREP (asserts DATAPTR
# is a real heap pointer) — which would force us to redesign.
# skip_if_not_installed() keeps CI green if a package isn't present.

make_test_dgC_files <- function() {
    set.seed(42)
    nr <- 200
    nc <- 100
    dense <- matrix(ifelse(runif(nr * nc) < 0.1, round(rexp(nr * nc) * 10), 0),
        nrow = nr, ncol = nc
    )
    m <- methods::as(dense, "CsparseMatrix")
    d <- new_tempdir(envir = parent.frame())
    writeBin(m@x, file.path(d, "x.bin"), size = 8L)
    writeBin(as.integer(m@i), file.path(d, "i.bin"), size = 4L)
    writeBin(as.integer(m@p), file.path(d, "p.bin"), size = 4L)
    list(dir = d, reference = m, nrow = nr, ncol = nc, nnz = length(m@x))
}

test_that("Seurat::CreateSeuratObject accepts mmap-backed dgCMatrix", {
    skip_if_not_installed("Seurat")
    tf <- make_test_dgC_files()
    rn <- paste0("gene", seq_len(tf$nrow))
    cn <- paste0("cell", seq_len(tf$ncol))
    m <- mmap_dgCMatrix(
        x_path = file.path(tf$dir, "x.bin"),
        i_path = file.path(tf$dir, "i.bin"),
        p_path = file.path(tf$dir, "p.bin"),
        nrow = tf$nrow, ncol = tf$ncol, nnz = tf$nnz,
        dimnames = list(rn, cn)
    )
    obj <- Seurat::CreateSeuratObject(counts = m)
    expect_s4_class(obj, "Seurat")
    expect_equal(ncol(obj), tf$ncol)
})

test_that("scran::quickCluster tolerates mmap-backed dgCMatrix", {
    skip_if_not_installed("scran")
    skip_if_not_installed("SingleCellExperiment")
    tf <- make_test_dgC_files()
    m <- mmap_dgCMatrix(
        x_path = file.path(tf$dir, "x.bin"),
        i_path = file.path(tf$dir, "i.bin"),
        p_path = file.path(tf$dir, "p.bin"),
        nrow = tf$nrow, ncol = tf$ncol, nnz = tf$nnz,
        dimnames = list(
            paste0("g", seq_len(tf$nrow)),
            paste0("c", seq_len(tf$ncol))
        )
    )
    sce <- SingleCellExperiment::SingleCellExperiment(assays = list(counts = m))
    cl <- scran::quickCluster(sce, min.size = 10)
    expect_length(cl, tf$ncol)
})
