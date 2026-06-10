# Incremental consolidated-metadata refresh. Setting a property updates the
# root consolidated metadata by editing the existing index (zarr_v3_consolidate_upsert)
# instead of re-scanning + re-parsing the whole store every time. These tests
# pin (a) that the incremental index stays byte-equal to a full rebuild and
# (b) that bulk writes are no longer O(N^2).

# Read the root's inline consolidated metadata index (path -> node) off disk.
.read_root_index <- function(store) {
    raw <- store_get_bytes(store, "zarr.json")
    root <- jsonlite::fromJSON(rawToChar(raw), simplifyVector = FALSE)
    root$consolidated_metadata$metadata
}

test_that("incremental consolidation matches a full rebuild across all kinds", {
    d <- zarr_daf(tempfile(fileext = ".daf.zarr"), "w")
    add_axis(d, "cell", paste0("c", 1:6))
    add_axis(d, "gene", paste0("g", 1:4))
    set_scalar(d, "name", "x")
    set_scalar(d, "n", 3.5)
    set_vector(d, "cell", "score", as.numeric(1:6))
    set_vector(d, "cell", "sv",
               Matrix::sparseVector(c(1, 2), i = c(2L, 5L), length = 6L))
    set_matrix(d, "cell", "gene", "dense", matrix(as.numeric(1:24), 6, 4))
    set_matrix(d, "cell", "gene", "sp",
               Matrix::sparseMatrix(i = c(1L, 6L), j = c(1L, 4L),
                                    x = c(1, 2), dims = c(6L, 4L)))
    store <- S7::prop(d, "store")

    incremental <- .read_root_index(store)
    # Force a full rebuild and re-read; the two indices must be identical.
    dafr:::zarr_v3_write_consolidated(store)
    rebuilt <- .read_root_index(store)

    expect_setequal(names(incremental), names(rebuilt))
    for (k in names(rebuilt)) {
        expect_identical(incremental[[k]], rebuilt[[k]],
                         info = paste("node mismatch at", k))
    }
})

test_that("overwriting sparse with dense drops the stale nzind/nzval from the index", {
    d <- zarr_daf(tempfile(fileext = ".daf.zarr"), "w")
    add_axis(d, "cell", paste0("c", 1:5))
    set_vector(d, "cell", "v",
               Matrix::sparseVector(c(7, 9), i = c(1L, 3L), length = 5L))
    store <- S7::prop(d, "store")
    expect_true("vectors/cell/v/nzind" %in% names(.read_root_index(store)))
    # Overwrite with a dense vector: the sparse children must vanish from md.
    set_vector(d, "cell", "v", as.numeric(1:5), overwrite = TRUE)
    idx <- .read_root_index(store)
    expect_false("vectors/cell/v/nzind" %in% names(idx))
    expect_false("vectors/cell/v/nzval" %in% names(idx))
    expect_identical(idx[["vectors/cell/v"]]$node_type, "array")
    # And it still equals a full rebuild.
    dafr:::zarr_v3_write_consolidated(store)
    expect_setequal(names(idx), names(.read_root_index(store)))
})

test_that("bulk vector writes are sub-quadratic (incremental consolidation)", {
    skip_on_cran()
    write_n <- function(K) {
        d <- zarr_daf(tempfile(fileext = ".daf.zarr"), "w")
        add_axis(d, "cell", paste0("c", 1:20))
        system.time(for (i in seq_len(K))
            set_vector(d, "cell", paste0("v", i), as.numeric(1:20)))[["elapsed"]]
    }
    t100 <- write_n(100)
    t200 <- write_n(200)
    # O(N^2) would roughly quadruple (t200/t100 ~ 4); incremental keeps the
    # ratio near-linear. Allow generous slack for noise but well below quadratic.
    expect_lt(t200 / max(t100, 1e-3), 3)
    # Absolute guard: 200 writes should be quick once consolidation is O(1)-ish.
    expect_lt(t200, 4)
})
