# Parity test for is_leaf().
#
# Upstream DataAxesFormats.jl's `is_leaf` returns TRUE for storage formats
# that own their on-disk/in-memory state directly (MemoryDaf, FilesDaf,
# ZarrDaf, HttpDaf) and FALSE for wrappers (ReadOnlyChain, WriteChain,
# ContractDaf, DafView, DafReadOnlyWrapper). Used by reorder_axes! to
# reject non-leaf inputs with a clear error.

test_that("is_leaf is TRUE for MemoryDaf", {
    expect_true(is_leaf(memory_daf("m")))
})

test_that("is_leaf is TRUE for FilesDaf and FilesDafReadOnly", {
    p <- withr::local_tempdir("dafr-leaf-fd-")
    fd <- files_daf(p, "w+", "fd")
    expect_true(is_leaf(fd))
    rm(fd); gc()
    fdro <- files_daf(p, "r", "fd")
    expect_true(is_leaf(fdro))
})

test_that("is_leaf is TRUE for ZarrDaf and ZarrDafReadOnly", {
    p <- tempfile("dafr-leaf-zd-", fileext = ".daf.zarr")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    zd <- zarr_daf(p, "w+", "zd"); rm(zd); gc()
    zdro <- zarr_daf(p, "r", "zd")
    expect_true(is_leaf(zdro))
    zd2 <- zarr_daf(":memory:", "w", "zdm")
    expect_true(is_leaf(zd2))
})

test_that("is_leaf is FALSE for chain wrappers", {
    a <- memory_daf("a"); add_axis(a, "cell", c("c1", "c2"))
    b <- memory_daf("b"); add_axis(b, "cell", c("c1", "c2"))
    ch <- chain_reader(list(a, b), name = "chain")
    expect_false(is_leaf(ch))
})

test_that("is_leaf is FALSE for view wrapper", {
    a <- memory_daf("a"); add_axis(a, "cell", c("c1", "c2"))
    v <- viewer(a, name = "view")
    expect_false(is_leaf(v))
})

test_that("reorder_axes rejects non-leaf chain with clear message", {
    a <- memory_daf("a"); add_axis(a, "cell", c("c1", "c2"))
    b <- memory_daf("b"); add_axis(b, "cell", c("c1", "c2"))
    ch <- chain_writer(list(a, b), name = "chain")
    expect_error(
        reorder_axes(ch, cell = c(2L, 1L)),
        "non-leaf type"
    )
})
