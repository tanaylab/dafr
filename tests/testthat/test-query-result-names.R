# Regression: query results must carry axis-entry names so users can
# index/slice them by entry. This was lost in the native port; the old
# Julia-wrapper version preserved names from NamedArrays.

test_that("LookupVector result has names = axis entries", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10, 20, 30))
    out <- get_query(d, "@ cell : age")
    expect_equal(names(out), c("c1", "c2", "c3"))
    expect_equal(unname(out), c(10, 20, 30))
})

test_that("LookupVector with mask filter has names = surviving entries", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "age", c(10, 20, 30))
    out <- get_query(d, "@ cell [ age > 15 ] : age")
    expect_equal(names(out), c("c2", "c3"))
    expect_equal(unname(out), c(20, 30))
})

test_that("LookupMatrix result has dimnames = (rows entries, cols entries)", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(c(1, 2, 3, 4), 2, 2))
    out <- get_query(d, "@ cell @ gene :: UMIs")
    expect_equal(rownames(out), c("c1", "c2"))
    expect_equal(colnames(out), c("g1", "g2"))
})

test_that("LookupMatrix with row mask carries surviving rownames", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE))
    set_matrix(d, "cell", "gene", "UMIs", matrix(1:6, 3, 2))
    out <- get_query(d, "@ cell [ keep ] @ gene :: UMIs")
    expect_equal(rownames(out), c("c1", "c3"))
    expect_equal(colnames(out), c("g1", "g2"))
})

test_that("LookupVector with IfMissing default still carries names", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    out <- get_query(d, "@ cell : missing || 0 Int64")
    expect_equal(names(out), c("c1", "c2"))
})

test_that("LookupMatrix with IfMissing default still carries dimnames", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2"))
    out <- get_query(d, "@ cell @ gene :: missing || 0 Int64")
    expect_equal(rownames(out), c("c1", "c2"))
    expect_equal(colnames(out), c("g1", "g2"))
})

test_that("string-typed LookupVector also has names", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
    out <- get_query(d, "@ cell : donor")
    expect_equal(names(out), c("c1", "c2", "c3"))
    expect_equal(unname(out), c("d1", "d2", "d1"))
})

test_that("transposed LookupMatrix gets dimnames in queried order", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(1:6, 2, 3))
    out <- get_query(d, "@ gene @ cell :: UMIs")
    expect_equal(rownames(out), c("g1", "g2", "g3"))
    expect_equal(colnames(out), c("c1", "c2"))
})

test_that("matrix-column slice `@ axis :: m @ other-axis = entry` is named", {
    # Shape used by mcsharp/scripts/analyze_sharpening.r:
    #   @ base_block :: correlation_with_most @ gene = <gene>
    d <- memory_daf(name = "t")
    add_axis(d, "block", c("B01", "B02", "B03"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    set_matrix(d, "block", "gene", "score",
               matrix(seq_len(9) / 10, 3, 3))
    out <- get_query(d, "@ block :: score @ gene = g2")
    expect_equal(names(out), c("B01", "B02", "B03"))
    expect_equal(unname(out), c(0.4, 0.5, 0.6))
})

test_that("square col-slice `@ axis :: m @| entry` is named", {
    d <- memory_daf(name = "t")
    add_axis(d, "metacell", c("M01", "M02", "M03"))
    set_matrix(d, "metacell", "metacell", "edge",
               matrix(seq_len(9) / 10, 3, 3))
    out <- get_query(d, "@ metacell :: edge @| M02")
    expect_equal(names(out), c("M01", "M02", "M03"))
})

test_that("square row-slice `@ axis :: m @- entry` is named", {
    d <- memory_daf(name = "t")
    add_axis(d, "metacell", c("M01", "M02", "M03"))
    set_matrix(d, "metacell", "metacell", "edge",
               matrix(seq_len(9) / 10, 3, 3))
    out <- get_query(d, "@ metacell :: edge @- M02")
    expect_equal(names(out), c("M01", "M02", "M03"))
})

test_that("entry-pick on a matrix `@ rows @ cols :: m @ rows = entry` is named", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(1:6, 2, 3))
    # Pick a row -> remaining axis is gene; result indexed by gene.
    out <- get_query(d, "@ cell @ gene :: UMIs @ cell = c1")
    expect_equal(names(out), c("g1", "g2", "g3"))
})
