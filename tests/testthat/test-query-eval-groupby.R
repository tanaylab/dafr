test_that("/ GroupBy groups vector entries", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("d1", "d1", "d2", "d2"))
    set_vector(d, "cell", "UMIs", c(1, 2, 10, 20))
    v <- get_query(d, "@ cell : UMIs / donor >| Sum")
    expect_equal(v, c(d1 = 3, d2 = 30))
})

test_that("* CountBy builds co-occurrence matrix", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "sex", c("M", "M", "F", "F"))
    set_vector(d, "cell", "type", c("A", "B", "A", "B"))
    m <- get_query(d, "@ cell : sex * type")
    expect_equal(dim(m), c(2L, 2L))
    expect_equal(sort(rownames(m)), c("F", "M"))
    expect_equal(sort(colnames(m)), c("A", "B"))
})
