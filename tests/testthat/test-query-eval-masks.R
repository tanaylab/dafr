test_that("mask with '>' comparator filters axis", {
    d <- memory_daf(name = "t")
    add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
    set_vector(d, "donor", "age", c(10, 50, 70, 90))
    expect_equal(get_query(d, "@ donor [ age > 60 ]"), c(d3 = "d3", d4 = "d4"))
})

test_that("mask with '=' comparator filters axis", {
    d <- memory_daf(name = "t")
    add_axis(d, "donor", c("d1", "d2", "d3"))
    set_vector(d, "donor", "sex", c("M", "F", "M"))
    expect_equal(get_query(d, "@ donor [ sex = M ]"), c(d1 = "d1", d3 = "d3"))
})

test_that("negated mask filters axis", {
    d <- memory_daf(name = "t")
    add_axis(d, "gene", c("g1", "g2", "g3"))
    set_vector(d, "gene", "is_lateral", c(TRUE, FALSE, TRUE))
    expect_equal(get_query(d, "@ gene [ ! is_lateral ]"), c(g2 = "g2"))
})

test_that("mask with '~' regex match filters axis", {
    d <- memory_daf(name = "t")
    add_axis(d, "gene", c("HOX1", "MYC", "HOX2"))
    set_vector(d, "gene", "symbol", c("HOX1", "MYC", "HOX2"))
    expect_equal(get_query(d, "@ gene [ symbol ~ ^HOX ]"), c(HOX1 = "HOX1", HOX2 = "HOX2"))
})

test_that("mask AND combines two properties", {
    d <- memory_daf(name = "t")
    add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
    set_vector(d, "donor", "age", c(10, 70, 70, 10))
    set_vector(d, "donor", "sex", c("M", "M", "F", "F"))
    expect_equal(get_query(d, "@ donor [ age > 60 & sex = M ]"), c(d2 = "d2"))
})

test_that("mask OR combines two properties", {
    d <- memory_daf(name = "t")
    add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
    set_vector(d, "donor", "age", c(10, 70, 70, 10))
    set_vector(d, "donor", "sex", c("M", "M", "F", "F"))
    expect_setequal(
        get_query(d, "@ donor [ age > 60 | sex = F ]"),
        c("d2", "d3", "d4")
    )
})

test_that("mask XOR and negated variants work", {
    d <- memory_daf(name = "t")
    add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
    set_vector(d, "donor", "a", c(TRUE, TRUE, FALSE, FALSE))
    set_vector(d, "donor", "b", c(TRUE, FALSE, TRUE, FALSE))
    expect_setequal(get_query(d, "@ donor [ a ^ b ]"), c("d2", "d3"))
    expect_setequal(get_query(d, "@ donor [ a & ! b ]"), "d2")
})

test_that("NA in masked property drops entries (Julia parity)", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    set_vector(d, "cell", "score", c(1.0, NA_real_, 3.0, NA_real_))
    # '> 0' on NA returns NA; Julia drops NA mask entries silently.
    # Expected kept entries: A, C.
    result <- get_query(d, "@ cell [ score > 0 ]")
    expect_identical(result, c(A = "A", C = "C"))
})

# E1: mask after the second axis filters the cols axis. Pre-fix this
# raised "'[' mask requires axis in scope".

test_that("E1: cols-axis mask `@ rows @ cols [ filter ] :: M` narrows the matrix", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_vector(d, "gene", "is_q", c(TRUE, FALSE, TRUE))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, byrow = FALSE,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    res <- get_query(d, "@ cell @ gene [ is_q ] :: UMIs")
    expect_equal(dim(res), c(2L, 2L))
    expect_equal(colnames(res), c("A", "C"))
    expect_equal(unname(res), matrix(c(0, 3, 2, 5), 2, 2))
})

test_that("E1: cols-axis mask flowing into >| / >- reductions", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_vector(d, "gene", "is_q", c(FALSE, FALSE, FALSE))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, byrow = FALSE,
        dimnames = list(c("X", "Y"), c("A", "B", "C"))
    ))
    # All cols filtered out: >| reduces an empty cols dim per row -> needs IfMissing.
    out <- get_query(d, "@ cell @ gene [ is_q ] :: UMIs >| Sum || 0")
    expect_equal(out, c(X = 0, Y = 0))
    # >- reduces rows -> result is along the (empty) cols axis -> empty vector.
    out2 <- get_query(d, "@ cell @ gene [ is_q ] :: UMIs >- Sum || 0")
    expect_length(out2, 0L)
    # Without IfMissing, the empty-matrix reduction errors uniformly.
    expect_error(
        get_query(d, "@ cell @ gene [ is_q ] :: UMIs >| Sum"),
        "IfMissing|empty"
    )
})

# E2: virtual `name` property — both as a mask comparator and as a `: name`
# lookup — returns the axis-entry vector.

test_that("E2: `[ name = X ]` matches the axis entry name", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("X", "Y", "Z"))
    expect_equal(get_query(d, "@ cell [ name = Y ]"), c(Y = "Y"))
    expect_setequal(get_query(d, "@ cell [ ! name = Y ]"), c("X", "Z"))
})

test_that("E2: `: name` returns the axis-entry vector", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("X", "Y", "Z"))
    out <- get_query(d, "@ cell : name")
    expect_equal(unname(out), c("X", "Y", "Z"))
})

test_that("E2: `[ name ~ pattern ]` regex-matches axis entries", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("HOX1", "MYC", "HOX2"))
    expect_setequal(get_query(d, "@ cell [ name ~ ^HOX ]"), c("HOX1", "HOX2"))
})
