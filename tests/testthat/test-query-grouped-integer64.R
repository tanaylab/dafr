# integer64 input to grouped reductions was reinterpreted as Float64
# byte-for-byte by .grouped_vector_builtin (rowsum on the REALSXP storage),
# producing subnormal garbage. Julia DAF.jl returns the correct sums.
# Reference: DataAxesFormats.jl src/queries.jl reduce_vector_group_by.

.fx <- function() {
    d <- memory_daf(name = "i64")
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    add_axis(d, "type", c("U", "V", "W"))
    set_vector(d, "cell", "age",
        bit64::as.integer64(c(10L, 20L, 30L, 40L, 50L)))
    set_vector(d, "cell", "type", c("U", "V", "U", "W", "V"))
    set_vector(d, "cell", "is_low", c(TRUE, TRUE, FALSE, FALSE, FALSE))
    d
}

test_that("grouped Sum on integer64 vector returns actual sums (not byte-reinterpret garbage)", {
    d <- .fx()
    v <- get_query(d, "@ cell : age / type >> Sum")
    expect_equal(unname(v), c(40, 70, 40))
    expect_equal(names(v), c("U", "V", "W"))
})

test_that("grouped Mean on integer64 vector matches Julia", {
    d <- .fx()
    v <- get_query(d, "@ cell : age / type >> Mean")
    expect_equal(unname(v), c(20, 35, 40))
})

test_that("grouped Min/Max on integer64 vector matches Julia", {
    d <- .fx()
    expect_equal(unname(get_query(d, "@ cell : age / type >> Min")),
        c(10, 20, 40))
    expect_equal(unname(get_query(d, "@ cell : age / type >> Max")),
        c(30, 50, 40))
})

test_that("grouped Var on integer64 vector matches Julia", {
    d <- .fx()
    # Julia DAF.jl >> Var is population variance (sum((x-mean)^2)/N).
    # U: ages 10, 30 -> mean 20, var = ((10-20)^2 + (30-20)^2)/2 = 100
    # V: ages 20, 50 -> mean 35, var = ((20-35)^2 + (50-35)^2)/2 = 225
    # W: ages 40     -> mean 40, var = 0
    v <- get_query(d, "@ cell : age / type >> Var")
    expect_equal(unname(v), c(100, 225, 0))
})

test_that("grouped Median on integer64 vector matches Julia (was: \"only type==0 ('qtile') supported\")", {
    d <- .fx()
    v <- get_query(d, "@ cell : age / type >> Median")
    expect_equal(unname(v), c(20, 35, 40))
})

test_that("grouped Sum on integer64 with =@ axis broadcast matches Julia", {
    d <- .fx()
    v <- get_query(d, "@ cell : age / type =@ >> Sum || 0")
    expect_equal(unname(v[c("U", "V", "W")]), c(40, 70, 40))
})

test_that("grouped reduction on bool group key still works for integer64 values", {
    d <- .fx()
    # is_low = [T,T,F,F,F] -> FALSE: 30+40+50=120, TRUE: 10+20=30
    v <- get_query(d, "@ cell : age / is_low >> Sum")
    expect_equal(sort(unname(v)), c(30, 120))
})

# Matrix int64 path: reductions on a stored int64 matrix (e.g. when the
# upstream backend returns int64 UMIs) used to dispatch through rowSums /
# matrixStats with the int64 bytes interpreted as doubles.

.fx_mat <- function() {
    d <- memory_daf(name = "i64m")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    set_vector(d, "cell", "type", c("U", "V", "U", "V"))
    M <- bit64::as.integer64(c(1L, 2L, 3L, 4L, 5L, 6L,
                               7L, 8L, 9L, 10L, 11L, 12L))
    dim(M) <- c(4, 3)
    dimnames(M) <- list(c("A", "B", "C", "D"), c("g1", "g2", "g3"))
    set_matrix(d, "cell", "gene", "UMIs", M)
    d
}

test_that(">| Sum on integer64 matrix returns per-row sums (not byte garbage)", {
    d <- .fx_mat()
    v <- get_query(d, "@ cell @ gene :: UMIs >| Sum")
    # Row sums: A=1+5+9=15, B=2+6+10=18, C=3+7+11=21, D=4+8+12=24
    expect_equal(unname(v), c(15, 18, 21, 24))
})

test_that(">- Sum on integer64 matrix returns per-column sums", {
    d <- .fx_mat()
    v <- get_query(d, "@ cell @ gene :: UMIs >- Sum")
    # Col sums: g1=1+2+3+4=10, g2=5+6+7+8=26, g3=9+10+11+12=42
    expect_equal(unname(v), c(10, 26, 42))
})

test_that(">> Sum on integer64 matrix returns total sum", {
    d <- .fx_mat()
    v <- get_query(d, "@ cell @ gene :: UMIs >> Sum")
    expect_equal(unname(as.double(v)), 78)
})

test_that("grouped matrix rows reduction (-/) on int64 matrix matches Julia", {
    d <- .fx_mat()
    # rows grouped by type: U = rows A,C; V = rows B,D.
    # Per-col Sum: g1 U=1+3=4, V=2+4=6; g2 U=5+7=12, V=6+8=14; g3 U=9+11=20, V=10+12=22
    v <- get_query(d, "@ cell @ gene :: UMIs -/ type >- Sum")
    expect_equal(unname(v["U", ]), c(4, 12, 20))
    expect_equal(unname(v["V", ]), c(6, 14, 22))
})
