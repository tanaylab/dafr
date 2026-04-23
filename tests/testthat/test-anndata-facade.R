# ---- Helper ----
.make_facade_daf <- function() {
    d <- memory_daf(name = "facade_test")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(1:6, 3, 2))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    set_vector(d, "cell", "age", c(1L, 2L, 3L))
    set_vector(d, "gene", "chrom", c("1", "2"))
    set_scalar(d, "organism", "human")
    d
}

# ---- Tests ----
test_that("DafAnnData constructs with explicit axes", {
    d <- .make_facade_daf()
    ann <- DafAnnData$new(d, obs_axis = "cell", var_axis = "gene", x_name = "UMIs")
    expect_s3_class(ann, "DafAnnData")
    expect_identical(ann$obs_axis, "cell")
    expect_identical(ann$var_axis, "gene")
    expect_identical(ann$x_name, "UMIs")
})

test_that("DafAnnData auto-detects obs and var axes", {
    d <- .make_facade_daf()
    ann <- DafAnnData$new(d)
    expect_identical(ann$obs_axis, "cell")
    expect_identical(ann$var_axis, "gene")
})

test_that("DafAnnData auto-detects metacell obs axis when cell absent", {
    d <- memory_daf()
    add_axis(d, "metacell", c("m1", "m2"))
    add_axis(d, "gene", c("g1", "g2"))
    set_matrix(d, "metacell", "gene", "UMIs", matrix(1:4, 2, 2))
    ann <- DafAnnData$new(d)
    expect_identical(ann$obs_axis, "metacell")
})

test_that("DafAnnData errors when no suitable obs axis exists", {
    d <- memory_daf()
    add_axis(d, "gene", c("g1"))
    expect_error(DafAnnData$new(d), "obs_axis")
})

test_that("DafAnnData rejects non-daf input", {
    expect_error(DafAnnData$new(NULL), "DafReader")
})

test_that("as_anndata returns a DafAnnData", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_s3_class(ann, "DafAnnData")
})

test_that("X active binding returns the matrix", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    m <- ann$X
    expect_identical(dim(m), c(3L, 2L))
    expect_identical(m[1, 1], 1L)
})

test_that("obs active binding returns a data.frame with axis entries as rownames", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    obs <- ann$obs
    expect_s3_class(obs, "data.frame")
    expect_identical(rownames(obs), c("c1", "c2", "c3"))
    expect_setequal(colnames(obs), c("donor", "age"))
})

test_that("var active binding returns a data.frame", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    var <- ann$var
    expect_s3_class(var, "data.frame")
    expect_identical(rownames(var), c("g1", "g2"))
})

test_that("obs_names / var_names return character vectors", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_identical(ann$obs_names, c("c1", "c2", "c3"))
    expect_identical(ann$var_names, c("g1", "g2"))
})

test_that("n_obs / n_vars / shape return correct integers", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_identical(ann$n_obs, 3L)
    expect_identical(ann$n_vars, 2L)
    expect_identical(ann$shape, c(3L, 2L))
})

test_that("uns returns named list of scalars", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    u <- ann$uns
    expect_type(u, "list")
    expect_identical(u$organism, "human")
})

test_that("layers returns empty list when no other matrices exist", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_length(ann$layers, 0L)
})

test_that("layers returns named list of extra matrices", {
    d <- .make_facade_daf()
    set_matrix(d, "cell", "gene", "normalized",
        matrix(as.numeric(1:6) / 10, 3, 2))
    ann <- as_anndata(d)
    l <- ann$layers
    expect_length(l, 1L)
    expect_identical(names(l), "normalized")
})

# ---- Read-only guards ----

test_that("X write fails with exact wrapper message", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    expect_error(
        ann$X <- matrix(0, 3, 2),
        "DafAnnData facade is read-only. Use the underlying Daf object to modify data."
    )
})

test_that("all 10 active bindings error on assignment", {
    d <- .make_facade_daf()
    ann <- as_anndata(d)
    for (nm in c("X", "obs", "var", "layers", "uns",
                 "obs_names", "var_names", "n_obs", "n_vars", "shape")) {
        expect_error(eval(parse(text = sprintf("ann$%s <- NULL", nm))),
            "read-only", info = nm)
    }
})
