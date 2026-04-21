test_that("memory_daf() returns a DafWriter with the right class ancestry", {
    d <- memory_daf()
    expect_s3_class(d, "dafr::MemoryDaf")
    expect_true(inherits(d, "dafr::DafWriter"))
    expect_true(inherits(d, "dafr::DafReader"))
})

test_that("memory_daf() default name is 'memory'", {
    expect_equal(S7::prop(memory_daf(), "name"), "memory")
})

test_that("memory_daf(name = ...) sets the name", {
    expect_equal(S7::prop(memory_daf(name = "test!"), "name"), "test!")
})

test_that("memory_daf() initialises empty scalars/axes/vectors/matrices envs", {
    d <- memory_daf()
    internal <- S7::prop(d, "internal")
    expect_true(is.environment(internal$scalars))
    expect_true(is.environment(internal$axes))
    expect_true(is.environment(internal$vectors))
    expect_true(is.environment(internal$matrices))
    expect_equal(length(ls(internal$scalars, all.names = TRUE)), 0L)
    expect_equal(length(ls(internal$axes, all.names = TRUE)), 0L)
    expect_equal(length(ls(internal$vectors, all.names = TRUE)), 0L)
    expect_equal(length(ls(internal$matrices, all.names = TRUE)), 0L)
})

test_that("memory_daf() gets fresh cache / counter envs per instance", {
    a <- memory_daf()
    b <- memory_daf()
    expect_false(identical(S7::prop(a, "cache"), S7::prop(b, "cache")))
    expect_false(identical(S7::prop(a, "axis_version_counter"), S7::prop(b, "axis_version_counter")))
    expect_false(identical(S7::prop(a, "vector_version_counter"), S7::prop(b, "vector_version_counter")))
    expect_false(identical(S7::prop(a, "matrix_version_counter"), S7::prop(b, "matrix_version_counter")))
})
