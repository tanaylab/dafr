test_that("DafReader is abstract (cannot be instantiated directly)", {
    expect_error(DafReader(), "abstract")
})

test_that("DafReadOnly and DafWriter both inherit from DafReader", {
    expect_identical(S7::prop(DafReadOnly, "parent"), DafReader)
    expect_identical(S7::prop(DafWriter, "parent"), DafReader)
})

test_that("DafReader declares the expected mutable-env properties", {
    props <- S7::prop(DafReader, "properties")
    expect_setequal(
        names(props),
        c(
            "name", "internal", "cache",
            "axis_version_counter", "vector_version_counter", "matrix_version_counter"
        )
    )
})

test_that("new_internal_env returns a mutable env with closed=FALSE", {
    e <- new_internal_env()
    expect_true(is.environment(e))
    expect_false(e$closed)
})

test_that("new_cache_env has three tier sub-envs + LRU bookkeeping", {
    e <- new_cache_env()
    expect_setequal(
        ls(e, all.names = TRUE),
        c("mapped", "memory", "query", "lru", "bytes", "cap")
    )
    expect_true(is.environment(e$mapped))
    expect_true(is.environment(e$memory))
    expect_true(is.environment(e$query))
    expect_identical(e$lru, character(0L))
    expect_equal(e$bytes, 0)
    expect_true(is.numeric(e$cap) && e$cap > 0)
})

test_that("new_counter_env is an empty environment", {
    e <- new_counter_env()
    expect_true(is.environment(e))
    expect_length(ls(e, all.names = TRUE), 0L)
})
