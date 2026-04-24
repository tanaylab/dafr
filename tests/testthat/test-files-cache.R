test_that("FilesDaf format_get_vector caches ALTREP view in mapped tier", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "x", c(1.5, 2.5, 3.5))
    v1 <- format_get_vector(d, "cell", "x")
    v2 <- format_get_vector(d, "cell", "x")
    ce <- S7::prop(d, "cache")
    expect_true(exists(cache_key_vector("cell", "x"), envir = ce$mapped))
    expect_equal(v1, v2)
})

test_that("writing bumps the counter and invalidates mapped cache", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1.0, 2.0))
    v1 <- format_get_vector(d, "cell", "x")
    set_vector(d, "cell", "x", c(10.0, 20.0), overwrite = TRUE)
    v2 <- format_get_vector(d, "cell", "x")
    expect_equal(unname(v2), c(10.0, 20.0))
})

test_that("FilesDaf format_get_matrix caches in mapped tier", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "m", matrix(1:4, 2))
    m1 <- format_get_matrix(d, "cell", "gene", "m")
    m2 <- format_get_matrix(d, "cell", "gene", "m")
    ce <- S7::prop(d, "cache")
    expect_true(exists(cache_key_matrix("cell", "gene", "m"), envir = ce$mapped))
    expect_equal(m1, m2)
})

test_that("dafr.mmap = FALSE returns non-ALTREP vectors for dense reads", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "x", c(1.0, 2.0))
    # Clear mapped cache before the fallback read so we're exercising the fallback, not a hit
    empty_cache(d, clear = "mapped")
    v <- withr::with_options(
        list(dafr.mmap = FALSE),
        format_get_vector(d, "cell", "x")
    )
    expect_equal(unname(v), c(1.0, 2.0))
    expect_false(is_altrep(v))
})

test_that("dafr.mmap = FALSE matrix reads also skip ALTREP", {
    dir <- new_tempdir()
    d <- files_daf(dir, mode = "w+")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    set_matrix(d, "cell", "gene", "m", matrix(c(1.0, 2.0, 3.0, 4.0), 2, 2))
    empty_cache(d, clear = "mapped")
    m <- withr::with_options(
        list(dafr.mmap = FALSE),
        format_get_matrix(d, "cell", "gene", "m")
    )
    expect_false(is_altrep(m))
})
