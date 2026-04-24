test_that("default options are set on load", {
    expect_equal(dafr_opt("dafr.cache.memory_mb"), 1024L)
    expect_equal(dafr_opt("dafr.inefficient"), "warn")
    expect_true(dafr_opt("dafr.mmap"))
})

test_that("overridden options flow through dafr_opt", {
    withr::with_options(list(dafr.cache.memory_mb = 512L), {
        expect_equal(dafr_opt("dafr.cache.memory_mb"), 512L)
    })
})

test_that("dafr_opt rejects unknown names", {
    expect_error(dafr_opt("dafr.bogus"), "name %in% names")
})

test_that("empty_cache emits a cli message when dafr.verbose = TRUE", {
    d <- memory_daf(name = "t")
    withr::with_options(list(dafr.verbose = TRUE), {
        expect_message(empty_cache(d), "empty_cache")
    })
    withr::with_options(list(dafr.verbose = FALSE), {
        expect_no_message(empty_cache(d))
    })
})

test_that("add_axis emits a cli message when dafr.verbose = TRUE", {
    d <- memory_daf(name = "t")
    withr::with_options(list(dafr.verbose = TRUE), {
        expect_message(add_axis(d, "cell", c("A", "B")), "add_axis")
    })
})
