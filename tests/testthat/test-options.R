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
