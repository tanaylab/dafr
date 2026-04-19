test_that("default inefficient handler warns", {
  # Ensure no per-category override is set
  if (exists("inefficient", envir = dafr:::.dafr_handlers, inherits = FALSE)) {
    rm("inefficient", envir = dafr:::.dafr_handlers)
  }
  withr::with_options(list(dafr.inefficient = "warn"), {
    expect_warning(emit_action("inefficient", "slow path"), "slow path")
  })
})

test_that("registered function handler receives the message", {
  register_dafr_handler("inefficient", function(msg) {
    stop("custom: ", msg)
  })
  on.exit(register_dafr_handler("inefficient", "warn"))
  expect_error(emit_action("inefficient", "x"), "custom: x")
})

test_that("ignore handler is silent", {
  register_dafr_handler("inefficient", "ignore")
  on.exit(register_dafr_handler("inefficient", "warn"))
  expect_silent(emit_action("inefficient", "quiet"))
})

test_that("error handler raises", {
  register_dafr_handler("inefficient", "error")
  on.exit(register_dafr_handler("inefficient", "warn"))
  expect_error(emit_action("inefficient", "boom"), "boom")
})
