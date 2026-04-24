test_that("handler constants equal lowercase action tokens", {
    expect_equal(ERROR_HANDLER, "error")
    expect_equal(WARN_HANDLER, "warn")
    expect_equal(IGNORE_HANDLER, "ignore")
})

test_that("inefficient_action_handler registers a string handler", {
    old <- tryCatch(
        get("inefficient", envir = dafr:::.dafr_handlers),
        error = function(e) NULL
    )
    on.exit(
        {
            if (is.null(old)) {
                if (exists("inefficient", envir = dafr:::.dafr_handlers, inherits = FALSE)) {
                    rm("inefficient", envir = dafr:::.dafr_handlers)
                }
            } else {
                assign("inefficient", old, envir = dafr:::.dafr_handlers)
            }
        },
        add = TRUE
    )
    inefficient_action_handler(IGNORE_HANDLER)
    expect_identical(get("inefficient", envir = dafr:::.dafr_handlers), "ignore")
    inefficient_action_handler(WARN_HANDLER)
    expect_identical(get("inefficient", envir = dafr:::.dafr_handlers), "warn")
    inefficient_action_handler(ERROR_HANDLER)
    expect_identical(get("inefficient", envir = dafr:::.dafr_handlers), "error")
})

test_that("inefficient_action_handler accepts functions", {
    old <- tryCatch(
        get("inefficient", envir = dafr:::.dafr_handlers),
        error = function(e) NULL
    )
    on.exit(
        {
            if (is.null(old)) {
                if (exists("inefficient", envir = dafr:::.dafr_handlers, inherits = FALSE)) {
                    rm("inefficient", envir = dafr:::.dafr_handlers)
                }
            } else {
                assign("inefficient", old, envir = dafr:::.dafr_handlers)
            }
        },
        add = TRUE
    )
    captured <- NULL
    inefficient_action_handler(function(msg) captured <<- msg)
    dafr:::emit_action("inefficient", "hello")
    expect_identical(captured, "hello")
})

test_that("inefficient_action_handler rejects bad input", {
    expect_error(inefficient_action_handler(42L),
        "string or a function"
    )
})

test_that("emit_action round-trip via ERROR_HANDLER", {
    old <- tryCatch(
        get("inefficient", envir = dafr:::.dafr_handlers),
        error = function(e) NULL
    )
    on.exit(
        {
            if (is.null(old)) {
                if (exists("inefficient", envir = dafr:::.dafr_handlers, inherits = FALSE)) {
                    rm("inefficient", envir = dafr:::.dafr_handlers)
                }
            } else {
                assign("inefficient", old, envir = dafr:::.dafr_handlers)
            }
        },
        add = TRUE
    )
    inefficient_action_handler(ERROR_HANDLER)
    expect_error(dafr:::emit_action("inefficient", "bad"), "bad")
})
