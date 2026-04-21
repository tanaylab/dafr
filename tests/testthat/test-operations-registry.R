# Helper: remove any test-created names from the shared ops env so tests
# do not pollute each other.  Call withr::defer(.clear_test_ops()) inside
# each test_that block that registers a name.
.clear_test_ops <- function() {
    test_names <- c("TestSum", "TestAdd1", "DupTest", "OverTest")
    for (n in test_names) {
        .ops_env$reductions[[n]] <- NULL
        .ops_env$eltwise[[n]] <- NULL
    }
}

test_that("register_reduction stores a function retrievable by name", {
    withr::defer(.clear_test_ops())
    f <- function(x, ...) sum(x)
    register_reduction("TestSum", f)
    expect_identical(get_reduction("TestSum"), f)
    expect_true("TestSum" %in% registered_reductions())
})

test_that("register_eltwise stores a function retrievable by name", {
    withr::defer(.clear_test_ops())
    f <- function(x, ...) x + 1
    register_eltwise("TestAdd1", f)
    expect_identical(get_eltwise("TestAdd1"), f)
    expect_true("TestAdd1" %in% registered_eltwise())
})

test_that("get_reduction raises for unknown name", {
    expect_error(get_reduction("NoSuchOp"), "unknown reduction operation")
})

test_that("get_eltwise raises for unknown name", {
    expect_error(get_eltwise("NoSuchOp"), "unknown eltwise operation")
})

test_that("register_reduction errors on non-function fn", {
    expect_error(
        register_reduction("x", "not a function"),
        "must be a function"
    )
})

test_that("register_eltwise errors on non-function fn", {
    expect_error(
        register_eltwise("x", list(a = 1)),
        "must be a function"
    )
})

test_that("register_reduction rejects names with forbidden chars", {
    expect_error(register_reduction("a/b", identity), "forbidden")
})

test_that("register_reduction errors on collision without overwrite", {
    withr::defer(.clear_test_ops())
    register_reduction("DupTest", function(x, ...) sum(x))
    expect_error(
        register_reduction("DupTest", function(x, ...) mean(x)),
        "already registered"
    )
})

test_that("register_reduction accepts overwrite = TRUE", {
    withr::defer(.clear_test_ops())
    register_reduction("OverTest", function(x, ...) sum(x))
    register_reduction("OverTest", function(x, ...) 0, overwrite = TRUE)
    expect_equal(get_reduction("OverTest")(c(1, 2)), 0)
})

test_that("default reductions are registered on load", {
    for (op in c("Sum", "Mean", "Max", "Min", "Count",
                 "Var", "Std", "VarN", "StdN",
                 "Median", "Quantile", "GeoMean", "Mode")) {
        expect_true(op %in% registered_reductions(), info = op)
    }
})

test_that("Sum reduces a numeric vector", {
    expect_equal(get_reduction("Sum")(c(1, 2, 3)), 6)
    expect_equal(get_reduction("Sum")(c(1, NA, 3)), NA_real_)
    expect_equal(get_reduction("Sum")(c(1, NA, 3), na_rm = TRUE), 4)
})

test_that("Mean reduces a numeric vector", {
    expect_equal(get_reduction("Mean")(c(1, 2, 3)), 2)
})

test_that("Max/Min reduce a numeric vector", {
    expect_equal(get_reduction("Max")(c(3, 1, 4, 1, 5)), 5)
    expect_equal(get_reduction("Min")(c(3, 1, 4, 1, 5)), 1)
})

test_that("Count returns length of input", {
    expect_equal(get_reduction("Count")(c(1, 2, 3)), 3L)
    expect_equal(get_reduction("Count")(character(5)), 5L)
})

test_that("default eltwise ops are registered on load", {
    for (op in c("Log", "Abs", "Exp", "Sqrt", "Round",
                 "Clamp", "Convert", "Fraction", "Significant")) {
        expect_true(op %in% registered_eltwise(), info = op)
    }
})

test_that("Log applies log with eps + base", {
    fn <- get_eltwise("Log")
    expect_equal(fn(c(1, 10, 100)), log(c(1, 10, 100)))
    expect_equal(fn(c(1, 10, 100), base = 10), log10(c(1, 10, 100)))
    expect_equal(fn(c(0, 9, 99), eps = 1, base = 10), log10(c(1, 10, 100)))
})

test_that("Abs / Exp / Sqrt / Round behave as expected", {
    expect_equal(get_eltwise("Abs")(c(-1, 2, -3)), c(1, 2, 3))
    expect_equal(get_eltwise("Exp")(c(0, 1)), c(1, exp(1)))
    expect_equal(get_eltwise("Sqrt")(c(0, 4, 9)), c(0, 2, 3))
    expect_equal(get_eltwise("Round")(c(1.4, 1.5, 1.6)), c(1, 2, 2))
    expect_equal(get_eltwise("Round")(c(1.44, 1.55), digits = 1), c(1.4, 1.6))
})
