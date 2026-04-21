test_that("computation() wraps and is callable; returns fn's result", {
    c <- Contract()
    fn <- function(daf) "result-sentinel"
    wrapped <- computation("noop", c, fn)
    expect_true(is.function(wrapped))

    d <- memory_daf(name = "t1")
    expect_identical(wrapped(d), "result-sentinel")
})

test_that("computation() rejects non-Contract second argument", {
    expect_error(
        computation("bad", list(), function(d) d),
        "must be a Contract"
    )
})

test_that("computation() rejects non-DafReader first call-arg", {
    c <- Contract()
    w <- computation("typed", c, function(d) d)
    expect_error(w(42), "must be a DafReader")
})

test_that("computation() raises on missing RequiredInput under enforcement", {
    withr::local_options(list(dafr.enforce_contracts = TRUE))
    c <- Contract(
        axes = list(cell = list(RequiredInput, "cell axis")),
        data = list(contract_vector(
            axis = "cell", name = "donor",
            expectation = RequiredInput, type = "character",
            description = "donor id"
        ))
    )
    fn <- function(daf) get_vector(daf, "cell", "donor")
    w <- computation("needs-donor", c, fn)

    d <- memory_daf(name = "empty")
    add_axis(d, "cell", c("c1", "c2"))
    expect_error(w(d), "missing input vector: donor")
})

test_that("computation() raises on missing CreatedOutput after call", {
    withr::local_options(list(dafr.enforce_contracts = TRUE))
    c <- Contract(
        axes = list(cell = list(RequiredInput, "cell axis")),
        data = list(contract_vector(
            axis = "cell", name = "score",
            expectation = CreatedOutput, type = "double",
            description = "per-cell score"
        ))
    )
    fn <- function(daf) "did-nothing"
    w <- computation("produces-score", c, fn)

    d <- memory_daf(name = "e")
    add_axis(d, "cell", c("c1"))
    expect_error(w(d), "missing output vector: score")
})

test_that("computation() succeeds when contract is honored", {
    withr::local_options(list(dafr.enforce_contracts = TRUE))
    c <- Contract(
        axes = list(cell = list(RequiredInput, "cell axis")),
        data = list(
            contract_vector("cell", "donor", RequiredInput, "character", "in"),
            contract_vector("cell", "age", CreatedOutput, "integer", "out")
        )
    )
    fn <- function(daf) {
        donors <- get_vector(daf, "cell", "donor")
        set_vector(daf, "cell", "age", rep(0L, length(donors)))
        "ok"
    }
    w <- computation("ages", c, fn)
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    expect_identical(w(d), "ok")
    expect_identical(unname(get_vector(d, "cell", "age")), c(0L, 0L))
})

test_that("function_contract() retrieves the bound contract", {
    c <- Contract(
        axes = list(cell = list(RequiredInput, "per-cell axis")),
        data = list(contract_vector("cell", "donor", RequiredInput, "character", "id"))
    )
    w <- computation("demo", c, function(d) d)
    got <- function_contract(w)
    expect_true(S7::S7_inherits(got, Contract))
    expect_identical(got, c)
})

test_that("function_contract() errors on an unwrapped function", {
    expect_error(function_contract(function(x) x), "no dafr contract bound")
})

test_that("contract_description() produces a non-empty string", {
    c <- Contract(
        axes = list(cell = list(RequiredInput, "per-cell axis")),
        data = list(contract_vector("cell", "donor", RequiredInput, "character", "id"))
    )
    s <- contract_description(c)
    expect_true(is.character(s) && length(s) == 1L && nzchar(s))
    expect_true(grepl("RequiredInput", s))
    expect_true(grepl("cell", s))
    expect_true(grepl("donor", s))
})
