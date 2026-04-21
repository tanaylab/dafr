test_that("get_scalar on ContractDaf marks scalar as accessed", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    expect_identical(get_scalar(cd, "version"), 1L)
    expect_true(S7::prop(cd, "data")[["scalar:version"]]$accessed)
})

test_that("get_scalar on out-of-contract scalar errors (non-relaxed)", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract()
    cd <- contractor("comp", cn, d)
    expect_error(get_scalar(cd, "version"), "non-contract scalar")
})

test_that("relaxed contract allows out-of-contract access", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract(is_relaxed = TRUE)
    cd <- contractor("comp", cn, d)
    expect_identical(get_scalar(cd, "version"), 1L)
})
