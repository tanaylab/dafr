# Parity fix (COMP-01): re-running a computation with overwrite=TRUE must succeed.
# Julia threads kwargs_overwrite into the contractor (verify allows a pre-existing
# CreatedOutput) AND keeps overwrite in the kwargs for the inner fn. dafr's
# computation() wrapper called contractor() WITHOUT overwrite, so verify_input
# raised "pre-existing CreatedOutput" on the second run. Audit probe COMP-01.

test_that("computation re-run with overwrite=TRUE succeeds", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        ct <- Contract(data = list(
            contract_scalar("result", CreatedOutput, "numeric", "the result")))
        comp <- computation("comp", ct, function(daf, overwrite = FALSE) {
            set_scalar(daf, "result", 1.0, overwrite = overwrite)
        })
        d <- memory_daf(name = "d")
        comp(d)  # first run creates 'result'
        expect_identical(get_scalar(d, "result"), 1.0)

        # second run with overwrite=TRUE must succeed (was: pre-existing CreatedOutput)
        expect_no_error(comp(d, overwrite = TRUE))
        expect_identical(get_scalar(d, "result"), 1.0)
    })
})

test_that("computation re-run WITHOUT overwrite still errors on a pre-existing CreatedOutput", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        ct <- Contract(data = list(
            contract_scalar("result", CreatedOutput, "numeric", "the result")))
        comp <- computation("comp", ct, function(daf, overwrite = FALSE) {
            set_scalar(daf, "result", 1.0, overwrite = overwrite)
        })
        d <- memory_daf(name = "d")
        comp(d)
        expect_error(comp(d), "pre-existing")
    })
})
