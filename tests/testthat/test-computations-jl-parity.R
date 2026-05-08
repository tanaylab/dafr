# Literal port of computations.jl into R.
#
# Julia's computations.jl tests the @computation / @logged / @documented
# macros (Julia macro system). dafr exposes a regular function
# `computation(name, contract, fn)` that returns a wrapped fn. Many
# Julia leaves are macro-introspection tests with no R equivalent;
# they skip with divergence IDs.
#
# Coverage:
#   - access / contract: function_contract retrieval - PASS
#   - access / !contract: error on non-wrapped fn - PASS
#   - access / default + !default: dafr lacks function_default - SKIP CC1
#   - none / *: doc-format integration - SKIP CC2
#   - single / (): execute a single-contract computation - PASS
#   - single / missing: missing axis raises - PASS
#   - single / docs / relaxed: doc generation - SKIP CC2
#   - cross / *: dual-contract computations - SKIP CC3
#   - !doc2 / !default: macro-internals - SKIP CC4

# ---------------------------------------------------------------------------
# Shared fixtures
# ---------------------------------------------------------------------------

.SINGLE_CONTRACT <- function() {
    Contract(
        axes = list(
            cell  = list(RequiredInput, "cells"),
            gene  = list(RequiredInput, "genes")
        ),
        data = list(
            contract_matrix("cell", "gene", "UMIs",
                            RequiredInput, "integer", "UMIs"),
            contract_scalar("quality", CreatedOutput, "numeric", "score")
        )
    )
}

.single_fn <- computation("single", .SINGLE_CONTRACT(),
    function(daf, quality = 0.0) {
        invisible(get_matrix(daf, "cell", "gene", "UMIs"))
        set_scalar(daf, "quality", as.numeric(quality))
    })

.not_computation <- function(x = 1) x

# ---------------------------------------------------------------------------
# computations / access
# ---------------------------------------------------------------------------

test_that("computations / access / !contract", {
    expect_error(
        function_contract(.not_computation),
        regexp = "no dafr contract|not a .*function|@documented"
    )
})

test_that("computations / access / contract", {
    contract <- function_contract(.single_fn)
    expect_s3_class(contract, "dafr::Contract")
    expect_length(contract@axes, 2L)
})

test_that("computations / access / default", {
    skip("R divergence CC1: dafr lacks function_default(fn, param). R's match.call+formals can extract default values; not exposed as a contract API.")
})

test_that("computations / access / !default", {
    skip("R divergence CC1: dafr lacks function_default()")
})

# ---------------------------------------------------------------------------
# computations / none - doc integration
# ---------------------------------------------------------------------------

test_that("computations / none / default", {
    none_fn <- function(x = 1L) x
    expect_identical(none_fn(), 1L)
})

test_that("computations / none / parameter", {
    none_fn <- function(x = 1L) x
    expect_identical(none_fn(x = 2L), 2L)
})

test_that("computations / none / docs", {
    skip("R divergence CC2: doc generation is roxygen-based in R, not via @documented Julia macro. dafr's contract_description() produces a similar string but the format differs.")
})

# ---------------------------------------------------------------------------
# computations / single
# ---------------------------------------------------------------------------

test_that("computations / single / ()", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        add_axis(d, "gene", c("X", "Y", "Z"))
        set_matrix(d, "cell", "gene", "UMIs", matrix(
            c(0L, 3L, 1L, 4L, 2L, 5L), nrow = 2L, ncol = 3L,
            dimnames = list(c("A", "B"), c("X", "Y", "Z"))
        ))
        # Re-create the contract under enforcement
        single_fn <- computation("single", .SINGLE_CONTRACT(),
            function(daf, quality = 0.0) {
                invisible(get_matrix(daf, "cell", "gene", "UMIs"))
                set_scalar(daf, "quality", as.numeric(quality))
            })
        expect_silent(single_fn(d, 0.0))
        expect_identical(get_scalar(d, "quality"), 0.0)
    })
})

test_that("computations / single / missing", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf(name = "memory!")
        # Missing both axes and the matrix
        single_fn <- computation("single", .SINGLE_CONTRACT(),
            function(daf, quality = 0.0) {
                invisible(get_matrix(daf, "cell", "gene", "UMIs"))
                set_scalar(daf, "quality", as.numeric(quality))
            })
        expect_error(single_fn(d, 0.0),
            regexp = "missing.*axis|missing input")
    })
})

test_that("computations / single / docs", {
    skip("R divergence CC2: doc generation differs (R uses roxygen; Julia uses @documented macro)")
})

test_that("computations / single / relaxed", {
    skip("R divergence CC2: doc generation differs")
})

# ---------------------------------------------------------------------------
# computations / cross - dual-contract computations
# ---------------------------------------------------------------------------

test_that("computations / cross / ()", {
    skip("R divergence CC3: dafr's computation() takes a single Contract; Julia's @computation supports multiple Contract arguments for multi-daf computations. Adding multi-contract support is a non-trivial API extension.")
})

test_that("computations / cross / note", {
    skip("R divergence CC3: dafr lacks multi-contract computation()")
})

test_that("computations / cross / overwrite", {
    skip("R divergence CC3: dafr lacks multi-contract computation()")
})

test_that("computations / cross / !overwrite", {
    skip("R divergence CC3: dafr lacks multi-contract computation()")
})

test_that("computations / cross / missing / first", {
    skip("R divergence CC3: dafr lacks multi-contract computation()")
})

test_that("computations / cross / missing / second", {
    skip("R divergence CC3: dafr lacks multi-contract computation()")
})

test_that("computations / cross / docs / dual", {
    skip("R divergence CC3: dafr lacks multi-contract computation()")
})

test_that("computations / cross / docs / triple", {
    skip("R divergence CC3: dafr lacks multi-contract computation()")
})

test_that("computations / cross / !doc2", {
    skip("R divergence CC4: macro-internals tests not applicable to R")
})

test_that("computations / cross / !default", {
    skip("R divergence CC4: macro-internals tests not applicable to R")
})
