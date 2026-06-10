# Vector-reduction (`>>`) behavior on special-value inputs (NaN / +-Inf),
# pinned against DataAxesFormats.jl. An adversarial R-vs-Julia sweep over
# {Median, Mean, Sum, Max, Min, Var, Std} x {NaN+Inf mix, all-Inf, NaN-only,
# negative} found dafr matches DAF.jl exactly on every case EXCEPT one (see the
# documented divergence below). These tests guard that parity + the divergence.

.sv_daf <- function() {
    d <- memory_daf(name = "sv")
    add_axis(d, "cell", paste0("c", 1:5))
    set_vector(d, "cell", "ninf", c(1, NaN, 3, Inf, -Inf))
    set_vector(d, "cell", "allinf", c(Inf, Inf, -Inf, -Inf, Inf))
    set_vector(d, "cell", "nan1", c(1, 2, NaN, 4, 5))
    set_vector(d, "cell", "neg", c(-5, -3, -1, 2, 4))
    d
}

test_that("reductions propagate NaN like DAF.jl (NaN anywhere -> NaN)", {
    d <- .sv_daf()
    # nan1 = [1,2,NaN,4,5]: every reduction is NaN in both R and Julia.
    for (op in c("Median", "Mean", "Sum", "Max", "Min", "Var", "Std")) {
        expect_true(is.nan(get_query(d, sprintf("@ cell : nan1 >> %s", op))),
                    info = paste0("nan1 ", op))
    }
})

test_that("reductions on +-Inf match DAF.jl", {
    d <- .sv_daf()
    # allinf = [Inf,Inf,-Inf,-Inf,Inf]
    expect_equal(get_query(d, "@ cell : allinf >> Max"), Inf)
    expect_equal(get_query(d, "@ cell : allinf >> Min"), -Inf)
    expect_equal(get_query(d, "@ cell : allinf >> Median"), Inf)
    expect_true(is.nan(get_query(d, "@ cell : allinf >> Mean")))  # Inf + -Inf
    expect_true(is.nan(get_query(d, "@ cell : allinf >> Sum")))
})

test_that("non-special reductions are exact vs DAF.jl", {
    d <- .sv_daf()
    expect_equal(get_query(d, "@ cell : neg >> Median"), -1)
    expect_equal(get_query(d, "@ cell : neg >> Mean"), -0.6)
    expect_equal(get_query(d, "@ cell : neg >> Sum"), -3)
    expect_equal(get_query(d, "@ cell : neg >> Var"), 10.64)
})

test_that("Median of NaN+Inf mix: R returns NaN (documented divergence from DAF.jl)", {
    # KNOWN DIVERGENCE: Median([1, NaN, 3, Inf, -Inf]) is NaN in dafr (R's
    # NaN-propagating median) but -Inf in DataAxesFormats.jl (its sort-based
    # median kernel orders NaN/Inf differently). Every other reduction on this
    # input is NaN in BOTH. R's NaN is arguably the more correct answer when an
    # input is NaN; matching Julia's -Inf would require reimplementing its exact
    # median-with-NaN ordering in the C kernel - deferred as out-of-scope (same
    # class as the other documented median/quantile kernel divergences). This
    # test pins R's current behavior so any change is intentional.
    d <- .sv_daf()
    expect_true(is.nan(get_query(d, "@ cell : ninf >> Median")))
    # the rest of the ninf reductions agree with Julia (all NaN):
    for (op in c("Mean", "Sum", "Max", "Min", "Var", "Std")) {
        expect_true(is.nan(get_query(d, sprintf("@ cell : ninf >> %s", op))),
                    info = paste0("ninf ", op))
    }
})
