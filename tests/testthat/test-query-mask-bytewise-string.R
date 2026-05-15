# DAF.jl compares strings bytewise (Vector{String} `<`). R's `<` honours
# LC_COLLATE, so under the default en_US.UTF-8 locale uppercase letters
# fold after their lowercase counterparts, and "Z" < "b" returns FALSE
# (the opposite of Julia). Mask filters with mixed-case data therefore
# silently picked the wrong entries. Fix routes character ordering
# through a C-locale wrapper.
#
# devtools::test sets LC_COLLATE=C globally for reproducible test output,
# which would mask the bug. Each test below forces en_US.UTF-8 explicitly
# (skipping if unavailable on the host) so the regression actually fires.

.with_en_us_locale <- function(expr) {
    # Use en_US.UTF-8 to reproduce real-user locale (not the C-locale that
    # devtools::test forces). Skip if the system doesn't have it installed.
    old <- Sys.getlocale("LC_COLLATE")
    new <- suppressWarnings(Sys.setlocale("LC_COLLATE", "en_US.UTF-8"))
    if (!nzchar(new) || identical(new, "C")) {
        skip("en_US.UTF-8 locale not available on this system")
    }
    on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
    force(expr)
}

.fx <- function() {
    d <- memory_daf(name = "bw")
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    set_vector(d, "cell", "label", c("", "x", "y", "a b", "Z"))
    d
}

test_that("mask uses bytewise ordering: '< b' picks Z but not x/y", {
    .with_en_us_locale({
        d <- .fx()
        v <- get_query(d, "@ cell [ label < b ]")
        # Julia: A (""), D ("a b"), E ("Z").  R w/o fix: only A, D.
        expect_equal(sort(names(v)), c("A", "D", "E"))
    })
})

test_that("mask uses bytewise ordering: '< Z' picks only the empty string", {
    .with_en_us_locale({
        d <- .fx()
        v <- get_query(d, "@ cell [ label < Z ]")
        # ASCII: only "" (0x00) < "Z" (0x5A). All other entries sort above.
        expect_equal(names(v), "A")
    })
})

test_that("mask uses bytewise ordering: '>= Z' includes Z and all lowercase", {
    .with_en_us_locale({
        d <- .fx()
        v <- get_query(d, "@ cell [ label >= Z ]")
        expect_equal(sort(names(v)), c("B", "C", "D", "E"))
    })
})

test_that("equality / inequality remain bytewise (regression guard)", {
    .with_en_us_locale({
        d <- .fx()
        expect_equal(names(get_query(d, "@ cell [ label = Z ]")), "E")
        expect_equal(sort(names(get_query(d, "@ cell [ label != Z ]"))),
            c("A", "B", "C", "D"))
    })
})

test_that("numeric comparisons remain numeric (regression guard)", {
    .with_en_us_locale({
        d <- memory_daf(name = "nbw")
        add_axis(d, "cell", c("A", "B", "C"))
        set_vector(d, "cell", "age", c(10, 20, 30))
        expect_equal(sort(names(get_query(d, "@ cell [ age > 15 ]"))),
            c("B", "C"))
    })
})

test_that("LC_COLLATE is restored after a string comparison", {
    .with_en_us_locale({
        d <- .fx()
        before <- Sys.getlocale("LC_COLLATE")
        get_query(d, "@ cell [ label < b ]")
        expect_identical(Sys.getlocale("LC_COLLATE"), before)
    })
})
