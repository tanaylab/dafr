# Parity fix: GroupBy / CountBy result labels must be ordered BYTEWISE (matching
# Julia's sort!(unique(values)) on String, which is bytewise), not by the process
# LC_COLLATE. The C-locale (radix) ordering used elsewhere in dafr was never
# propagated to the group/count factor-sort in query_eval.R, so under a collating
# locale (e.g. en_US.UTF-8) mixed-case group labels came back in dictionary order,
# silently mismatching Julia positionally. Audit probes qe-groupby-bytewise-order,
# qe-countby-bytewise-order.

.with_collate <- function(loc, code) {
    old <- Sys.getlocale("LC_COLLATE")
    set_ok <- tryCatch({ Sys.setlocale("LC_COLLATE", loc); TRUE },
                       warning = function(w) FALSE)
    on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
    testthat::skip_if_not(set_ok && Sys.getlocale("LC_COLLATE") == loc,
                          paste("needs", loc, "locale"))
    force(code)
}

test_that("GroupBy result labels use bytewise order (not LC_COLLATE)", {
    .with_collate("en_US.UTF-8", {
        d <- memory_daf()
        add_axis(d, "cell", c("c1", "c2", "c3", "c4", "c5"))
        set_vector(d, "cell", "grp", c("b", "A", "b", "Z", "a"))
        set_vector(d, "cell", "val", c(1, 2, 3, 4, 5))
        r <- get_query(d, "@ cell : val / grp >> Sum")
        # Julia bytewise: A(65) < Z(90) < a(97) < b(98)
        expect_identical(names(r), c("A", "Z", "a", "b"))
        expect_identical(unname(r[c("A", "Z", "a", "b")]), c(2, 4, 5, 4))
    })
})

test_that("CountBy cross-tab dimnames use bytewise order (not LC_COLLATE)", {
    .with_collate("en_US.UTF-8", {
        d <- memory_daf()
        add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
        set_vector(d, "cell", "a_lab", c("a", "Z", "a", "Z"))
        set_vector(d, "cell", "b_lab", c("b", "Y", "Y", "b"))
        m <- get_query(d, "@ cell : a_lab * b_lab")
        expect_identical(rownames(m), c("Z", "a"))
        expect_identical(colnames(m), c("Y", "b"))
    })
})
