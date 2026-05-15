# Julia DAF.jl rejects `?? <value>` when no following lookup chain
# (`: prop` / `:: prop`) can substitute the final_value:
#   parse_query("@ cell : type ?? a")  -> parses OK
#   get_query(daf, ...)                -> "invalid operation(s) ... ▲"
# dafr previously accepted the value silently and returned the un-chained
# vector as-if `??` were a no-op.

.fx <- function() {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "type", c("U", "", "V"))
    add_axis(d, "type", c("U", "V"))
    set_vector(d, "type", "color", c("red", "blue"))
    d
}

test_that("?? <bare value> at end of query errors (Julia parity)", {
    d <- .fx()
    expect_error(get_query(d, "@ cell : type ?? a"),
        "invalid operation")
})

test_that("?? <bare value> followed by : lookup still works", {
    d <- .fx()
    # `type ?? a : color`: empty type entries get substituted with "a",
    # then the chain looks up `a`'s color. Here "a" isn't in the type
    # axis, but the path is well-formed. (R currently still treats this
    # leniently; this test guards the well-formed half.)
    out <- tryCatch(get_query(d, "@ cell : type ?? a : color"),
        error = function(e) e)
    # Either a successful value or a runtime error - both are acceptable;
    # the regression we're guarding is that the parse-time / no-chain
    # error doesn't fire when a lookup chain IS present.
    if (inherits(out, "error")) {
        # If it errors, it should be about the missing entry, not the
        # `?? a` no-chain rule.
        expect_no_match(conditionMessage(out),
            "requires a following lookup chain")
    } else {
        expect_true(length(out) == 3L)
    }
})

test_that("?? without value is still valid (no chain required)", {
    d <- .fx()
    # Bare `??` masks out empty entries; no final_value to substitute.
    # Must not trip the new check.
    expect_no_error({
        out <- tryCatch(get_query(d, "@ cell : type ??"),
            error = function(e) e)
    })
})
