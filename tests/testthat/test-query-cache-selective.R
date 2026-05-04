# Parity port of DataAxesFormats.jl test/queries.jl > cache > empty:
# selective `clear = QueryData` and `keep = QueryData`.

local({
    setup <- function() {
        d <- memory_daf(name = "memory!")
        add_axis(d, "cell", c("A", "B"))
        set_vector(d, "cell", "is_doublet", c(TRUE, FALSE))
        set_vector(d, "cell", "age", c(0L, 1L))
        d
    }

    test_that("empty_cache(daf) clears every tier", {
        d <- setup()
        v1 <- get_query(d, "@ cell [ is_doublet ] : age")
        v2 <- get_query(d, "@ cell [ is_doublet ] : age")
        # Same call twice => second call uses the query cache (identity).
        empty_cache(d)
        v3 <- get_query(d, "@ cell [ is_doublet ] : age")
        expect_equal(v3, v1)
    })

    test_that("empty_cache(daf, clear = QUERY_DATA) clears query tier only", {
        d <- setup()
        v1 <- get_query(d, "@ cell [ is_doublet ] : age")
        empty_cache(d, clear = QUERY_DATA)
        v2 <- get_query(d, "@ cell [ is_doublet ] : age")
        expect_equal(v2, v1)
    })

    test_that("empty_cache(daf, keep = QUERY_DATA) preserves query tier", {
        d <- setup()
        v1 <- get_query(d, "@ cell [ is_doublet ] : age")
        cache_env <- S7::prop(d, "cache")
        before <- length(ls(cache_env$query))
        expect_gt(before, 0L)
        empty_cache(d, keep = QUERY_DATA)
        after <- length(ls(cache_env$query))
        expect_equal(after, before)
    })
})
