test_that("parse_query returns correct AST on first call", {
    dafr:::.parse_query_cache_clear()
    ast <- parse_query("@ cell : donor")
    expect_type(ast, "list")
    expect_identical(ast[[1L]]$op, "Axis")
    expect_identical(ast[[2L]]$op, "LookupVector")
})

test_that("parse_query cache hit returns identical AST", {
    dafr:::.parse_query_cache_clear()
    ast1 <- parse_query("@ cell : donor")
    ast2 <- parse_query("@ cell : donor")
    expect_identical(ast1, ast2)
})

test_that("cache stores distinct queries separately", {
    dafr:::.parse_query_cache_clear()
    a <- parse_query("@ cell")
    b <- parse_query("@ gene")
    expect_false(identical(a, b))
    # Re-retrieve both after evicting nothing.
    expect_identical(parse_query("@ cell"), a)
    expect_identical(parse_query("@ gene"), b)
})

test_that("cache evicts oldest entries beyond cap", {
    cache <- dafr:::.parse_query_cache
    dafr:::.parse_query_cache_clear()
    old_cap <- cache$cap
    cache$cap <- 3L
    on.exit(cache$cap <- old_cap, add = TRUE)

    parse_query(". organism")
    parse_query("@ cell")
    parse_query("@ gene")
    parse_query("@ donor")   # evicts ". organism"

    expect_false(exists(". organism", envir = cache$entries, inherits = FALSE))
    expect_true(exists("@ donor", envir = cache$entries, inherits = FALSE))
})

test_that("cache correctness under repeated get_query calls", {
    dafr:::.parse_query_cache_clear()
    d <- example_cells_daf()
    # Exercise the path via get_query (which calls parse_query internally).
    r1 <- get_query(d, "@ cell : donor")
    r2 <- get_query(d, "@ cell : donor")
    expect_identical(r1, r2)
})
