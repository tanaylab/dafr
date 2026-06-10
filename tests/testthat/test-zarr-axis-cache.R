# ZarrDaf axis-name decode memoization.
#
# `.zarr_axis_entries` previously re-decoded the vlen-utf8 axis-name strings
# from the store on EVERY call. On a query over a large axis (4000+ entries)
# that vlen decode is ~45% of the query time, paid afresh for every distinct
# query (only an exact-repeat query hits the QueryData result cache). The
# decode is now memoized at the "memory" tier keyed by cache_key_axis + the
# axis_stamp, so distinct queries over the same axes decode once.
#
# These guard correctness + the invalidation contract (delete_axis bumps the
# axis counter -> cache miss -> re-decode), matching how vector/matrix caches
# already invalidate.

.axis_cache_daf <- function(entries = c("a", "b", "c")) {
    d <- zarr_daf(tempfile(fileext = ".daf.zarr"), "w")
    add_axis(d, "cell", entries)
    d
}

test_that("axis entries are correct and unchanged by memoization", {
    d <- .axis_cache_daf(c("a", "b", "c"))
    expect_identical(axis_vector(d, "cell"), c("a", "b", "c"))
    # repeat call returns the same value (now served from cache)
    expect_identical(axis_vector(d, "cell"), c("a", "b", "c"))
})

test_that("first read populates the memory-tier axis cache", {
    d <- .axis_cache_daf(c("a", "b", "c"))
    cache_env <- S7::prop(d, "cache")
    key <- dafr:::cache_key_axis("cell")
    # nothing cached before the first decode
    expect_null(dafr:::cache_lookup(cache_env, "memory", key,
                                    dafr:::axis_stamp(d, "cell")))
    invisible(axis_vector(d, "cell"))
    hit <- dafr:::cache_lookup(cache_env, "memory", key,
                               dafr:::axis_stamp(d, "cell"))
    expect_identical(hit, c("a", "b", "c"))
})

test_that("delete_axis + recreate invalidates the cached entries", {
    d <- .axis_cache_daf(c("a", "b", "c"))
    expect_identical(axis_vector(d, "cell"), c("a", "b", "c"))  # caches
    delete_axis(d, "cell")
    add_axis(d, "cell", c("x", "y"))
    # stamp bumped -> stale entry ignored -> fresh decode
    expect_identical(axis_vector(d, "cell"), c("x", "y"))
})

test_that("empty_cache(clear = 'memory') drops the axis cache", {
    d <- .axis_cache_daf(c("a", "b", "c"))
    invisible(axis_vector(d, "cell"))
    key <- dafr:::cache_key_axis("cell")
    cache_env <- S7::prop(d, "cache")
    expect_false(is.null(dafr:::cache_lookup(cache_env, "memory", key,
                                             dafr:::axis_stamp(d, "cell"))))
    empty_cache(d, clear = "memory")
    expect_null(dafr:::cache_lookup(cache_env, "memory", key,
                                    dafr:::axis_stamp(d, "cell")))
    # still correct after eviction (re-decodes)
    expect_identical(axis_vector(d, "cell"), c("a", "b", "c"))
})

test_that("clear = 'query' does NOT evict axis entries (not query results)", {
    d <- .axis_cache_daf(c("a", "b", "c"))
    invisible(axis_vector(d, "cell"))
    key <- dafr:::cache_key_axis("cell")
    cache_env <- S7::prop(d, "cache")
    empty_cache(d, clear = "query")
    expect_false(is.null(dafr:::cache_lookup(cache_env, "memory", key,
                                             dafr:::axis_stamp(d, "cell"))))
})
