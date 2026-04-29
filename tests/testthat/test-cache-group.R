# Tests for cache_group constants and the per-item classification
# returned by format_get_* / format_set_*. Phase 2 populates the bulk
# of these; Phase 9 adds boundary-case assertions.

test_that("cache_group constants have expected values", {
    expect_identical(MEMORY_DATA, "MemoryData")
    expect_identical(MAPPED_DATA, "MappedData")
    expect_identical(QUERY_DATA, "QueryData")
})

test_that(".is_cache_group accepts the three constants", {
    .is_cache_group <- dafr:::.is_cache_group
    expect_true(.is_cache_group(MEMORY_DATA))
    expect_true(.is_cache_group(MAPPED_DATA))
    expect_true(.is_cache_group(QUERY_DATA))
})

test_that(".is_cache_group rejects bad inputs", {
    .is_cache_group <- dafr:::.is_cache_group
    expect_false(.is_cache_group("memory"))
    expect_false(.is_cache_group(""))
    expect_false(.is_cache_group(NA_character_))
    expect_false(.is_cache_group(c(MEMORY_DATA, MAPPED_DATA)))
    expect_false(.is_cache_group(NULL))
    expect_false(.is_cache_group(1L))
})

# ---- Phase-A signature tests (un-skipped at Phase 8) -----------------------

test_that("memory_daf format_get_scalar returns list(value, cache_group)", {
    skip("Phase A not yet implemented")
    d <- memory_daf()
    set_scalar(d, "n", 42L)
    out <- dafr:::format_get_scalar(d, "n")
    expect_named(out, c("value", "cache_group"))
    expect_identical(out$value, 42L)
    expect_identical(out$cache_group, MEMORY_DATA)
})

test_that("memory_daf format_axis_array returns list(value, cache_group)", {
    skip("Phase A not yet implemented")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    out <- dafr:::format_axis_array(d, "cell")
    expect_named(out, c("value", "cache_group"))
    expect_identical(out$value, c("c1", "c2"))
    expect_identical(out$cache_group, MEMORY_DATA)
})

test_that("memory_daf format_get_vector returns list(value, cache_group)", {
    skip("Phase A not yet implemented")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    out <- dafr:::format_get_vector(d, "cell", "donor")
    expect_named(out, c("value", "cache_group"))
    expect_equal(unname(out$value), c("A", "B"))
    expect_identical(out$cache_group, MEMORY_DATA)
})

test_that("memory_daf format_get_matrix returns list(value, cache_group)", {
    skip("Phase A not yet implemented")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2"))
    set_matrix(d, "cell", "gene", "UMIs", matrix(1:4, 2, 2))
    out <- dafr:::format_get_matrix(d, "cell", "gene", "UMIs")
    expect_named(out, c("value", "cache_group"))
    expect_identical(out$cache_group, MEMORY_DATA)
})

test_that("files_daf format_get_vector returns MAPPED_DATA on mmap path", {
    skip("Phase A not yet implemented")
    tmp <- tempfile(fileext = ".daf")
    d <- memory_daf(); add_axis(d, "cell", paste0("c", 1:3))
    set_vector(d, "cell", "x", c(1.0, 2.0, 3.0))
    files_daf_save(d, tmp)
    fd <- files_daf_open(tmp, mode = "r")
    out <- dafr:::format_get_vector(fd, "cell", "x")
    expect_named(out, c("value", "cache_group"))
    expect_identical(out$cache_group, MAPPED_DATA)
})

test_that("files_daf format_get_scalar returns MEMORY_DATA (not mmap'd)", {
    skip("Phase A not yet implemented")
    tmp <- tempfile(fileext = ".daf")
    d <- memory_daf(); set_scalar(d, "n", 7L)
    files_daf_save(d, tmp)
    fd <- files_daf_open(tmp, mode = "r")
    out <- dafr:::format_get_scalar(fd, "n")
    expect_named(out, c("value", "cache_group"))
    expect_identical(out$cache_group, MEMORY_DATA)
})

test_that("memory_daf format_set_scalar returns MEMORY_DATA", {
    skip("Phase A not yet implemented")
    d <- memory_daf()
    rv <- dafr:::format_set_scalar(d, "n", 1L, FALSE)
    expect_identical(rv, MEMORY_DATA)
})

test_that("memory_daf format_set_vector returns MEMORY_DATA", {
    skip("Phase A not yet implemented")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    rv <- dafr:::format_set_vector(d, "cell", "x", c(1, 2), FALSE)
    expect_identical(rv, MEMORY_DATA)
})

test_that("memory_daf format_set_matrix returns MEMORY_DATA", {
    skip("Phase A not yet implemented")
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2"))
    rv <- dafr:::format_set_matrix(
        d, "cell", "gene", "M", matrix(1:4, 2, 2), FALSE
    )
    expect_identical(rv, MEMORY_DATA)
})

# Sanity: cache classification round-trips through public get_* paths
# (no behavior change visible to user — these are infrastructure tests).

test_that("after get_vector, cache contains entry under returned tier", {
    skip("Phase A not yet implemented")
    d <- memory_daf(); add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "x", c(1.0, 2.0))
    empty_cache(d)
    get_vector(d, "cell", "x")
    cache_env <- S7::prop(d, "cache")
    # MEMORY_DATA → "memory" tier
    expect_true(exists(
        cache_key_vector("cell", "x"),
        envir = cache_env$memory, inherits = FALSE
    ))
    expect_false(exists(
        cache_key_vector("cell", "x"),
        envir = cache_env$mapped, inherits = FALSE
    ))
})

test_that("after get_vector on files_daf, cache hits mapped tier", {
    skip("Phase A not yet implemented")
    tmp <- tempfile(fileext = ".daf")
    d <- memory_daf(); add_axis(d, "cell", paste0("c", 1:3))
    set_vector(d, "cell", "x", c(1.0, 2.0, 3.0))
    files_daf_save(d, tmp)
    fd <- files_daf_open(tmp, mode = "r")
    empty_cache(fd)
    get_vector(fd, "cell", "x")
    cache_env <- S7::prop(fd, "cache")
    # MAPPED_DATA → "mapped" tier (already happens internally — verify)
    expect_true(exists(
        cache_key_vector("cell", "x"),
        envir = cache_env$mapped, inherits = FALSE
    ))
})

test_that("empty_cache(daf, clear = MAPPED_DATA) clears mapped tier only", {
    skip("Phase A not yet implemented")
    tmp <- tempfile(fileext = ".daf")
    d <- memory_daf(); add_axis(d, "cell", paste0("c", 1:3))
    set_vector(d, "cell", "x", c(1.0, 2.0, 3.0))
    files_daf_save(d, tmp)
    fd <- files_daf_open(tmp, mode = "r")
    get_vector(fd, "cell", "x")  # primes mapped tier
    empty_cache(fd, clear = MAPPED_DATA)
    cache_env <- S7::prop(fd, "cache")
    expect_equal(length(ls(cache_env$mapped, all.names = TRUE)), 0L)
})
