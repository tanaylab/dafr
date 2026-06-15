# Parity fix: the reader API (has_vector / get_vector) must expose the reserved
# virtual vectors "name" (axis entry names) and "index" (1..N), matching Julia
# readers.jl (which special-cases them before format_has_vector). dafr only
# handled these in the query DSL, so the reader API raised "missing vector".
# Audit probe P1-name-index-reserved.

test_that("has_vector is TRUE for reserved 'name' and 'index'", {
    d <- memory_daf()
    add_axis(d, "type", c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
    expect_true(has_vector(d, "type", "name"))
    expect_true(has_vector(d, "type", "index"))
})

test_that("get_vector returns axis entries for 'name' and 1..N for 'index'", {
    d <- memory_daf()
    add_axis(d, "type", c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
    nm <- get_vector(d, "type", "name")
    expect_identical(unname(nm), c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
    expect_identical(names(nm), c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
    ix <- get_vector(d, "type", "index")
    expect_identical(unname(ix), 1:4)
    expect_identical(names(ix), c("memory-B", "MEBEMP-E", "MEBEMP-L", "MPP"))
})
