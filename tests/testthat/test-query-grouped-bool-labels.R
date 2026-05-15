# Julia DAF.jl labels grouped-by-Bool result entries with lowercase
# "true" / "false" (Julia's string(true) == "true"). dafr used R's
# as.character(TRUE) == "TRUE", so the names were uppercase and diverged.

.fx <- function() {
    d <- memory_daf()
    add_axis(d, "cell", c("A", "B", "C", "D", "E"))
    set_vector(d, "cell", "age", c(10L, 20L, 30L, 40L, 50L))
    set_vector(d, "cell", "type", c("U", "V", "U", "W", "V"))
    set_vector(d, "cell", "is_low", c(TRUE, TRUE, FALSE, FALSE, FALSE))
    d
}

test_that("grouped reduction by Bool vector uses lowercase 'true'/'false'", {
    d <- .fx()
    v <- get_query(d, "@ cell : age / is_low >> Sum")
    expect_equal(sort(names(v)), c("false", "true"))
    expect_equal(v[["true"]], 30)
    expect_equal(v[["false"]], 120)
})

test_that("grouped Mean by Bool also yields 'true'/'false'", {
    d <- .fx()
    v <- get_query(d, "@ cell : age / is_low >> Mean")
    expect_equal(sort(names(v)), c("false", "true"))
})

test_that("grouped reduction by string vector remains unchanged", {
    d <- .fx()
    v <- get_query(d, "@ cell : age / type >> Sum")
    expect_equal(sort(names(v)), c("U", "V", "W"))
})

test_that("count-by Bool vector uses lowercase dim names", {
    d <- .fx()
    m <- get_query(d, "@ cell : is_low * type")
    expect_equal(sort(rownames(m)), c("false", "true"))
})
