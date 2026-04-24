test_that("R chain_reader matches Julia fixture", {
    skip_if_not(file.exists("fixtures/julia-chains/fixture.json"),
        "Julia chain fixture absent")
    fx <- jsonlite::fromJSON("fixtures/julia-chains/fixture.json",
        simplifyVector = TRUE
    )
    d1 <- memory_daf(name = "first")
    set_scalar(d1, "version", 1L)
    add_axis(d1, "cell", c("A", "B", "C"))
    set_vector(d1, "cell", "age", c(10L, 20L, 30L))

    d2 <- memory_daf(name = "second")
    set_scalar(d2, "version", 2L)
    set_scalar(d2, "owner", "me")
    add_axis(d2, "cell", c("A", "B", "C"))
    set_vector(d2, "cell", "age", c(100L, 200L, 300L))
    set_vector(d2, "cell", "donor", c("d1", "d2", "d1"))

    ch <- chain_reader(list(d1, d2), name = fx$chain_name)
    expect_identical(get_scalar(ch, "version"), as.integer(fx$scalars$version$value))
    expect_identical(get_scalar(ch, "owner"), as.character(fx$scalars$owner$value))
    expect_identical(axis_vector(ch, "cell"), fx$axes$cell)
    expect_identical(unname(get_vector(ch, "cell", "age")),
        as.integer(fx$vectors$cell$age)
    )
    expect_identical(unname(get_vector(ch, "cell", "donor")),
        as.character(fx$vectors$cell$donor)
    )
})
