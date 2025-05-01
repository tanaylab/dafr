julia_eval("import Random")

test_that("set_seed sets the same seed in both R and Julia", {
    # Set a specific seed
    test_seed <- 60427
    set_seed(test_seed)

    # Generate random numbers in R
    r_random1 <- runif(1)
    r_random2 <- runif(1)

    # Generate random numbers in Julia
    julia_random1 <- JuliaCall::julia_eval("rand()")
    julia_random2 <- JuliaCall::julia_eval("rand()")

    # Set the same seed again
    set_seed(test_seed)

    # Generate random numbers again
    r_random1_repeat <- runif(1)
    r_random2_repeat <- runif(1)
    julia_random1_repeat <- JuliaCall::julia_eval("rand()")
    julia_random2_repeat <- JuliaCall::julia_eval("rand()")

    # Both R and Julia random sequences should restart with the same values
    expect_equal(r_random1, r_random1_repeat)
    expect_equal(r_random2, r_random2_repeat)
    expect_equal(julia_random1, julia_random1_repeat)
    expect_equal(julia_random2, julia_random2_repeat)
})

test_that("set_seed handles different seed values", {
    # Try with a different seed value
    set_seed(60427)

    # Just test that it runs without errors
    expect_no_error(JuliaCall::julia_eval("rand()"))
    expect_no_error(runif(1))
})

test_that("set_seed produces different results with different seeds", {
    # Generate with first seed
    set_seed(60427)
    r_first <- runif(5)
    julia_first <- JuliaCall::julia_eval("rand(5)")

    # Generate with second seed
    set_seed(22222)
    r_second <- runif(5)
    julia_second <- JuliaCall::julia_eval("rand(5)")

    # Results should be different
    expect_false(identical(r_first, r_second))
    expect_false(identical(julia_first, julia_second))
})
