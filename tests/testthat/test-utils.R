JuliaCall::julia_eval("import Random")

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

test_that("julia_project_status doesn't fail", {
    expect_no_error(julia_project_status())
})

test_that("R type name strings are converted to correct Julia types", {
    # Test basic type mappings
    expect_true(julia_call("==", jl_R_to_julia_type("logical"), julia_eval("Bool")))
    expect_true(julia_call("==", jl_R_to_julia_type("integer"), julia_eval("Int64")))
    expect_true(julia_call("==", jl_R_to_julia_type("double"), julia_eval("Float64")))
    expect_true(julia_call("==", jl_R_to_julia_type("int8"), julia_eval("Int8")))
    expect_true(julia_call("==", jl_R_to_julia_type("int16"), julia_eval("Int16")))
    expect_true(julia_call("==", jl_R_to_julia_type("int32"), julia_eval("Int32")))
    expect_true(julia_call("==", jl_R_to_julia_type("int64"), julia_eval("Int64")))
    expect_true(julia_call("==", jl_R_to_julia_type("uint8"), julia_eval("UInt8")))
    expect_true(julia_call("==", jl_R_to_julia_type("uint16"), julia_eval("UInt16")))
    expect_true(julia_call("==", jl_R_to_julia_type("uint32"), julia_eval("UInt32")))
    expect_true(julia_call("==", jl_R_to_julia_type("uint64"), julia_eval("UInt64")))
    expect_true(julia_call("==", jl_R_to_julia_type("float32"), julia_eval("Float32")))
    expect_true(julia_call("==", jl_R_to_julia_type("float64"), julia_eval("Float64")))
})

test_that("R values are converted to correct Julia types", {
    # Test logical values
    expect_true(julia_call("==", jl_R_to_julia_type(TRUE), julia_eval("Bool")))
    expect_true(julia_call("==", jl_R_to_julia_type(FALSE), julia_eval("Bool")))

    # Test integer values
    expect_true(julia_call("==", jl_R_to_julia_type(1L), julia_eval("Int64")))
    expect_true(julia_call("==", jl_R_to_julia_type(as.integer(-100)), julia_eval("Int64")))

    # Test double values
    expect_true(julia_call("==", jl_R_to_julia_type(2.5), julia_eval("Float64")))
    expect_true(julia_call("==", jl_R_to_julia_type(-3.14), julia_eval("Float64")))

    # Test character values
    expect_true(julia_call("==", jl_R_to_julia_type("logical"), julia_eval("Bool")))
    expect_true(julia_call("==", jl_R_to_julia_type("integer"), julia_eval("Int64")))

    # Test vectors (ensure the function handles these correctly)
    expect_true(julia_call("==", jl_R_to_julia_type(c(TRUE, FALSE)), julia_eval("Bool")))
    expect_true(julia_call("==", jl_R_to_julia_type(c(1L, 2L)), julia_eval("Int64")))
    expect_true(julia_call("==", jl_R_to_julia_type(c(1.0, 2.5)), julia_eval("Float64")))
})

test_that("jl_R_to_julia_type handles special cases correctly", {
    # Test NULL handling
    null_result <- jl_R_to_julia_type(NULL)
    expect_true(julia_call("==", null_result, julia_eval("Nothing")))

    # Test custom Julia type expressions
    vector_type <- jl_R_to_julia_type("Vector{Float64}")
    expect_true(julia_call("==", vector_type, julia_eval("Vector{Float64}")))

    expect_error(jl_R_to_julia_type("UnknownType"))
})
