# Test element-wise operations

test_that("Abs operation works", {
    daf <- setup_test_data()

    # Test without piping
    query <- Axis("cell") |>
        Lookup("values") |>
        Abs()
    result <- get_query(daf, query)
    expect_equal(result, c(1.5, 0, 2.5))

    # Test with piping
    result <- get_query(daf, Abs(Axis("cell") |> Lookup("values")))
    expect_equal(result, c(1.5, 0, 2.5))
})

test_that("Clamp operation works", {
    daf <- setup_test_data()

    # Test with min only - must explicitly set max = NULL
    query <- Axis("cell") |>
        Lookup("values") |>
        Clamp(min = 0, max = NULL)
    result <- get_query(daf, query)
    expect_equal(result, c(0, 0, 2.5))

    # Test with max only - must explicitly set min = NULL
    query <- Axis("cell") |>
        Lookup("values") |>
        Clamp(min = NULL, max = 1)
    result <- get_query(daf, query)
    expect_equal(result, c(-1.5, 0, 1))

    # Test with both min and max
    query <- Axis("cell") |>
        Lookup("values") |>
        Clamp(min = -1, max = 1)
    result <- get_query(daf, query)
    expect_equal(result, c(-1, 0, 1))

    # Test with piping
    result <- get_query(daf, Clamp(min = 0, max = 2, Axis("cell") |> Lookup("values")))
    expect_equal(result, c(0, 0, 2))
})

test_that("Convert operation works", {
    daf <- setup_test_data()

    # Test converting to Int32
    query <- Axis("cell") |>
        Lookup("values") |>
        Convert("Float32")
    result <- get_query(daf, query)
    expect_equal(result, c(-1.5, 0, 2.5))

    # Test with piping
    result <- get_query(daf, Convert("Float32", Axis("cell") |> Lookup("values")))
    expect_equal(result, c(-1.5, 0, 2.5))

    # Test missing type parameter
    expect_error(Convert())
})

test_that("Fraction operation works", {
    daf <- setup_test_data()

    # Create a test vector with positive values only
    set_vector(daf, "cell", "positive", c(2, 3, 5))

    # Test Fraction
    query <- Axis("cell") |>
        Lookup("positive") |>
        Fraction()
    result <- get_query(daf, query)
    expect_equal(result, c(2 / 10, 3 / 10, 5 / 10))

    # Test with piping
    result <- get_query(daf, Fraction(Axis("cell") |> Lookup("positive")))
    expect_equal(result, c(2 / 10, 3 / 10, 5 / 10))
})

test_that("Log operation works", {
    daf <- setup_test_data()

    # Create a test vector with positive values only
    set_vector(daf, "cell", "positive", c(1, 10, 100))

    # Test with default base (e)
    query <- Axis("cell") |>
        Lookup("positive") |>
        Log()
    result <- get_query(daf, query)
    expect_equal(result, log(c(1, 10, 100)))

    # Test with base 10
    query <- Axis("cell") |>
        Lookup("positive") |>
        Log(base = 10)
    result <- get_query(daf, query)
    expect_equal(result, log10(c(1, 10, 100)))

    # Test with base 2
    query <- Axis("cell") |>
        Lookup("positive") |>
        Log(base = 2)
    result <- get_query(daf, query)
    expect_equal(result, log2(c(1, 10, 100)))

    # Test with eps to avoid log(0)
    set_vector(daf, "cell", "with_zero", c(0, 1, 10))
    query <- Axis("cell") |>
        Lookup("with_zero") |>
        Log(eps = 1e-10)
    result <- get_query(daf, query)
    expect_equal(result[1], log(1e-10), tolerance = 1e-10)

    # Test with piping
    result <- get_query(daf, Log(base = 10, Axis("cell") |> Lookup("positive")))
    expect_equal(result, log10(c(1, 10, 100)))
})

test_that("Round operation works", {
    daf <- setup_test_data()

    # Create a test vector with decimal values
    set_vector(daf, "cell", "decimals", c(1.4, 2.5, 3.6))

    # Test Round
    query <- Axis("cell") |>
        Lookup("decimals") |>
        Round()
    result <- get_query(daf, query)
    expect_equal(result, c(1, 2, 4)) # Julia rounds to nearest even integer for ties

    # Test with piping
    result <- get_query(daf, Round(Axis("cell") |> Lookup("decimals")))
    expect_equal(result, c(1, 2, 4))
})

test_that("Significant operation works", {
    daf <- setup_test_data()

    # Create test data with a column that has one significant value
    set_vector(daf, "cell", "effect_sizes", c(1.0, 4.0, 0.5))

    # Test with high threshold only
    query <- Axis("cell") |>
        Lookup("effect_sizes") |>
        Significant(high = 3.0)
    result <- get_query(daf, query)
    expect_equal(result, c(0, 4.0, 0))

    # Test with high and low thresholds
    query <- Axis("cell") |>
        Lookup("effect_sizes") |>
        Significant(high = 3.0, low = 1.0)
    result <- get_query(daf, query)
    expect_equal(result, c(1.0, 4.0, 0))

    # Test with no significant values
    set_vector(daf, "cell", "low_effects", c(1.0, 2.0, 0.5))
    query <- Axis("cell") |>
        Lookup("low_effects") |>
        Significant(high = 3.0)
    result <- get_query(daf, query)
    expect_equal(result, c(0, 0, 0))

    # Test with missing high parameter
    expect_error(Significant(), "argument high must be provided")

    # Test with piping
    result <- get_query(daf, Significant(high = 3.0, Axis("cell") |> Lookup("effect_sizes")))
    expect_equal(result, c(0, 4.0, 0))
})

# Test reduction operations

test_that("Max operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        Lookup("values") |>
        Max()
    result <- get_query(daf, query)
    expect_equal(result, 2.5)

    # Verify the matrix exists so we can test matrix reduction
    expect_true(has_matrix(daf, "cell", "gene", "data"))

    # Test matrix reduction on columns
    query <- Axis("cell") |>
        Axis("gene") |>
        Lookup("data") |>
        Max()
    result <- get_query(daf, query)
    expect_equal(result, c(3, 6, 9)) # Max of each column

    # Test with piping
    result <- get_query(daf, Max(Axis("cell") |> Lookup("values")))
    expect_equal(result, 2.5)
})

test_that("Min operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        Lookup("values") |>
        Min()
    result <- get_query(daf, query)
    expect_equal(result, -1.5)

    # Test matrix reduction
    query <- Axis("cell") |>
        Axis("gene") |>
        Lookup("data") |>
        Min()
    result <- get_query(daf, query)
    expect_equal(result, c(1, 4, 7)) # Min of each column

    # Test with piping
    result <- get_query(daf, Min(Axis("cell") |> Lookup("values")))
    expect_equal(result, -1.5)
})

test_that("Mean operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        Lookup("values") |>
        Mean()
    result <- get_query(daf, query)
    expect_equal(result, sum(c(-1.5, 0, 2.5)) / 3)

    # Test matrix reduction
    query <- Axis("cell") |>
        Axis("gene") |>
        Lookup("data") |>
        Mean()
    result <- get_query(daf, query)
    expect_equal(result, c(2, 5, 8)) # Mean of each column

    # Test with piping
    result <- get_query(daf, Mean(Axis("cell") |> Lookup("values")))
    expect_equal(result, sum(c(-1.5, 0, 2.5)) / 3)
})

test_that("Median operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        Lookup("values") |>
        Median()
    result <- get_query(daf, query)
    expect_equal(result, 0) # Middle value is 0

    # Test matrix reduction
    query <- Axis("cell") |>
        Axis("gene") |>
        Lookup("data") |>
        Median()
    result <- get_query(daf, query)
    expect_equal(result, c(2, 5, 8)) # Median of each column

    # Test with piping
    result <- get_query(daf, Median(Axis("cell") |> Lookup("values")))
    expect_equal(result, 0)
})

test_that("Quantile operation works", {
    daf <- setup_test_data()

    # Test vector reduction with default p=0.5 (median)
    query <- Axis("cell") |>
        Lookup("values") |>
        Quantile()
    result <- get_query(daf, query)
    expect_equal(result, 0) # Middle value is 0

    # Test with p=0.25 (first quartile)
    query <- Axis("cell") |>
        Lookup("values") |>
        Quantile(p = 0.25)
    result <- get_query(daf, query)
    expect_equal(result, -0.75) # Interpolated value

    # Test with invalid p value
    expect_error(
        Quantile(p = 2)
    )

    # Test with piping
    result <- get_query(daf, Quantile(p = 0.75, Axis("cell") |> Lookup("values")))
    expect_equal(result, 1.25) # Interpolated value
})

test_that("Sum operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        Lookup("values") |>
        Sum()
    result <- get_query(daf, query)
    expect_equal(result, sum(c(-1.5, 0, 2.5)))

    # Test matrix reduction
    query <- Axis("cell") |>
        Axis("gene") |>
        Lookup("data") |>
        Sum()
    result <- get_query(daf, query)
    expect_equal(result, c(6, 15, 24)) # Sum of each column

    # Test with piping
    result <- get_query(daf, Sum(Axis("cell") |> Lookup("values")))
    expect_equal(result, sum(c(-1.5, 0, 2.5)))
})

test_that("Std operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        Lookup("values") |>
        Std()
    result <- get_query(daf, query)
    expect_equal(result, sd_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, Std(Axis("cell") |> Lookup("values")))
    expect_equal(result, sd_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)
})

test_that("StdN operation works", {
    daf <- setup_test_data()

    # Create a test vector with non-zero mean
    set_vector(daf, "cell", "positive", c(2, 3, 7))

    # Test StdN (normalized standard deviation: std / mean)
    query <- Axis("cell") |>
        Lookup("positive") |>
        StdN()
    result <- get_query(daf, query)
    expect_equal(result, sd_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, StdN(Axis("cell") |> Lookup("positive")))
    expect_equal(result, sd_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)
})

test_that("Var operation works", {
    daf <- setup_test_data()

    # Test vector reduction
    query <- Axis("cell") |>
        Lookup("values") |>
        Var()
    result <- get_query(daf, query)
    expect_equal(result, var_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, Var(Axis("cell") |> Lookup("values")))
    expect_equal(result, var_uncorrected(c(-1.5, 0, 2.5)), tolerance = 1e-5)
})

test_that("VarN operation works", {
    daf <- setup_test_data()

    # Create a test vector with non-zero mean
    set_vector(daf, "cell", "positive", c(2, 3, 7))

    # Test VarN (normalized variance: var / mean)
    query <- Axis("cell") |>
        Lookup("positive") |>
        VarN()
    result <- get_query(daf, query)
    expect_equal(result, var_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)

    # Test with piping
    result <- get_query(daf, VarN(Axis("cell") |> Lookup("positive")))
    expect_equal(result, var_uncorrected(c(2, 3, 7)) / mean(c(2, 3, 7)), tolerance = 1e-5)
})
