test_that("viewer over Julia-fixture example-daf exposes base data", {
  fixture_daf <- test_path("fixtures", "julia-queries", "example-daf")
  skip_if_not(file.exists(file.path(fixture_daf, "daf.json")),
              "Julia example daf fixture missing")
  daf <- files_daf(fixture_daf, mode = "r")
  v <- viewer(daf, name = "v")

  # Identity view exposes all of base's axes, scalars, vectors, matrices.
  expect_setequal(axes_set(v), axes_set(daf))
  expect_setequal(scalars_set(v), scalars_set(daf))
  for (axis in axes_set(daf)) {
    expect_setequal(vectors_set(v, axis), vectors_set(daf, axis))
  }
})

test_that("viewer with axis rename exposes new name but same entries", {
  fixture_daf <- test_path("fixtures", "julia-queries", "example-daf")
  skip_if_not(file.exists(file.path(fixture_daf, "daf.json")),
              "Julia example daf fixture missing")
  daf <- files_daf(fixture_daf, mode = "r")

  # Rename 'cell' -> 'obs'
  v <- viewer(daf, axes = list(list("obs", "@ cell")))
  expect_true("obs" %in% axes_set(v))
  expect_equal(axis_length(v, "obs"), axis_length(daf, "cell"))
  expect_equal(axis_vector(v, "obs"), axis_vector(daf, "cell"))
})

test_that("viewer with filtered axis returns subset", {
  fixture_daf <- test_path("fixtures", "julia-queries", "example-daf")
  skip_if_not(file.exists(file.path(fixture_daf, "daf.json")),
              "Julia example daf fixture missing")
  daf <- files_daf(fixture_daf, mode = "r")

  # Pre-condition: fixture has 'donor' axis with 'age' vector
  skip_if_not("donor" %in% axes_set(daf), "donor axis missing from fixture")
  skip_if_not("age" %in% vectors_set(daf, "donor"), "donor/age missing from fixture")

  # Filter donors by age > 60
  v <- viewer(daf, axes = list(list("donor", "@ donor [ age > 60 ]")))
  base_ages    <- get_vector(daf, "donor", "age")
  filtered_len <- sum(base_ages > 60)
  expect_equal(axis_length(v, "donor"), filtered_len)
})

test_that("viewer with scalar rename exposes new scalar name", {
  fixture_daf <- test_path("fixtures", "julia-queries", "example-daf")
  skip_if_not(file.exists(file.path(fixture_daf, "daf.json")),
              "Julia example daf fixture missing")
  daf <- files_daf(fixture_daf, mode = "r")
  skip_if_not("organism" %in% scalars_set(daf), "organism scalar missing from fixture")

  v <- viewer(daf, data = list(list("species", ". organism")))
  expect_true("species" %in% scalars_set(v))
  expect_equal(get_scalar(v, "species"), get_scalar(daf, "organism"))
})
