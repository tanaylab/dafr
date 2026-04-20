# Julia query fixture parity tests.
#
# Each record in fixture.json contains {query, canonical, kind, value} as
# produced by DataAxesFormats.jl against example_cells_daf().
#
# We parse and evaluate every fixture query in R and compare the numeric /
# character values (not the canonical string, which may differ in whitespace)
# against the fixture's `value` field.
#
# R's get_query() returns plain vectors / matrices without axis names attached,
# so comparisons are made on the sorted set of values rather than name-keyed
# element positions.
#
# All 17 fixture queries are evaluated; reduction axis semantics were corrected
# in Slice 3 so queries 15 & 16 (`>|` / `>-`) now match Julia's output.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.julia_value_to_r <- function(kind, value) {
  switch(kind,
    scalar = value,
    vector = {
      raw <- value$values
      first <- if (length(raw) > 0L) raw[[1L]] else NULL
      if (is.character(first)) {
        vapply(raw, function(v) {
          if (is.null(v) || (is.list(v) && length(v) == 0L)) NA_character_
          else as.character(v)
        }, character(1))
      } else if (is.logical(first)) {
        vapply(raw, function(v) {
          if (is.null(v) || (is.list(v) && length(v) == 0L)) NA
          else as.logical(v)
        }, logical(1))
      } else {
        vapply(raw, function(v) {
          if (is.null(v) || (is.list(v) && length(v) == 0L)) NA_real_
          else as.numeric(v)
        }, numeric(1))
      }
    },
    matrix = {
      rn  <- vapply(value$rownames, identity, character(1))
      cn  <- vapply(value$colnames, identity, character(1))
      raw <- value$values
      vals <- vapply(raw, function(v) {
        if (is.null(v) || (is.list(v) && length(v) == 0L)) NA_real_
        else as.numeric(v)
      }, numeric(1))
      # fixture stores values in row-major order matching R's cell×gene layout
      list(nrow = length(rn), ncol = length(cn), values = vals)
    },
    names = unlist(value),
    stop("unknown fixture kind: ", kind)
  )
}

.compare_julia_result <- function(r_val, expected, query, kind) {
  if (kind == "scalar") {
    expect_equal(as.character(r_val), as.character(expected),
                 info = query)

  } else if (kind == "vector") {
    # R does not attach axis names to the returned vector, so compare the
    # sorted multiset of values.
    if (is.character(expected) || is.character(r_val)) {
      expect_equal(sort(as.character(unname(r_val))),
                   sort(as.character(expected)),
                   info = query)
    } else if (is.logical(expected) || is.logical(r_val)) {
      # Logical: compare frequency table (count of TRUE/FALSE/NA)
      expect_equal(table(unname(r_val)), table(unname(expected)),
                   info = query)
    } else {
      # Numeric: compare sorted values with tolerance
      expect_equal(sort(as.numeric(unname(r_val))),
                   sort(as.numeric(expected)),
                   tolerance = 1e-5, info = query)
    }

  } else if (kind == "matrix") {
    # expected is a list(nrow, ncol, values) from .julia_value_to_r
    expect_equal(nrow(r_val), expected$nrow, info = paste("matrix nrow:", query))
    expect_equal(ncol(r_val), expected$ncol, info = paste("matrix ncol:", query))
    expect_equal(sort(as.vector(r_val)), sort(expected$values),
                 tolerance = 1e-5, info = query)

  } else if (kind == "names") {
    expect_setequal(r_val, unlist(expected))
  }
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

test_that("every fixture query parses and evaluates in R", {
  fixture_dir <- test_path("fixtures", "julia-queries")
  skip_if_not(file.exists(file.path(fixture_dir, "fixture.json")),
              "Julia query fixture missing")
  skip_if_not(file.exists(file.path(fixture_dir, "example-daf", "daf.json")),
              "Julia example daf fixture missing")

  fixture <- jsonlite::fromJSON(file.path(fixture_dir, "fixture.json"),
                                simplifyVector = FALSE)
  daf <- files_daf(file.path(fixture_dir, "example-daf"), mode = "r")

  for (idx in seq_along(fixture)) {
    rec <- fixture[[idx]]

    # (no per-query skips; >| / >- axis semantics fixed in Slice 3)

    r_val <- tryCatch(
      get_query(daf, rec$query),
      error = function(e) {
        fail(sprintf("query #%d (%s) failed in R: %s",
                     idx, rec$query, conditionMessage(e)))
        NULL
      }
    )
    if (is.null(r_val)) next

    expected <- .julia_value_to_r(rec$kind, rec$value)
    .compare_julia_result(r_val, expected, rec$query, rec$kind)
  }
})
