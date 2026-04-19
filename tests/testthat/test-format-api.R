test_that("all format_* generics exist with expected dispatch arity", {
  expected <- list(
    format_has_scalar    = c("daf", "name"),
    format_get_scalar    = c("daf", "name"),
    format_set_scalar    = c("daf", "name", "value", "overwrite"),
    format_delete_scalar = c("daf", "name", "must_exist"),
    format_scalars_set   = "daf",

    format_has_axis      = c("daf", "axis"),
    format_add_axis      = c("daf", "axis", "entries"),
    format_delete_axis   = c("daf", "axis", "must_exist"),
    format_axes_set      = "daf",
    format_axis_array    = c("daf", "axis"),
    format_axis_length   = c("daf", "axis"),
    format_axis_dict     = c("daf", "axis"),

    format_has_vector    = c("daf", "axis", "name"),
    format_get_vector    = c("daf", "axis", "name"),
    format_set_vector    = c("daf", "axis", "name", "vec", "overwrite"),
    format_delete_vector = c("daf", "axis", "name", "must_exist"),
    format_vectors_set   = c("daf", "axis"),

    format_has_matrix      = c("daf", "rows_axis", "columns_axis", "name"),
    format_get_matrix      = c("daf", "rows_axis", "columns_axis", "name"),
    format_set_matrix      = c("daf", "rows_axis", "columns_axis", "name", "mat", "overwrite"),
    format_delete_matrix   = c("daf", "rows_axis", "columns_axis", "name", "must_exist"),
    format_matrices_set    = c("daf", "rows_axis", "columns_axis"),
    format_relayout_matrix = c("daf", "rows_axis", "columns_axis", "name")
  )

  ns <- asNamespace("dafr")
  for (nm in names(expected)) {
    expect_true(exists(nm, envir = ns, inherits = FALSE), info = nm)
    gen <- get(nm, envir = ns)
    # S7 generic detection — use whichever of these works on S7 0.2.1:
    expect_true(
      inherits(gen, "S7_generic") || (is.function(gen) && !is.null(attr(gen, "dispatch_args"))),
      info = paste(nm, "is not an S7 generic")
    )
    expect_identical(
      S7::prop(gen, "dispatch_args"),
      expected[[nm]],
      info = nm
    )
  }
})

test_that("format_* generics are not exported (internal API)", {
  ns_exports <- getNamespaceExports("dafr")
  expect_false("format_has_scalar" %in% ns_exports)
  expect_false("format_get_vector" %in% ns_exports)
  expect_false("format_set_matrix" %in% ns_exports)
})
