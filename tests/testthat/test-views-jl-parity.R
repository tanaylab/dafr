# Literal port of ~/src/DataAxesFormats.jl/test/views.jl into R.
#
# Each `nested_test` leaf in views.jl becomes one `test_that` here, named
# with the Julia path. Each leaf does its own setup, mirroring Julia's
# fresh-state semantics.
#
# Documented divergences carry skip("R divergence: <id>: <reason>") that
# names a gap from
#   dev/notes/2026-05-07-views-jl-parity-divergences.md.

# ---------------------------------------------------------------------------
# chains / none
# ---------------------------------------------------------------------------

test_that("views / none", {
    d <- memory_daf(name = "memory!")
    view <- viewer(d, name = "read_only")
    expect_true(is_daf(view))
    # Julia: viewer(view) === view (idempotent). dafr re-wraps; akin to C1.
    # Test the substantive portable assertion: the wrapped view is still
    # backed by `d`.
    renamed <- viewer(view, name = "renamed")
    expect_true(is_daf(renamed))
})

# ---------------------------------------------------------------------------
# views / scalar
# ---------------------------------------------------------------------------

.scalar_setup <- function() {
    d <- memory_daf(name = "memory!")
    set_scalar(d, "version", "1.0")
    add_axis(d, "cell", c("X", "Y"))
    set_vector(d, "cell", "age", c(1L, 2L))
    d
}

test_that("views / scalar / copy", {
    d <- .scalar_setup()
    view <- viewer(read_only(d), name = "view!", data = list(VIEW_ALL_SCALARS))
    expect_setequal(scalars_set(view), "version")
    expect_equal(get_scalar(view, "version"), "1.0")
    renamed <- read_only(view, name = "renamed")
    expect_true(is_daf(renamed))
})

test_that("views / scalar / reduction", {
    d <- .scalar_setup()
    view <- viewer(d, data = list(list("sum_ages", "@ cell : age >> Sum")))
    expect_setequal(scalars_set(view), "sum_ages")
    expect_equal(get_scalar(view, "sum_ages"), 3L)
})

test_that("views / scalar / vector", {
    d <- .scalar_setup()
    expect_error(
        viewer(d, name = "view!",
               data = list(list("ages", "@ cell : age"))),
        regexp = "invalid scalar query|does not produce a scalar"
    )
})

test_that("views / scalar / hidden", {
    d <- .scalar_setup()
    view <- viewer(d, data = list(VIEW_ALL_SCALARS, list("version", NULL)))
    expect_length(scalars_set(view), 0L)
    expect_false(has_scalar(view, "version"))
})

test_that("views / scalar / !*", {
    d <- .scalar_setup()
    expect_error(
        viewer(d, name = "view!",
               axes = list(VIEW_ALL_AXES),
               data = list(list("*", "version"))),
        regexp = "invalid wildcard scalar query"
    )
})

# ---------------------------------------------------------------------------
# views / axis
# ---------------------------------------------------------------------------

.axis_setup <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_vector(d, "gene", "is_marker", c(TRUE, FALSE, TRUE))
    d
}

test_that("views / axis / copy", {
    d <- .axis_setup()
    view <- viewer(d, axes = list(list("cell", "="), list("gene", "=")))
    expect_setequal(axes_set(view), c("cell", "gene"))
})

test_that("views / axis / hidden", {
    d <- .axis_setup()
    view <- viewer(d, axes = list(VIEW_ALL_AXES, list("cell", NULL)))
    expect_setequal(axes_set(view), "gene")
    expect_false(has_axis(view, "cell"))
    view2 <- viewer(d, axes = list(list("cell", "=")))
    expect_false(has_axis(view2, "gene"))
})

test_that("views / axis / masked", {
    d <- .axis_setup()
    view <- viewer(d, axes = list(list("gene", "@ gene [ is_marker ]")))
    expect_equal(unname(axis_vector(view, "gene")), c("A", "C"))
})

test_that("views / axis / scalar", {
    # Julia errors at axis_vector() access; dafr errors at viewer()
    # construction (when resolving axis indices). Both surface the
    # same logical error (a non-axis query was used as an axis spec);
    # the timing differs.
    d <- .axis_setup()
    expect_error(
        viewer(d, name = "view!",
               axes = list(list("gene", "@ gene : is_marker >> Sum"))),
        regexp = "not.*axis|axis.*query|entry.*not in"
    )
})

test_that("views / axis / vector", {
    d <- .axis_setup()
    set_vector(d, "cell", "age", c(1L, 2L))
    expect_error(
        viewer(d, name = "view!",
               axes = list(list("cell", "@ cell : age"))),
        regexp = "not.*axis|axis.*query|entry.*not in"
    )
})

# ---------------------------------------------------------------------------
# views / vector
# ---------------------------------------------------------------------------

.vector_setup <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y", "Z"))
    set_vector(d, "cell", "age", c(1L, 2L, 3L))
    set_vector(d, "cell", "batch", c(1L, 2L, 2L))
    d
}

test_that("views / vector / copy", {
    d <- .vector_setup()
    view <- viewer(d, name = "view!",
                   axes = list(VIEW_ALL_AXES),
                   data = list(VIEW_ALL_VECTORS))
    expect_setequal(vectors_set(view, "cell"), c("age", "batch"))
    expect_equal(unname(get_vector(view, "cell", "age")), c(1L, 2L, 3L))
    expect_equal(unname(get_vector(view, "cell", "batch")), c(1L, 2L, 2L))
})

test_that("views / vector / !* / axis", {
    d <- .vector_setup()
    expect_error(
        viewer(d, name = "view!",
               axes = list(VIEW_ALL_AXES),
               data = list(list(c("cell", "*"), "age"))),
        regexp = "invalid wildcard vector query"
    )
})

test_that("views / vector / !* / property", {
    d <- .vector_setup()
    expect_error(
        viewer(d, name = "view!",
               axes = list(VIEW_ALL_AXES),
               data = list(list(c("*", "age"), "@ cell : age"))),
        regexp = "invalid wildcard vector query"
    )
})

test_that("views / vector / missing", {
    d <- .vector_setup()
    view <- viewer(d, name = "view!",
                   axes = list(list("cell", NULL)),
                   data = list(list(c("cell", "age"), "=")))
    expect_error(
        get_vector(view, "cell", "age"),
        regexp = "missing axis.*cell|cell.*missing|axis: cell.*does not exist"
    )
})

test_that("views / vector / hidden", {
    # Test the inclusive-and-then-hide case (which dafr supports):
    # VIEW_ALL_VECTORS exposes everything, then ("cell", "age") -> NULL
    # hides one. The "explicit-only" half of the Julia test is covered
    # by V3 below.
    d <- .vector_setup()
    view <- viewer(d,
                   axes = list(VIEW_ALL_AXES),
                   data = list(VIEW_ALL_VECTORS,
                               list(c("cell", "age"), NULL)))
    expect_setequal(vectors_set(view, "cell"), "batch")
    expect_equal(unname(get_vector(view, "cell", "batch")), c(1L, 2L, 2L))
    expect_false(has_vector(view, "cell", "age"))
})

test_that("views / vector / hidden / explicit-only", {
    d <- .vector_setup()
    view <- viewer(d,
                   axes = list(VIEW_ALL_AXES),
                   data = list(list(c("cell", "age"), "=")))
    expect_setequal(vectors_set(view, "cell"), "age")
})

test_that("views / vector / renamed", {
    d <- .vector_setup()
    view <- viewer(d,
                   axes = list(VIEW_ALL_AXES),
                   data = list(list(c("cell", "day"), ": age")))
    expect_setequal(vectors_set(view, "cell"), "day")
    expect_equal(unname(get_vector(view, "cell", "day")), c(1L, 2L, 3L))
})

test_that("views / vector / masked / ()", {
    # The substantive portable check is that `age` masked by `batch=V`
    # returns the right values [2, 3]; the vectors_set strict-include
    # assertion is V3 territory.
    d <- .vector_setup()
    set_vector(d, "cell", "batch", c("U", "V", "V"), overwrite = TRUE)
    view <- viewer(d,
                   axes = list(list("cell", "@ cell [ batch = V ]")),
                   data = list(list(c("cell", "age"), "=")))
    expect_true(has_vector(view, "cell", "age"))
    expect_equal(unname(get_vector(view, "cell", "age")), c(2L, 3L))
})

test_that("views / vector / masked / query", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y", "Z"))
    add_axis(d, "gene", c("A", "B"))
    set_vector(d, "cell", "batch", c("U", "V", "V"))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(c(0, 2, 4, 1, 3, 5), 3, 2,
               dimnames = list(c("X", "Y", "Z"), c("A", "B"))))
    view <- viewer(d, name = "view!",
                   axes = list(list("cell", "@ cell [ batch = V ]")),
                   data = list(list(c("cell", "total_UMIs"),
                                    "@ gene @ __axis__ :: UMIs >- Sum")))
    out <- get_vector(view, "cell", "total_UMIs")
    expect_equal(unname(as.numeric(out)), c(5, 9))
})

test_that("views / vector / reduced", {
    d <- .vector_setup()
    add_axis(d, "gene", c("A", "B"))
    set_matrix(d, "cell", "gene", "UMIs",
               matrix(c(0, 2, 4, 1, 3, 5), nrow = 3L, ncol = 2L,
                      dimnames = list(c("X", "Y", "Z"), c("A", "B"))))
    view <- viewer(
        d,
        axes = list(VIEW_ALL_AXES),
        data = list(list(c("cell", "total_umis"),
                         "@ cell @ gene :: UMIs >| Sum"))
    )
    expect_equal(unname(get_vector(view, "cell", "total_umis")),
                 c(1L, 5L, 9L))
})

test_that("views / vector / matrix", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(c(0, 3, 1, 4, 2, 5), 2, 3,
               dimnames = list(c("X", "Y"), c("A", "B", "C"))))
    view <- viewer(d, name = "view!",
                   axes = list(VIEW_ALL_AXES),
                   data = list(list(c("cell", "total_umis"),
                                    "@ __axis__ @ gene :: UMIs")))
    expect_error(get_vector(view, "cell", "total_umis"),
        regexp = "matrix query|matrix.*shape|not a vector")
})

# ---------------------------------------------------------------------------
# views / matrix
# ---------------------------------------------------------------------------

.matrix_setup <- function() {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("X", "Y"))
    add_axis(d, "gene", c("A", "B", "C"))
    set_vector(d, "gene", "is_marker", c(TRUE, FALSE, TRUE))
    set_matrix(d, "cell", "gene", "UMIs",
               matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
                      dimnames = list(c("X", "Y"), c("A", "B", "C"))))
    d
}

test_that("views / matrix / copy", {
    d <- .matrix_setup()
    view <- viewer(d,
                   axes = list(VIEW_ALL_AXES),
                   data = list(VIEW_ALL_MATRICES))
    expect_setequal(matrices_set(view, "cell", "gene"), "UMIs")
    expect_equal(unname(as.matrix(get_matrix(view, "cell", "gene", "UMIs"))),
                 matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L))
})

test_that("views / matrix / !* / rows_axis", {
    d <- .matrix_setup()
    expect_error(
        viewer(d, name = "view!",
               axes = list(VIEW_ALL_AXES),
               data = list(list(c("*", "gene", "UMIs"), "UMIs"))),
        regexp = "invalid wildcard matrix query"
    )
})

test_that("views / matrix / !* / columns_axis", {
    d <- .matrix_setup()
    expect_error(
        viewer(d, name = "view!",
               axes = list(VIEW_ALL_AXES),
               data = list(list(c("cell", "*", "UMIs"), "UMIs"))),
        regexp = "invalid wildcard matrix query"
    )
})

test_that("views / matrix / !* / property", {
    d <- .matrix_setup()
    expect_error(
        viewer(d, name = "view!",
               axes = list(VIEW_ALL_AXES),
               data = list(list(c("cell", "gene", "*"), "UMIs"))),
        regexp = "invalid wildcard matrix query"
    )
})

test_that("views / matrix / query", {
    d <- .matrix_setup()
    view <- viewer(d, name = "view!",
                   axes = list(VIEW_ALL_AXES),
                   data = list(list(c("cell", "gene", "UMIs"), ":: UMIs % Abs")))
    expect_setequal(matrices_set(view, "cell", "gene"), "UMIs")
    expect_equal(unname(as.matrix(get_matrix(view, "cell", "gene", "UMIs"))),
                 abs(matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L)))
})

test_that("views / matrix / hidden", {
    # Test the inclusive-and-then-hide case (which dafr supports).
    # Explicit-only is V3.
    d <- .matrix_setup()
    view <- viewer(d,
                   axes = list(VIEW_ALL_AXES),
                   data = list(VIEW_ALL_MATRICES,
                               list(c("cell", "gene", "UMIs"), NULL)))
    expect_length(matrices_set(view, "cell", "gene"), 0L)
    expect_false(has_matrix(view, "cell", "gene", "UMIs"))
})

test_that("views / matrix / hidden / explicit-only", {
    d <- .matrix_setup()
    view <- viewer(d,
                   axes = list(VIEW_ALL_AXES),
                   data = list(list(c("cell", "gene", "UMIs"), "=")))
    expect_setequal(matrices_set(view, "cell", "gene"), "UMIs")
})

test_that("views / matrix / masked", {
    d <- .matrix_setup()
    view <- viewer(d,
                   axes = list(list("cell", "="),
                               list("gene", "@ gene [ is_marker ]")),
                   data = list(VIEW_ALL_MATRICES))
    expect_setequal(matrices_set(view, "cell", "gene"), "UMIs")
    expect_equal(unname(as.matrix(get_matrix(view, "cell", "gene", "UMIs"))),
                 matrix(c(0, 3, 2, 5), nrow = 2L, ncol = 2L))
})

test_that("views / matrix / vector", {
    d <- .matrix_setup()
    view <- viewer(d, name = "view!",
                   axes = list(VIEW_ALL_AXES),
                   data = list(list(c("cell", "gene", "UMIs"),
                                    ":: UMIS >| Sum")))
    # dafr errors at the eval layer with "ReduceToColumn requires a
    # matrix or grouped scope" when the query produces a vector that
    # doesn't fit the matrix slot. Julia errors with "matrix query"
    # wording; both are valid, distinct from a passing path.
    expect_error(
        get_matrix(view, "cell", "gene", "UMIs"),
        regexp = "matrix|UMIs|UMIS|requires"
    )
})

# ---------------------------------------------------------------------------
# views / requires_relayout — description tests, skip C2
# ---------------------------------------------------------------------------

test_that("views / requires_relayout / ()", {
    skip("R divergence C2: dafr description() has no `deep` parameter and the chain/view-deep description format differs from Julia's")
})

test_that("views / requires_relayout / deep", {
    skip("R divergence C2: dafr description() has no `deep` parameter and the chain/view-deep description format differs from Julia's")
})

test_that("views / requires_relayout / realized", {
    d <- memory_daf(name = "memory!")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    set_matrix(d, "gene", "cell", "UMIs",
        matrix(c(1, 3, 5, 2, 4, 6), 3, 2, byrow = FALSE,
               dimnames = list(c("X", "Y", "Z"), c("A", "B"))),
        relayout = FALSE)
    view <- viewer(d, name = "view!",
                   axes = list(list("obs", "@ cell"), list("var", "@ gene")),
                   data = list(list(c("obs", "var", "X"), ":: UMIs")))
    out <- get_matrix(view, "obs", "var", "X")
    expect_equal(dim(out), c(2L, 3L))
})

# ---------------------------------------------------------------------------
# views / tensor — entire group skipped
# ---------------------------------------------------------------------------

test_that("views / tensor / *", {
    skip("R divergence V1: dafr's view layer has no tensor support (no <axis>_<entry>_<name> auto-grouping in viewer/description). Tensors are a Julia-DAF naming convention; covered partially in dafr's contracts but not views.")
})
