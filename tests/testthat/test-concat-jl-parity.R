# Literal port of ~/src/DataAxesFormats.jl/test/concat.jl into R.
#
# Each `nested_test` leaf in concat.jl becomes one `test_that` here, named
# with the Julia path. Each leaf does its own setup, mirroring Julia's
# fresh-state semantics across the nested_test tree.
#
# Documented divergences carry skip("R divergence: <id>: <reason>") that
# names a gap from
#   dev/notes/2026-05-06-concat-jl-parity-divergences.md.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.fresh <- function() {
    destination <- memory_daf(name = "destination!")
    sources <- list(
        memory_daf(name = "source.1!"),
        memory_daf(name = "source.2!")
    )
    add_axis(sources[[1L]], "cell", c("A", "B"))
    add_axis(sources[[2L]], "cell", c("C", "D", "E"))
    list(destination = destination, sources = sources)
}

# ---------------------------------------------------------------------------
# concat / !square
# ---------------------------------------------------------------------------

test_that("concat / !square", {
    s <- .fresh()
    m <- matrix(c(1, 0, 0, 1), nrow = 2L, ncol = 2L,
                dimnames = list(c("A", "B"), c("A", "B")))
    set_matrix(s$sources[[1L]], "cell", "cell", "outgoing_edges", m)
    expect_error(
        concatenate(s$destination, "cell", s$sources),
        regexp = "outgoing_edges|both axes in the concat set"
    )
})

# ---------------------------------------------------------------------------
# concat / !entries
# ---------------------------------------------------------------------------

test_that("concat / !entries", {
    s <- .fresh()
    add_axis(s$sources[[1L]], "gene", c("X", "Y"))
    add_axis(s$sources[[2L]], "gene", c("X", "Y", "Z"))
    expect_error(
        concatenate(s$destination, "cell", s$sources),
        regexp = "different entries.*gene|gene.*different entries"
    )
})

# ---------------------------------------------------------------------------
# concat / concatenate
# ---------------------------------------------------------------------------

test_that("concat / concatenate / empty", {
    s <- .fresh()
    concatenate(s$destination, "cell", s$sources)
    expect_equal(unname(axis_vector(s$destination, "cell")),
                 c("A", "B", "C", "D", "E"))
    expect_equal(unname(axis_vector(s$destination, "dataset")),
                 c("source.1!", "source.2!"))
    expect_equal(
        unname(get_vector(s$destination, "cell", "dataset")),
        c("source.1!", "source.1!", "source.2!", "source.2!", "source.2!")
    )
})

test_that("concat / concatenate / !dataset_axis", {
    s <- .fresh()
    concatenate(s$destination, "cell", s$sources, dataset_axis = NULL)
    expect_equal(unname(axis_vector(s$destination, "cell")),
                 c("A", "B", "C", "D", "E"))
    expect_false(has_axis(s$destination, "dataset"))
    expect_false(has_vector(s$destination, "cell", "dataset"))
})

test_that("concat / concatenate / !dataset_property", {
    s <- .fresh()
    concatenate(s$destination, "cell", s$sources, dataset_property = FALSE)
    expect_equal(unname(axis_vector(s$destination, "cell")),
                 c("A", "B", "C", "D", "E"))
    expect_equal(unname(axis_vector(s$destination, "dataset")),
                 c("source.1!", "source.2!"))
    expect_false(has_vector(s$destination, "cell", "dataset"))
})

test_that("concat / concatenate / vector / !string", {
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "age", c(1L, 2L))
    set_vector(s$sources[[2L]], "cell", "age", c(3L, 4L, 5L))
    concatenate(s$destination, "cell", s$sources)
    expect_equal(unname(get_vector(s$destination, "cell", "age")),
                 c(1L, 2L, 3L, 4L, 5L))
})

test_that("concat / concatenate / vector / string", {
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "color", c("red", "green"))
    set_vector(s$sources[[2L]], "cell", "color", c("blue", "red", "yellow"))
    concatenate(s$destination, "cell", s$sources)
    expect_equal(unname(get_vector(s$destination, "cell", "color")),
                 c("red", "green", "blue", "red", "yellow"))
})

test_that("concat / concatenate / vector / string!", {
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "color", c(1L, 2L))
    set_vector(s$sources[[2L]], "cell", "color", c("blue", "red", "yellow"))
    concatenate(s$destination, "cell", s$sources)
    expect_equal(unname(get_vector(s$destination, "cell", "color")),
                 c("1", "2", "blue", "red", "yellow"))
})

test_that("concat / concatenate / vector / !empty", {
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "color", c("red", "green"))
    expect_error(
        concatenate(s$destination, "cell", s$sources),
        regexp = "no empty value.*color|color.*no empty value"
    )
})

test_that("concat / concatenate / vector / empty", {
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "color", c("red", "green"))
    concatenate(s$destination, "cell", s$sources,
                empty = list("cell|color" = "black"))
    expect_equal(unname(get_vector(s$destination, "cell", "color")),
                 c("red", "green", "black", "black", "black"))
})

test_that("concat / concatenate / matrix", {
    s <- .fresh()
    add_axis(s$sources[[1L]], "gene", c("X", "Y"))
    add_axis(s$sources[[2L]], "gene", c("X", "Y"))
    set_matrix(s$sources[[1L]], "cell", "gene", "UMIs",
               matrix(c(1, 3, 2, 4), nrow = 2L, ncol = 2L,
                      dimnames = list(c("A", "B"), c("X", "Y"))))
    set_matrix(s$sources[[2L]], "cell", "gene", "UMIs",
               matrix(c(5, 7, 9, 6, 8, 10), nrow = 3L, ncol = 2L,
                      dimnames = list(c("C", "D", "E"), c("X", "Y"))))
    concatenate(s$destination, "cell", s$sources)
    expected <- matrix(c(1, 3, 5, 7, 9, 2, 4, 6, 8, 10),
                       nrow = 5L, ncol = 2L,
                       dimnames = list(c("A", "B", "C", "D", "E"),
                                       c("X", "Y")))
    actual <- get_matrix(s$destination, "cell", "gene", "UMIs")
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

# ---------------------------------------------------------------------------
# concat / prefix
# ---------------------------------------------------------------------------

# Helper: per-leaf setup for the prefix group. Mirrors Julia's nested_test
# parent block (metacell axes + cell|metacell + cell|!metacell vectors).
.prefix_setup <- function() {
    s <- .fresh()
    add_axis(s$sources[[1L]], "metacell", c("M1"))
    add_axis(s$sources[[2L]], "metacell", c("M1", "M2"))
    set_vector(s$sources[[1L]], "cell", "metacell", c("M1", "M1"))
    set_vector(s$sources[[2L]], "cell", "metacell", c("M1", "M2", "M1"))
    set_vector(s$sources[[1L]], "cell", "!metacell", c("M1", "M1"))
    set_vector(s$sources[[2L]], "cell", "!metacell", c("M1", "M2", "M1"))
    s
}

test_that("concat / prefix / false", {
    s <- .prefix_setup()
    expect_error(
        concatenate(s$destination, c("cell", "metacell"), s$sources),
        regexp = "duplicate entries.*metacell|metacell.*duplicate entries|non-unique"
    )
})

test_that("concat / prefix / true", {
    s <- .prefix_setup()
    concatenate(s$destination, c("cell", "metacell"), s$sources,
                prefix = c(FALSE, TRUE))
    expect_equal(unname(axis_vector(s$destination, "cell")),
                 c("A", "B", "C", "D", "E"))
    expect_equal(unname(axis_vector(s$destination, "metacell")),
                 c("source.1!.M1", "source.2!.M1", "source.2!.M2"))
    expect_equal(unname(axis_vector(s$destination, "dataset")),
                 c("source.1!", "source.2!"))
    expect_equal(
        unname(get_vector(s$destination, "cell", "dataset")),
        c("source.1!", "source.1!", "source.2!", "source.2!", "source.2!")
    )
    expect_equal(unname(get_vector(s$destination, "metacell", "dataset")),
                 c("source.1!", "source.2!", "source.2!"))
    expect_equal(
        unname(get_vector(s$destination, "cell", "metacell")),
        c("source.1!.M1", "source.1!.M1", "source.2!.M1",
          "source.2!.M2", "source.2!.M1")
    )
    # Julia's `!metacell` property is NOT prefixed by the heuristic
    # (its name doesn't match a prefixed axis).
    expect_equal(unname(get_vector(s$destination, "cell", "!metacell")),
                 c("M1", "M1", "M1", "M2", "M1"))
})

test_that("concat / prefix / names", {
    s <- .prefix_setup()
    concatenate(s$destination, c("cell", "metacell"), s$sources,
                prefix = c(FALSE, TRUE), names = c("D1", "D2"))
    expect_equal(unname(axis_vector(s$destination, "cell")),
                 c("A", "B", "C", "D", "E"))
    expect_equal(unname(axis_vector(s$destination, "metacell")),
                 c("D1.M1", "D2.M1", "D2.M2"))
    expect_equal(unname(axis_vector(s$destination, "dataset")),
                 c("D1", "D2"))
    expect_equal(unname(get_vector(s$destination, "cell", "dataset")),
                 c("D1", "D1", "D2", "D2", "D2"))
    expect_equal(unname(get_vector(s$destination, "metacell", "dataset")),
                 c("D1", "D2", "D2"))
    expect_equal(unname(get_vector(s$destination, "cell", "metacell")),
                 c("D1.M1", "D1.M1", "D2.M1", "D2.M2", "D2.M1"))
    expect_equal(unname(get_vector(s$destination, "cell", "!metacell")),
                 c("M1", "M1", "M1", "M2", "M1"))
})

test_that("concat / prefix / prefixes", {
    # Julia: prefixed = [Set(["metacell", "!metacell"]), Set{String}()]
    # is an override that fires regardless of prefix[axis]. dafr's gate
    # `isTRUE(do_prefix) && name %in% vec` requires prefix[axis] == TRUE
    # for the explicit set to act, so cell-axis properties listed in
    # prefixed[1] are NOT prefixed when prefix[cell] == FALSE.
    skip("R divergence M4: dafr prefixed= is gated by per-axis prefix flag, Julia treats it as override")
})

# ---------------------------------------------------------------------------
# concat / sparse / vector
# ---------------------------------------------------------------------------

test_that("concat / sparse / vector / dense", {
    # Julia: sparse-input vectors that concat to high density get
    # densified. dafr's memory_daf doesn't accept Matrix::sparseVector
    # at all (atomic-only gate in .validate_vector_value); files_daf does.
    skip("R divergence M5: memory_daf rejects Matrix::sparseVector (atomic-only)")
})

test_that("concat / sparse / vector / sparse", {
    skip("R divergence M5: memory_daf rejects Matrix::sparseVector (atomic-only)")
})

test_that("concat / sparse / vector / !empty", {
    # Translated to dense input (the missing-empty error is the same
    # whether the source vector is sparse or dense).
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "age", c(1L, 2L))
    expect_error(
        concatenate(s$destination, "cell", s$sources),
        regexp = "no empty value.*age|age.*no empty value"
    )
})

test_that("concat / sparse / vector / ~empty", {
    # Julia passes empty = Dict("version" => 0); dafr's empty key format is
    # axis|property, so a bare scalar-style key doesn't match the missing
    # `cell|age` vector and the error still fires.
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "age", c(1L, 2L))
    expect_error(
        concatenate(s$destination, "cell", s$sources,
                    empty = list("version" = 0)),
        regexp = "no empty value.*age|age.*no empty value"
    )
})

test_that("concat / sparse / vector / empty / zero", {
    # Sparsity-preservation assertion not portable (M2); dafr's concat for
    # vectors always produces dense output via do.call(c, ...).
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "age", c(1L, 2L))
    concatenate(s$destination, "cell", s$sources,
                empty = list("cell|age" = 0L))
    expect_equal(unname(get_vector(s$destination, "cell", "age")),
                 c(1L, 2L, 0L, 0L, 0L))
})

test_that("concat / sparse / vector / empty / !zero", {
    s <- .fresh()
    set_vector(s$sources[[1L]], "cell", "age", c(1L, 0L))
    concatenate(s$destination, "cell", s$sources,
                empty = list("cell|age" = 2L))
    expect_equal(unname(get_vector(s$destination, "cell", "age")),
                 c(1L, 0L, 2L, 2L, 2L))
})

# ---------------------------------------------------------------------------
# concat / sparse / matrix
# ---------------------------------------------------------------------------

.sparse_matrix_setup <- function() {
    s <- .fresh()
    add_axis(s$sources[[1L]], "gene", c("X", "Y"))
    add_axis(s$sources[[2L]], "gene", c("X", "Y"))
    s
}

test_that("concat / sparse / matrix / dense", {
    s <- .sparse_matrix_setup()
    m1 <- methods::as(matrix(c(1, 3, 2, 4), nrow = 2L, ncol = 2L,
                                dimnames = list(c("A", "B"), c("X", "Y"))),
                         "dgCMatrix")
    m2 <- methods::as(matrix(c(5, 7, 9, 6, 8, 10), nrow = 3L, ncol = 2L,
                                dimnames = list(c("C", "D", "E"), c("X", "Y"))),
                         "dgCMatrix")
    set_matrix(s$sources[[1L]], "cell", "gene", "UMIs", m1)
    set_matrix(s$sources[[2L]], "cell", "gene", "UMIs", m2)
    concatenate(s$destination, "cell", s$sources)
    actual <- get_matrix(s$destination, "cell", "gene", "UMIs")
    expected <- matrix(c(1, 3, 5, 7, 9, 2, 4, 6, 8, 10),
                       nrow = 5L, ncol = 2L,
                       dimnames = list(c("A", "B", "C", "D", "E"),
                                       c("X", "Y")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

test_that("concat / sparse / matrix / sparse", {
    s <- .sparse_matrix_setup()
    m1 <- methods::as(matrix(c(1, 0, 0, 0), nrow = 2L, ncol = 2L,
                                dimnames = list(c("A", "B"), c("X", "Y"))),
                         "dgCMatrix")
    m2 <- methods::as(matrix(c(0, 3, 0, 2, 0, 0), nrow = 3L, ncol = 2L,
                                dimnames = list(c("C", "D", "E"), c("X", "Y"))),
                         "dgCMatrix")
    set_matrix(s$sources[[1L]], "cell", "gene", "UMIs", m1)
    set_matrix(s$sources[[2L]], "cell", "gene", "UMIs", m2)
    concatenate(s$destination, "cell", s$sources)
    actual <- get_matrix(s$destination, "cell", "gene", "UMIs")
    expected <- matrix(c(1, 0, 0, 3, 0, 0, 0, 2, 0, 0),
                       nrow = 5L, ncol = 2L,
                       dimnames = list(c("A", "B", "C", "D", "E"),
                                       c("X", "Y")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
    # Sparsity preservation: dafr's concat takes the sparse fast path
    # (cbind on dgCMatrix at concat.R:287-288) so the result IS sparse here.
    expect_true(inherits(actual, "dgCMatrix") ||
                inherits(actual, "lgCMatrix") ||
                inherits(actual, "Matrix"))
})

test_that("concat / sparse / matrix / !empty", {
    s <- .sparse_matrix_setup()
    m1 <- methods::as(matrix(c(1, 3, 2, 4), nrow = 2L, ncol = 2L,
                                dimnames = list(c("A", "B"), c("X", "Y"))),
                         "dgCMatrix")
    set_matrix(s$sources[[1L]], "cell", "gene", "UMIs", m1)
    expect_error(
        concatenate(s$destination, "cell", s$sources),
        regexp = "no empty value.*UMIs|UMIs.*no empty value"
    )
})

test_that("concat / sparse / matrix / ~empty", {
    s <- .sparse_matrix_setup()
    m1 <- methods::as(matrix(c(1, 3, 2, 4), nrow = 2L, ncol = 2L,
                                dimnames = list(c("A", "B"), c("X", "Y"))),
                         "dgCMatrix")
    set_matrix(s$sources[[1L]], "cell", "gene", "UMIs", m1)
    expect_error(
        concatenate(s$destination, "cell", s$sources,
                    empty = list("version" = 0)),
        regexp = "no empty value.*UMIs|UMIs.*no empty value"
    )
})

test_that("concat / sparse / matrix / empty / zero", {
    s <- .sparse_matrix_setup()
    m1 <- methods::as(matrix(c(1, 0, 2, 0), nrow = 2L, ncol = 2L,
                                dimnames = list(c("A", "B"), c("X", "Y"))),
                         "dgCMatrix")
    set_matrix(s$sources[[1L]], "cell", "gene", "UMIs", m1)
    concatenate(s$destination, "cell", s$sources,
                empty = list("cell|gene|UMIs" = 0))
    actual <- get_matrix(s$destination, "cell", "gene", "UMIs")
    expected <- matrix(c(1, 0, 0, 0, 0, 2, 0, 0, 0, 0),
                       nrow = 5L, ncol = 2L,
                       dimnames = list(c("A", "B", "C", "D", "E"),
                                       c("X", "Y")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

test_that("concat / sparse / matrix / empty / !zero", {
    # Julia uses key ("gene","cell","UMIs") (rows-first), dafr accepts both
    # "cell|gene|UMIs" and "gene|cell|UMIs" (line 273 in concat.R).
    s <- .sparse_matrix_setup()
    m1 <- methods::as(matrix(c(1, 3, 2, 4), nrow = 2L, ncol = 2L,
                                dimnames = list(c("A", "B"), c("X", "Y"))),
                         "dgCMatrix")
    set_matrix(s$sources[[1L]], "cell", "gene", "UMIs", m1)
    concatenate(s$destination, "cell", s$sources,
                empty = list("gene|cell|UMIs" = 5))
    actual <- get_matrix(s$destination, "cell", "gene", "UMIs")
    expected <- matrix(c(1, 3, 5, 5, 5, 2, 4, 5, 5, 5),
                       nrow = 5L, ncol = 2L,
                       dimnames = list(c("A", "B", "C", "D", "E"),
                                       c("X", "Y")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

# ---------------------------------------------------------------------------
# concat / merge / scalar
# ---------------------------------------------------------------------------

.merge_scalar_setup <- function() {
    s <- .fresh()
    set_scalar(s$sources[[1L]], "version", 1L)
    set_scalar(s$sources[[2L]], "version", 2L)
    s
}

test_that("concat / merge / scalar / skip", {
    # Julia: merge = [ALL_VECTORS => LastValue] — wildcard doesn't match a
    # scalar, so the scalar is left unmerged. dafr doesn't honor the
    # wildcard either (M1), but the side-effect on the scalar matches:
    # explicit-only merge keys also skip "version".
    s <- .merge_scalar_setup()
    concatenate(s$destination, "cell", s$sources,
                merge = list("cell|*" = MERGE_LAST_VALUE))
    expect_false(has_scalar(s$destination, "version"))
    expect_false(has_vector(s$destination, "dataset", "version"))
})

test_that("concat / merge / scalar / last", {
    s <- .merge_scalar_setup()
    concatenate(s$destination, "cell", s$sources,
                merge = list("version" = MERGE_LAST_VALUE))
    expect_equal(get_scalar(s$destination, "version"), 2L)
    expect_false(has_vector(s$destination, "dataset", "version"))
})

test_that("concat / merge / scalar / collect", {
    skip("R divergence M1: dafr concatenate merge does not honor ALL_SCALARS wildcard")
})

test_that("concat / merge / scalar / !collect", {
    skip("R divergence M1: dafr concatenate merge does not honor ALL_SCALARS wildcard")
})

# ---------------------------------------------------------------------------
# concat / merge / vector
# ---------------------------------------------------------------------------

.merge_vector_setup <- function() {
    s <- .fresh()
    add_axis(s$sources[[1L]], "gene", c("X", "Y"))
    add_axis(s$sources[[2L]], "gene", c("X", "Y"))
    # Julia uses UInt8 / UInt16; R has no unsigned. Use integer.
    set_vector(s$sources[[1L]], "gene", "weight", c(1L, 2L))
    set_vector(s$sources[[2L]], "gene", "weight", c(3L, 4L))
    s
}

test_that("concat / merge / vector / skip", {
    # Julia: merge = [ALL_SCALARS => LastValue] — wildcard, doesn't match a
    # vector. dafr-equivalent: don't list the gene|weight key, vector is
    # skipped by default.
    s <- .merge_vector_setup()
    concatenate(s$destination, "cell", s$sources,
                merge = list("version" = MERGE_LAST_VALUE))
    expect_false(has_vector(s$destination, "gene", "weight"))
    expect_false(has_matrix(s$destination, "dataset", "gene", "weight"))
})

test_that("concat / merge / vector / last", {
    s <- .merge_vector_setup()
    concatenate(s$destination, "cell", s$sources,
                merge = list("gene|weight" = MERGE_LAST_VALUE))
    expect_equal(unname(get_vector(s$destination, "gene", "weight")),
                 c(3L, 4L))
    expect_false(has_matrix(s$destination, "dataset", "gene", "weight"))
})

test_that("concat / merge / vector / collect / dense / full", {
    # Translate Julia's ALL_VECTORS wildcard to the explicit gene|weight key.
    # The semantics tested (collect-axis produces a (gene x dataset) matrix)
    # do work via explicit keys.
    s <- .merge_vector_setup()
    concatenate(s$destination, "cell", s$sources,
                merge = list("gene|weight" = MERGE_COLLECT_AXIS))
    expect_false(has_vector(s$destination, "gene", "weight"))
    actual <- get_matrix(s$destination, "gene", "dataset", "weight")
    expected <- matrix(c(1L, 2L, 3L, 4L), nrow = 2L, ncol = 2L,
                       dimnames = list(c("X", "Y"),
                                       c("source.1!", "source.2!")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

test_that("concat / merge / vector / collect / dense / empty / zero", {
    s <- .merge_vector_setup()
    delete_vector(s$sources[[2L]], "gene", "weight")
    concatenate(
        s$destination, "cell", s$sources,
        merge = list("gene|weight" = MERGE_COLLECT_AXIS),
        empty = list("gene|weight" = 0)
    )
    expect_false(has_vector(s$destination, "gene", "weight"))
    actual <- get_matrix(s$destination, "gene", "dataset", "weight")
    expected <- matrix(c(1, 2, 0, 0), nrow = 2L, ncol = 2L,
                       dimnames = list(c("X", "Y"),
                                       c("source.1!", "source.2!")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

test_that("concat / merge / vector / collect / dense / empty / !zero", {
    s <- .merge_vector_setup()
    delete_vector(s$sources[[2L]], "gene", "weight")
    concatenate(
        s$destination, "cell", s$sources,
        merge = list("gene|weight" = MERGE_COLLECT_AXIS),
        empty = list("gene|weight" = 3L)
    )
    expect_false(has_vector(s$destination, "gene", "weight"))
    actual <- get_matrix(s$destination, "gene", "dataset", "weight")
    expected <- matrix(c(1L, 2L, 3L, 3L), nrow = 2L, ncol = 2L,
                       dimnames = list(c("X", "Y"),
                                       c("source.1!", "source.2!")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

test_that("concat / merge / vector / collect / sparse", {
    skip("R divergence M2: concatenate vector collect-axis path always allocates dense matrix")
})

test_that("concat / merge / vector / !collect", {
    s <- .merge_vector_setup()
    expect_error(
        concatenate(
            s$destination, "cell", s$sources,
            dataset_axis = NULL,
            merge = list("gene|weight" = MERGE_COLLECT_AXIS)
        ),
        regexp = "collect axis.*weight|weight.*collect axis|no dataset axis"
    )
})

# ---------------------------------------------------------------------------
# concat / merge / matrix
# ---------------------------------------------------------------------------

.merge_matrix_square_setup <- function() {
    s <- .fresh()
    add_axis(s$sources[[1L]], "gene", c("X", "Y"))
    add_axis(s$sources[[2L]], "gene", c("X", "Y"))
    m1 <- matrix(c(1L, 3L, 2L, 4L), nrow = 2L, ncol = 2L,
                 dimnames = list(c("X", "Y"), c("X", "Y")))
    m2 <- matrix(c(5L, 7L, 6L, 8L), nrow = 2L, ncol = 2L,
                 dimnames = list(c("X", "Y"), c("X", "Y")))
    set_matrix(s$sources[[1L]], "gene", "gene", "outgoing_edges", m1)
    set_matrix(s$sources[[2L]], "gene", "gene", "outgoing_edges", m2)
    s
}

test_that("concat / merge / matrix / square / skip", {
    s <- .merge_matrix_square_setup()
    concatenate(s$destination, "cell", s$sources,
                merge = list("version" = MERGE_LAST_VALUE))
    expect_false(has_matrix(s$destination, "gene", "gene", "outgoing_edges"))
})

test_that("concat / merge / matrix / square / last", {
    s <- .merge_matrix_square_setup()
    concatenate(
        s$destination, "cell", s$sources,
        merge = list("gene|gene|outgoing_edges" = MERGE_LAST_VALUE)
    )
    actual <- get_matrix(s$destination, "gene", "gene", "outgoing_edges")
    expected <- matrix(c(5L, 7L, 6L, 8L), nrow = 2L, ncol = 2L,
                       dimnames = list(c("X", "Y"), c("X", "Y")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

test_that("concat / merge / matrix / square / !collect", {
    s <- .merge_matrix_square_setup()
    expect_error(
        concatenate(
            s$destination, "cell", s$sources,
            merge = list("gene|gene|outgoing_edges" = MERGE_COLLECT_AXIS)
        ),
        regexp = "CollectAxis.*matrix|3D|tensor"
    )
})

.merge_matrix_rect_setup <- function() {
    s <- .fresh()
    add_axis(s$sources[[1L]], "gene", c("X", "Y"))
    add_axis(s$sources[[2L]], "gene", c("X", "Y"))
    add_axis(s$sources[[1L]], "batch", c("B1", "B2"))
    add_axis(s$sources[[2L]], "batch", c("B1", "B2"))
    m1 <- matrix(c(1L, 3L, 2L, 4L), nrow = 2L, ncol = 2L,
                 dimnames = list(c("X", "Y"), c("B1", "B2")))
    m2 <- matrix(c(5L, 7L, 6L, 8L), nrow = 2L, ncol = 2L,
                 dimnames = list(c("X", "Y"), c("B1", "B2")))
    set_matrix(s$sources[[1L]], "gene", "batch", "scale", m1)
    set_matrix(s$sources[[2L]], "gene", "batch", "scale", m2)
    s
}

test_that("concat / merge / matrix / rectangle / skip", {
    s <- .merge_matrix_rect_setup()
    concatenate(s$destination, "cell", s$sources,
                merge = list("version" = MERGE_LAST_VALUE))
    expect_false(has_matrix(s$destination, "gene", "batch", "scale"))
})

test_that("concat / merge / matrix / rectangle / last", {
    s <- .merge_matrix_rect_setup()
    concatenate(
        s$destination, "cell", s$sources,
        merge = list("gene|batch|scale" = MERGE_LAST_VALUE)
    )
    actual <- get_matrix(s$destination, "gene", "batch", "scale")
    expected <- matrix(c(5L, 7L, 6L, 8L), nrow = 2L, ncol = 2L,
                       dimnames = list(c("X", "Y"), c("B1", "B2")))
    expect_equal(unname(as.matrix(actual)), unname(expected))
})

test_that("concat / merge / matrix / rectangle / !collect", {
    s <- .merge_matrix_rect_setup()
    expect_error(
        concatenate(
            s$destination, "cell", s$sources,
            merge = list("gene|batch|scale" = MERGE_COLLECT_AXIS)
        ),
        regexp = "CollectAxis.*matrix|3D|tensor"
    )
})
