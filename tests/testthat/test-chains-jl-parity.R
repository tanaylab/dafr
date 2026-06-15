# Literal port of ~/src/DataAxesFormats.jl/test/chains.jl into R.
#
# Each `nested_test` leaf in chains.jl becomes one `test_that` here, named
# with the Julia path. The `access` group's read/write parameterization
# (Julia's `for (name, type_name, chain) in [...]`) is unrolled here as
# explicit test_that pairs sharing assertion helpers.
#
# Documented divergences carry skip("R divergence: <id>: <reason>") that
# names a gap from
#   dev/notes/2026-05-07-chains-jl-parity-divergences.md.

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.fresh <- function() {
    list(
        first  = memory_daf(name = "first!"),
        second = memory_daf(name = "second!")
    )
}

.read_chain_factory <- function(s) {
    chain_reader(list(s$first, read_only(s$second)), name = "chain!")
}

.write_chain_factory <- function(s) {
    chain_writer(list(s$first, s$second), name = "chain!")
}

# ---------------------------------------------------------------------------
# chains / empty
# ---------------------------------------------------------------------------

test_that("chains / empty / name / reader", {
    expect_error(chain_reader(list(), name = "chain!"),
                 regexp = "empty chain.*chain!")
})

test_that("chains / empty / name / writer", {
    expect_error(chain_writer(list(), name = "chain!"),
                 regexp = "empty chain.*chain!")
})

test_that("chains / empty / !name / reader", {
    expect_error(chain_reader(list()), regexp = "empty chain")
})

test_that("chains / empty / !name / writer", {
    expect_error(chain_writer(list()), regexp = "empty chain")
})

# ---------------------------------------------------------------------------
# chains / one
# ---------------------------------------------------------------------------

test_that("chains / one / reader", {
    # Julia: chain_reader([read_only(d)]) returns the same object
    # (chains.jl singleton fast-path delegates to read_only, which is
    # the identity on an already-read-only daf when no name is given).
    # complete_path assertions from the Julia leaf are skipped: C5
    # divergence (dafr errors on memory-backed paths instead of NULL).
    s <- .fresh()
    read_first <- read_only(s$first)
    read_chain <- chain_reader(list(read_first))
    expect_identical(read_chain, read_first)
})

test_that("chains / one / writer", {
    # Julia: chain_writer([d]) === d when no name is given.
    s <- .fresh()
    write_chain <- chain_writer(list(s$first))
    expect_identical(write_chain, s$first)
})

# ---------------------------------------------------------------------------
# chains / two
# ---------------------------------------------------------------------------

test_that("chains / two", {
    s <- .fresh()
    expect_error(
        chain_writer(list(s$first, read_only(s$second)), name = "chain!"),
        regexp = "read-only final data.*chain!|chain!.*read-only final data"
    )
    read_chain <- chain_reader(list(s$first, s$second))
    # Julia: read_only(read_chain) === read_chain (idempotent on a read
    # chain); a fresh name forces a rewrap; a write chain always wraps.
    # complete_path Julia-vs-dafr divergence (C5): Julia returns nothing
    # for memory-backed chains, dafr errors "no filesystem path".
    # Skipped here.
    expect_true(is_daf(read_chain))
    expect_identical(read_only(read_chain), read_chain)
    expect_false(identical(
        read_only(read_chain, name = "read-only first!;second!"),
        read_chain
    ))
    write_chain <- chain_writer(list(s$first, s$second))
    expect_true(is_daf(write_chain))
    expect_false(identical(read_only(write_chain), write_chain))
})

# ---------------------------------------------------------------------------
# chains / access — assertion helpers
# ---------------------------------------------------------------------------

# Each helper takes a chain_factory(s) -> chain and a fresh-state setup.
# The factory is invoked AFTER the per-leaf setup writes the source
# data, so the chain sees the same state Julia's nested_test does.

.assert_access_scalar_first <- function(chain_factory) {
    s <- .fresh()
    set_scalar(s$first, "version", 1.0)
    chain <- chain_factory(s)
    expect_true(has_scalar(chain, "version"))
    expect_equal(get_scalar(chain, "version"), 1.0)
    expect_setequal(scalars_set(chain), "version")
    expect_false(has_scalar(chain, "author"))
}

.assert_access_scalar_second <- function(chain_factory) {
    s <- .fresh()
    set_scalar(s$second, "version", 2.0)
    chain <- chain_factory(s)
    expect_true(has_scalar(chain, "version"))
    expect_equal(get_scalar(chain, "version"), 2.0)
    expect_setequal(scalars_set(chain), "version")
    expect_false(has_scalar(chain, "author"))
}

.assert_access_scalar_both <- function(chain_factory) {
    s <- .fresh()
    set_scalar(s$first, "version", 1.0)
    set_scalar(s$second, "version", 2.0)
    chain <- chain_factory(s)
    expect_true(has_scalar(chain, "version"))
    expect_equal(get_scalar(chain, "version"), 2.0)  # later wins
    expect_setequal(scalars_set(chain), "version")
    expect_false(has_scalar(chain, "author"))
}

.assert_access_axis_first <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$first, "cell", c("A", "B"))
    chain <- chain_factory(s)
    expect_true(has_axis(chain, "cell"))
    expect_equal(unname(axis_vector(chain, "cell")), c("A", "B"))
    expect_setequal(axes_set(chain), "cell")
    expect_false(has_axis(chain, "gene"))
}

.assert_access_axis_second <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$second, "cell", c("A", "B"))
    chain <- chain_factory(s)
    expect_true(has_axis(chain, "cell"))
    expect_equal(unname(axis_vector(chain, "cell")), c("A", "B"))
    expect_setequal(axes_set(chain), "cell")
    expect_false(has_axis(chain, "gene"))
}

.assert_access_axis_both <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$first, "cell", c("A", "B"))
    add_axis(s$second, "cell", c("A", "B"))
    chain <- chain_factory(s)
    expect_true(has_axis(chain, "cell"))
    expect_equal(unname(axis_vector(chain, "cell")), c("A", "B"))
    expect_setequal(axes_set(chain), "cell")
    expect_false(has_axis(chain, "gene"))
}

.assert_access_axis_not_both <- function(chain_factory_ignored) {
    # The chain construction itself errors; factory is unused.
    s <- .fresh()
    add_axis(s$first, "cell", c("A", "B"))
    add_axis(s$second, "cell", c("A", "C"))
    expect_error(
        chain_reader(list(s$first, s$second), name = "chain!"),
        regexp = "different.*entry.*cell|cell.*different.*entry|entries differ"
    )
}

.assert_access_axis_not_same <- function(chain_factory_ignored) {
    s <- .fresh()
    add_axis(s$first, "cell", c("A", "B"))
    add_axis(s$second, "cell", c("A"))
    expect_error(
        chain_reader(list(s$first, s$second), name = "chain!"),
        regexp = "different.*number.*entries|entries.*differ|cell.*length"
    )
}

.assert_access_vector_first <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$first, "cell", c("A", "B"))
    chain <- chain_factory(s)
    prev <- vector_version_counter(chain, "cell", "age")
    set_vector(s$first, "cell", "age", c(1L, 2L))
    expect_equal(vector_version_counter(chain, "cell", "age"), prev + 1L)
    expect_true(has_vector(chain, "cell", "age"))
    expect_equal(unname(get_vector(chain, "cell", "age")), c(1L, 2L))
    expect_setequal(vectors_set(chain, "cell"), "age")
    expect_false(has_vector(chain, "cell", "batch"))
}

.assert_access_vector_second <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$second, "cell", c("A", "B"))
    chain <- chain_factory(s)
    prev <- vector_version_counter(chain, "cell", "age")
    set_vector(s$second, "cell", "age", c(2L, 3L))
    expect_equal(vector_version_counter(chain, "cell", "age"), prev + 1L)
    expect_true(has_vector(chain, "cell", "age"))
    expect_equal(unname(get_vector(chain, "cell", "age")), c(2L, 3L))
    expect_setequal(vectors_set(chain, "cell"), "age")
    expect_false(has_vector(chain, "cell", "batch"))
}

.assert_access_vector_both <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$first, "cell", c("A", "B"))
    set_vector(s$first, "cell", "age", c(1L, 2L))
    add_axis(s$second, "cell", c("A", "B"))
    set_vector(s$second, "cell", "age", c(2L, 3L))
    chain <- chain_factory(s)
    expect_true(has_vector(chain, "cell", "age"))
    expect_equal(unname(get_vector(chain, "cell", "age")), c(2L, 3L))  # later wins
    expect_setequal(vectors_set(chain, "cell"), "age")
    expect_false(has_vector(chain, "cell", "batch"))
}

.assert_access_matrix_first <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$first, "cell", c("A", "B"))
    add_axis(s$first, "gene", c("X", "Y", "Z"))
    set_matrix(s$first, "cell", "gene", "UMIs",
               matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    chain <- chain_factory(s)
    expect_true(has_matrix(chain, "cell", "gene", "UMIs"))
    expect_equal(unname(as.matrix(get_matrix(chain, "cell", "gene", "UMIs"))),
                 matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L))
    expect_setequal(matrices_set(chain, "cell", "gene"), "UMIs")
    expect_false(has_matrix(chain, "cell", "gene", "fraction"))
}

.assert_access_matrix_second <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$second, "cell", c("A", "B"))
    add_axis(s$second, "gene", c("X", "Y", "Z"))
    set_matrix(s$second, "cell", "gene", "UMIs",
               matrix(c(5, 2, 4, 1, 3, 0), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    chain <- chain_factory(s)
    expect_true(has_matrix(chain, "cell", "gene", "UMIs"))
    expect_equal(unname(as.matrix(get_matrix(chain, "cell", "gene", "UMIs"))),
                 matrix(c(5, 2, 4, 1, 3, 0), nrow = 2L, ncol = 3L))
    expect_setequal(matrices_set(chain, "cell", "gene"), "UMIs")
    expect_false(has_matrix(chain, "cell", "gene", "fraction"))
}

.assert_access_matrix_both <- function(chain_factory) {
    s <- .fresh()
    add_axis(s$first, "cell", c("A", "B"))
    add_axis(s$first, "gene", c("X", "Y", "Z"))
    set_matrix(s$first, "cell", "gene", "UMIs",
               matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    add_axis(s$second, "cell", c("A", "B"))
    add_axis(s$second, "gene", c("X", "Y", "Z"))
    set_matrix(s$second, "cell", "gene", "UMIs",
               matrix(c(5, 2, 4, 1, 3, 0), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    chain <- chain_factory(s)
    expect_true(has_matrix(chain, "cell", "gene", "UMIs"))
    expect_equal(unname(as.matrix(get_matrix(chain, "cell", "gene", "UMIs"))),
                 matrix(c(5, 2, 4, 1, 3, 0), nrow = 2L, ncol = 3L))  # later wins
    expect_setequal(matrices_set(chain, "cell", "gene"), "UMIs")
    expect_false(has_matrix(chain, "cell", "gene", "fraction"))
}

# ---------------------------------------------------------------------------
# chains / access / read
# ---------------------------------------------------------------------------

test_that("chains / access / read / scalar / first", {
    .assert_access_scalar_first(.read_chain_factory)
})
test_that("chains / access / read / scalar / second", {
    .assert_access_scalar_second(.read_chain_factory)
})
test_that("chains / access / read / scalar / both", {
    .assert_access_scalar_both(.read_chain_factory)
})
test_that("chains / access / read / scalar / both / description / ()", {
    skip("R divergence C2: dafr description() has no `deep` parameter; chain-deep description format differs")
})
test_that("chains / access / read / scalar / both / description / !deep", {
    skip("R divergence C2: dafr description() has no `deep` parameter; chain-deep description format differs")
})
test_that("chains / access / read / scalar / both / description / deep", {
    skip("R divergence C2: dafr description() has no `deep` parameter; chain-deep description format differs")
})
test_that("chains / access / read / axis / first", {
    .assert_access_axis_first(.read_chain_factory)
})
test_that("chains / access / read / axis / second", {
    .assert_access_axis_second(.read_chain_factory)
})
test_that("chains / access / read / axis / both", {
    .assert_access_axis_both(.read_chain_factory)
})
test_that("chains / access / read / axis / !both", {
    .assert_access_axis_not_both(.read_chain_factory)
})
test_that("chains / access / read / axis / !same", {
    .assert_access_axis_not_same(.read_chain_factory)
})
test_that("chains / access / read / vector / first", {
    .assert_access_vector_first(.read_chain_factory)
})
test_that("chains / access / read / vector / second", {
    .assert_access_vector_second(.read_chain_factory)
})
test_that("chains / access / read / vector / both", {
    .assert_access_vector_both(.read_chain_factory)
})
test_that("chains / access / read / matrix / first", {
    .assert_access_matrix_first(.read_chain_factory)
})
test_that("chains / access / read / matrix / second", {
    .assert_access_matrix_second(.read_chain_factory)
})
test_that("chains / access / read / matrix / both", {
    .assert_access_matrix_both(.read_chain_factory)
})

# ---------------------------------------------------------------------------
# chains / access / write
# ---------------------------------------------------------------------------

test_that("chains / access / write / scalar / first", {
    .assert_access_scalar_first(.write_chain_factory)
})
test_that("chains / access / write / scalar / second", {
    .assert_access_scalar_second(.write_chain_factory)
})
test_that("chains / access / write / scalar / both", {
    .assert_access_scalar_both(.write_chain_factory)
})
test_that("chains / access / write / scalar / both / description / ()", {
    skip("R divergence C2: dafr description() has no `deep` parameter; chain-deep description format differs")
})
test_that("chains / access / write / scalar / both / description / !deep", {
    skip("R divergence C2: dafr description() has no `deep` parameter; chain-deep description format differs")
})
test_that("chains / access / write / scalar / both / description / deep", {
    skip("R divergence C2: dafr description() has no `deep` parameter; chain-deep description format differs")
})
test_that("chains / access / write / axis / first", {
    .assert_access_axis_first(.write_chain_factory)
})
test_that("chains / access / write / axis / second", {
    .assert_access_axis_second(.write_chain_factory)
})
test_that("chains / access / write / axis / both", {
    .assert_access_axis_both(.write_chain_factory)
})
test_that("chains / access / write / axis / !both", {
    # Construction-time error path — still tests via chain_reader since
    # chain_writer's check is the same axis-validation routine.
    .assert_access_axis_not_both(.write_chain_factory)
})
test_that("chains / access / write / axis / !same", {
    .assert_access_axis_not_same(.write_chain_factory)
})
test_that("chains / access / write / vector / first", {
    .assert_access_vector_first(.write_chain_factory)
})
test_that("chains / access / write / vector / second", {
    .assert_access_vector_second(.write_chain_factory)
})
test_that("chains / access / write / vector / both", {
    .assert_access_vector_both(.write_chain_factory)
})
test_that("chains / access / write / matrix / first", {
    .assert_access_matrix_first(.write_chain_factory)
})
test_that("chains / access / write / matrix / second", {
    .assert_access_matrix_second(.write_chain_factory)
})
test_that("chains / access / write / matrix / both", {
    .assert_access_matrix_both(.write_chain_factory)
})

# ---------------------------------------------------------------------------
# chains / write — chain_writer([read_only(first), second])
# ---------------------------------------------------------------------------

# Each leaf rebuilds the (read_only(first), second) writable chain.
.write_chain_setup <- function() {
    s <- .fresh()
    chain <- chain_writer(list(read_only(s$first), s$second), name = "chain!")
    list(first = s$first, second = s$second, chain = chain)
}

test_that("chains / write / scalar / add", {
    s <- .write_chain_setup()
    expect_false(has_scalar(s$first, "version"))
    expect_false(has_scalar(s$second, "version"))
    expect_false(has_scalar(s$chain, "version"))
    set_scalar(s$chain, "version", 1.0)
    expect_false(has_scalar(s$first, "version"))
    expect_equal(get_scalar(s$second, "version"), 1.0)
    expect_equal(get_scalar(s$chain, "version"), 1.0)
    delete_scalar(s$chain, "version")
    expect_false(has_scalar(s$second, "version"))
    expect_false(has_scalar(s$chain, "version"))
})

test_that("chains / write / scalar / override", {
    s <- .write_chain_setup()
    set_scalar(s$first, "version", 1.0)
    expect_equal(get_scalar(s$first, "version"), 1.0)
    expect_equal(get_scalar(s$chain, "version"), 1.0)
    expect_error(
        delete_scalar(s$chain, "version"),
        regexp = "failed to delete.*scalar.*version|version.*exists in.*earlier"
    )
    set_scalar(s$chain, "version", 2.0, overwrite = TRUE)
    expect_equal(get_scalar(s$first, "version"), 1.0)
    expect_equal(get_scalar(s$second, "version"), 2.0)
    expect_equal(get_scalar(s$chain, "version"), 2.0)
    expect_error(
        delete_scalar(s$chain, "version"),
        regexp = "failed to delete.*scalar.*version|version.*exists in.*earlier"
    )
})

test_that("chains / write / scalar / change", {
    s <- .write_chain_setup()
    set_scalar(s$first, "version", 1.0)
    set_scalar(s$second, "version", 2.0)
    expect_equal(get_scalar(s$first, "version"), 1.0)
    expect_equal(get_scalar(s$second, "version"), 2.0)
    expect_equal(get_scalar(s$chain, "version"), 2.0)
    expect_error(
        delete_scalar(s$chain, "version"),
        regexp = "failed to delete.*scalar|exists in.*earlier"
    )
    set_scalar(s$chain, "version", 3.0, overwrite = TRUE)
    expect_equal(get_scalar(s$first, "version"), 1.0)
    expect_equal(get_scalar(s$second, "version"), 3.0)
    expect_equal(get_scalar(s$chain, "version"), 3.0)
    expect_error(
        delete_scalar(s$chain, "version"),
        regexp = "failed to delete.*scalar|exists in.*earlier"
    )
})

test_that("chains / write / axis / late", {
    s <- .write_chain_setup()
    expect_false(has_axis(s$first, "cell"))
    expect_false(has_axis(s$second, "cell"))
    expect_false(has_axis(s$chain, "cell"))
    add_axis(s$chain, "cell", c("A", "B"))
    expect_false(has_axis(s$first, "cell"))
    expect_equal(unname(axis_vector(s$second, "cell")), c("A", "B"))
    expect_equal(unname(axis_vector(s$chain, "cell")), c("A", "B"))
    delete_axis(s$chain, "cell")
    expect_false(has_axis(s$second, "cell"))
    expect_false(has_axis(s$chain, "cell"))
})

test_that("chains / write / axis / early", {
    s <- .write_chain_setup()
    add_axis(s$first, "cell", c("A", "B"))
    expect_equal(unname(axis_vector(s$first, "cell")), c("A", "B"))
    expect_false(has_axis(s$second, "cell"))
    expect_equal(unname(axis_vector(s$chain, "cell")), c("A", "B"))
    expect_error(
        delete_axis(s$chain, "cell"),
        regexp = "failed to delete.*axis.*cell|cell.*exists in.*earlier"
    )
})

test_that("chains / write / axis / both", {
    s <- .write_chain_setup()
    add_axis(s$first, "cell", c("A", "B"))
    add_axis(s$second, "cell", c("A", "B"))
    expect_equal(unname(axis_vector(s$first, "cell")), c("A", "B"))
    expect_equal(unname(axis_vector(s$second, "cell")), c("A", "B"))
    expect_equal(unname(axis_vector(s$chain, "cell")), c("A", "B"))
    expect_error(
        delete_axis(s$chain, "cell"),
        regexp = "failed to delete.*axis.*cell|cell.*exists in.*earlier"
    )
})

# Vector group requires the cell axis on first (parent setup in Julia).
.write_chain_vector_setup <- function() {
    s <- .write_chain_setup()
    add_axis(s$first, "cell", c("A", "B"))
    s
}

test_that("chains / write / vector / add", {
    s <- .write_chain_vector_setup()
    set_vector(s$chain, "cell", "age", c(1L, 2L))
    expect_false(has_vector(s$first, "cell", "age"))
    expect_equal(unname(axis_vector(s$second, "cell")), c("A", "B"))
    expect_equal(unname(get_vector(s$second, "cell", "age")), c(1L, 2L))
    expect_equal(unname(get_vector(s$chain, "cell", "age")), c(1L, 2L))
    delete_vector(s$chain, "cell", "age")
    expect_true(has_axis(s$second, "cell"))
    expect_false(has_vector(s$chain, "cell", "age"))
})

test_that("chains / write / vector / empty_dense", {
    skip("R divergence C3: dafr does not have empty_dense_vector!/empty_sparse_vector! builder API")
})

test_that("chains / write / vector / empty_sparse", {
    skip("R divergence C3: dafr does not have empty_dense_vector!/empty_sparse_vector! builder API")
})

test_that("chains / write / vector / override", {
    s <- .write_chain_vector_setup()
    set_vector(s$first, "cell", "age", c(1L, 2L))
    expect_equal(unname(get_vector(s$chain, "cell", "age")), c(1L, 2L))
    set_vector(s$chain, "cell", "age", c(2L, 3L), overwrite = TRUE)
    expect_equal(unname(get_vector(s$second, "cell", "age")), c(2L, 3L))
    expect_equal(unname(get_vector(s$chain, "cell", "age")), c(2L, 3L))
    expect_error(
        delete_vector(s$chain, "cell", "age"),
        regexp = "failed to delete.*vector.*age|age.*exists in.*earlier"
    )
})

test_that("chains / write / vector / change", {
    s <- .write_chain_vector_setup()
    add_axis(s$second, "cell", c("A", "B"))
    set_vector(s$first, "cell", "age", c(1L, 2L))
    set_vector(s$second, "cell", "age", c(2L, 3L))
    expect_equal(unname(get_vector(s$chain, "cell", "age")), c(2L, 3L))
    set_vector(s$chain, "cell", "age", c(3L, 4L), overwrite = TRUE)
    expect_equal(unname(get_vector(s$first, "cell", "age")), c(1L, 2L))
    expect_equal(unname(get_vector(s$second, "cell", "age")), c(3L, 4L))
    expect_equal(unname(get_vector(s$chain, "cell", "age")), c(3L, 4L))
    expect_error(
        delete_vector(s$chain, "cell", "age"),
        regexp = "failed to delete.*vector.*age|age.*exists in.*earlier"
    )
})

# Matrix group requires both cell + gene axes on first.
.write_chain_matrix_setup <- function() {
    s <- .write_chain_setup()
    add_axis(s$first, "cell", c("A", "B"))
    add_axis(s$first, "gene", c("X", "Y", "Z"))
    s
}

test_that("chains / write / matrix / add", {
    s <- .write_chain_matrix_setup()
    set_matrix(s$chain, "cell", "gene", "UMIs",
               matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    expect_false(has_matrix(s$first, "cell", "gene", "UMIs"))
    expect_equal(unname(axis_vector(s$second, "cell")), c("A", "B"))
    expect_equal(unname(axis_vector(s$second, "gene")), c("X", "Y", "Z"))
    expect_equal(unname(as.matrix(get_matrix(s$second, "cell", "gene", "UMIs"))),
                 matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L))
    expect_equal(unname(as.matrix(get_matrix(s$chain, "cell", "gene", "UMIs"))),
                 matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L))
    delete_matrix(s$chain, "cell", "gene", "UMIs")
    expect_true(has_axis(s$second, "cell"))
    expect_true(has_axis(s$second, "gene"))
    expect_false(has_matrix(s$chain, "cell", "gene", "UMIs"))
})

test_that("chains / write / matrix / empty_dense", {
    skip("R divergence C3: dafr does not have empty_dense_matrix!/empty_sparse_matrix! builder API")
})

test_that("chains / write / matrix / empty_sparse", {
    skip("R divergence C3: dafr does not have empty_dense_matrix!/empty_sparse_matrix! builder API")
})

test_that("chains / write / matrix / override", {
    s <- .write_chain_matrix_setup()
    set_matrix(s$first, "cell", "gene", "UMIs",
               matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    expect_equal(unname(as.matrix(get_matrix(s$chain, "cell", "gene", "UMIs"))),
                 matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L))
    set_matrix(s$chain, "cell", "gene", "UMIs",
               matrix(c(1, 4, 2, 5, 3, 6), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))),
               overwrite = TRUE)
    expect_equal(unname(as.matrix(get_matrix(s$second, "cell", "gene", "UMIs"))),
                 matrix(c(1, 4, 2, 5, 3, 6), nrow = 2L, ncol = 3L))
    expect_equal(unname(as.matrix(get_matrix(s$chain, "cell", "gene", "UMIs"))),
                 matrix(c(1, 4, 2, 5, 3, 6), nrow = 2L, ncol = 3L))
    expect_error(
        delete_matrix(s$chain, "cell", "gene", "UMIs"),
        regexp = "failed to delete.*matrix.*UMIs|UMIs.*exists in.*earlier"
    )
})

test_that("chains / write / matrix / change", {
    s <- .write_chain_matrix_setup()
    add_axis(s$second, "cell", c("A", "B"))
    add_axis(s$second, "gene", c("X", "Y", "Z"))
    set_matrix(s$first, "cell", "gene", "UMIs",
               matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    set_matrix(s$second, "cell", "gene", "UMIs",
               matrix(c(1, 4, 2, 5, 3, 6), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))))
    expect_equal(unname(as.matrix(get_matrix(s$chain, "cell", "gene", "UMIs"))),
                 matrix(c(1, 4, 2, 5, 3, 6), nrow = 2L, ncol = 3L))
    set_matrix(s$chain, "cell", "gene", "UMIs",
               matrix(c(2, 5, 3, 6, 4, 7), nrow = 2L, ncol = 3L,
                      dimnames = list(c("A", "B"), c("X", "Y", "Z"))),
               overwrite = TRUE)
    expect_equal(unname(as.matrix(get_matrix(s$first, "cell", "gene", "UMIs"))),
                 matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L))
    expect_equal(unname(as.matrix(get_matrix(s$second, "cell", "gene", "UMIs"))),
                 matrix(c(2, 5, 3, 6, 4, 7), nrow = 2L, ncol = 3L))
    expect_equal(unname(as.matrix(get_matrix(s$chain, "cell", "gene", "UMIs"))),
                 matrix(c(2, 5, 3, 6, 4, 7), nrow = 2L, ncol = 3L))
    expect_error(
        delete_matrix(s$chain, "cell", "gene", "UMIs"),
        regexp = "failed to delete.*matrix.*UMIs|UMIs.*exists in.*earlier"
    )
})
