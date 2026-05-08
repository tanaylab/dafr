# Literal port of contracts.jl `access` subgroup (lines 936-1486) into R.
#
# Five sub-blocks:
#   - relaxed (~16 leaves) - is_relaxed=TRUE; all access ops pass
#   - empty   (~14 leaves) - empty contract; access errors with "non-contract"
#   - axes    (~5 leaves)  - axes-only contract; data access errors
#   - full    (~22 leaves) - full OptionalInput contract; modifies error
#                            with "modifying OptionalInput"
#   - fill    (~10 leaves) - OptionalOutput on fresh daf; modifies pass
#                            (empty_* builders are dafr-missing, see C3)
#
# Some leaves use `brief()` (Julia-only API not in dafr) - skipped with CX1.
# Some leaves use `empty_dense_*` / `empty_sparse_*` (dafr lacks the
# empty_*/builder API) - skipped with C3 (see chains slice).

# ---------------------------------------------------------------------------
# Shared setup
# ---------------------------------------------------------------------------

.access_fresh_daf <- function() {
    d <- memory_daf(name = "memory!")
    set_scalar(d, "version", 1L)
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    set_vector(d, "cell", "age", c(1.0, 2.0))
    set_matrix(d, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
        dimnames = list(c("A", "B"), c("X", "Y", "Z"))
    ))
    d
}

# ---------------------------------------------------------------------------
# access / relaxed - is_relaxed=TRUE allows all access
# ---------------------------------------------------------------------------

test_that("contracts / access / relaxed / axes_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_setequal(axes_set(cd), axes_set(d))
        # dafr query syntax uses '@ ?' to list axes (Julia uses '? axes').
        expect_setequal(get_query(cd, "@ ?"), axes_set(d))
    })
})

test_that("contracts / access / relaxed / axis_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_identical(unname(axis_vector(cd, "cell")), c("A", "B"))
        # dafr query syntax uses '@ cell' (Julia uses '/ cell').
        expect_identical(unname(get_query(cd, "@ cell")), c("A", "B"))
    })
})

test_that("contracts / access / relaxed / axis_dict", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        ad <- axis_dict(cd, "cell")
        expect_setequal(names(ad), c("A", "B"))
        expect_identical(as.integer(ad[["A"]]), 1L)
        expect_identical(as.integer(ad[["B"]]), 2L)
    })
})

test_that("contracts / access / relaxed / axis_indices", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_identical(unname(axis_indices(cd, "cell", c("A", "B"))),
                         c(1L, 2L))
    })
})

test_that("contracts / access / relaxed / axis_length", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_identical(axis_length(cd, "cell"), 2L)
    })
})

test_that("contracts / access / relaxed / axis_version_counter", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_silent(axis_version_counter(cd, "cell"))
    })
})

test_that("contracts / access / relaxed / brief", {
    skip("R divergence CX1: dafr does not export a brief() function. Julia uses brief() to format a one-line summary; dafr would need a brief.R helper. Out of scope for parity.")
})

test_that("contracts / access / relaxed / description", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        desc <- description(cd)
        # dafr's description currently elides Julia's "x Float64 (Dense)"
        # element-type/layout suffix on vectors and matrices. Match the
        # name-line shape but not the suffix.
        expect_match(desc, "name: memory!")
        expect_match(desc, "type: MemoryDaf")
        expect_match(desc, "version: 1")
        expect_match(desc, "cell: 2 entries")
        expect_match(desc, "gene: 3 entries")
        expect_match(desc, "age")
        expect_match(desc, "UMIs")
    })
})

test_that("contracts / access / relaxed / empty_cache", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_silent(empty_cache(cd))
    })
})

test_that("contracts / access / relaxed / get_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_identical(get_scalar(cd, "version"), 1L)
        # dafr query syntax uses '. version' (Julia uses ': version').
        expect_identical(get_query(cd, ". version"), 1L)
    })
})

test_that("contracts / access / relaxed / has_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_true(has_scalar(cd, "version"))
        expect_false(has_scalar(cd, "quality"))
    })
})

test_that("contracts / access / relaxed / scalars_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_setequal(scalars_set(cd), scalars_set(d))
    })
})

test_that("contracts / access / relaxed / has_axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_true(has_axis(cd, "cell"))
        expect_false(has_axis(cd, "type"))
    })
})

test_that("contracts / access / relaxed / has_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_true(has_vector(cd, "cell", "age"))
        expect_false(has_vector(cd, "cell", "batch"))
    })
})

test_that("contracts / access / relaxed / vectors_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_setequal(vectors_set(cd, "cell"), "age")
    })
})

test_that("contracts / access / relaxed / has_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_true(has_matrix(cd, "cell", "gene", "UMIs"))
        expect_false(has_matrix(cd, "cell", "gene", "fraction"))
    })
})

test_that("contracts / access / relaxed / matrices_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(is_relaxed = TRUE), d)
        expect_setequal(matrices_set(cd, "cell", "gene"), "UMIs")
    })
})

# ---------------------------------------------------------------------------
# access / empty - empty Contract; data access errors
# ---------------------------------------------------------------------------

test_that("contracts / access / empty / axes_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        # axes_set is an enumeration not an axis access; should be permitted.
        expect_setequal(axes_set(cd), axes_set(d))
    })
})

test_that("contracts / access / empty / axis_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_error(axis_vector(cd, "cell"),
            regexp = "accessing non-contract axis.*cell")
    })
})

test_that("contracts / access / empty / axis_indices", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_error(axis_indices(cd, "cell", c("A", "B")),
            regexp = "accessing non-contract axis.*cell")
    })
})

test_that("contracts / access / empty / axis_length", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_error(axis_length(cd, "cell"),
            regexp = "accessing non-contract axis.*cell")
    })
})

test_that("contracts / access / empty / brief", {
    skip("R divergence CX1: dafr does not export a brief() function.")
})

test_that("contracts / access / empty / description", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        desc <- description(cd)
        expect_match(desc, "name: memory!")
        expect_match(desc, "type: MemoryDaf")
    })
})

test_that("contracts / access / empty / empty_cache", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_silent(empty_cache(cd))
    })
})

test_that("contracts / access / empty / get_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_error(get_scalar(cd, "version"),
            regexp = "accessing non-contract scalar.*version")
    })
})

test_that("contracts / access / empty / has_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        # has_scalar is a query, not an access; should be silent
        expect_identical(has_scalar(cd, "version"), has_scalar(d, "version"))
    })
})

test_that("contracts / access / empty / scalars_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_setequal(scalars_set(cd), scalars_set(d))
    })
})

test_that("contracts / access / empty / has_axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_identical(has_axis(cd, "cell"), has_axis(d, "cell"))
    })
})

test_that("contracts / access / empty / vectors_set / cell", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_setequal(vectors_set(cd, "cell"), vectors_set(d, "cell"))
    })
})

test_that("contracts / access / empty / vectors_set / gene", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_setequal(vectors_set(cd, "gene"), vectors_set(d, "gene"))
    })
})

test_that("contracts / access / empty / matrices_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", Contract(), d)
        expect_setequal(matrices_set(cd, "cell", "gene"),
                        matrices_set(d, "cell", "gene"))
    })
})

# ---------------------------------------------------------------------------
# access / axes - contract with axes only, no data; data access errors
# ---------------------------------------------------------------------------

test_that("contracts / access / axes / vectors_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        contract <- Contract(axes = list(
            cell = list(OptionalInput, "description"),
            gene = list(OptionalInput, "description")
        ))
        cd <- contractor("computation", contract, d)
        expect_setequal(vectors_set(cd, "cell"), vectors_set(d, "cell"))
    })
})

test_that("contracts / access / axes / get_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        contract <- Contract(axes = list(
            cell = list(OptionalInput, "description"),
            gene = list(OptionalInput, "description")
        ))
        cd <- contractor("computation", contract, d)
        expect_error(get_vector(cd, "cell", "age"),
            regexp = "accessing non-contract vector.*age.*cell")
    })
})

test_that("contracts / access / axes / has_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        contract <- Contract(axes = list(
            cell = list(OptionalInput, "description"),
            gene = list(OptionalInput, "description")
        ))
        cd <- contractor("computation", contract, d)
        expect_identical(has_vector(cd, "cell", "age"),
                         has_vector(d, "cell", "age"))
    })
})

test_that("contracts / access / axes / matrices_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        contract <- Contract(axes = list(
            cell = list(OptionalInput, "description"),
            gene = list(OptionalInput, "description")
        ))
        cd <- contractor("computation", contract, d)
        expect_setequal(matrices_set(cd, "cell", "gene"),
                        matrices_set(d, "cell", "gene"))
    })
})

test_that("contracts / access / axes / get_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        contract <- Contract(axes = list(
            cell = list(OptionalInput, "description"),
            gene = list(OptionalInput, "description")
        ))
        cd <- contractor("computation", contract, d)
        expect_error(get_matrix(cd, "cell", "gene", "UMIs"),
            regexp = "accessing non-contract matrix.*UMIs")
    })
})

test_that("contracts / access / axes / has_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        contract <- Contract(axes = list(
            cell = list(OptionalInput, "description"),
            gene = list(OptionalInput, "description")
        ))
        cd <- contractor("computation", contract, d)
        expect_identical(has_matrix(cd, "cell", "gene", "UMIs"),
                         has_matrix(d, "cell", "gene", "UMIs"))
    })
})

# ---------------------------------------------------------------------------
# access / full - full OptionalInput contract; reads pass, modifies error
# ---------------------------------------------------------------------------

.full_contract <- function() {
    Contract(
        axes = list(
            cell = list(OptionalInput, "description"),
            gene = list(OptionalInput, "description")
        ),
        data = list(
            contract_scalar("version", OptionalInput, "integer", "description"),
            contract_vector("cell", "age", OptionalInput, "numeric", "description"),
            contract_matrix("cell", "gene", "UMIs", OptionalInput, "numeric", "description")
        )
    )
}

test_that("contracts / access / full / axes_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_setequal(axes_set(cd), axes_set(d))
    })
})

test_that("contracts / access / full / axis_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_setequal(unname(axis_vector(cd, "cell")),
                        unname(axis_vector(d, "cell")))
    })
})

test_that("contracts / access / full / axis_dict", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(names(axis_dict(cd, "cell")),
                         names(axis_dict(d, "cell")))
    })
})

test_that("contracts / access / full / axis_indices", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(unname(axis_indices(cd, "cell", c("A", "B"))),
                         unname(axis_indices(d, "cell", c("A", "B"))))
    })
})

test_that("contracts / access / full / axis_length", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(axis_length(cd, "cell"), axis_length(d, "cell"))
    })
})

test_that("contracts / access / full / axis_version_counter", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(axis_version_counter(cd, "cell"),
                         axis_version_counter(d, "cell"))
    })
})

test_that("contracts / access / full / empty_cache", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_silent(empty_cache(cd))
    })
})

test_that("contracts / access / full / get_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(get_scalar(cd, "version"), get_scalar(d, "version"))
    })
})

test_that("contracts / access / full / has_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(has_scalar(cd, "version"), has_scalar(d, "version"))
    })
})

test_that("contracts / access / full / scalars_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_setequal(scalars_set(cd), scalars_set(d))
    })
})

test_that("contracts / access / full / has_axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(has_axis(cd, "cell"), has_axis(d, "cell"))
    })
})

test_that("contracts / access / full / vectors_set", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_setequal(vectors_set(cd, "cell"), vectors_set(d, "cell"))
    })
})

test_that("contracts / access / full / get_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(unname(get_vector(cd, "cell", "age")),
                         unname(get_vector(d, "cell", "age")))
    })
})

test_that("contracts / access / full / has_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(has_vector(cd, "cell", "age"),
                         has_vector(d, "cell", "age"))
    })
})

test_that("contracts / access / full / vector_version_counter", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(vector_version_counter(cd, "cell", "age"),
                         vector_version_counter(d, "cell", "age"))
    })
})

test_that("contracts / access / full / get_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(unname(get_matrix(cd, "cell", "gene", "UMIs")),
                         unname(get_matrix(d, "cell", "gene", "UMIs")))
    })
})

test_that("contracts / access / full / has_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(has_matrix(cd, "cell", "gene", "UMIs"),
                         has_matrix(d, "cell", "gene", "UMIs"))
    })
})

test_that("contracts / access / full / matrix_version_counter", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_identical(matrix_version_counter(cd, "cell", "gene", "UMIs"),
                         matrix_version_counter(d, "cell", "gene", "UMIs"))
    })
})

# Modify ops on full/OptionalInput contract should error with
# "modifying OptionalInput".

test_that("contracts / access / full / add_axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_error(add_axis(cd, "cell", c("C", "D")),
            regexp = "modifying OptionalInput axis.*cell")
    })
})

test_that("contracts / access / full / delete_axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_error(delete_axis(cd, "cell"),
            regexp = "modifying OptionalInput axis.*cell")
    })
})

test_that("contracts / access / full / delete_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_error(delete_matrix(cd, "cell", "gene", "UMIs"),
            regexp = "modifying OptionalInput.*UMIs")
    })
})

test_that("contracts / access / full / delete_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_error(delete_vector(cd, "cell", "age"),
            regexp = "modifying OptionalInput vector.*age")
    })
})

test_that("contracts / access / full / delete_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_error(delete_scalar(cd, "version"),
            regexp = "modifying OptionalInput scalar.*version")
    })
})

test_that("contracts / access / full / relayout_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_error(relayout_matrix(cd, "cell", "gene", "UMIs", overwrite = TRUE),
            regexp = "modifying OptionalInput.*UMIs")
    })
})

test_that("contracts / access / full / set_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_error(set_scalar(cd, "version", 2L, overwrite = TRUE),
            regexp = "modifying OptionalInput scalar.*version")
    })
})

test_that("contracts / access / full / set_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        m <- matrix(c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
                    dimnames = list(c("A", "B"), c("X", "Y", "Z")))
        expect_error(set_matrix(cd, "cell", "gene", "UMIs", m, overwrite = TRUE),
            regexp = "modifying OptionalInput.*UMIs")
    })
})

test_that("contracts / access / full / set_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- .access_fresh_daf()
        cd <- contractor("computation", .full_contract(), d)
        expect_error(set_vector(cd, "cell", "age", c(1.0, 2.0), overwrite = TRUE),
            regexp = "modifying OptionalInput vector.*age")
    })
})

# ---------------------------------------------------------------------------
# access / fill - OptionalOutput on fresh daf; modifies pass
# ---------------------------------------------------------------------------

.fill_contract <- function() {
    Contract(
        axes = list(
            cell = list(OptionalOutput, "description"),
            gene = list(OptionalOutput, "description")
        ),
        data = list(
            contract_scalar("version", OptionalOutput, "integer", "description"),
            contract_vector("cell", "age", OptionalOutput, "numeric", "description"),
            contract_matrix("cell", "gene", "UMIs", OptionalOutput, "numeric", "description")
        )
    )
}

.fill_setup <- function() {
    cd <- contractor("computation", .fill_contract(), memory_daf(name = "memory!"))
    set_scalar(cd, "version", 1L)
    add_axis(cd, "cell", c("A", "B"))
    add_axis(cd, "gene", c("X", "Y", "Z"))
    set_vector(cd, "cell", "age", c(1.0, 2.0))
    set_matrix(cd, "cell", "gene", "UMIs", matrix(
        c(0, 3, 1, 4, 2, 5), nrow = 2L, ncol = 3L,
        dimnames = list(c("A", "B"), c("X", "Y", "Z"))
    ))
    cd
}

test_that("contracts / access / fill / delete_axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        cd <- .fill_setup()
        # Need to delete dependent vector/matrix first per axis-deletion
        # constraint in dafr.
        delete_vector(cd, "cell", "age")
        delete_matrix(cd, "cell", "gene", "UMIs")
        expect_silent(delete_axis(cd, "cell"))
        expect_false(has_axis(cd, "cell"))
    })
})

test_that("contracts / access / fill / delete_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        cd <- .fill_setup()
        expect_silent(delete_matrix(cd, "cell", "gene", "UMIs"))
        expect_false(has_matrix(cd, "cell", "gene", "UMIs"))
    })
})

test_that("contracts / access / fill / delete_vector", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        cd <- .fill_setup()
        expect_silent(delete_vector(cd, "cell", "age"))
        expect_false(has_vector(cd, "cell", "age"))
    })
})

test_that("contracts / access / fill / delete_scalar", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        cd <- .fill_setup()
        expect_silent(delete_scalar(cd, "version"))
        expect_false(has_scalar(cd, "version"))
    })
})

test_that("contracts / access / fill / empty_dense_matrix", {
    skip("R divergence C3: dafr lacks the empty_*/builder API (Julia's empty_dense_matrix!, empty_sparse_*, etc.). Filed in chains slice as C3.")
})

test_that("contracts / access / fill / empty_dense_vector", {
    skip("R divergence C3: dafr lacks the empty_*/builder API.")
})

test_that("contracts / access / fill / empty_sparse_matrix", {
    skip("R divergence C3: dafr lacks the empty_*/builder API.")
})

test_that("contracts / access / fill / empty_sparse_vector", {
    skip("R divergence C3: dafr lacks the empty_*/builder API.")
})

test_that("contracts / access / fill / relayout_matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        cd <- .fill_setup()
        expect_silent(relayout_matrix(cd, "cell", "gene", "UMIs", overwrite = TRUE))
    })
})
