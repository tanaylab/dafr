## ---- B1: scaffold ----

test_that("adapter() requires a DafWriter and rejects no-op invocations", {
    expect_error(adapter("not a daf", function(d) d), "must be a DafWriter")
    expect_error(adapter(memory_daf(), function(d) d), "no-op adapter")
})

## ---- B2: copy_all — scalars ----

test_that("copy_all copies scalars from a view into dest", {
    src <- memory_daf(name = "src")
    set_scalar(src, "alpha", 1L)
    set_scalar(src, "beta", "b")
    v <- viewer(src)
    dest <- memory_daf(name = "dest")

    copy_all(destination = dest, source = v,
             empty = NULL, relayout = FALSE, overwrite = FALSE)
    expect_identical(get_scalar(dest, "alpha"), 1L)
    expect_identical(get_scalar(dest, "beta"), "b")
})

test_that("copy_all errors on pre-existing scalars unless overwrite", {
    src <- memory_daf(name = "s")
    set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "d")
    set_scalar(dest, "x", 2L)

    expect_error(
        copy_all(dest, viewer(src), relayout = FALSE, overwrite = FALSE),
        "existing scalar:"
    )
    copy_all(dest, viewer(src), relayout = FALSE, overwrite = TRUE)
    expect_identical(get_scalar(dest, "x"), 1L)
})

## ---- B3: copy_all — axes and vectors ----

test_that("copy_all copies axes + vectors (view axis names preserved)", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    set_vector(src, "cell", "donor", c("d1", "d2", "d3"))
    v <- viewer(src)
    dest <- memory_daf(name = "d")

    copy_all(dest, v, relayout = FALSE)
    expect_identical(axis_vector(dest, "cell"), c("c1", "c2", "c3"))
    expect_identical(unname(get_vector(dest, "cell", "donor")),
                     c("d1", "d2", "d3"))
})

test_that("copy_all copies renamed axes under their view name", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1", "c2"))
    set_vector(src, "cell", "donor", c("d1", "d2"))
    v <- viewer(src, axes = list(list("obs", "@ cell"), list("cell", NULL)))
    dest <- memory_daf(name = "d")

    copy_all(dest, v, relayout = FALSE)
    expect_true(has_axis(dest, "obs"))
    expect_false(has_axis(dest, "cell"))
    expect_identical(axis_vector(dest, "obs"), c("c1", "c2"))
    expect_identical(unname(get_vector(dest, "obs", "donor")),
                     c("d1", "d2"))
})

test_that("copy_all: pre-existing axis with disjoint entries errors on vector copy", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1", "c2"))
    set_vector(src, "cell", "donor", c("d1", "d2"))
    dest <- memory_daf(name = "d")
    add_axis(dest, "cell", c("x1", "x2"))   # different entries → disjoint

    expect_error(
        copy_all(dest, viewer(src), relayout = FALSE),
        "disjoint"
    )
})

## ---- B4: copy_all — matrices + relayout ----

test_that("copy_all copies matrices + relayouts when asked", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2", "g3"))
    m <- matrix(1:6, nrow = 2, dimnames = list(c("c1","c2"), c("g1","g2","g3")))
    set_matrix(src, "cell", "gene", "UMIs", m)
    v <- viewer(src)
    dest <- memory_daf(name = "d")

    copy_all(dest, v, relayout = TRUE)
    got <- get_matrix(dest, "cell", "gene", "UMIs")
    expect_equal(unname(got), unname(m))
    expect_true(format_has_matrix(dest, "gene", "cell", "UMIs"))
})

test_that("copy_all matrix without relayout skips transpose store", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1"))
    add_axis(src, "gene", c("g1", "g2"))
    set_matrix(src, "cell", "gene", "UMIs",
        matrix(c(5, 9), nrow = 1,
               dimnames = list("c1", c("g1","g2"))))
    v <- viewer(src)
    dest <- memory_daf(name = "d")

    copy_all(dest, v, relayout = FALSE)
    expect_true(format_has_matrix(dest, "cell", "gene", "UMIs"))
})

## ---- B5: copy_all — empty pad mode ----

test_that("copy_all honors `empty` for a subset-axis vector", {
    src <- memory_daf(name = "src-subset")
    add_axis(src, "cell", c("c1", "c3"))
    set_vector(src, "cell", "donor", c("d1", "d3"))

    dest <- memory_daf(name = "dest-full")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    copy_all(dest, viewer(src),
        empty = list("cell|donor" = "MISSING"),
        relayout = FALSE
    )
    expect_identical(
        unname(get_vector(dest, "cell", "donor")),
        c("d1", "MISSING", "d3")
    )
})

## ---- B6/B7: adapter() full flow ----

test_that("adapter() runs fn on an input view and copies output back", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))

    fn <- function(adapted) {
        entries <- axis_vector(adapted, "obs")
        set_vector(adapted, "obs", "squared_idx",
            as.integer(seq_along(entries)^2L)
        )
        "done"
    }

    result <- adapter(d, fn,
        input_axes = list(list("obs", "@ cell"), list("cell", NULL)),
        input_data = VIEW_ALL_VECTORS,
        output_axes = list(list("cell", "@ obs"), list("obs", NULL)),
        output_data = list(list(ALL_VECTORS, NULL), list(c("cell", "squared_idx"), "="))
    )
    expect_identical(result, "done")
    expect_identical(
        unname(get_vector(d, "cell", "squared_idx")),
        c(1L, 4L, 9L)
    )
})

test_that("adapter() returns fn's value untouched (passthrough)", {
    d <- memory_daf(name = "b")
    add_axis(d, "cell", c("c1"))
    fn <- function(adapted) list(value = 42L, note = "hi")
    res <- adapter(d, fn,
        input_axes = list(list("obs", "@ cell"), list("cell", NULL)),
        input_data = VIEW_ALL_DATA,
        output_axes = list(list("cell", "@ obs"), list("obs", NULL)),
        output_data = list()
    )
    expect_identical(res, list(value = 42L, note = "hi"))
})

## ---- B8: adapter() + computation() integration ----

test_that("adapter() + computation() enforces contract on the adapted chain", {
    withr::local_options(list(dafr.enforce_contracts = TRUE))

    c <- Contract(
        axes = list(obs = list(RequiredInput, "renamed cell axis")),
        data = list(
            contract_vector("obs", "donor",      RequiredInput, "character", "id"),
            contract_vector("obs", "squared_idx", CreatedOutput, "integer", "out")
        )
    )
    inner <- function(adapted) {
        entries <- get_vector(adapted, "obs", "donor")
        set_vector(adapted, "obs", "squared_idx",
                   as.integer(seq_along(entries)^2L))
        "ok"
    }
    comp <- computation("adapt-demo", c, inner)

    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("d1", "d2"))

    res <- adapter(d, comp,
        input_axes = list(list("obs", "@ cell"), list("cell", NULL)),
        input_data = VIEW_ALL_DATA,
        output_axes = list(list("cell", "@ obs"), list("obs", NULL)),
        output_data = list(list(ALL_VECTORS, NULL), list(c("cell", "squared_idx"), "="))
    )
    expect_identical(res, "ok")
    expect_identical(unname(get_vector(d, "cell", "squared_idx")), c(1L, 4L))
})

## ---- B9: smoke test on example_cells_daf ----

test_that("adapter() on example_cells_daf computes a per-cell total UMI count", {
    d <- example_cells_daf()
    fn <- function(adapted) {
        m <- get_matrix(adapted, "obs", "var", "UMIs")
        totals <- as.integer(rowSums(as.matrix(m)))
        set_vector(adapted, "obs", "total_umis", totals)
        totals
    }
    totals <- adapter(d, fn,
        input_axes = list(
            list("obs", "@ cell"), list("var", "@ gene"),
            list("cell", NULL), list("gene", NULL)
        ),
        input_data = VIEW_ALL_DATA,
        output_axes = list(
            list("cell", "@ obs"), list("gene", "@ var"),
            list("obs", NULL), list("var", NULL)
        ),
        output_data = list(
            list(ALL_VECTORS, NULL), list(ALL_MATRICES, NULL),
            list(c("cell", "total_umis"), "=")
        )
    )
    expect_identical(length(totals), 856L)
    expect_true(all(totals >= 0L))
    expect_identical(
        unname(get_vector(d, "cell", "total_umis")),
        totals
    )
})

test_that("adapter: sparse pad-mode copy-back preserves sparsity", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))

    result <- adapter(
        d,
        function(adapted) {
            add_axis(adapted, "cell_sub", c("c1", "c2"))
            mat <- Matrix::sparseMatrix(
                i = c(1L, 2L), j = c(1L, 2L), x = c(10, 20),
                dims = c(2L, 2L),
                dimnames = list(c("c1", "c2"), c("g1", "g2"))
            )
            set_matrix(adapted, "cell_sub", "gene", "UMIs", mat)
            "ok"
        },
        output_axes = list(
            list("cell",     "@ cell_sub"),
            list("cell_sub", NULL),
            list("gene",     "@ gene")
        ),
        output_data = list(list(ALL_MATRICES, "=")),
        empty = list("cell|gene|UMIs" = 0),
        relayout = FALSE
    )
    expect_identical(result, "ok")
    res <- format_get_matrix(d, "cell", "gene", "UMIs")$value
    expect_s4_class(res, "dgCMatrix")
})
