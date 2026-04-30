test_that("copy_all copies everything into an empty destination", {
    src <- memory_daf(name = "src")
    set_scalar(src, "organism", "human")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1"))
    set_vector(src, "cell", "age", c(1L, 2L))
    set_matrix(src, "cell", "gene", "UMIs",
               matrix(c(10, 20), 2, 1, dimnames = list(c("c1","c2"), "g1")))

    dest <- memory_daf(name = "dest")
    copy_all(dest, src, relayout = FALSE)

    expect_identical(get_scalar(dest, "organism"), "human")
    expect_setequal(axes_set(dest), c("cell", "gene"))
    expect_identical(unname(get_vector(dest, "cell", "age")), c(1L, 2L))
    expect_equal(as.numeric(get_matrix(dest, "cell", "gene", "UMIs")),
                 c(10, 20))
})

test_that("copy_all: overwrite=FALSE insist=TRUE errors on conflict", {
    src <- memory_daf(name = "src")
    set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "dest")
    set_scalar(dest, "x", 2L)
    expect_error(copy_all(dest, src, relayout = FALSE), "existing scalar:")
})

test_that("copy_all: insist=FALSE skips existing destination entries", {
    src <- memory_daf(name = "src"); set_scalar(src, "x", 1L)
    add_axis(src, "cell", c("c1")); set_vector(src, "cell", "age", c(10L))
    dest <- memory_daf(name = "dest"); set_scalar(dest, "x", 2L)
    add_axis(dest, "cell", c("c1")); set_vector(dest, "cell", "age", c(999L))

    copy_all(dest, src, insist = FALSE, relayout = FALSE)
    expect_identical(get_scalar(dest, "x"), 2L)
    expect_identical(unname(get_vector(dest, "cell", "age")), 999L)
})

test_that("copy_all: overwrite=TRUE replaces scalar/vector/matrix", {
    src <- memory_daf(name = "src")
    set_scalar(src, "x", 1L)
    add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(10L))

    dest <- memory_daf(name = "dest")
    set_scalar(dest, "x", 2L)
    add_axis(dest, "cell", c("c1"))
    set_vector(dest, "cell", "age", c(999L))

    copy_all(dest, src, overwrite = TRUE, relayout = FALSE)
    expect_identical(get_scalar(dest, "x"), 1L)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(10L))
})

test_that("copy_all: pad-mode uses empty map for missing destination entries", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(10L))

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))

    copy_all(dest, src,
             empty = list("cell|age" = -1L),
             relayout = FALSE)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(10L, -1L))
})

test_that("empty_data() builds a flat-key list", {
    e <- empty_data(
        vectors  = list(list(axis = "cell", name = "age", value = 0L)),
        matrices = list(list(rows_axis = "cell", columns_axis = "gene",
                             name = "UMIs", value = 0)),
        tensors  = list(list(main_axis = "batch", rows_axis = "gene",
                             columns_axis = "cell", name = "counts",
                             value = 0))
    )
    expect_identical(e$`cell|age`, 0L)
    expect_identical(e$`cell|gene|UMIs`, 0)
    expect_identical(e$`batch|gene|cell|counts`, 0)
})

test_that("empty_data() integrates with copy_all", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(10L))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))

    copy_all(dest, src,
             empty = empty_data(
                 vectors = list(list(axis = "cell", name = "age", value = -1L))
             ),
             relayout = FALSE)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(10L, -1L))
})
