test_that("open_daf opens a FilesDaf directory in read mode", {
    tmp <- withr::local_tempdir()
    f <- files_daf(tmp, name = "orig", mode = "w+")
    add_axis(f, "cell", c("c1", "c2"))
    set_vector(f, "cell", "age", c(1L, 2L))

    d <- open_daf(tmp, "r")
    expect_true(S7::S7_inherits(d, DafReadOnly) || S7::S7_inherits(d, DafReader))
    expect_identical(unname(get_vector(d, "cell", "age")), c(1L, 2L))
})

test_that("open_daf opens a FilesDaf directory in r+ mode", {
    tmp <- withr::local_tempdir()
    f <- files_daf(tmp, name = "orig", mode = "w+")
    add_axis(f, "cell", c("c1"))

    d <- open_daf(tmp, "r+")
    set_vector(d, "cell", "tag", c("x"))
    expect_identical(unname(get_vector(d, "cell", "tag")), c("x"))
})

test_that("open_daf dispatches .h5df and rejects grouped h5dfs", {
    skip_if_not_installed("hdf5r")
    # .h5df now dispatches to h5df(); a nonexistent path errors on open.
    expect_error(open_daf("x.h5df", "r"), "not a daf store")
    # Grouped .h5dfs#/group archives remain unsupported (rejected in open_daf).
    expect_error(open_daf("x.h5dfs#/grp", "r"), "not supported")
})

test_that("complete_chain sets base_daf_repository and returns a write chain", {
    tmp_base <- withr::local_tempdir()
    tmp_new <- withr::local_tempdir()
    base <- files_daf(tmp_base, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1", "c2"))
    set_vector(base, "cell", "age", c(1L, 2L))

    new <- files_daf(tmp_new, name = "new", mode = "w+")
    chain <- complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    expect_true(format_has_scalar(new, "base_daf_repository"))
    expect_identical(format_get_scalar(new, "base_daf_repository")$value,
                     normalizePath(tmp_base, winslash = "/"))
    expect_identical(unname(get_vector(chain, "cell", "age")), c(1L, 2L))
    set_vector(chain, "cell", "tag", c("x", "y"))
    expect_true(has_vector(new, "cell", "tag"))
})

test_that("complete_daf reopens a chain that complete_chain persisted", {
    tmp_root <- withr::local_tempdir()
    base_dir <- file.path(tmp_root, "base")
    new_dir <- file.path(tmp_root, "new")
    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1", "c2"))
    set_vector(base, "cell", "age", c(10L, 20L))
    new <- files_daf(new_dir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    chain <- complete_daf(new_dir, mode = "r")
    expect_identical(unname(get_vector(chain, "cell", "age")), c(10L, 20L))
})

test_that("complete_daf in r+ mode allows writes to leaf", {
    tmp_root <- withr::local_tempdir()
    base_dir <- file.path(tmp_root, "base")
    new_dir <- file.path(tmp_root, "new")
    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1"))
    new <- files_daf(new_dir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    chain <- complete_daf(new_dir, mode = "r+")
    set_vector(chain, "cell", "tag", c("t1"))
    leaf_reopen <- open_daf(new_dir, "r")
    expect_true(has_vector(leaf_reopen, "cell", "tag"))
})

test_that("complete_daf rejects invalid mode", {
    tmp <- withr::local_tempdir()
    files_daf(tmp, name = "t", mode = "w+")
    expect_error(complete_daf(tmp, "w"), "must be")
})

test_that(".is_absolute_path recognises unix, windows and UNC paths", {
    f <- dafr:::.is_absolute_path
    expect_true(f("/tmp/foo"))
    expect_true(f("C:/tmp/foo"))
    expect_true(f("c:/tmp/foo"))
    expect_true(f("C:\\tmp\\foo"))
    expect_true(f("\\\\server\\share"))
    expect_false(f("relative/path"))
    expect_false(f("foo"))
    expect_false(f("."))
    expect_false(f("./x"))
})

# --- DAG of repositories (port of DataAxesFormats' test/complete.jl) ---

.complete_diamond <- function(root) {
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B", "C"))
    add_axis(cells, "gene", c("X", "Y"))
    set_vector(cells, "cell", "age", c(10L, 20L, 30L))

    results <- files_daf(file.path(root, "results"), name = "results!", mode = "w+")
    results_chain <- complete_chain(base_daf = cells, new_daf = results)
    set_vector(results_chain, "cell", "score", c(1, 2, 3))

    masks <- files_daf(file.path(root, "masks"), name = "masks!", mode = "w+")
    masks_chain <- complete_chain(base_daf = cells, new_daf = masks)
    set_vector(masks_chain, "gene", "is_marker", c(TRUE, FALSE))

    list(cells = cells, results = results_chain, masks = masks_chain)
}

.chain_names <- function(chain) {
    vapply(dafr:::.chain_dafs(chain), function(d) S7::prop(d, "name"), character(1))
}

test_that("complete_chain records a lone unviewed base as its relative path", {
    root <- withr::local_tempdir()
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B"))
    metacells <- files_daf(file.path(root, "metacells"), name = "metacells!", mode = "w+")
    complete_chain(base_daf = cells, new_daf = metacells)
    expect_identical(get_scalar(metacells, "base_daf_repository"), "cells")
})

test_that("complete_chain records several bases as a JSON array", {
    root <- withr::local_tempdir()
    d <- .complete_diamond(root)
    leaf <- files_daf(file.path(root, "leaf"), name = "leaf!", mode = "w+")
    chain <- complete_chain(base_daf = list(d$results, d$masks), new_daf = leaf)

    # Only the immediate bases are recorded; that both rest on the cells is
    # recorded in them.
    expect_identical(
        jsonlite::fromJSON(get_scalar(leaf, "base_daf_repository")),
        c("results", "masks")
    )
    # The cells are reached through both arms and appear once, before
    # everything resting on them.
    expect_identical(.chain_names(chain),
                     c("cells!", "results!", "masks!", "leaf!"))
})

test_that("complete_chain records the same base twice only once", {
    root <- withr::local_tempdir()
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B"))
    metacells <- files_daf(file.path(root, "metacells"), name = "metacells!", mode = "w+")
    chain <- complete_chain(base_daf = list(cells, cells), new_daf = metacells)
    expect_identical(get_scalar(metacells, "base_daf_repository"), "cells")
    expect_identical(.chain_names(chain), c("cells!", "metacells!"))
})

test_that("complete_chain keeps two different views of one base apart", {
    root <- withr::local_tempdir()
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B"))
    add_axis(cells, "gene", c("X", "Y"))
    metacells <- files_daf(file.path(root, "metacells"), name = "metacells!", mode = "w+")
    chain <- complete_chain(
        base_daf = list(
            base_daf(cells, axes = list(list("cell", "="))),
            base_daf(cells, axes = list(list("gene", "=")))
        ),
        new_daf = metacells
    )
    expect_length(
        jsonlite::fromJSON(get_scalar(metacells, "base_daf_repository"),
                           simplifyVector = FALSE),
        2L
    )
    expect_length(dafr:::.chain_dafs(chain), 3L)
    expect_setequal(axes_set(chain), c("cell", "gene"))
})

test_that("complete_daf reopens a diamond of repositories", {
    root <- withr::local_tempdir()
    d <- .complete_diamond(root)
    leaf <- files_daf(file.path(root, "leaf"), name = "leaf!", mode = "w+")
    complete_chain(base_daf = list(d$results, d$masks), new_daf = leaf)

    reopened <- complete_daf(file.path(root, "leaf"), name = "reopened!")
    expect_identical(unname(get_vector(reopened, "cell", "age")), c(10L, 20L, 30L))
    expect_identical(unname(get_vector(reopened, "cell", "score")), c(1, 2, 3))
    expect_identical(unname(get_vector(reopened, "gene", "is_marker")),
                     c(TRUE, FALSE))
})

test_that("complete_daf reads a hand-written object-form base_daf_repository", {
    root <- withr::local_tempdir()
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B"))
    add_axis(cells, "gene", c("X", "Y"))

    metacells <- files_daf(file.path(root, "metacells"), name = "metacells!", mode = "w+")
    set_scalar(metacells, "base_daf_repository",
               '{"path": "cells", "axes": [{"cell": "="}]}')

    reopened <- complete_daf(file.path(root, "metacells"), name = "reopened!")
    expect_setequal(axes_set(reopened), "cell")
    expect_false(has_axis(reopened, "gene"))
})

test_that("complete_daf still reads a legacy base_daf_view scalar", {
    # dafr up to 0.9.0 stored a lone base path plus a separate base_daf_view.
    root <- withr::local_tempdir()
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B"))
    add_axis(cells, "gene", c("X", "Y"))

    metacells <- files_daf(file.path(root, "metacells"), name = "metacells!", mode = "w+")
    set_scalar(metacells, "base_daf_repository", "cells")
    set_scalar(metacells, "base_daf_view",
               dafr:::.view_spec_to_julia_json(list(list("cell", "=")), NULL))

    reopened <- complete_daf(file.path(root, "metacells"), name = "reopened!")
    expect_setequal(axes_set(reopened), "cell")
    expect_false(has_axis(reopened, "gene"))
})

test_that("a chain reports its complete path only when the records lead to it", {
    root <- withr::local_tempdir()
    d <- .complete_diamond(root)
    leaf <- files_daf(file.path(root, "leaf"), name = "leaf!", mode = "w+")
    chain <- complete_chain(base_daf = list(d$results, d$masks), new_daf = leaf)
    expect_identical(complete_path(chain),
                     normalizePath(file.path(root, "leaf"), winslash = "/"))

    # A repository the records do not lead to means reopening the leaf would
    # not give this chain.
    other <- files_daf(file.path(root, "other"), name = "other!", mode = "w+")
    expect_null(complete_path(
        chain_writer(list(d$cells, other, leaf), name = "extra!")))

    # A base the records name but which is not here means this is only part of
    # the complete chain.
    expect_null(complete_path(chain_writer(list(leaf), name = "missing!")))

    # A repository which is not persistent cannot be reopened.
    expect_null(complete_path(chain_writer(
        list(memory_daf(name = "memory!"), d$cells, leaf), name = "pathless!")))
})

test_that("a chain rejects a repository that is a base of itself", {
    root <- withr::local_tempdir()
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B"))
    expect_error(chain_writer(list(cells, cells), name = "cycle!"),
                 "cyclic repository")
})

test_that("a chain of chains is one long chain", {
    root <- withr::local_tempdir()
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B"))
    metacells <- files_daf(file.path(root, "metacells"), name = "metacells!", mode = "w+")
    metacells_chain <- complete_chain(base_daf = cells, new_daf = metacells)

    blocks <- files_daf(file.path(root, "blocks"), name = "blocks!", mode = "w+")
    chain <- chain_writer(list(metacells_chain, blocks), name = "chain!")
    expect_identical(.chain_names(chain), c("cells!", "metacells!", "blocks!"))
})

test_that("two pathless repositories are never the same one", {
    root <- withr::local_tempdir()
    cells <- files_daf(file.path(root, "cells"), name = "cells!", mode = "w+")
    add_axis(cells, "cell", c("A", "B"))
    chain <- chain_writer(
        list(cells, memory_daf(name = "first!"), memory_daf(name = "second!")),
        name = "chain!"
    )
    expect_identical(.chain_names(chain), c("cells!", "first!", "second!"))
})

test_that("a recorded base path is relative to the new repository's directory", {
    # `.norm_path()` spells every path with forward slashes, on Windows too, so
    # this is one comparison rather than one per separator. Runs everywhere,
    # which is the point: the Windows spelling is what got this wrong.
    rel <- dafr:::.relative_base_path
    expect_identical(rel("C:/tmp/root/cells", "C:/tmp/root"), "cells")
    expect_identical(rel("C:/tmp/root/sub/cells", "C:/tmp/root"), "sub/cells")
    expect_identical(rel("/tmp/root/cells", "/tmp/root"), "cells")
    expect_identical(rel("/tmp/root", "/tmp/root"), ".")
    # Not under it: nothing relative to give, so the absolute path stands.
    expect_identical(rel("/other/cells", "/tmp/root"), "/other/cells")
})
