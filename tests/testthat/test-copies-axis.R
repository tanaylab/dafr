test_that("copy_axis copies axis entries verbatim", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    copy_axis(dest, src, "cell")
    expect_true(has_axis(dest, "cell"))
    expect_identical(axis_vector(dest, "cell"), c("c1", "c2", "c3"))
})

test_that("copy_axis honors rename", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    add_axis(src, "obs", c("o1", "o2"))
    copy_axis(dest, src, "obs", rename = "cell")
    expect_true(has_axis(dest, "cell"))
    expect_false(has_axis(dest, "obs"))
})

test_that("copy_axis raises on missing source axis", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    expect_error(copy_axis(dest, src, "nope"), "missing axis")
})

test_that("copy_axis insist=TRUE raises when dest has the axis", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("x"))
    expect_error(copy_axis(dest, src, "cell"), "existing axis:")
})

test_that("copy_axis insist=FALSE silently skips when dest has the axis", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("x"))
    copy_axis(dest, src, "cell", insist = FALSE)
    expect_identical(axis_vector(dest, "cell"), c("x"))
})

test_that("copy_axis overwrite=TRUE deletes and recreates destination axis", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("x", "y"))
    set_vector(dest, "cell", "tag", c("A", "B"))
    copy_axis(dest, src, "cell", overwrite = TRUE)
    expect_identical(axis_vector(dest, "cell"), c("c1"))
    expect_false(has_vector(dest, "cell", "tag"))
})

test_that("copy_axis validates rename / overwrite / insist", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest")
    expect_error(copy_axis(dest, src, "cell", rename = ""),
                 "rename")
    expect_error(copy_axis(dest, src, "cell", overwrite = 1),
                 "overwrite")
    expect_error(copy_axis(dest, src, "cell", insist = NA),
                 "insist")
})
