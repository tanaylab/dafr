test_that(".verify_axis_relation detects same/subset/superset/disjoint", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "donor", c("d1"))
    add_axis(d, "cell_sub", c("c1", "c2"))
    add_axis(d, "cell_ext", c("c1", "c2", "c3", "c4"))
    add_axis(d, "cell_disj", c("x1"))

    src <- d
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    expect_identical(
        dafr:::.verify_axis_relation(src, "cell", dest, "cell"),
        "same"
    )
    add_axis(dest, "cell_sub", c("c1", "c2"))
    expect_identical(
        dafr:::.verify_axis_relation(src, "cell", dest, "cell_sub"),
        "destination_is_subset"
    )
    add_axis(dest, "cell_ext", c("c1", "c2", "c3", "c4"))
    expect_identical(
        dafr:::.verify_axis_relation(src, "cell_sub", dest, "cell_ext"),
        "source_is_subset"
    )
    add_axis(dest, "cell_disj", c("zz"))
    expect_error(
        dafr:::.verify_axis_relation(src, "cell", dest, "cell_disj"),
        "disjoint"
    )
})
