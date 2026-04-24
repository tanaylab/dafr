test_that("AsAxis drop: @ A : v =@ : w drops empty-value rows when IfNot bare", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("C1", "C2", "C3"))
    add_axis(d, "metacell", c("M1", "M2"))
    set_vector(d, "cell", "metacell", c("M1", "", "M2"))
    set_vector(d, "metacell", "type", c("T1", "T2"))
    result <- get_query(d, "@ cell : metacell ?? =@ : type")
    expect_identical(as.character(result), c("T1", "T2"))
    expect_identical(names(result), c("C1", "C3"))
})

test_that("AsAxis default: @ A : v ?? X =@ : w substitutes X for empty rows", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("C1", "C2", "C3"))
    add_axis(d, "metacell", c("M1", "M2"))
    set_vector(d, "cell", "metacell", c("M1", "", "M2"))
    set_vector(d, "metacell", "type", c("T1", "T2"))
    result <- get_query(d, '@ cell : metacell ?? "UNK" =@ : type')
    expect_identical(as.character(result), c("T1", "UNK", "T2"))
    expect_identical(names(result), c("C1", "C2", "C3"))
})

test_that("AsAxis with explicit target: @ A : v =@ B : w", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("C1", "C2"))
    add_axis(d, "batch", c("B1", "B2"))
    set_vector(d, "cell", "origin", c("B1", "B2"))
    set_vector(d, "batch", "year", c(2023L, 2024L))
    result <- get_query(d, "@ cell : origin =@ batch : year")
    expect_identical(as.integer(result), c(2023L, 2024L))
    expect_identical(names(result), c("C1", "C2"))
})

test_that("bare 2-hop '=@:x =@:y' chains through two axes", {
    d <- memory_daf(name = "multi-hop")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
    set_vector(d, "donor", "lab", c("lA", "lB"))
    set_vector(d, "lab", "country", c("IL", "US"))

    out <- get_query(d, "@ cell : donor =@ : lab =@ : country")
    expect_equal(unname(out), c("IL", "US", "IL"))
    expect_equal(names(out), c("c1", "c2", "c3"))
})

test_that("explicit-axis 2-hop '=@axis:x =@:y' chains through two axes", {
    d <- memory_daf(name = "multi-hop-explicit")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    set_vector(d, "cell", "d_alias", c("d1", "d2"))
    set_vector(d, "donor", "l_alias", c("lA", "lB"))
    set_vector(d, "lab", "country", c("IL", "US"))

    out <- get_query(d, "@ cell : d_alias =@ donor : l_alias =@ lab : country")
    expect_equal(unname(out), c("IL", "US"))
    expect_equal(names(out), c("c1", "c2"))
})

test_that("3-hop chain '=@:x =@:y =@:z' resolves", {
    d <- memory_daf(name = "3-hop")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    add_axis(d, "country", c("IL", "US"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    set_vector(d, "donor", "lab", c("lA", "lB"))
    set_vector(d, "lab", "country", c("IL", "US"))
    set_vector(d, "country", "language", c("Hebrew", "English"))

    out <- get_query(d, "@ cell : donor =@ : lab =@ : country =@ : language")
    expect_equal(unname(out), c("Hebrew", "English"))
    expect_equal(names(out), c("c1", "c2"))
})

test_that("intermediate '??' drops missing rows but does not leak to next hop", {
    d <- memory_daf(name = "if-not-clear")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    set_vector(d, "donor", "lab", c("lA", ""))
    set_vector(d, "lab", "country", c("IL", "US"))

    expect_error(
        get_query(d, "@ cell : donor ?? =@ : lab =@ : country"),
        "empty pivot values"
    )
})

test_that("'??' at hop 1 drops missing rows; survivors pass through hop 2 correctly", {
    d <- memory_daf(name = "if-not-drop")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    set_vector(d, "cell", "donor", c("d1", "", "d2"))
    set_vector(d, "donor", "lab", c("lA", "lB"))
    set_vector(d, "lab", "country", c("IL", "US"))

    out <- get_query(d, "@ cell : donor ?? =@ : lab =@ : country")
    expect_equal(unname(out), c("IL", "US"))
    expect_equal(names(out), c("c1", "c3"))
})

test_that("hop 2 raises when the pivot property names a non-axis", {
    d <- memory_daf(name = "bad-hop")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "donor", c("d1", "d2"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    set_vector(d, "donor", "not_an_axis", c("x", "y"))

    expect_error(
        get_query(d, "@ cell : donor =@ : not_an_axis =@ : anything"),
        "AsAxis target axis"
    )
})
