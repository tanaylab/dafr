test_that("copy_all matches Julia roundtrip", {
    fx_path <- test_path("fixtures", "julia-copies", "copy_all_fixture.json")
    skip_if(!file.exists(fx_path), "Julia fixture not generated")
    expected <- jsonlite::fromJSON(fx_path, simplifyVector = FALSE)

    src <- memory_daf(name = "src")
    set_scalar(src, "organism", "human")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2", "g3"))
    set_vector(src, "cell", "age", as.integer(c(10, 20)))
    set_matrix(src, "cell", "gene", "UMIs",
               matrix(as.integer(c(1, 4, 2, 5, 3, 6)), nrow = 2,
                      dimnames = list(c("c1","c2"), c("g1","g2","g3"))))

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))
    add_axis(dest, "gene", c("g1", "g2", "g3"))
    copy_all(dest, src,
        empty = list("cell|age" = -1L, "cell|gene|UMIs" = 0L),
        relayout = FALSE)

    expect_identical(get_scalar(dest, "organism"),
                     expected$scalars$organism)
    expect_identical(as.character(axis_vector(dest, "cell")),
                     unlist(expected$axes$cell))
    expect_identical(as.integer(unname(get_vector(dest, "cell", "age"))),
                     as.integer(unlist(expected$vectors$`cell|age`)))
})

test_that("concatenate matches Julia roundtrip", {
    fx_path <- test_path("fixtures", "julia-copies", "concat_fixture.json")
    skip_if(!file.exists(fx_path), "Julia fixture not generated")
    expected <- jsonlite::fromJSON(fx_path, simplifyVector = FALSE)

    a <- memory_daf(name = "A"); add_axis(a, "cell", c("c1", "c2"))
    set_vector(a, "cell", "age", as.integer(c(10, 20)))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("c1", "c3"))
    set_vector(b, "cell", "age", as.integer(c(30, 40)))

    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b), prefix = TRUE)

    expect_identical(as.character(axis_vector(dest, "cell")),
                     unlist(expected$axes$cell))
    expect_identical(as.character(unname(get_vector(dest, "cell", "dataset"))),
                     unlist(expected$vectors$`cell|dataset`))
})
