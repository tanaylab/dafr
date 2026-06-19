test_that(".view_spec_from_julia_json reads the array form (and tolerates object form)", {
    arr <- '{"axes":[{"cell":"="},{"renamed_cell":"@ cell"}],"data":[{"quality":"="},{"(\\"cell\\", \\"age\\")":"="},{"(\\"cell\\", \\"gene\\", \\"umi\\")":"="}]}'
    s <- jsonlite::fromJSON(arr, simplifyVector = FALSE)
    ax <- dafr:::.view_spec_from_julia_json(s$axes, is_data = FALSE)
    dt <- dafr:::.view_spec_from_julia_json(s$data, is_data = TRUE)
    expect_equal(ax[[1]], list("cell", "="))
    expect_equal(ax[[2]], list("renamed_cell", "@ cell"))
    expect_equal(dt[[1]], list("quality", "="))
    expect_equal(dt[[2]], list(c("cell", "age"), "="))
    expect_equal(dt[[3]], list(c("cell", "gene", "umi"), "="))
    # tolerate Julia's (broken) object-form output too:
    objform <- jsonlite::fromJSON('{"cell":"=","gene":"="}', simplifyVector = FALSE)
    ax2 <- dafr:::.view_spec_from_julia_json(objform, is_data = FALSE)
    expect_equal(ax2[[1]], list("cell", "="))
    expect_equal(ax2[[2]], list("gene", "="))
})

test_that("writer + reader round-trip (data keys preserved)", {
    axes <- list(list("cell", "="))
    data <- list(list("quality", "="), list(c("cell", "age"), "="),
                 list(c("cell", "gene", "umi"), "="))
    js <- dafr:::.view_spec_to_julia_json(axes, data)
    spec <- jsonlite::fromJSON(js, simplifyVector = FALSE)
    dt <- dafr:::.view_spec_from_julia_json(spec$data, is_data = TRUE)
    expect_equal(dt[[2]], list(c("cell", "age"), "="))
    expect_equal(dt[[3]], list(c("cell", "gene", "umi"), "="))
})

test_that(".view_spec_to_julia_json matches Julia's reader (array) schema", {
    axes <- list(list("cell", "="), list("renamed_cell", "@ cell"))
    data <- list(list("quality", "="), list(c("cell", "age"), "="),
                 list(c("cell", "gene", "umi"), "="))
    js <- dafr:::.view_spec_to_julia_json(axes, data)
    obj <- jsonlite::fromJSON(js, simplifyVector = FALSE)
    expect_equal(obj$axes[[1]]$cell, "=")
    expect_equal(obj$axes[[2]]$renamed_cell, "@ cell")
    expect_equal(obj$data[[1]]$quality, "=")
    expect_equal(obj$data[[2]][['("cell", "age")']], "=")
    expect_equal(obj$data[[3]][['("cell", "gene", "umi")']], "=")
    expect_false(grepl("data", dafr:::.view_spec_to_julia_json(axes, NULL)))
})

test_that(".view_decode_key falls back for a paren-wrapped plain name", {
    # A scalar/column genuinely named "(foo)" is not a valid tuple; must not crash.
    expect_equal(dafr:::.view_decode_key("(foo)"), "(foo)")
    # A real tuple still decodes to a character vector.
    expect_equal(dafr:::.view_decode_key('("cell", "age")'), c("cell", "age"))
})
