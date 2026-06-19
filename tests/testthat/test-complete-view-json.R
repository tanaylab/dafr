test_that(".view_spec_from_julia_json round-trips and reads Julia bytes", {
    julia <- '{"axes":{"cell":"=","renamed_cell":"@ cell"},"data":{"quality":"=","(\\"cell\\", \\"age\\")":"=","(\\"cell\\", \\"gene\\", \\"umi\\")":"="}}'
    spec <- jsonlite::fromJSON(julia, simplifyVector = FALSE)
    ax <- dafr:::.view_spec_from_julia_json(spec$axes, is_data = FALSE)
    dt <- dafr:::.view_spec_from_julia_json(spec$data, is_data = TRUE)
    # axes: list of list(name, query)
    expect_equal(ax[[1]], list("cell", "="))
    expect_equal(ax[[2]], list("renamed_cell", "@ cell"))
    # data: scalar key is a string; vector/matrix keys are char vectors
    expect_equal(dt[[1]], list("quality", "="))
    expect_equal(dt[[2]], list(c("cell", "age"), "="))
    expect_equal(dt[[3]], list(c("cell", "gene", "umi"), "="))
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

test_that(".view_spec_to_julia_json matches the Julia object schema", {
    axes <- list(list("cell", "="), list("renamed_cell", "@ cell"))
    data <- list(list("quality", "="), list(c("cell", "age"), "="),
                 list(c("cell", "gene", "umi"), "="))
    js <- dafr:::.view_spec_to_julia_json(axes, data)
    obj <- jsonlite::fromJSON(js, simplifyVector = FALSE)
    expect_equal(obj$axes$cell, "=")
    expect_equal(obj$axes$renamed_cell, "@ cell")
    expect_equal(obj$data$quality, "=")
    expect_equal(obj$data[['("cell", "age")']], "=")
    expect_equal(obj$data[['("cell", "gene", "umi")']], "=")
    # empty axes/data omitted entirely
    expect_false(grepl("data", dafr:::.view_spec_to_julia_json(axes, NULL)))
})
