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
