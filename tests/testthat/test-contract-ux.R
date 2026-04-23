test_that("axis_contract builds an axis record", {
    a <- axis_contract("cell", RequiredInput, "per-cell axis")
    expect_identical(a$kind, "axis")
    expect_identical(a$name, "cell")
    expect_identical(a$expectation, RequiredInput)
    expect_identical(a$description, "per-cell axis")
})

test_that("tensor_contract builds a tensor record with kind=tensor", {
    t <- tensor_contract("batch", "cell", "gene", "UMIs",
        RequiredInput, "integer", "per-batch umi matrix"
    )
    expect_identical(t$kind, "tensor")
    expect_identical(t$main_axis, "batch")
    expect_identical(t$rows_axis, "cell")
    expect_identical(t$columns_axis, "gene")
    expect_identical(t$name, "UMIs")
    expect_identical(t$expectation, RequiredInput)
    expect_identical(t$type, "integer")
})

test_that("create_contract returns a Contract with concatenated data", {
    c <- create_contract(
        scalars = list(contract_scalar("organism", RequiredInput, "character", "species")),
        vectors = list(contract_vector("cell", "donor", RequiredInput, "character", "donor")),
        matrices = list(contract_matrix("cell", "gene", "UMIs", RequiredInput, "integer", "UMIs")),
        axes = list(
            axis_contract("cell", RequiredInput, "per-cell axis"),
            axis_contract("gene", RequiredInput, "per-gene axis")
        )
    )
    expect_true(S7::S7_inherits(c, Contract))
    expect_length(c@data, 3L)
    expect_named(c@axes, c("cell", "gene"))
})

test_that("create_contract accepts tensors and stores them in data with kind=tensor", {
    c <- create_contract(
        tensors = list(tensor_contract("batch", "cell", "gene", "UMIs",
            RequiredInput, "integer", "per-batch"
        )),
        axes = list(axis_contract("batch", RequiredInput, ""),
                    axis_contract("cell", RequiredInput, ""),
                    axis_contract("gene", RequiredInput, ""))
    )
    expect_length(c@data, 1L)
    expect_identical(c@data[[1L]]$kind, "tensor")
})

test_that("create_contract rejects wrong-kind elements in typed lists", {
    v <- contract_vector("cell", "donor", RequiredInput, "character", "d")
    expect_error(
        create_contract(scalars = list(v)),
        "scalars"
    )
})

test_that("contract_docs returns a character scalar", {
    c <- create_contract(
        axes = list(axis_contract("cell", RequiredInput, "per-cell axis")),
        vectors = list(contract_vector("cell", "donor", RequiredInput, "character", "donor id"))
    )
    md <- contract_docs(c, format = "markdown")
    expect_type(md, "character")
    expect_length(md, 1L)
    expect_true(grepl("cell", md, fixed = TRUE))
    txt <- contract_docs(c, format = "text")
    expect_type(txt, "character")
})

test_that("verify_contract green path succeeds", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf()
        add_axis(d, "cell", c("c1", "c2"))
        set_vector(d, "cell", "donor", c("A", "B"))
        c <- create_contract(
            axes = list(axis_contract("cell", RequiredInput, "")),
            vectors = list(contract_vector("cell", "donor", RequiredInput,
                "character", "donor id"))
        )
        expect_silent(verify_contract(c, d))
    })
})

test_that("verify_contract errors on missing required axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf()
        c <- create_contract(
            axes = list(axis_contract("cell", RequiredInput, ""))
        )
        expect_error(verify_contract(c, d), "missing.*axis.*cell")
    })
})

test_that("verify_contract succeeds on tensor when main-axis matrices exist", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf()
        add_axis(d, "batch", c("b1", "b2"))
        add_axis(d, "cell", c("c1", "c2"))
        add_axis(d, "gene", c("g1", "g2", "g3"))
        set_matrix(d, "cell", "gene", "b1_UMIs",
            matrix(1:6, 2, 3))
        set_matrix(d, "cell", "gene", "b2_UMIs",
            matrix(7:12, 2, 3))
        c <- create_contract(
            axes = list(
                axis_contract("batch", RequiredInput, ""),
                axis_contract("cell", RequiredInput, ""),
                axis_contract("gene", RequiredInput, "")
            ),
            tensors = list(tensor_contract("batch", "cell", "gene",
                "UMIs", RequiredInput, "integer", ""))
        )
        expect_silent(verify_contract(c, d))
    })
})

test_that("verify_contract errors on tensor with missing per-entry matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf()
        add_axis(d, "batch", c("b1", "b2"))
        add_axis(d, "cell", c("c1", "c2"))
        add_axis(d, "gene", c("g1", "g2"))
        set_matrix(d, "cell", "gene", "b1_UMIs",
            matrix(1:4, 2, 2))
        # b2_UMIs is deliberately missing.
        c <- create_contract(
            axes = list(
                axis_contract("batch", RequiredInput, ""),
                axis_contract("cell", RequiredInput, ""),
                axis_contract("gene", RequiredInput, "")
            ),
            tensors = list(tensor_contract("batch", "cell", "gene",
                "UMIs", RequiredInput, "integer", ""))
        )
        expect_error(verify_contract(c, d), "b2_UMIs")
    })
})

test_that("existing slice-7 Contract() construction path still works", {
    # Regression guard: flat data slot still accepts mixed records.
    c <- Contract(
        axes = list(cell = list(RequiredInput, "per-cell axis")),
        data = list(contract_scalar("organism", RequiredInput, "character", "s"))
    )
    expect_length(c@data, 1L)
    expect_identical(c@data[[1L]]$kind, "scalar")
})

test_that("Contract validator accepts tensor records in data", {
    rec <- tensor_contract("batch", "cell", "gene", "UMIs",
        RequiredInput, "integer", "")
    expect_silent(Contract(data = list(rec)))
})
