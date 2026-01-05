# test-contracts.R - Tests for DAF contract validation

test_that("create_contract creates valid contract object", {
    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Gene identifiers"),
            axis_contract("metacell", RequiredInput, "Metacell identifiers")
        ),
        data = list(
            vector_contract("metacell", "type", RequiredInput, "character", "Cell type"),
            matrix_contract("metacell", "gene", "UMIs", RequiredInput, "numeric", "UMI counts")
        )
    )

    expect_s3_class(contract, "DafContract")
    expect_length(contract$axes, 2)
    expect_length(contract$data, 2)
    expect_false(contract$is_relaxed)
})

test_that("axis_contract creates proper specification", {
    spec <- axis_contract("gene", RequiredInput, "Gene identifiers")

    expect_equal(spec$type, "axis")
    expect_equal(spec$name, "gene")
    expect_equal(spec$expectation, RequiredInput)
    expect_equal(spec$description, "Gene identifiers")
})

test_that("vector_contract creates proper specification", {
    spec <- vector_contract("metacell", "type", RequiredInput, "character", "Cell type")

    expect_equal(spec$type, "vector")
    expect_equal(spec$axis, "metacell")
    expect_equal(spec$name, "type")
    expect_equal(spec$expectation, RequiredInput)
    expect_equal(spec$dtype, "character")
})

test_that("matrix_contract creates proper specification", {
    spec <- matrix_contract("metacell", "gene", "UMIs", RequiredInput, "numeric", "UMI counts")

    expect_equal(spec$type, "matrix")
    expect_equal(spec$rows_axis, "metacell")
    expect_equal(spec$cols_axis, "gene")
    expect_equal(spec$name, "UMIs")
    expect_equal(spec$expectation, RequiredInput)
})

test_that("scalar_contract creates proper specification", {
    spec <- scalar_contract("title", OptionalInput, "character", "Dataset title")

    expect_equal(spec$type, "scalar")
    expect_equal(spec$name, "title")
    expect_equal(spec$expectation, OptionalInput)
    expect_equal(spec$dtype, "character")
})

test_that("verify_contract validates against contract", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    # Create a simple in-memory DAF for testing
    daf <- memory_daf("test")
    daf <- add_axis(daf, "gene", c("A", "B", "C"))
    daf <- add_axis(daf, "metacell", c("M1", "M2"))
    daf <- set_vector(daf, "metacell", "type", c("T1", "T2"))

    # Contract that should pass
    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Gene identifiers"),
            axis_contract("metacell", RequiredInput, "Metacell identifiers")
        ),
        data = list(
            vector_contract("metacell", "type", RequiredInput, "character", "Cell type")
        )
    )

    result <- verify_contract(daf, contract)
    expect_true(result$valid)
    expect_length(result$errors, 0)

    # Contract that should fail (missing axis)
    contract_fail <- create_contract(
        axes = list(
            axis_contract("missing_axis", RequiredInput, "This doesn't exist")
        )
    )

    result_fail <- verify_contract(daf, contract_fail)
    expect_false(result_fail$valid)
    expect_true(length(result_fail$errors) > 0)
    expect_true(any(grepl("missing_axis", result_fail$errors)))
})

test_that("verify_input throws on invalid DAF", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    daf <- memory_daf("test")
    daf <- add_axis(daf, "gene", c("A", "B"))

    # Contract requiring missing axis
    contract <- create_contract(
        axes = list(
            axis_contract("missing", RequiredInput, "Missing axis")
        )
    )

    expect_error(verify_input(daf, contract), "validation failed")
})

test_that("optional data does not cause validation failure", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    daf <- memory_daf("test")
    daf <- add_axis(daf, "gene", c("A", "B"))

    # Contract with optional data
    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Gene identifiers"),
            axis_contract("optional_axis", OptionalInput, "Optional axis")
        ),
        data = list(
            vector_contract("gene", "optional_vec", OptionalInput, "numeric", "Optional vector")
        )
    )

    result <- verify_contract(daf, contract)
    expect_true(result$valid)
})

test_that("contract_docs generates markdown documentation", {
    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Gene identifiers")
        ),
        data = list(
            vector_contract("gene", "name", OptionalInput, "character", "Gene names"),
            matrix_contract("gene", "metacell", "UMIs", RequiredInput, "numeric", "UMI counts")
        )
    )

    docs <- contract_docs(contract, "markdown")

    expect_true(grepl("# DAF Contract", docs))
    expect_true(grepl("## Axes", docs))
    expect_true(grepl("## Vectors", docs))
    expect_true(grepl("## Matrices", docs))
    expect_true(grepl("gene", docs))
})

test_that("print.DafContract prints contract summary", {
    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Gene identifiers")
        ),
        data = list(
            scalar_contract("title", OptionalInput, "character", "Title")
        )
    )

    output <- capture.output(print(contract))
    expect_true(any(grepl("DAF Contract", output)))
    expect_true(any(grepl("Axes:", output)))
    expect_true(any(grepl("Scalars:", output)))
})

test_that("relaxed contract flag is preserved", {
    contract <- create_contract(
        axes = list(),
        data = list(),
        is_relaxed = TRUE
    )

    expect_true(contract$is_relaxed)
})

test_that("tensor_contract creates proper specification", {
    spec <- tensor_contract(
        "batch", "cell", "gene", "UMIs",
        RequiredInput, "numeric",
        "Per-batch UMI counts"
    )

    expect_equal(spec$type, "tensor")
    expect_equal(spec$main_axis, "batch")
    expect_equal(spec$rows_axis, "cell")
    expect_equal(spec$cols_axis, "gene")
    expect_equal(spec$name, "UMIs")
    expect_equal(spec$expectation, RequiredInput)
    expect_equal(spec$dtype, "numeric")
    expect_equal(spec$description, "Per-batch UMI counts")
})

test_that("contract with tensor is created correctly", {
    contract <- create_contract(
        axes = list(
            axis_contract("batch", RequiredInput, "Batch identifiers"),
            axis_contract("cell", RequiredInput, "Cell identifiers"),
            axis_contract("gene", RequiredInput, "Gene identifiers")
        ),
        data = list(
            tensor_contract(
                "batch", "cell", "gene", "UMIs",
                RequiredInput, "numeric",
                "Per-batch UMI counts"
            )
        )
    )

    expect_s3_class(contract, "DafContract")
    expect_length(contract$axes, 3)
    expect_length(contract$data, 1)
    expect_equal(contract$data[[1]]$type, "tensor")
})

test_that("contract_docs includes tensor information", {
    contract <- create_contract(
        axes = list(
            axis_contract("batch", RequiredInput, "Batches")
        ),
        data = list(
            tensor_contract(
                "batch", "cell", "gene", "UMIs",
                RequiredInput, "numeric",
                "UMI counts per batch"
            )
        )
    )

    docs <- contract_docs(contract, "markdown")

    expect_true(grepl("Tensors", docs))
    expect_true(grepl("batch", docs))
    expect_true(grepl("UMIs", docs))
})

test_that("contractor creates a contract-aware Daf wrapper", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    # Create a valid DAF
    daf <- memory_daf("test_contractor")
    add_axis(daf, "gene", c("A", "B", "C"))
    add_axis(daf, "cell", c("C1", "C2"))
    set_vector(daf, "cell", "type", c("T1", "T2"))

    # Create a contract
    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Gene identifiers"),
            axis_contract("cell", RequiredInput, "Cell identifiers")
        ),
        data = list(
            vector_contract("cell", "type", RequiredInput, "character", "Cell type")
        )
    )

    # Create contractor wrapper
    contract_daf <- contractor("test.computation", contract, daf)

    # Should be able to verify input
    expect_no_error(verify_input(contract_daf$daf, contract_daf$contract))

    # Should include computation name
    expect_equal(contract_daf$computation, "test.computation")
})

test_that("contractor with invalid input throws on verify", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    # Create a DAF missing required data
    daf <- memory_daf("test_missing")
    add_axis(daf, "gene", c("A", "B"))
    # Missing "cell" axis and "type" vector

    contract <- create_contract(
        axes = list(
            axis_contract("cell", RequiredInput, "Cell identifiers")
        ),
        data = list(
            vector_contract("cell", "type", RequiredInput, "character", "Cell type")
        )
    )

    contract_daf <- contractor("test.computation", contract, daf)

    # Verification should fail
    expect_error(verify_input(contract_daf$daf, contract_daf$contract))
})

test_that("verify_output validates output data", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    daf <- memory_daf("test_output")
    add_axis(daf, "gene", c("A", "B", "C"))
    set_vector(daf, "gene", "score", c(1.0, 2.0, 3.0))

    # Contract with guaranteed output that exists
    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Genes")
        ),
        data = list(
            vector_contract("gene", "score", GuaranteedOutput, "numeric", "Scores")
        )
    )

    # Should pass since the output exists
    expect_no_error(verify_output(daf, contract))
})

test_that("contract_docs generates text format documentation", {
    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Gene identifiers")
        ),
        data = list(
            scalar_contract("title", OptionalInput, "character", "Title"),
            vector_contract("gene", "name", OptionalInput, "character", "Names"),
            matrix_contract("gene", "cell", "expr", RequiredInput, "numeric", "Expression")
        )
    )

    docs <- contract_docs(contract, "text")

    expect_true(grepl("DAF Contract", docs))
    expect_true(grepl("============", docs))
    expect_true(grepl("Axes:", docs))
    expect_true(grepl("Scalars:", docs))
    expect_true(grepl("Vectors:", docs))
    expect_true(grepl("Matrices:", docs))
})

test_that("verify_contract detects missing required scalar", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    daf <- memory_daf("test_scalar")
    add_axis(daf, "gene", c("A", "B"))

    contract <- create_contract(
        data = list(
            scalar_contract("required_scalar", RequiredInput, "character", "A required scalar")
        )
    )

    result <- verify_contract(daf, contract)
    expect_false(result$valid)
    expect_true(any(grepl("required_scalar", result$errors)))
})

test_that("verify_contract detects missing required matrix", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    daf <- memory_daf("test_matrix")
    add_axis(daf, "gene", c("A", "B"))
    add_axis(daf, "cell", c("C1", "C2"))

    contract <- create_contract(
        axes = list(
            axis_contract("gene", RequiredInput, "Genes"),
            axis_contract("cell", RequiredInput, "Cells")
        ),
        data = list(
            matrix_contract("gene", "cell", "expr", RequiredInput, "numeric", "Expression matrix")
        )
    )

    result <- verify_contract(daf, contract)
    expect_false(result$valid)
    expect_true(any(grepl("expr", result$errors)))
})

test_that("print.DafContract includes tensors", {
    contract <- create_contract(
        axes = list(
            axis_contract("batch", RequiredInput, "Batches")
        ),
        data = list(
            tensor_contract(
                "batch", "cell", "gene", "counts",
                RequiredInput, "numeric",
                "Per-batch counts"
            )
        )
    )

    output <- capture.output(print(contract))
    expect_true(any(grepl("Tensors:", output)))
    expect_true(any(grepl("batch", output)))
})

test_that("contractor validates its arguments", {
    skip_if_not(dafr:::is_julia_initialized(), "Julia not initialized")

    daf <- memory_daf("test")
    add_axis(daf, "gene", c("A", "B"))

    contract <- create_contract(
        axes = list(axis_contract("gene", RequiredInput, "Genes"))
    )

    # Invalid contract type
    expect_error(contractor("test", list(), daf), "DafContract")

    # Invalid daf type
    expect_error(contractor("test", contract, list()), "Daf object")
})

test_that("contract with all expectation types works", {
    contract <- create_contract(
        axes = list(
            axis_contract("input_axis", RequiredInput, "Required input"),
            axis_contract("optional_axis", OptionalInput, "Optional input"),
            axis_contract("output_axis", GuaranteedOutput, "Guaranteed output"),
            axis_contract("maybe_output", OptionalOutput, "Optional output")
        )
    )

    expect_s3_class(contract, "DafContract")
    expect_length(contract$axes, 4)

    # Check all expectation types are preserved
    expectations <- sapply(contract$axes, function(a) a$expectation)
    expect_true(RequiredInput %in% expectations)
    expect_true(OptionalInput %in% expectations)
    expect_true(GuaranteedOutput %in% expectations)
    expect_true(OptionalOutput %in% expectations)
})
