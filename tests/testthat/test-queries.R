expect_c_equal <- function(query, expected) {
    expect_equal(as.character(query), expected)
}

# Test query formatting
test_that("query formatting works correctly", {
    expect_c_equal(Axis("cell"), "/ cell")
    expect_c_equal(Names("axes"), "? axes")
    expect_c_equal(Lookup("version"), ": version")
    expect_c_equal(Axis("cell") |> Lookup("batch") |> Fetch("age"), "/ cell : batch => age")
    expect_c_equal(Axis("cell") |> Lookup("manual") |> AsAxis("type") |> Fetch("color"), "/ cell : manual ! type => color")
    expect_c_equal(Axis("cell") |> Lookup("batch") |> AsAxis() |> CountBy("manual") |> AsAxis("type"), "/ cell : batch ! * manual ! type")
    expect_c_equal(Axis("cell") |> Lookup("age") |> GroupBy("batch") |> AsAxis() |> Mean(), "/ cell : age @ batch ! %> Mean")
    expect_c_equal(Axis("cell") |> Lookup("batch") |> IfNot() |> Fetch("age"), "/ cell : batch ?? => age")
    expect_c_equal(Axis("cell") |> Lookup("type") |> IfNot("Outlier"), "/ cell : type ?? Outlier")
    expect_c_equal(Axis("cell") |> Lookup("batch") |> IfNot(1) |> Fetch("age"), "/ cell : batch ?? 1.0 => age")
    expect_c_equal(Axis("gene") |> Lookup("is_marker") |> IfMissing(FALSE), "/ gene : is_marker || false Bool")
    expect_c_equal(Axis("cell") |> Lookup("type") |> IfMissing("red") |> Fetch("color"), "/ cell : type || red => color")
    expect_c_equal(Axis("cell") |> Lookup("type") |> IsEqual("LMPP") |> Fetch("age") |> Max() |> IfMissing(as.integer(0)), "/ cell : type = LMPP => age %> Max || 0 Int64")
    expect_c_equal(Axis("cell") |> Axis("gene") |> Lookup("UMIs") |> Log(base = 2, eps = 1), "/ cell / gene : UMIs % Log base 2.0 eps 1.0")
    expect_c_equal(Axis("cell") |> Axis("gene") |> Lookup("UMIs") |> Sum(), "/ cell / gene : UMIs %> Sum")
    expect_c_equal(Axis("cell") |> Lookup("age") |> CountBy("type"), "/ cell : age * type")
    expect_c_equal(Axis("cell") |> Lookup("age") |> GroupBy("type") |> Mean(), "/ cell : age @ type %> Mean")
    expect_c_equal(Axis("cell") |> Axis("gene") |> Lookup("UMIs") |> GroupBy("type") |> Max(), "/ cell / gene : UMIs @ type %> Max")
    expect_c_equal(Axis("gene") |> And("is_marker"), "/ gene & is_marker")
    expect_c_equal(Axis("gene") |> AndNot("is_marker"), "/ gene &! is_marker")
    expect_c_equal(Axis("gene") |> And("is_marker") |> Or("is_noisy"), "/ gene & is_marker | is_noisy")
    expect_c_equal(Axis("gene") |> And("is_marker") |> OrNot("is_noisy"), "/ gene & is_marker |! is_noisy")
    expect_c_equal(Axis("gene") |> And("is_marker") |> Xor("is_noisy"), "/ gene & is_marker ^ is_noisy")
    expect_c_equal(Axis("gene") |> And("is_marker") |> XorNot("is_noisy"), "/ gene & is_marker ^! is_noisy")
    expect_c_equal(Axis("cell") |> Axis("gene") |> IsEqual("FOX1") |> Lookup("UMIs"), "/ cell / gene = FOX1 : UMIs")
    expect_c_equal(Axis("cell") |> And("age") |> IsEqual(1), "/ cell & age = 1.0")
    expect_c_equal(Axis("cell") |> And("age") |> IsNotEqual(1), "/ cell & age != 1.0")
    expect_c_equal(Axis("cell") |> And("age") |> IsLess(1), "/ cell & age < 1.0")
    expect_c_equal(Axis("cell") |> And("age") |> IsLessEqual(1), "/ cell & age <= 1.0")
    expect_c_equal(Axis("cell") |> And("age") |> IsGreater(1), "/ cell & age > 1.0")
    expect_c_equal(Axis("cell") |> And("age") |> IsGreaterEqual(1), "/ cell & age >= 1.0")
    expect_c_equal(Axis("gene") |> And("name") |> IsMatch("RP[SL]"), "/ gene & name ~ RP\\[SL\\]")
    expect_c_equal(Axis("gene") |> And("name") |> IsNotMatch("RP[SL]"), "/ gene & name !~ RP\\[SL\\]")
})

test_that("IfMissing handles all parameter cases correctly", {
    # Setup a daf for testing
    daf <- memory_daf(name = "test_if_missing")
    set_scalar(daf, "version", "1.0")
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # Create a query using Axis to be used as input
    gene_query <- Axis("gene")

    # Case 1: missing_value is a Julia object, no dots
    # query=missing_value, missing_value=type, type=NULL
    expect_c_equal(
        IfMissing(gene_query, FALSE),
        "/ gene || false Bool"
    )

    # Case 2: missing_value is a Julia object, with dots
    # query=missing_value, missing_value=dots[[1]], type=type
    expect_c_equal(
        IfMissing(gene_query, type = "Int64", FALSE),
        "/ gene || false Int64"
    )

    # Case 3: missing_value explicit, query in dots
    # query=dots[[1]], missing_value=missing_value, type=type
    expect_c_equal(
        IfMissing(FALSE, type = "Bool", gene_query),
        "/ gene || false Bool"
    )

    # Case 4: missing_value explicit, no Julia object in dots
    # query=NULL, missing_value=missing_value, type=type
    expect_c_equal(
        IfMissing(FALSE, type = "Bool"),
        "|| false Bool"
    )

    # Case 5: missing_value is not a Julia object, no dots
    # query=NULL, missing_value=missing_value, type=type
    expect_c_equal(
        IfMissing("default", type = "String"),
        "|| default String"
    )

    # Test with real queries in action
    if (has_query(daf, "/ gene : is_marker")) {
        # Skip if property already exists
        skip("gene already has is_marker property in test")
    } else {
        # Test actual query results with IfMissing
        result <- get_query(daf, Axis("gene") |> Lookup("is_marker") |> IfMissing(FALSE))
        expect_equal(result, rep(FALSE, 3))

        # Set the property for one gene
        set_vector(daf, "gene", "is_marker", c(TRUE, FALSE, FALSE))

        # Now the result should reflect the actual values
        result <- get_query(daf, Axis("gene") |> Lookup("is_marker") |> IfMissing(FALSE))
        expect_equal(result, c(TRUE, FALSE, FALSE))
    }
})

# Test query results
test_that("query results are correct", {
    daf <- memory_daf(name = "test!")
    set_scalar(daf, "version", "1.0")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    add_axis(daf, "batch", c("U", "V", "W"))
    set_vector(daf, "cell", "batch", c("U", "V"))
    set_vector(daf, "cell", "age", c(-1.0, 2.0))
    set_vector(daf, "batch", "sex", c("Male", "Female", "Male"))
    set_matrix(daf, "gene", "cell", "UMIs", matrix(c(1, 4, 2, 5, 3, 6), nrow = 3, ncol = 2))

    # Test query result dimensions
    expect_equal(query_result_dimensions(": version"), 0)
    expect_equal(query_result_dimensions("/ cell : age"), 1)
    expect_equal(query_result_dimensions("/ cell / gene : UMIs"), 2)

    # Test scalar queries
    expect_equal(get_query(daf, ": version"), "1.0")
    expect_equal(set(get_query(daf, "? scalars")), set(c("version")))
    expect_equal(set(get_query(daf, "/ cell ?")), set(c("batch", "age")))
    expect_equal(set(get_query(daf, "/ gene / cell ?")), set(c("UMIs")))

    # Test has_query
    expect_true(has_query(daf, "/ cell : age"))
    expect_false(has_query(daf, "/ cell : youth"))

    # Test direct vector queries
    expect_equal(get_query(daf, "/ cell : age"), c(-1, 2))
    expect_equal(get_query(daf, "/ cell : batch"), c("U", "V"))

    # Test direct matrix queries
    expect_equal(get_query(daf, "/ cell / gene : UMIs"), matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3, byrow = TRUE))

    # Test query operations
    expect_equal(get_query(daf, "/ cell : age % Abs"), c(1.0, 2.0))
    expect_equal(get_query(daf, "/ cell : age % Clamp min 0.5"), c(0.5, 2.0))
    expect_equal(get_query(daf, "/ cell : age % Convert type Int8"), c(-1L, 2L))
    expect_equal(get_query(daf, "/ cell : age % Abs % Fraction"), c(1 / 3, 2 / 3))
    expect_equal(get_query(daf, "/ cell : age % Abs % Log base 2"), c(0.0, 1.0))
    expect_equal(get_query(daf, "/ cell : age % Significant high 2"), c(0, 2))

    # Test reduction operations
    expect_equal(get_query(daf, "/ cell : age %> Max"), 2)
    expect_equal(get_query(daf, "/ cell : age %> Min"), -1)
    expect_equal(get_query(daf, "/ cell : age %> Median"), 0.5)
    expect_equal(get_query(daf, "/ cell : age %> Quantile p 0.5"), 0.5)
    expect_equal(get_query(daf, "/ cell : age %> Mean"), 0.5)
    expect_equal(get_query(daf, "/ cell : age %> Mean % Round"), 0)
    expect_equal(get_query(daf, "/ cell : age %> Std"), 1.5)
    expect_equal(get_query(daf, "/ cell : age %> StdN"), 3.0)
    expect_equal(get_query(daf, "/ cell : age %> Var"), 2.25)
    expect_equal(get_query(daf, "/ cell : age %> VarN"), 4.5)

    sparse_matrix <- Matrix::sparseMatrix(
        i = c(1, 1, 1, 2, 2),
        j = c(1, 2, 3, 1, 2),
        x = c(0, 1, 2, 3, 4),
        dims = c(2, 3)
    )
    rownames(sparse_matrix) <- c("A", "B")
    colnames(sparse_matrix) <- c("X", "Y", "Z")

    set_matrix(daf, "cell", "gene", "UMIs", sparse_matrix, overwrite = TRUE)
    m <- get_matrix(daf, "cell", "gene", "UMIs")
    expect_equal(m, sparse_matrix, ignore_attr = TRUE)
    frame <- get_dataframe_query(daf, "/ cell / gene : UMIs")
    expect_equal(as.matrix(frame), matrix(c(0, 1, 2, 3, 4, 0), nrow = 2, ncol = 3, byrow = TRUE), ignore_attr = TRUE)
    expect_equal(rownames(frame), c("A", "B"))
    expect_equal(colnames(frame), c("X", "Y", "Z"))

    frame <- get_dataframe(daf, "cell")
    expect_true(is.data.frame(frame))
    expect_equal(nrow(frame), 2)
    expect_true("age" %in% names(frame))
    expect_true("batch" %in% names(frame))

    frame <- get_dataframe(daf, "cell", "age")
    expect_true(is.data.frame(frame))
    expect_equal(names(frame), "age")
    expect_equal(nrow(frame), 2)

    frame <- get_dataframe(daf, "cell", list(age = ": age", sex = ": batch => sex"))
    expect_true(is.data.frame(frame))
    expect_equal(names(frame), c("age", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))

    frame <- get_dataframe(daf, "cell", list(
        age = "age",
        list(sex = ": batch => sex")
    ))
    expect_true(is.data.frame(frame))
    expect_equal(names(frame), c("age", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))
})

test_that("get_dataframe_query handles scalar query results correctly", {
    daf <- memory_daf(name = "test_df_query")
    set_scalar(daf, "version", "1.0")
    set_scalar(daf, "numeric_val", 42)

    # Test string scalar
    result <- get_dataframe_query(daf, ": version")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 1)
    expect_equal(result[1, 1], "1.0")
    expect_equal(colnames(result), "value")

    # Test numeric scalar
    result <- get_dataframe_query(daf, ": numeric_val")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), 1)
    expect_equal(result[1, 1], 42)
    expect_equal(colnames(result), "value")
})

test_that("get_dataframe_query handles vector query results correctly", {
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1.5, 2.5, 3.5))
    set_vector(daf, "cell", "type", c("T", "B", "NK"))

    # Test numeric vector
    result <- get_dataframe_query(daf, "/ cell : age")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 1)
    expect_equal(result$value, c(1.5, 2.5, 3.5))
    expect_equal(rownames(result), c("A", "B", "C"))

    # Test character vector
    result <- get_dataframe_query(daf, "/ cell : type")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(ncol(result), 1)
    expect_equal(result$value, c("T", "B", "NK"))
    expect_equal(rownames(result), c("A", "B", "C"))

    # Test vector with operations
    result <- get_dataframe_query(daf, "/ cell : age % Abs")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, c(1.5, 2.5, 3.5))
})

test_that("get_dataframe_query handles matrix query results correctly", {
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # Create a dense matrix
    dense_matrix <- matrix(1:6, nrow = 2, ncol = 3)
    rownames(dense_matrix) <- c("A", "B")
    colnames(dense_matrix) <- c("X", "Y", "Z")
    set_matrix(daf, "cell", "gene", "expression", dense_matrix)

    # Test dense matrix
    result <- get_dataframe_query(daf, "/ cell / gene : expression")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 3)
    expect_equal(rownames(result), c("A", "B"))
    expect_equal(colnames(result), c("X", "Y", "Z"))
    expect_equal(as.matrix(result), dense_matrix)

    # Create a sparse matrix
    sparse_matrix <- Matrix::sparseMatrix(
        i = c(1, 2),
        j = c(1, 3),
        x = c(10, 20),
        dims = c(2, 3)
    )
    rownames(sparse_matrix) <- c("A", "B")
    colnames(sparse_matrix) <- c("X", "Y", "Z")
    set_matrix(daf, "cell", "gene", "sparse_expression", sparse_matrix)

    # Test sparse matrix
    result <- get_dataframe_query(daf, "/ cell / gene : sparse_expression")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 2)
    expect_equal(ncol(result), 3)
    expect_equal(rownames(result), c("A", "B"))
    expect_equal(colnames(result), c("X", "Y", "Z"))
    expected <- matrix(c(10, 0, 0, 0, 0, 20), nrow = 2, byrow = TRUE)
    rownames(expected) <- c("A", "B")
    colnames(expected) <- c("X", "Y", "Z")
    expect_equal(as.matrix(result), expected)
})

test_that("get_dataframe_query handles sets of names correctly", {
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))

    # Test axes names
    result <- get_dataframe_query(daf, "? axes")
    expect_true(is.character(result)) # This will return a character vector per your implementation
    expect_true(all(c("cell", "gene") %in% result))

    # Test vector properties
    set_vector(daf, "cell", "age", c(1, 2, 3))
    set_vector(daf, "cell", "type", c("T", "B", "NK"))

    result <- get_dataframe_query(daf, "/ cell ?")
    expect_true(is.character(result))
    expect_true(all(c("age", "type") %in% result))
})

test_that("get_dataframe_query can be used with pipes", {
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1, 2, 3))

    # Test with query first
    result <- "/ cell : age" |> get_dataframe_query(daf)
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, c(1, 2, 3))

    # Test with daf first
    result <- daf |> get_dataframe_query("/ cell : age")
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, c(1, 2, 3))
})

test_that("get_dataframe_query handles complex query operations", {
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    add_axis(daf, "batch", c("B1", "B2"))
    add_axis(daf, "donor", c("D1", "D2"))
    set_vector(daf, "cell", "age", c(1, 2, 3))
    set_vector(daf, "cell", "batch", c("B1", "B2", "B1"))
    set_vector(daf, "batch", "donor", c("D1", "D2"))

    # Test complex vector query with fetch
    result <- get_dataframe_query(daf, Axis("cell") |> Lookup("batch") |> Fetch("donor"))
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, c("D1", "D2", "D1"))

    # Test with filtering
    result <- get_dataframe_query(daf, Axis("cell") |> And("batch") |> IsEqual("B1") |> Lookup("age"))
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 2)
    expect_equal(result$value, c(1, 3))

    # Test with element-wise operations
    result <- get_dataframe_query(daf, Axis("cell") |> Lookup("age") |> Log(base = 2))
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)
    expect_equal(result$value, log2(c(1, 2, 3)), tolerance = 1e-10)
})

test_that("get_dataframe_query handles missing data appropriately", {
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))

    # Test with missing vector
    expect_error(result <- get_dataframe_query(daf, Axis("cell") |> Lookup("missing") |> IfMissing(NA)))
    result <- get_dataframe_query(daf, Axis("cell") |> Lookup("missing") |> IfMissing(8))
    expect_true(is.data.frame(result))
    expect_equal(nrow(result), 3)

    expect_equal(result$value, c(8, 8, 8))
    # Test with partial missing data
    expect_error(set_vector(daf, "cell", "partial", c(1, NA, 3)))
})

test_that("get_dataframe_query handles caching parameter", {
    daf <- memory_daf(name = "test_df_query")
    add_axis(daf, "cell", c("A", "B", "C"))
    set_vector(daf, "cell", "age", c(1, 2, 3))

    # Should work the same with cache=TRUE and cache=FALSE
    result1 <- get_dataframe_query(daf, "/ cell : age", cache = TRUE)
    result2 <- get_dataframe_query(daf, "/ cell : age", cache = FALSE)

    expect_equal(result1, result2)
})

test_that("get_dataframe_query reports appropriate errors", {
    daf <- memory_daf(name = "test_df_query")

    expect_error(get_dataframe_query(daf), "Please provide both a Daf object and a query")
    expect_error(get_dataframe_query(query = "/ missing_axis : value"), "Please provide both a Daf object and a query")

    add_axis(daf, "cell", c("A", "B", "C"))
    expect_error(get_dataframe_query(daf, "/ missing_axis : value"))
})

test_that("process_frame_columns handles different column formats correctly", {
    # Test with NULL
    result <- process_frame_columns(NULL)
    expect_null(result)

    # Test with single string
    columns <- "age"
    result <- process_frame_columns(columns)
    expect_equal(length(result), 1)

    # Simple named list/vector
    columns <- list(age = "age", batch = "batch")
    result <- process_frame_columns(columns)

    # Named list with nested lists
    columns <- list(
        age = "age",
        list(sex = ": batch => sex")
    )
    result <- process_frame_columns(columns)

    # Complex mixed format
    columns <- list(
        age = "age",
        list(sex = ": batch => sex"),
        batch = "batch"
    )
    result <- process_frame_columns(columns)
})

test_that("get_dataframe handles various column formats correctly", {
    daf <- memory_daf(name = "test_frame_columns")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "batch", c("U", "V", "W"))
    set_vector(daf, "cell", "batch", c("U", "V"))
    set_vector(daf, "cell", "age", c(1.0, 2.0))
    set_vector(daf, "batch", "sex", c("Male", "Female", "Male"))

    # Test with NULL columns (should return all columns)
    frame <- get_dataframe(daf, "cell")
    expect_true(is.data.frame(frame))
    expect_equal(nrow(frame), 2)
    expect_true("age" %in% names(frame))
    expect_true("batch" %in% names(frame))

    # Test with single string
    frame <- get_dataframe(daf, "cell", "age")
    expect_true(is.data.frame(frame))
    expect_equal(names(frame), "age")
    expect_equal(nrow(frame), 2)

    # Test with simple named list
    frame <- get_dataframe(daf, "cell", list(age = "age", batch = "batch"))
    expect_true(is.data.frame(frame))
    expect_equal(sort(names(frame)), c("age", "batch"))
    expect_equal(nrow(frame), 2)

    # Test with nested lists in columns parameter
    frame <- get_dataframe(daf, "cell", list(
        age = "age",
        list(sex = ": batch => sex")
    ))
    expect_true(is.data.frame(frame))
    expect_equal(sort(names(frame)), c("age", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))

    # Test with complex mixed format
    frame <- get_dataframe(daf, "cell", list(
        age = "age",
        list(sex = ": batch => sex"),
        batch = "batch"
    ))
    expect_true(is.data.frame(frame))
    expect_equal(sort(names(frame)), c("age", "batch", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))

    # Test with query objects
    frame <- get_dataframe(daf, "cell", list(
        age = Lookup("age"),
        list(sex = Lookup("batch") |> Fetch("sex"))
    ))
    expect_true(is.data.frame(frame))
    expect_equal(sort(names(frame)), c("age", "sex"))
    expect_equal(frame$sex, c("Male", "Female"))
})

test_that("get_tidy returns data in long format", {
    daf <- memory_daf(name = "test_tidy")
    add_axis(daf, "cell", c("A", "B", "C"))
    add_axis(daf, "batch", c("B1", "B2"))
    set_vector(daf, "cell", "age", c(1, 2, 3))
    set_vector(daf, "cell", "batch", c("B1", "B2", "B1"))
    set_vector(daf, "batch", "sex", c("Male", "Female"))

    # Test with all columns
    result <- get_tidy(daf, "cell", values_transform = list(value = as.character))
    expect_true(tibble::is_tibble(result))
    expect_equal(names(result), c("name", "key", "value"))
    expect_equal(nrow(result), 6) # 3 cells * 2 columns (age, batch)
    expect_equal(unique(result$name), c("A", "B", "C"))
    expect_equal(unique(result$key), c("age", "batch"))
    expect_equal(result$value[result$name == "A" & result$key == "age"], "1")
    expect_equal(result$value[result$name == "B" & result$key == "batch"], "B2")

    # Test with specific columns
    result <- get_tidy(daf, "cell", list(age = "age", sex = ": batch => sex"), values_transform = list(value = as.character))
    expect_true(tibble::is_tibble(result))
    expect_equal(names(result), c("name", "key", "value"))
    expect_equal(nrow(result), 6) # 3 cells * 2 columns (age, sex)
    expect_equal(unique(result$key), c("age", "sex"))
    expect_equal(result$value[result$name == "A" & result$key == "sex"], "Male")
    expect_equal(result$value[result$name == "B" & result$key == "sex"], "Female")

    # Test with matrix data
    add_axis(daf, "gene", c("G1", "G2"))
    set_matrix(daf, "cell", "gene", "expression", matrix(1:6, nrow = 3, ncol = 2))

    # Test matrix data by getting expression for each gene separately
    result <- get_tidy(daf, "cell", list(
        age = "age",
        g1_expr = "/ gene = G1 : expression",
        g2_expr = "/ gene = G2 : expression"
    ), values_transform = list(value = as.character))

    expect_true(tibble::is_tibble(result))
    expect_equal(names(result), c("name", "key", "value"))
    expect_equal(nrow(result), 9) # 3 cells * 3 columns (age, g1_expr, g2_expr)
    expect_equal(unique(result$key), c("age", "g1_expr", "g2_expr"))
    expect_equal(result$value[result$name == "A" & result$key == "g1_expr"], "1")
    expect_equal(result$value[result$name == "B" & result$key == "g2_expr"], "5")
})

# Test the new [ operator for Daf objects
test_that("[ operator for Daf objects works correctly", {
    daf <- memory_daf(name = "test_subscript_operator")
    set_scalar(daf, "version", "1.0")
    add_axis(daf, "cell", c("A", "B"))
    add_axis(daf, "gene", c("X", "Y", "Z"))
    set_vector(daf, "cell", "batch", c("U", "V"))
    set_vector(daf, "cell", "age", c(-1.0, 2.0))
    set_matrix(daf, "gene", "cell", "UMIs", matrix(c(1, 4, 2, 5, 3, 6), nrow = 3, ncol = 2))

    # Test scalar queries
    expect_equal(daf[": version"], "1.0")
    expect_equal(daf[": version"], get_query(daf, ": version", cache = FALSE))

    # Test axes and property sets
    expect_equal(set(daf["? scalars"]), set(c("version")))
    expect_equal(set(daf["/ cell ?"]), set(c("batch", "age")))
    expect_equal(set(daf["/ gene / cell ?"]), set(c("UMIs")))

    # Test vector queries
    expect_equal(daf["/ cell : age"], c(-1, 2))
    expect_equal(daf["/ cell : batch"], c("U", "V"))
    expect_equal(daf["/ cell : age"], get_query(daf, "/ cell : age", cache = FALSE))

    # Test matrix queries
    expect_equal(daf["/ cell / gene : UMIs"], matrix(c(1, 4, 2, 5, 3, 6), nrow = 2, ncol = 3, byrow = TRUE))
    expect_equal(daf["/ cell / gene : UMIs"], get_query(daf, "/ cell / gene : UMIs", cache = FALSE))

    # Test with query operations
    expect_equal(daf["/ cell : age % Abs"], c(1.0, 2.0))
    expect_equal(daf["/ cell : age %> Max"], 2)
    expect_equal(daf["/ cell : age %> Min"], -1)

    # Test with query objects
    age_query <- Axis("cell") |> Lookup("age")
    expect_equal(daf[age_query], c(-1, 2))
    expect_equal(daf[age_query], get_query(daf, age_query, cache = FALSE))

    # Test with complex query objects
    complex_query <- Axis("cell") |>
        Lookup("age") |>
        Abs() |>
        Log(base = 2)
    expect_equal(daf[complex_query], log2(c(1, 2)))
    expect_equal(daf[complex_query], get_query(daf, complex_query, cache = FALSE))

    # Test caching behavior by modifying data then querying again
    result1 <- daf["/ cell : age"]
    set_vector(daf, "cell", "age", c(3.0, 4.0), overwrite = TRUE)
    result2 <- daf["/ cell : age"]
    expect_false(identical(result1, result2))
    expect_equal(result2, c(3.0, 4.0))
})

test_that("[ operator handles errors properly", {
    daf <- memory_daf(name = "test_subscript_errors")

    # Test with invalid query
    expect_error(daf["invalid query"])

    # Test with non-existent axis
    expect_error(daf["/ non_existent_axis : property"])

    # Test with non-existent property
    add_axis(daf, "cell", c("A", "B"))
    expect_error(daf["/ cell : non_existent_property"])
})
