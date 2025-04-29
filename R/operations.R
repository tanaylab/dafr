#' Query operations for DataAxesFormats
#'
#' @description
#' A `Daf` query can use operations to process the data: Element-wise operations that preserve
#' the shape of the data, and Reduction operations that reduce a matrix to a vector, or a vector to a scalar.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/operations.html) for details.
#'

# Mathematical operations

#' @title Abs query operation
#' @description
#' Element-wise operation that converts every element to its absolute value. This operation preserves
#' the shape of the data (scalar, vector, or matrix) but changes the values. By default, the output
#' data type is the unsigned version of the input data type. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Abs) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Abs <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Abs} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Abs")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Clamp query operation
#' @description
#' Element-wise operation that converts every element to a value inside a range.
#' This operation preserves the shape of the data (scalar, vector, or matrix) but changes
#' the values, setting each value to be within the specified range. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Clamp) for
#' details.
#' @param min Minimum value; values less than this will be set to this value
#' @param max Maximum value; values greater than this will be set to this value
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Clamp <- function(min = NULL, max = NULL, ...) {
    min_res <- extract_query_and_value(min, missing(min), list(...), required = FALSE)
    max_res <- extract_query_and_value(max, missing(max), list(...), required = FALSE)

    # Determine which one has the query (if any)
    query <- min_res$query
    if (is.null(query)) {
        query <- max_res$query
    }

    # Validation
    if (!is.null(min_res$value) && !is.numeric(min_res$value)) {
        cli::cli_abort("{.field min} must be numeric or NULL")
    }
    if (!is.null(max_res$value) && !is.numeric(max_res$value)) {
        cli::cli_abort("{.field max} must be numeric or NULL")
    }

    # Pass arguments by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Clamp", min = min_res$value, max = max_res$value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }
    return(result)
}

#' @title Convert query operation
#' @description
#' Element-wise operation that converts every element to a given data type.
#' This operation preserves the shape of the data (scalar, vector, or matrix) but
#' changes the data type of the elements. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Convert) for
#' details.
#' @param type Type to convert to (e.g., "Int32", "Float64")
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Convert <- function(type, ...) {
    res <- extract_query_and_value(type, missing(type), list(...),
        required = TRUE
    )
    if (!res$provided) {
        cli::cli_abort("{.field type} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field type} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Operations.Convert", type = res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Fraction query operation
#' @description
#' Element-wise operation that converts every element to its fraction out of the total.
#' This operation preserves the shape of the data (scalar, vector, or matrix) but changes
#' each value to be the fraction of the total sum of all values in the data. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Fraction) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Fraction <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Fraction} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Fraction")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Log query operation
#' @description
#' Element-wise operation that converts every element to its logarithm.
#' This operation preserves the shape of the data (scalar, vector, or matrix) but
#' changes each value to its logarithm with the specified base. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Log) for
#' details.
#' @param base Base of the logarithm (default is e ≈ 2.718)
#' @param eps Small value added to avoid log(0) (default is 0)
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Log <- function(base = exp(1), eps = 0, ...) {
    base_res <- extract_query_and_value(base, missing(base), list(...), required = FALSE, default = exp(1))
    eps_res <- extract_query_and_value(eps, missing(eps), list(...), required = FALSE, default = 0)

    # Determine which one has the query (if any)
    query <- base_res$query
    if (is.null(query)) {
        query <- eps_res$query
    }

    # Validation
    if (!is.numeric(base_res$value)) {
        cli::cli_abort("{.field base} must be numeric")
    }
    if (!is.numeric(eps_res$value)) {
        cli::cli_abort("{.field eps} must be numeric")
    }

    # Pass arguments by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Log", base = base_res$value, eps = eps_res$value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }
    return(result)
}

#' @title Round query operation
#' @description
#' Element-wise operation that converts every element to the nearest integer value.
#' This operation preserves the shape of the data (scalar, vector, or matrix) but
#' rounds each value to the nearest integer. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Round) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Round <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Round} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Round")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Significant query operation
#' @description
#' Element-wise operation that zeros all "insignificant" values. Significant values have
#' a high absolute value. This is typically used to prune matrices of effect sizes (log
#' of ratio between a baseline and some result) for heatmap display.
#'
#' For example, log base 2 of gene expression ratio is typically considered significant
#' if it is at least 3 (that is, a ratio at least 8x or at most 1/8x); for genes that
#' have a significant effect, we typically display all entries with a log of at least 2
#' (that is, a ratio of at least 4x or at most 1/4x). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Significant) for
#' details.
#' @param high A value is considered significant if its absolute value is higher than this.
#'   If all values in a vector (or a matrix column) are less than this, then all the vector
#'   (or matrix column) entries are zeroed. There's no default.
#' @param low If there is at least one significant value in a vector (or a matrix column),
#'   then zero all entries that are lower than this. By default, this is the same as the `high` value.
#'   Setting it to a lower value will preserve more entries, but only for vectors (or matrix columns)
#'   which contain at least some significant data.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Significant <- function(high, low = NULL, ...) {
    high_res <- extract_query_and_value(high, missing(high), list(...), required = TRUE)
    low_res <- extract_query_and_value(low, missing(low), list(...), required = FALSE)

    # Check if high was provided
    if (!high_res$provided) {
        cli::cli_abort("argument {.field high} must be provided")
    }

    # Set default for low if NULL
    actual_low <- low_res$value
    if (is.null(actual_low)) {
        actual_low <- high_res$value # Default low is high
    }

    # Validation
    if (!is.numeric(high_res$value)) {
        cli::cli_abort("{.field high} must be numeric")
    }
    if (!is.numeric(actual_low)) {
        cli::cli_abort("{.field low} must be numeric")
    }

    # Use query from either high or low
    query <- high_res$query
    if (is.null(query)) {
        query <- low_res$query
    }

    # Pass arguments by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Significant", high = high_res$value, low = actual_low)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }
    return(result)
}

# Reduction operations

#' @title Max query operation
#' @description
#' Reduction operation that returns the maximal element. This operation reduces the
#' dimensionality of the data: a matrix becomes a vector (maximum of each column),
#' and a vector becomes a scalar (maximum of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Max) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Max <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Max} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Max")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Min query operation
#' @description
#' Reduction operation that returns the minimal element. This operation reduces the
#' dimensionality of the data: a matrix becomes a vector (minimum of each column),
#' and a vector becomes a scalar (minimum of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Min) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Min <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Min} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Min")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Mean query operation
#' @description
#' Reduction operation that returns the mean value. This operation reduces the
#' dimensionality of the data: a matrix becomes a vector (mean of each column),
#' and a vector becomes a scalar (mean of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Mean) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Mean <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Mean} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Mean")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Median query operation
#' @description
#' Reduction operation that returns the median value. This operation reduces the
#' dimensionality of the data: a matrix becomes a vector (median of each column),
#' and a vector becomes a scalar (median of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Median) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Median <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Median} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Median")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Quantile query operation
#' @description
#' Reduction operation that returns the quantile value, that is, a value such that
#' a certain fraction of the values is lower. This operation reduces the
#' dimensionality of the data: a matrix becomes a vector (quantile of each column),
#' and a vector becomes a scalar (quantile of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Quantile) for
#' details.
#' @param p Quantile to compute (between 0 and 1). Default is 0.5 (median).
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Quantile <- function(p = 0.5, ...) {
    res <- extract_query_and_value(p, missing(p), list(...), required = FALSE, default = 0.5)

    # Validation
    if (!is.numeric(res$value) || length(res$value) != 1 || res$value < 0 || res$value > 1) {
        cli::cli_abort("{.field p} must be a single numeric value between 0 and 1")
    }

    # Pass argument by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Quantile", p = res$value)

    if (!is.null(res$query)) {
        result <- julia_call("|>", res$query, result)
    }
    return(result)
}

#' @title Sum query operation
#' @description
#' Reduction operation that sums elements. This operation reduces the
#' dimensionality of the data: a matrix becomes a vector (sum of each column),
#' and a vector becomes a scalar (sum of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Sum) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Sum <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Sum} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Sum")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Std query operation
#' @description
#' Reduction operation that returns the standard deviation of the values.
#' This operation reduces the dimensionality of the data: a matrix becomes
#' a vector (standard deviation of each column), and a vector becomes a scalar
#' (standard deviation of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Std) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Std <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Std} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Std")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title StdN query operation
#' @description
#' Reduction operation that returns the standard deviation of the values, normalized
#' (divided) by the mean of the values. This operation reduces the dimensionality
#' of the data: a matrix becomes a vector (normalized standard deviation of each column),
#' and a vector becomes a scalar (normalized standard deviation of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Std) for details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
StdN <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code StdN} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.StdN")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Var query operation
#' @description
#' Reduction operation that returns the variance of the values.
#' This operation reduces the dimensionality of the data: a matrix becomes
#' a vector (variance of each column), and a vector becomes a scalar
#' (variance of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Var) for
#' details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Var <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code Var} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.Var")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title VarN query operation
#' @description
#' Reduction operation that returns the variance of the values, normalized (divided)
#' by the mean of the values. This operation reduces the dimensionality of the data:
#' a matrix becomes a vector (normalized variance of each column), and a vector becomes
#' a scalar (normalized variance of all elements). See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.Var)
#' for details.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
VarN <- function(...) {
    res <- extract_query_and_value(NULL, TRUE, list(...), required = FALSE)
    if (length(list(...)) > 1 || (!is.null(res$query) && length(list(...)) > 1)) {
        cli::cli_abort("{.code VarN} expects zero arguments or one query object")
    }

    ans <- julia_call("DataAxesFormats.Operations.VarN")
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}
