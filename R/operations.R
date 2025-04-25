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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Abs", "Abs")
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
    dots <- list(...)
    query <- NULL
    actual_min <- min
    actual_max <- max

    # Check if query is passed via dots
    dots_has_query <- length(dots) > 0 && inherits(dots[[1]], "JuliaObject")
    if (dots_has_query) {
        query <- dots[[1]]
        # If query is in dots, min/max come from the named args
    } else {
        # Check if query was passed positionally (e.g., Clamp(query, min=1)) - less likely
        # Or if min/max themselves are query objects (Clamp(min=query, max=1) or Clamp(query))
        min_missing <- missing(min)
        max_missing <- missing(max)

        if (!min_missing && inherits(min, "JuliaObject")) {
            query <- min
            actual_min <- NULL # min was query, reset to default
            # If min was query, max must be from named arg or default
        } else if (!max_missing && inherits(max, "JuliaObject")) {
            query <- max
            actual_max <- NULL # max was query, reset to default
            # If max was query, min must be from named arg or default
        }
        # If neither min nor max was a query, actual values remain as passed (or defaults)
    }

    # Validation (optional args)
    if (!is.null(actual_min) && !is.numeric(actual_min)) {
        cli::cli_abort("{.field min} must be numeric or NULL")
    }
    if (!is.null(actual_max) && !is.numeric(actual_max)) {
        cli::cli_abort("{.field max} must be numeric or NULL")
    }

    # Pass arguments by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Clamp", min = actual_min, max = actual_max)

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
    # Use the helper defined in utils.R
    dots <- list(...)

    # Handle pipe and validation using the combined helper
    res <- handle_query_pipe_and_validate(type, missing(type), dots, "type",
        type_check_fun = is.character,
        type_error_msg = "{.field type} must be a character string"
    )

    # Pass argument by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Convert", type = res$value)

    if (!is.null(res$query)) {
        result <- julia_call("|>", res$query, result)
    }
    return(result)
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Fraction", "Fraction")
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
    dots <- list(...)
    query <- NULL
    actual_base <- base
    actual_eps <- eps

    # Check if query is passed via dots
    dots_has_query <- length(dots) > 0 && inherits(dots[[1]], "JuliaObject")
    if (dots_has_query) {
        query <- dots[[1]]
        # If query is in dots, base/eps come from the named args
    } else {
        # Check if base or eps themselves are query objects
        base_missing <- missing(base)
        eps_missing <- missing(eps)

        if (!base_missing && inherits(base, "JuliaObject")) {
            query <- base
            actual_base <- exp(1) # base was query, reset to default
        } else if (!eps_missing && inherits(eps, "JuliaObject")) {
            query <- eps
            actual_eps <- 0 # eps was query, reset to default
        }
    }

    # Validation
    if (!is.numeric(actual_base)) {
        cli::cli_abort("{.field base} must be numeric")
    }
    if (!is.numeric(actual_eps)) {
        cli::cli_abort("{.field eps} must be numeric")
    }

    # Pass arguments by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Log", base = actual_base, eps = actual_eps)

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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Round", "Round")
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
    dots <- list(...)
    query <- NULL
    actual_high <- high
    actual_low <- low
    high_missing <- missing(high)

    # Handle pipe logic - query can be 'high' or in dots
    high_is_query <- !high_missing && inherits(high, "JuliaObject")
    dots_has_query <- length(dots) > 0 && inherits(dots[[1]], "JuliaObject")
    dots_has_high_val <- length(dots) > 0 && !inherits(dots[[1]], "JuliaObject")
    dots_has_low_val <- length(dots) > 1 && !inherits(dots[[2]], "JuliaObject")

    if (high_is_query) {
        query <- high
        if (dots_has_high_val) {
            actual_high <- dots[[1]]
            if (dots_has_low_val) {
                actual_low <- dots[[2]] # low provided in dots[2]
            } else {
                actual_low <- NULL # low not provided after high in dots, use default
            }
        } else {
            # high was query, but no replacement value found in dots[1]
            cli::cli_abort("argument {.field high} is missing when passed after query object")
        }
    } else if (dots_has_query) {
        query <- dots[[1]]
        # high and low come from named args
        actual_high <- high
        actual_low <- low
        # Check if high was actually provided
        if (high_missing) {
            cli::cli_abort("argument {.field high} must be provided")
        }
    } else {
        # No query pipe involved
        # Check if high was provided
        if (high_missing) {
            cli::cli_abort("argument {.field high} must be provided")
        }
        actual_high <- high
        actual_low <- low
    }

    # Set default for low if NULL
    if (is.null(actual_low)) {
        actual_low <- actual_high # Default low is high
    }

    # Validation
    if (!is.numeric(actual_high)) {
        cli::cli_abort("{.field high} must be numeric")
    }
    if (!is.numeric(actual_low)) {
        cli::cli_abort("{.field low} must be numeric")
    }

    # Pass arguments by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Significant", high = actual_high, low = actual_low)

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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Max", "Max")
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Min", "Min")
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Mean", "Mean")
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Median", "Median")
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
    dots <- list(...)
    query <- NULL
    actual_p <- p
    p_missing <- missing(p)

    # Handle pipe logic for optional argument 'p'
    p_is_query <- !p_missing && inherits(p, "JuliaObject")
    dots_has_query <- length(dots) > 0 && inherits(dots[[1]], "JuliaObject")
    dots_has_p_val <- length(dots) > 0 && !inherits(dots[[1]], "JuliaObject")

    if (p_is_query) {
        query <- p
        if (dots_has_p_val) {
            actual_p <- dots[[1]]
        } else {
            actual_p <- 0.5 # p was query, no replacement in dots, reset to default
        }
    } else if (dots_has_query) {
        query <- dots[[1]]
        actual_p <- p # Use value from named arg (or its default)
    } else {
        # No query pipe
        actual_p <- p # Use value from named arg (or its default)
    }

    # Validation
    if (!is.numeric(actual_p) || length(actual_p) != 1 || actual_p < 0 || actual_p > 1) {
        cli::cli_abort("{.field p} must be a single numeric value between 0 and 1")
    }

    # Pass argument by name to Julia
    result <- julia_call("DataAxesFormats.Operations.Quantile", p = actual_p)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Sum", "Sum")
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Std", "Std")
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.StdN", "StdN")
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.Var", "Var")
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
    handle_parameterless_operation_pipe(list(...), "DataAxesFormats.Operations.VarN", "VarN")
}
