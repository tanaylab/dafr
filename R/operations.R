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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Abs")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    } else if (inherits(min, "JuliaObject")) {
        query <- min
        min <- NULL
    } else if (inherits(max, "JuliaObject")) {
        query <- max
        max <- NULL
    }

    result <- julia_call("DataAxesFormats.Operations.Clamp", min = min, max = max)

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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(type, "JuliaObject")) {
            query <- type
            type <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Convert", type = type)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Fraction")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }
    if (inherits(base, "JuliaObject")) {
        query <- base
        base <- exp(1)
    }
    if (inherits(eps, "JuliaObject")) {
        query <- eps
        eps <- 0
    }
    result <- julia_call("DataAxesFormats.Operations.Log", base = base, eps = eps)

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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Round")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    if (length(dots) > 0) {
        if (inherits(high, "JuliaObject")) {
            query <- high
            high <- dots[[1]]
            if (length(dots) > 1) {
                low <- dots[[2]]
            }
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (is.null(low)) {
        low <- high
    }

    result <- julia_call("DataAxesFormats.Operations.Significant", high = high, low = low)

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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Max")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Min")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Mean")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Median")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    if (length(dots) > 0) {
        if (inherits(p, "JuliaObject")) {
            query <- p
            p <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Quantile", p = p)

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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Sum")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Std")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.StdN")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.Var")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
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
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Operations.VarN")

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}
