#' Check if a query can be applied to a Daf object
#'
#' @param daf A Daf object
#' @param query Query string or object. Can be created using query operations such as
#'   Axis(), Lookup(), IsGreater(), etc.
#' @return TRUE if query can be applied, FALSE otherwise
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.has_query) for details.
#' @seealso Axis, Lookup, Names, IsEqual, IsGreater, IsLess, and other query operations.
#' @export
has_query <- function(daf, query) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.Queries.has_query", daf$jl_obj, query)
}

#' Apply a query to a Daf object
#'
#' @param daf A Daf object
#' @param query Query string or object. Can be created using query operations such as
#'   Axis(), Lookup(), IsGreater(), etc.
#' In order to support the use of pipe operators, the query can also be a Daf object and vice versa, see examples below.
#' @param cache Whether to cache the query result
#' @return The result of the query
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.get_query) for details.
#' @seealso Axis, Lookup, Names, IsEqual, IsGreater, IsLess, and other query operations.
#' @export
get_query <- function(daf = NULL, query = NULL, cache = TRUE) {
    if (is.null(daf) || is.null(query)) {
        cli::cli_abort("Please provide both a Daf object and a query.")
    }

    daf_arg <- daf
    query_arg <- query
    if (inherits(query, "Daf")) {
        daf <- query_arg
        query <- daf_arg
    }

    validate_daf_object(daf)
    result <- julia_call("DataAxesFormats.Queries.get_query", daf$jl_obj, query, cache = cache)

    return(from_julia_object(result))
}

#' Apply a query to a Daf object and return result as a data.frame
#'
#' @param daf A Daf object
#' @param query Query string or object. Can be created using query operations such as
#'   Axis(), Lookup(), IsGreater(), etc.
#' In order to support the use of pipe operators, the query can also be a Daf object and vice versa.
#' @param cache Whether to cache the query result
#' @return A data.frame representation of the query result. If the query result is a matrix, row names and column names are the axis entries. If the query result is a scalar, a data.frame with one row and one column is returned. If the query result is a vector, a data.frame with a column "value" is returned.
#' @seealso get_query, get_frame
#' @export
get_dataframe_query <- function(daf = NULL, query = NULL, cache = TRUE) {
    if (is.null(daf) || is.null(query)) {
        cli::cli_abort("Please provide both a Daf object and a query.")
    }

    # Handle pipe operator usage
    daf_arg <- daf
    query_arg <- query
    if (inherits(query, "Daf")) {
        daf <- query_arg
        query <- daf_arg
    }

    validate_daf_object(daf)

    # Get dimensions to determine how to process the result
    dims <- query_result_dimensions(query)

    result <- julia_call("DataAxesFormats.Queries.get_query",
        daf$jl_obj,
        query,
        cache = cache,
        need_return = if (dims == 0) "R" else "Julia"
    )

    if (dims == 0) {
        return(data.frame(value = result, stringsAsFactors = FALSE))
    }

    if (dims == 1) {
        # For vectors, extract the values and names directly from the NamedVector
        values <- from_julia_array(result)

        # Extract the names using NamedArrays.names
        names <- julia_call("NamedArrays.names", result, as.integer(1), need_return = "R")

        # Create an R data frame with proper names
        df <- data.frame(
            value = values,
            stringsAsFactors = FALSE,
            row.names = names
        )

        return(df)
    } else if (dims == 2) {
        # For matrices, extract the values and both row and column names
        values <- from_julia_array(result)

        # Convert sparse matrix to dense if needed
        if (inherits(values, "sparseMatrix")) {
            values <- as.matrix(values)
        }

        # Extract row and column names
        row_names <- julia_call("NamedArrays.names", result, as.integer(1), need_return = "R")
        col_names <- julia_call("NamedArrays.names", result, as.integer(2), need_return = "R")

        # Create data frame with proper row and column names
        df <- as.data.frame(values, stringsAsFactors = FALSE)
        colnames(df) <- col_names
        rownames(df) <- row_names

        return(df)
    } else {
        # For scalar results or name sets, convert directly
        return(from_julia_object(result))
    }
}

#' Parse a query string into a query object
#'
#' @param query_string String containing the query
#' @return A query object
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html) for details.
#' @export
parse_query <- function(query_string) {
    julia_call("DataAxesFormats.Queries.Query", query_string)
}

#' Get the number of dimensions of a query result
#'
#' @param query Query string or object
#' @return Number of dimensions (-1 - names, 0 - scalar, 1 - vector, 2 - matrix)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html) for details.
#' @export
query_result_dimensions <- function(query) {
    julia_call("DataAxesFormats.Queries.query_result_dimensions", query)
}

#' @title Axis query operation
#' @description A query operation for specifying a result axis. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Axis) for details.
#' @param axis String specifying the axis
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
Axis <- function(axis, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(axis, "JuliaObject")) {
            query <- axis
            axis <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(axis)) {
        if (inherits(axis, "JuliaObject")) {
            cli::cli_abort("argument {.field axis} is missing with no default")
        } else {
            cli::cli_abort("{.field axis} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.Axis", axis)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title Lookup query operation
#' @description A query operation for looking up the value of a property with some name.
#' See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Lookup)
#' for details.
#' @param property String specifying the property name
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
Lookup <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.Lookup", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}


#' @title Names query operation
#' @description A query operation for looking up a set of names. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Names)
#' for details.
#' @param kind Optional string specifying the kind
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
Names <- function(kind = NULL, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(kind, "JuliaObject")) {
            query <- kind
            kind <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    } else if (inherits(kind, "JuliaObject")) {
        query <- kind
        kind <- NULL
    }

    result <- julia_call("DataAxesFormats.Queries.Names", kind)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title Fetch query operation
#' @description A query operation for fetching the value of a property from another axis, based on a vector property whose values
#' are entry names of the axis. See the
#' Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Fetch) for
#' details.
#' @param property String specifying the property name
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
Fetch <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.Fetch", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IfMissing query operation
#' @description A query operation providing a value to use if the data is missing some property. See the
#' Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IfMissing)
#' for details.
#' @param missing_value Value to use when data is missing
#' @param type Optional type specification
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IfMissing <- function(missing_value, type = NULL, ...) {
    dots <- list(...)
    query <- NULL
    real_missing_value <- missing_value
    real_type <- type

    # option 1: missing_value is a Julia object, the real missing value is the type argument and the type argument is the first element of the dots
    # in that case: query=missing_value, missing_value=type, type=dots[[1]] (if exists, otherwise NULL)
    # option 2: missing_value is a Julia object, 'type' was given explicitly and the real missing value is the first element of the dots
    # in that case: query=missing_value, missing_value=dots[[1]], type=type
    # option 3: missing_value was given explicitly and there is a Julia object in the dots (as the first element of the dots)
    # in that case: query=dots[[1]], missing_value=missing_value, type=type
    # option 4: missing_value was given explicitly and there is no Julia object in the dots
    # in that case: query=NULL, missing_value=missing_value, type=type
    # option 5: missing_value is not a Julia object
    # in that case: query=NULL, missing_value=missing_value, type=type

    if (inherits(missing_value, "JuliaObject")) {
        query <- missing_value
        if (length(dots) > 0) {
            # Option 2: query is missing_value, real_missing_value is dots[[1]]
            real_missing_value <- dots[[1]]
            real_type <- type
        } else {
            # Option 1: query is missing_value, real_missing_value is type
            real_missing_value <- type
            real_type <- NULL
        }
    } else if (length(dots) > 0 && inherits(dots[[1]], "JuliaObject")) {
        # Option 3: query is dots[[1]], real_missing_value is missing_value
        query <- dots[[1]]
        real_missing_value <- missing_value
        real_type <- type
    }
    # Otherwise it's option 4 or 5 - everything is already correct

    if (is.na(real_missing_value)) {
        cli::cli_abort("{.field missing_value} cannot be NA. See the Julia documentation for details.")
    }

    # Call the Julia function with appropriate parameters
    if (is.null(real_type)) {
        result <- julia_call("DataAxesFormats.Queries.IfMissing", real_missing_value)
    } else {
        result <- julia_call("DataAxesFormats.Queries.IfMissing", real_missing_value, jl_R_to_julia_type(real_type))
    }

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IfNot query operation
#' @description A query operation providing a value to use for "false-ish" values in a vector (empty strings, zero numeric values,
#' or false Boolean values). See the
#' Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IfNot) for
#' details.
#' @param value Optional value to use
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IfNot <- function(value = NULL, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    } else if (inherits(value, "JuliaObject")) {
        query <- value
        value <- NULL
    }

    result <- julia_call("DataAxesFormats.Queries.IfNot", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title AsAxis query operation
#' @description There are three cases where we may want to take a vector property and consider each value to be the name of an entry
#' of some axis: `Fetch`, `CountBy` and `GroupBy`. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.AsAxis) for
#' details.
#' @param axis Optional string specifying the axis
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
AsAxis <- function(axis = NULL, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(axis, "JuliaObject")) {
            query <- axis
            axis <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    } else if (inherits(axis, "JuliaObject")) {
        query <- axis
        axis <- NULL
    }

    result <- julia_call("DataAxesFormats.Queries.AsAxis", axis)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title MaskSlice query operation
#' @description A query operation for using a slice of a matrix as a mask, when the other axis of the matrix is different from the
#' mask axis. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.MaskSlice) for
#' details.
#' @param axis String specifying the axis
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
MaskSlice <- function(axis, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(axis, "JuliaObject")) {
            query <- axis
            axis <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(axis)) {
        if (inherits(axis, "JuliaObject")) {
            cli::cli_abort("argument {.field axis} is missing with no default")
        } else {
            cli::cli_abort("{.field axis} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.MaskSlice", axis)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title SquareMaskColumn query operation
#' @description Similar to `MaskSlice` but is used when the mask matrix is square and we'd like to use a column as a mask. See the
#' Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.SquareMaskColumn)
#' for details.
#' @param value String specifying the value
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
SquareMaskColumn <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(value)) {
        if (inherits(value, "JuliaObject")) {
            cli::cli_abort("argument {.field value} is missing with no default")
        } else {
            cli::cli_abort("{.field value} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.SquareMaskColumn", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title SquareMaskRow query operation
#' @description Similar to `MaskSlice` but is used when the mask matrix is square and we'd like to use a row as a mask. See the
#' Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.SquareMaskRow) for
#' details.
#' @param value String specifying the value
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
SquareMaskRow <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(value)) {
        if (inherits(value, "JuliaObject")) {
            cli::cli_abort("argument {.field value} is missing with no default")
        } else {
            cli::cli_abort("{.field value} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.SquareMaskRow", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title And query operation
#' @description A query operation for restricting the set of entries of an `Axis`. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.And) for details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
And <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.And", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title AndNot query operation
#' @description Same as `And` but use the inverse of the mask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.AndNot) for
#' details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
AndNot <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.AndNot", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title Or query operation
#' @description A query operation for expanding the set of entries of an `Axis`. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Or) for details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
Or <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.Or", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title OrNot query operation
#' @description Same as `Or` but use the inverse of the mask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.OrNot) for details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
OrNot <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.OrNot", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title Xor query operation
#' @description A query operation for flipping the set of entries of an `Axis`. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Xor) for details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
Xor <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.Xor", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title XorNot query operation
#' @description Same as `Xor` but use the inverse of the mask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.XorNot) for details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
XorNot <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.XorNot", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IsLess query operation
#' @description A query operation for converting a vector value to a Boolean mask by comparing it some value. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.IsLess) for details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsLess <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Queries.IsLess", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IsLessEqual query operation
#' @description Similar to `IsLess` except that uses `<=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.IsLessEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsLessEqual <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Queries.IsLessEqual", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IsEqual query operation
#' @description Equality is used for two purposes: As a comparison operator, similar to `IsLess` except that uses `=` instead of
#' `<` for the comparison; and To select a single entry from a vector. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.IsEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsEqual <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Queries.IsEqual", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IsNotEqual query operation
#' @description Similar to `IsLess` except that uses `!=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.IsNotEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsNotEqual <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Queries.IsNotEqual", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IsGreater query operation
#' @description Similar to `IsLess` except that uses `>` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.IsGreater) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsGreater <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Queries.IsGreater", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IsGreaterEqual query operation
#' @description Similar to `IsLess` except that uses `>=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.IsGreaterEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsGreaterEqual <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Queries.IsGreaterEqual", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IsMatch query operation
#' @description Similar to `IsLess` except that the compared values must be strings, and the mask
#' is of the values that match the given regular expression. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.IsMatch) for
#' details.
#' @param value Regular expression pattern to match against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsMatch <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Queries.IsMatch", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IsNotMatch query operation
#' @description Similar to `IsMatch` except that looks for entries that do not match the pattern. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.IsNotMatch) for
#' details.
#' @param value Regular expression pattern to not match against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsNotMatch <- function(value, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(value, "JuliaObject")) {
            query <- value
            value <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    result <- julia_call("DataAxesFormats.Queries.IsNotMatch", value)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title CountBy query operation
#' @description A query operation that generates a matrix of counts of combinations of pairs of values for the same entries of an
#' axis. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.CountBy) for
#' details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
CountBy <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.CountBy", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title GroupBy query operation
#' @description A query operation that uses a (following) `ReductionOperation` to aggregate the values of each group of
#' values. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAccessFormats.Queries.GroupBY) for
#' details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
GroupBy <- function(property, ...) {
    dots <- list(...)
    query <- NULL
    if (length(dots) > 0) {
        if (inherits(property, "JuliaObject")) {
            query <- property
            property <- dots[[1]]
        } else if (inherits(dots[[1]], "JuliaObject")) {
            query <- dots[[1]]
        }
    }

    if (!is.character(property)) {
        if (inherits(property, "JuliaObject")) {
            cli::cli_abort("argument {.field property} is missing with no default")
        } else {
            cli::cli_abort("{.field property} must be a character string")
        }
    }

    result <- julia_call("DataAxesFormats.Queries.GroupBy", property)

    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}
