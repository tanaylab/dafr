#' Check if a query can be applied to a Daf object
#'
#' Determines whether a query can be validly applied to a Daf object. This is useful for
#' checking if the properties referenced in a query exist in the Daf object before
#' attempting to execute the query.
#'
#' @param daf A Daf object
#' @param query Query string or object. Can be created using query operations such as
#'   Axis(), Lookup(), IsGreater(), etc.
#' @return TRUE if query can be applied, FALSE otherwise
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.has_query) for details.
#' @seealso Axis, Lookup, Names, AsAxis, Fetch, IfMissing, IfNot, MaskSlice, SquareMaskColumn, SquareMaskRow,
#'   And, AndNot, Or, OrNot, Xor, XorNot, IsEqual, IsNotEqual, IsLess, IsLessEqual, IsGreater, IsGreaterEqual,
#'   IsMatch, IsNotMatch, CountBy, GroupBy and other query operations.
#' @export
has_query <- function(daf, query) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.Queries.has_query", daf$jl_obj, query)
}

#' Apply a query to a Daf object
#'
#' Executes a query on a Daf object and returns the result. Queries provide a way to extract,
#' filter, and manipulate data from a Daf object using a composable syntax. Queries can retrieve
#' scalars, vectors, matrices, or sets of names depending on the operations used.
#'
#' @param daf A Daf object
#' @param query Query string or object. Can be created using query operations such as
#'   Axis(), Lookup(), IsGreater(), etc.
#' In order to support the use of pipe operators, the query can also be a Daf object and vice versa, see examples below.
#' @param cache Whether to cache the query result
#' @return The result of the query, which could be a scalar, vector, matrix, or set of names depending on the query
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.get_query) for details.
#' @seealso Axis, Lookup, Names, AsAxis, Fetch, IfMissing, IfNot, MaskSlice, SquareMaskColumn, SquareMaskRow,
#'   And, AndNot, Or, OrNot, Xor, XorNot, IsEqual, IsNotEqual, IsLess, IsLessEqual, IsGreater, IsGreaterEqual,
#'   IsMatch, IsNotMatch, CountBy, GroupBy and other query operations.
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
#' Executes a query on a Daf object and returns the result formatted as a data.frame for
#' easier use in R analysis workflows. This is a convenience wrapper around get_query
#' that provides properly structured dataframes for different result types.
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
        df <- as.data.frame(values)

        return(df)
    } else {
        # For scalar results or name sets, convert directly
        return(from_julia_object(result))
    }
}

#' Parse a query string into a query object
#'
#' Converts a string representation of a query into a query object that can be applied to a Daf object.
#' This allows for a more concise syntax for creating complex queries. If you want something similar
#' to the `q` prefix used in Julia, you can write `q <- parse_query` in your R code.
#'
#' @param query_string String containing the query
#' @return A query object that can be used with get_query and has_query
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html) for details.
#' @export
parse_query <- function(query_string) {
    julia_call("DataAxesFormats.Queries.Query", query_string)
}

#' Get the number of dimensions of a query result
#'
#' Determines the dimensionality of the result that would be produced by applying a query.
#' This is useful for understanding what kind of data structure to expect from a query before
#' executing it, which can help with writing code that handles different result types.
#'
#' @param query Query string or object
#' @return Number of dimensions (-1 - names, 0 - scalar, 1 - vector, 2 - matrix)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html) for details.
#' @export
query_result_dimensions <- function(query) {
    julia_call("DataAxesFormats.Queries.query_result_dimensions", query)
}

#' @title Axis query operation
#' @description A query operation for specifying a result axis in a query sequence.
#' This is typically the first operation in a query sequence and determines which axis
#' the query will operate on. It sets the context for subsequent operations in the query.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Axis) for details.
#' @param axis String specifying the axis name
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Axis <- function(axis, ...) {
    res <- extract_query_and_value(axis, missing(axis), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field axis} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field axis} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.Axis", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Lookup query operation
#' @description A query operation for looking up the value of a property with a specific name.
#' This operation retrieves the data associated with the specified property for the current axis context.
#' It is typically used after an `Axis` operation to select a property from that axis.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Lookup)
#' for details.
#' @param property String specifying the property name to look up
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Lookup <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.Lookup", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Names query operation
#' @description A query operation for looking up a set of names in a Daf object.
#' This operation retrieves metadata names such as axis names, property names, or scalar names.
#' It is often used to discover what data is available in a Daf object.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Names)
#' for details.
#' @param kind Optional string specifying the kind of names to look up (e.g., "axes", "scalars")
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Names <- function(kind = NULL, ...) {
    res <- extract_query_and_value(kind, missing(kind), list(...),
        required = FALSE, default = NULL
    )
    if (!is.null(res$value) && !is.character(res$value)) {
        cli::cli_abort("{.field kind} must be a character string or NULL")
    }

    ans <- julia_call("DataAxesFormats.Queries.Names", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Fetch query operation
#' @description A query operation for fetching the value of a property from another axis.
#' This operation is typically used after a `Lookup` operation that returns a vector whose values
#' are entry names of another axis. The `Fetch` operation will then retrieve a property from
#' that other axis based on these entry names. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Fetch) for
#' details.
#' @param property String specifying the property name to fetch from the target axis
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Fetch <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.Fetch", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IfMissing query operation
#' @description A query operation providing a default value to use if the data is missing some property.
#' This is useful when querying for properties that might not exist for all entries, allowing you to
#' provide a fallback value instead of getting an error. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IfMissing)
#' for details.
#' @param missing_value Value to use when data is missing the property
#' @param type Optional type specification for the missing value (e.g., "Int64", "Float64", "Bool")
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
IfMissing <- function(missing_value, type = NULL, ...) {
    dots <- list(...)
    query <- NULL
    # Assign initial values from args, handle defaults later
    real_missing_value <- missing_value
    real_type <- type
    mv_missing <- missing(missing_value)

    # --- Pipe Handling ---
    mv_is_query <- !mv_missing && inherits(missing_value, "JuliaObject")
    dots_non_empty <- length(dots) > 0
    dots_has_value <- dots_non_empty && !inherits(dots[[1]], "JuliaObject")
    dots_has_query <- dots_non_empty && inherits(dots[[1]], "JuliaObject")

    if (mv_is_query) {
        query <- missing_value
        if (dots_has_value) {
            # Option 2: query=mv, real_mv=dots[[1]], real_type=type
            real_missing_value <- dots[[1]]
            # real_type remains `type` from args
        } else {
            # Option 1: query=mv, real_mv=type, real_type=NULL
            # (Covers cases: dots empty, or dots[[1]] is query)
            real_missing_value <- type # Assign value from type arg
            real_type <- NULL # No explicit type anymore
        }
    } else if (dots_has_query) {
        # Option 3: query=dots[[1]], real_mv=mv, real_type=type
        query <- dots[[1]]
        # real_missing_value remains `missing_value` from args
        # real_type remains `type` from args
    } else {
        # Options 4 & 5: No query involved via pipe.
        # real_missing_value remains `missing_value` from args
        # real_type remains `type` from args
    }

    # --- Validation --- Check if real_missing_value is missing
    # This happens in Option 1 if `type` was NULL, or Option 3/4/5 if `missing_value` was missing
    # Or Option 2 if dots[[1]] was NULL? Let's refine.
    value_is_missing <- FALSE
    if (mv_is_query && !dots_has_value && is.null(type)) {
        # Option 1 case where type (the source for real_mv) was NULL
        value_is_missing <- TRUE
    } else if (!mv_is_query && mv_missing) {
        # Option 3/4/5 case where the required missing_value argument wasn't provided
        value_is_missing <- TRUE
    }

    if (value_is_missing) {
        cli::cli_abort("argument {.field missing_value} is missing with no default")
    }

    # Check for NA (as per original code)
    # Use identical() to distinguish NA from NA_real_, NA_integer_ etc.
    if (identical(real_missing_value, NA)) {
        cli::cli_abort("{.field missing_value} cannot be NA. See the Julia documentation for details.")
    }

    # --- Call Julia --- Call with appropriate parameters
    if (is.null(real_type)) {
        result <- julia_call("DataAxesFormats.Queries.IfMissing", real_missing_value)
    } else {
        # Check type validity before calling helper?
        if (!is.character(real_type) || length(real_type) != 1) {
            cli::cli_abort("{.field type} must be a single string (e.g., \"Int64\")")
        }
        julia_type_obj <- jl_R_to_julia_type(real_type) # Assumes this helper exists and works
        result <- julia_call("DataAxesFormats.Queries.IfMissing", real_missing_value, julia_type_obj)
    }

    # --- Apply Pipe ---
    if (!is.null(query)) {
        result <- julia_call("|>", query, result)
    }

    return(result)
}

#' @title IfNot query operation
#' @description A query operation providing a value to use for "false-ish" values in a vector.
#' This replaces empty strings, zero numeric values, or false Boolean values with the specified value.
#' This is useful for handling default or missing values in data without completely replacing the property.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IfNot) for
#' details.
#' @param value Optional value to use for replacement. If NULL, uses the default replacement value.
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
IfNot <- function(value = NULL, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = FALSE, default = NULL)

    # No specific validation needed for value (can be NULL or various types)

    result <- julia_call("DataAxesFormats.Queries.IfNot", res$value)

    if (!is.null(res$query)) {
        result <- julia_call("|>", res$query, result)
    }
    return(result)
}

#' @title AsAxis query operation
#' @description A query operation that treats values in a vector property as names of entries in another axis.
#' This operation is commonly used with three other operations: `Fetch`, `CountBy`, and `GroupBy`,
#' where vector values need to be interpreted as axis entry names. If no axis is specified, values
#' are treated as entries of a default axis based on context. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.AsAxis) for
#' details.
#' @param axis Optional string specifying the axis name to use for interpreting the values
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
AsAxis <- function(axis = NULL, ...) {
    res <- extract_query_and_value(axis, missing(axis), list(...), required = FALSE, default = NULL)

    # Validation (optional arg)
    if (!is.null(res$value) && !is.character(res$value)) {
        cli::cli_abort("{.field axis} must be a character string or NULL")
    }

    result <- julia_call("DataAxesFormats.Queries.AsAxis", res$value)

    if (!is.null(res$query)) {
        result <- julia_call("|>", res$query, result)
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
    res <- extract_query_and_value(axis, missing(axis), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field axis} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field axis} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.MaskSlice", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
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
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field value} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.SquareMaskColumn", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
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
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field value} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.SquareMaskRow", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title And query operation
#' @description A query operation for filtering axis entries using a Boolean mask.
#' This operation restricts the set of entries of an axis to only those where the specified
#' property contains true values (or non-zero/non-empty values). It essentially performs
#' a logical AND between the current selection and the specified property, treating the
#' property as a Boolean mask.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.And) for details.
#' @param property String specifying the property to use as a filter mask
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
And <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.And", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
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
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.AndNot", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Or query operation
#' @description A query operation for expanding the set of entries of an axis.
#' This operation adds entries to the current selection where the specified property
#' contains true values (or non-zero/non-empty values). It performs a logical OR
#' between the current selection and the specified property, treating the property
#' as a Boolean mask.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Or) for details.
#' @param property String specifying the property to use for expanding the selection
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
Or <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.Or", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title OrNot query operation
#' @description Same as `Or` but use the inverse of the mask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.OrNot) for details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
OrNot <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.OrNot", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title Xor query operation
#' @description A query operation for flipping the set of entries of an `Axis`. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.Xor) for details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
Xor <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.Xor", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title XorNot query operation
#' @description Same as `Xor` but use the inverse of the mask. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.XorNot) for details.
#' @param property String specifying the property
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
XorNot <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.XorNot", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IsLess query operation
#' @description A query operation for filtering based on numeric comparison.
#' This operation converts a vector property to a Boolean mask by comparing each value
#' to the specified threshold using the less-than (`<`) operator. Only entries where the
#' comparison returns true are included in the result. Typically used after a Lookup operation
#' to filter entries based on numeric values.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsLess) for details.
#' @param value Threshold value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
IsLess <- function(value, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    # Numeric validation happens on Julia side

    ans <- julia_call("DataAxesFormats.Queries.IsLess", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IsLessEqual query operation
#' @description Similar to `IsLess` except that uses `<=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsLessEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsLessEqual <- function(value, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    # Numeric validation happens on Julia side

    ans <- julia_call("DataAxesFormats.Queries.IsLessEqual", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IsEqual query operation
#' @description Equality is used for two purposes: As a comparison operator, similar to `IsLess` except that uses `=` instead of
#' `<` for the comparison; and To select a single entry from a vector. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsEqual <- function(value, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    # Type validation happens on Julia side

    ans <- julia_call("DataAxesFormats.Queries.IsEqual", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IsNotEqual query operation
#' @description Similar to `IsLess` except that uses `!=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsNotEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsNotEqual <- function(value, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    # Type validation happens on Julia side

    ans <- julia_call("DataAxesFormats.Queries.IsNotEqual", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IsGreater query operation
#' @description Similar to `IsLess` except that uses `>` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsGreater) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsGreater <- function(value, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    # Numeric validation happens on Julia side

    ans <- julia_call("DataAxesFormats.Queries.IsGreater", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IsGreaterEqual query operation
#' @description Similar to `IsLess` except that uses `>=` instead of `<` for the comparison. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsGreaterEqual) for
#' details.
#' @param value Value to compare against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsGreaterEqual <- function(value, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    # Numeric validation happens on Julia side

    ans <- julia_call("DataAxesFormats.Queries.IsGreaterEqual", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IsMatch query operation
#' @description Similar to `IsLess` except that the compared values must be strings, and the mask
#' is of the values that match the given regular expression. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsMatch) for
#' details.
#' @param value Regular expression pattern to match against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsMatch <- function(value, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field value} must be a character string (regex pattern)")
    }

    ans <- julia_call("DataAxesFormats.Queries.IsMatch", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title IsNotMatch query operation
#' @description Similar to `IsMatch` except that looks for entries that do not match the pattern. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.IsNotMatch) for
#' details.
#' @param value Regular expression pattern to not match against
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object
#' @export
IsNotMatch <- function(value, ...) {
    res <- extract_query_and_value(value, missing(value), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field value} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field value} must be a character string (regex pattern)")
    }

    ans <- julia_call("DataAxesFormats.Queries.IsNotMatch", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title CountBy query operation
#' @description A query operation that generates a matrix of counts of combinations of pairs of values.
#' This operation creates a contingency table counting the occurrences of each combination of values
#' between the current property and the specified property, for the same entries of an axis.
#' The result is a matrix whose rows are the values of the first property, columns are the values
#' of the second property, and entries are the counts of occurrences of each combination.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.CountBy) for
#' details.
#' @param property String specifying the property to count combinations with
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
CountBy <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.CountBy", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}

#' @title GroupBy query operation
#' @description A query operation that aggregates values by groups.
#' This operation takes a property whose values define groups, and applies a subsequent
#' reduction operation (e.g., Mean, Sum, Max) to aggregate the values within each group.
#' If applied to a vector, the result is a vector with one entry per group. If applied to a matrix,
#' the result is a matrix with one row per group. This is typically followed by a reduction
#' operation that specifies how to aggregate the grouped values.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.GroupBY) for
#' details.
#' @param property String specifying the property to group by
#' @param ... Additional arguments needed to support usage of pipe operator
#' @return A query operation object that can be used in a query sequence
#' @export
GroupBy <- function(property, ...) {
    res <- extract_query_and_value(property, missing(property), list(...), required = TRUE)
    if (!res$provided) {
        cli::cli_abort("{.field property} is missing with no default")
    }
    if (!is.character(res$value)) {
        cli::cli_abort("{.field property} must be a character string")
    }

    ans <- julia_call("DataAxesFormats.Queries.GroupBy", res$value)
    if (!is.null(res$query)) {
        ans <- julia_call("|>", res$query, ans)
    }
    ans
}
