#' Check if a scalar exists in a Daf object
#'
#' Determines whether a scalar property with the specified name exists in the Daf data set.
#'
#' @param daf A Daf object
#' @param name Name of the scalar property to check
#' @return TRUE if scalar exists, FALSE otherwise
#' @details Scalar properties are global values associated with the entire Daf data set.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.has_scalar) for details.
#' @export
has_scalar <- function(daf, name) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.has_scalar", daf$jl_obj, name)
}

#' Get set of scalar names from a Daf object
#'
#' Returns the names of all scalar properties in the Daf data set.
#'
#' @param daf A Daf object
#' @return A character vector of scalar property names
#' @details This function provides the complete set of available scalar properties
#'   that can be retrieved using `get_scalar()`.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.scalars_set) for details.
#' @export
scalars_set <- function(daf) {
    validate_daf_object(daf)
    result <- julia_call("DataAxesFormats.scalars_set", daf$jl_obj)
    as.character(julia_call("collect", result)) # Convert KeySet to Array and then to R character vector
}

#' Get scalar value from a Daf object
#'
#' Retrieves the value of a scalar property with the given name from the Daf data set.
#'
#' @param daf A Daf object
#' @param name Name of the scalar property to retrieve
#' @param default Default value to return if the scalar doesn't exist. If NULL, an error is thrown.
#' @return The scalar value or default if the property is not found
#' @details Numeric scalars are returned as integers or doubles, regardless of the specific
#'   data type they are stored as in the Daf data set.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.get_scalar) for details.
#' @export
get_scalar <- function(daf, name, default = NULL) {
    validate_daf_object(daf)
    if (!is.null(default)) {
        julia_call("DataAxesFormats.get_scalar", daf$jl_obj, name, default = default)
    } else {
        julia_call("DataAxesFormats.get_scalar", daf$jl_obj, name)
    }
}

#' Check if an axis exists in a Daf object
#'
#' Determines whether an axis with the specified name exists in the Daf data set.
#'
#' @param daf A Daf object
#' @param axis Name of the axis to check
#' @return TRUE if the axis exists, FALSE otherwise
#' @details Axes are fundamental dimensions in a Daf data set along which vector and matrix
#'   data are stored. Each axis has a collection of unique named entries.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.has_axis) for details.
#' @export
has_axis <- function(daf, axis) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.has_axis", daf$jl_obj, axis)
}

#' Get set of axis names from a Daf object
#'
#' Returns the names of all axes in the Daf data set.
#'
#' @param daf A Daf object
#' @return A character vector of axis names
#' @details This function provides the complete set of available axes in the Daf data set.
#'   Common axis names might include "gene", "cell", "batch", etc., depending on the data.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axes_set) for details.
#' @export
axes_set <- function(daf) {
    validate_daf_object(daf)
    result <- julia_call("DataAxesFormats.axes_set", daf$jl_obj)
    as.character(julia_call("collect", result))
}

#' Get length of an axis in a Daf object
#'
#' Returns the number of entries along the specified axis in the Daf data set.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @return Length (number of entries) of the axis
#' @details The axis length corresponds to the size of vector properties for this axis
#'   and to one of the dimensions of matrix properties involving this axis.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_length) for details.
#' @export
axis_length <- function(daf, axis) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.axis_length", daf$jl_obj, axis)
}

#' Get vector of axis entries from a Daf object
#'
#' Returns a vector of the unique names for all entries of the specified axis.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @param null_if_missing Whether to return NULL if the axis doesn't exist
#' @return A character vector of axis entry names
#' @details Axis entries provide names for each position along an axis, such as gene names
#'   for a "gene" axis or cell barcodes for a "cell" axis. These entry names can be used
#'   to look up specific indices using the `axis_indices()` function.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_vector) for details.
#' @export
axis_vector <- function(daf, axis, null_if_missing = FALSE) {
    validate_daf_object(daf)
    if (null_if_missing) {
        julia_call("DataAxesFormats.axis_vector", daf$jl_obj, axis, default = NULL)
    } else {
        julia_call("DataAxesFormats.axis_vector", daf$jl_obj, axis)
    }
}

#' Get dictionary of axis entries to indices
#'
#' Returns a named vector that maps axis entry names to their corresponding integer indices.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @return A named vector mapping entry names to their 1-based indices
#' @details This function returns the mapping between entry names and their positions
#'   along the axis. This is useful for efficient lookups when you need to convert
#'   between names and indices repeatedly.
#'   In R, indices are 1-based (first element has index 1), consistent with R conventions.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_dict) for details.
#' @export
axis_dict <- function(daf, axis) {
    validate_daf_object(daf)
    result <- julia_call("DataAxesFormats.axis_dict", daf$jl_obj, axis)
    if (is.list(result)) {
        # Convert Julia Dict to R named vector
        indices <- unlist(result)
        names(indices) <- names(result)
        indices
    } else {
        result
    }
}

#' Get indices of entries in an axis
#'
#' Returns the integer indices for specified entry names along an axis.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @param entries Character vector of entry names to look up
#' @param allow_empty Whether to allow empty entries (return -1 for empty strings if TRUE)
#' @return A vector of 1-based indices corresponding to the entries
#' @details This function maps names to their position indices along the axis.
#'   If `allow_empty` is TRUE, empty strings are converted to index -1.
#'   Indices in R are 1-based (first element has index 1), consistent with R conventions.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_indices) for details.
#' @export
axis_indices <- function(daf, axis, entries, allow_empty = FALSE) {
    validate_daf_object(daf)
    if (!is.character(entries)) {
        cli::cli_abort("{.field entries} must be a character vector")
    }
    single_entry <- length(entries) == 1
    if (single_entry) {
        # For some reason, JuliaCall cannot convert a single string to a vector of length 2, hence this ugly workaround
        entries <- c(entries, entries)
    }
    result <- julia_call("DataAxesFormats.axis_indices", daf$jl_obj, axis, entries, allow_empty = allow_empty)
    if (single_entry) {
        result <- result[1]
    }
    result
}

#' Get entry names for indices in an axis
#'
#' Returns the entry names for specified indices along an axis.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @param indices Vector of 1-based integer indices (or NULL for all entries)
#' @param allow_empty Whether to allow empty/invalid indices (return empty strings if TRUE)
#' @return A character vector of entry names corresponding to the indices
#' @details This function maps position indices to their names along the axis.
#'   If `indices` is NULL, returns all entries of the axis.
#'   If `allow_empty` is TRUE and an invalid index is provided, an empty string is returned for that position.
#'   Indices must be positive integers and within the bounds of the axis length.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.axis_entries) for details.
#' @export
axis_entries <- function(daf, axis, indices = NULL, allow_empty = FALSE) {
    validate_daf_object(daf)
    single_index <- !is.null(indices) && length(indices) == 1
    if (!is.null(indices)) {
        if (!is.numeric(indices)) {
            cli::cli_abort("Indices must be a numeric vector")
        }

        axis_size <- axis_length(daf, axis)
        if (any(indices > axis_size)) {
            cli::cli_abort("Indices must be less than or equal to the size of the axis {.val ({axis_size})}")
        }
        if (any(indices < 1)) {
            cli::cli_abort("Indices must be positive integers")
        }

        if (single_index) {
            # For some reason, JuliaCall cannot convert a single integer to a vector of length 2, hence this ugly workaround
            indices <- c(indices, indices)
        }
        indices <- as.integer(indices)
    }
    result <- julia_call("DataAxesFormats.axis_entries", daf$jl_obj, axis, indices, allow_empty = allow_empty)
    if (single_index) {
        result <- result[1]
    }
    as.character(result)
}

#' Check if a vector exists in a Daf object
#'
#' Determines whether a vector property with the specified name exists for the given axis.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @param name Name of the vector property
#' @return TRUE if vector exists, FALSE otherwise
#' @details Vector properties store one-dimensional data along a specific axis.
#'   Each entry in the axis has a corresponding value in the vector.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.has_vector) for details.
#' @export
has_vector <- function(daf, axis, name) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.has_vector", daf$jl_obj, axis, name)
}

#' Get set of vector names for an axis in a Daf object
#'
#' Returns the names of all vector properties for the specified axis.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @return A character vector of vector property names
#' @details This function provides the complete set of available vector properties
#'   for a specific axis that can be retrieved using `get_vector()`.
#'   Vector properties store one-dimensional data along a specific axis.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.vectors_set) for details.
#' @export
vectors_set <- function(daf, axis) {
    validate_daf_object(daf)
    result <- julia_call("DataAxesFormats.vectors_set", daf$jl_obj, axis)
    as.character(julia_call("collect", result))
}

#' Get vector from a Daf object
#'
#' Retrieves a vector property with the specified name for a given axis.
#'
#' @param daf A Daf object
#' @param axis Axis name
#' @param name Name of the vector property
#' @param default Default value if vector doesn't exist (NULL by default)
#' @return A named vector containing the property values, with names set to the axis entry names,
#'   or the default value if the property doesn't exist
#' @details Vector properties store one-dimensional data along an axis, with one value
#'   for each entry in the axis. If the vector doesn't exist and default is NA,
#'   a vector of NAs with appropriate length is returned.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.get_vector) for details.
#' @export
get_vector <- function(daf, axis, name, default = NULL) {
    validate_daf_object(daf)
    if (!is.null(default)) {
        if (length(default) == 1 && is.na(default)) {
            result <- rep(NA, axis_length(daf, axis))
            names(result) <- axis_entries(daf, axis)
            return(result)
        }
        result <- julia_call("DataAxesFormats.get_vector", daf$jl_obj, axis, name, default = default, need_return = "Julia")
    } else {
        result <- julia_call("DataAxesFormats.get_vector", daf$jl_obj, axis, name, need_return = "Julia")
    }

    return(from_julia_array(result))
}

#' Check if a matrix exists in a Daf object
#'
#' Determines whether a matrix property with the specified name exists for the given axes.
#'
#' @param daf A Daf object
#' @param rows_axis Name of rows axis
#' @param columns_axis Name of columns axis
#' @param name Name of the matrix property
#' @param relayout Whether to check with flipped axes too (TRUE by default)
#' @return TRUE if matrix exists, FALSE otherwise
#' @details Matrix properties store two-dimensional data along two axes.
#'   If `relayout` is TRUE, this function will also check if the matrix exists with
#'   the axes flipped (i.e., rows as columns and columns as rows).
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.has_matrix) for details.
#' @export
has_matrix <- function(daf, rows_axis, columns_axis, name, relayout = TRUE) {
    validate_daf_object(daf)
    julia_call(
        "DataAxesFormats.has_matrix",
        daf$jl_obj,
        rows_axis,
        columns_axis,
        name,
        relayout = relayout
    )
}

#' Get set of matrix names for axes in a Daf object
#'
#' Returns the names of all matrix properties for the specified pair of axes.
#'
#' @param daf A Daf object
#' @param rows_axis Name of rows axis
#' @param columns_axis Name of columns axis
#' @param relayout Whether to include matrices with flipped axes (TRUE by default)
#' @return A character vector of matrix property names
#' @details This function provides the complete set of available matrix properties
#'   for specific axes that can be retrieved using `get_matrix()`.
#'   If `relayout` is TRUE, matrices stored with the axes flipped are also included.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.matrices_set) for details.
#' @export
matrices_set <- function(daf, rows_axis, columns_axis, relayout = TRUE) {
    validate_daf_object(daf)
    result <- julia_call(
        "DataAxesFormats.matrices_set",
        daf$jl_obj,
        rows_axis,
        columns_axis,
        relayout = relayout
    )
    as.character(julia_call("collect", result))
}

#' Set scalar value in a Daf object
#'
#' Sets the value of a scalar property with the specified name in the Daf data set.
#'
#' @param daf A Daf object
#' @param name Name of the scalar property
#' @param value Value to set (cannot be NA)
#' @param overwrite Whether to overwrite if scalar already exists (FALSE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details This function creates or updates a scalar property in the Daf data set.
#'   If the scalar already exists and `overwrite` is FALSE, an error will be raised.
#'   NA values are not supported in Daf.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.set_scalar!) for details.
#' @export
set_scalar <- function(daf, name, value, overwrite = FALSE) {
    validate_daf_object(daf)
    if (is.na(value)) {
        cli::cli_abort("{.field value} cannot be NA. See the Julia documentation for details.")
    }
    julia_call("DataAxesFormats.set_scalar!", daf$jl_obj, name, value, overwrite = overwrite)
    invisible(daf)
}

#' Delete scalar from a Daf object
#'
#' Removes a scalar property with the specified name from the Daf data set.
#'
#' @param daf A Daf object
#' @param name Name of the scalar property to delete
#' @param must_exist Whether to error if scalar doesn't exist (TRUE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details If `must_exist` is TRUE and the scalar doesn't exist, an error will be raised.
#'   Otherwise, the function will silently succeed even if the scalar doesn't exist.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.delete_scalar!) for details.
#' @export
delete_scalar <- function(daf, name, must_exist = TRUE) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.delete_scalar!", daf$jl_obj, name, must_exist = must_exist)
    invisible(daf)
}

#' Add axis to a Daf object
#'
#' Creates a new axis with the specified name and entries in the Daf data set.
#'
#' @param daf A Daf object
#' @param axis Name of the new axis
#' @param entries Vector of entry names (must be unique within the axis)
#' @param overwrite Whether to overwrite if axis already exists (FALSE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details This function creates a new axis with the specified unique entry names.
#'   If the axis already exists and `overwrite` is FALSE, an error will be raised.
#'   Entry names must be unique within the axis.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.add_axis!) for details.
#' @export
add_axis <- function(daf, axis, entries, overwrite = FALSE) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.add_axis!", daf$jl_obj, axis, entries, overwrite = overwrite)
    invisible(daf)
}

#' Delete axis from a Daf object
#'
#' Removes an axis and all its associated data from the Daf data set.
#'
#' @param daf A Daf object
#' @param axis Name of the axis to delete
#' @param must_exist Whether to error if axis doesn't exist (TRUE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details This function deletes an axis and all vector and matrix properties
#'   associated with it. If `must_exist` is TRUE and the axis doesn't exist,
#'   an error will be raised.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.delete_axis!) for details.
#' @export
delete_axis <- function(daf, axis, must_exist = TRUE) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.delete_axis!", daf$jl_obj, axis, must_exist = must_exist)
    invisible(daf)
}

#' Set vector in a Daf object
#'
#' Sets a vector property with the specified name for an axis in the Daf data set.
#'
#' @param daf A Daf object
#' @param axis Axis name
#' @param name Name of the vector property
#' @param value Vector of values to set (cannot contain NA values)
#' @param overwrite Whether to overwrite if vector already exists (FALSE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details This function creates or updates a vector property in the Daf data set.
#'   The length of the vector must match the length of the axis.
#'   If the vector already exists and `overwrite` is FALSE, an error will be raised.
#'   NA values are not supported in Daf.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.set_vector!) for details.
#' @export
set_vector <- function(daf, axis, name, value, overwrite = FALSE) {
    validate_daf_object(daf)
    if (any(is.na(value))) {
        cli::cli_abort("{.field value} cannot contain NA values. See the Julia documentation for details.")
    }
    julia_call("DataAxesFormats.set_vector!", daf$jl_obj, axis, name, value, overwrite = overwrite)
    invisible(daf)
}

#' Delete vector from a Daf object
#'
#' Removes a vector property with the specified name from the Daf data set.
#'
#' @param daf A Daf object
#' @param axis Axis name
#' @param name Name of the vector property to delete
#' @param must_exist Whether to error if vector doesn't exist (TRUE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details If `must_exist` is TRUE and the vector doesn't exist, an error will be raised.
#'   Otherwise, the function will silently succeed even if the vector doesn't exist.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.delete_vector!) for details.
#' @export
delete_vector <- function(daf, axis, name, must_exist = TRUE) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.delete_vector!", daf$jl_obj, axis, name, must_exist = must_exist)
    invisible(daf)
}

#' Set matrix in a Daf object
#'
#' Sets a matrix property with the specified name for the given axes in the Daf data set.
#'
#' @param daf A Daf object
#' @param rows_axis Name of rows axis
#' @param columns_axis Name of columns axis
#' @param name Name of the matrix property
#' @param value Matrix of values to set (cannot contain NA values)
#' @param overwrite Whether to overwrite if matrix already exists (FALSE by default)
#' @param relayout Whether to allow relayout with flipped axes (TRUE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details This function creates or updates a matrix property in the Daf data set.
#'   The dimensions of the matrix must match the lengths of the specified axes.
#'   If the matrix already exists and `overwrite` is FALSE, an error will be raised.
#'   If `relayout` is TRUE, the matrix will also be stored with axes flipped for faster access.
#'   NA values are not supported in Daf.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.set_matrix!) for details.
#' @export
set_matrix <- function(daf, rows_axis, columns_axis, name, value, overwrite = FALSE, relayout = TRUE) {
    validate_daf_object(daf)
    if (any(is.na(value))) {
        cli::cli_abort("{.field value} cannot contain NA values. See the Julia documentation for details.")
    }

    julia_call(
        "DataAxesFormats.set_matrix!",
        daf$jl_obj,
        rows_axis,
        columns_axis,
        name,
        to_julia_array(value),
        overwrite = overwrite,
        relayout = relayout
    )
    invisible(daf)
}

#' Get matrix from a Daf object
#'
#' Retrieves a matrix property with the specified name for the given axes from the Daf data set.
#'
#' @param daf A Daf object
#' @param rows_axis Name of rows axis
#' @param columns_axis Name of columns axis
#' @param name Name of the matrix property
#' @param default Default value if matrix doesn't exist (NULL by default)
#' @param relayout Whether to allow retrieving matrix with flipped axes (TRUE by default)
#' @return A matrix with row and column names set to the axis entry names,
#'   or the default value if the property doesn't exist
#' @details Matrix properties store two-dimensional data along two axes.
#'   If the matrix doesn't exist and default is NA, a matrix of NAs with appropriate dimensions is returned.
#'   If `relayout` is TRUE and the matrix exists with flipped axes, it will be transposed automatically.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.get_matrix) for details.
#' @export
get_matrix <- function(daf, rows_axis, columns_axis, name, default = NULL, relayout = TRUE) {
    validate_daf_object(daf)
    if (!is.null(default)) {
        if (length(default) == 1 && is.na(default)) {
            result <- matrix(NA, nrow = axis_length(daf, rows_axis), ncol = axis_length(daf, columns_axis))
            rownames(result) <- axis_entries(daf, rows_axis)
            colnames(result) <- axis_entries(daf, columns_axis)
            return(result)
        }
    }
    result <- julia_call(
        "DataAxesFormats.get_matrix",
        daf$jl_obj,
        rows_axis,
        columns_axis,
        name,
        relayout = relayout,
        default = default,
        need_return = "Julia"
    )

    result <- from_julia_array(result)

    return(result)
}

#' Relayout matrix in a Daf object
#'
#' Creates or updates a matrix property with flipped axes for more efficient access.
#'
#' @param daf A Daf object
#' @param rows_axis Name of rows axis
#' @param columns_axis Name of columns axis
#' @param name Name of the matrix property
#' @param overwrite Whether to overwrite if matrix already exists with flipped axes (FALSE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details This function creates a transposed version of an existing matrix property,
#'   allowing efficient access from either axis orientation.
#'   If a matrix with the flipped axes already exists and `overwrite` is FALSE, an error will be raised.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.relayout_matrix!) for details.
#' @export
relayout_matrix <- function(daf, rows_axis, columns_axis, name, overwrite = FALSE) {
    validate_daf_object(daf)
    julia_call(
        "DataAxesFormats.relayout_matrix!",
        daf$jl_obj,
        rows_axis,
        columns_axis,
        name,
        overwrite = overwrite
    )
    invisible(daf)
}

#' Delete matrix from a Daf object
#'
#' Removes a matrix property with the specified name from the Daf data set.
#'
#' @param daf A Daf object
#' @param rows_axis Name of rows axis
#' @param columns_axis Name of columns axis
#' @param name Name of the matrix property to delete
#' @param must_exist Whether to error if matrix doesn't exist (TRUE by default)
#' @return The Daf object (invisibly, for chaining operations)
#' @details If `must_exist` is TRUE and the matrix doesn't exist, an error will be raised.
#'   Otherwise, the function will silently succeed even if the matrix doesn't exist.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/writers.html#DataAxesFormats.Writers.delete_matrix!) for details.
#' @export
delete_matrix <- function(daf, rows_axis, columns_axis, name, must_exist = TRUE) {
    validate_daf_object(daf)
    julia_call(
        "DataAxesFormats.delete_matrix!",
        daf$jl_obj,
        rows_axis,
        columns_axis,
        name,
        must_exist = must_exist
    )
    invisible(daf)
}

#' Gets the name of a Daf object
#'
#' Returns the unique identifier name of the Daf data set.
#'
#' @param x A Daf object
#' @param ... Additional arguments (not used)
#' @return The name of the Daf data set as a character string
#' @details Each Daf data set has a unique name used in error messages and for identification.
#'   This is typically set when creating the object or derived from its contents.
#' @export
name <- function(x, ...) {
    validate_daf_object(x)
    x$jl_obj$name
}

#' Get a dataframe from a Daf object
#'
#' @param daf A Daf object
#' @param axis Axis name or query object
#' @param columns Vector of column specifications or named list / vector mapping column names to queries
#' @param cache Whether to cache the query results
#' @return A data.frame containing the specified columns for the axis. If columns is NULL, all columns are returned, with an additional column "name" containing the axis entries.
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.get_frame) for more details.
#' @noRd
get_frame <- function(daf, axis, columns = NULL, cache = FALSE) {
    validate_daf_object(daf)

    # If columns is a single string, convert it to a list with one element
    # This ensures it gets properly translated to a Vector{String} in Julia
    if (is.character(columns) && length(columns) == 1 && is.null(names(columns))) {
        columns <- list(columns)
    }

    if (!is.null(names(columns))) {
        # Process columns to ensure correct format for Julia
        columns <- process_frame_columns(columns)
    }

    result <- julia_call("DataAxesFormats.Queries.get_frame", daf$jl_obj, axis, columns, cache = cache)

    return(result)
}

#' Get a dataframe from a Daf object
#'
#' Retrieves multiple vector properties for an axis as a dataframe.
#'
#' @param daf A Daf object
#' @param axis Axis name or query object
#' @param columns Vector of column specifications or named list/vector mapping column names to queries
#' @param cache Whether to cache the query results (FALSE by default)
#' @param ... Additional arguments passed to `tidyr::pivot_longer`
#' @return A data.frame containing the specified columns for the axis, with row names set to the axis entries.
#'   If columns is NULL, all columns are returned with the "name" column removed if present.
#' @details This function allows retrieving multiple vectors for the same axis in a single operation.
#'   The `columns` parameter can be a vector of vector names, or a named list mapping output column names
#'   to vector names or query strings.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Queries.get_frame) for more details.
#' @export
get_dataframe <- function(daf, axis, columns = NULL, cache = FALSE) {
    result <- get_frame(daf, axis, columns, cache)
    if (is.null(columns) && has_name(result, "name")) {
        result$name <- NULL
    }
    rownames(result) <- axis_entries(daf, axis)
    return(result)
}

#' @rdname get_dataframe
#' @return For `get_tidy`, a tibble in long format with columns "name", "key", and "value".
#'   The "name" column contains the axis entries, "key" contains the column names,
#'   and "value" contains the corresponding values.
#'   Note that if the types of the columns are not homogeneous, an error will be thrown.
#'   Use the `values_transform` argument to transform the types of the values,
#'   e.g. `values_transform = list(value = as.character)`.
#' @export
get_tidy <- function(daf, axis, columns = NULL, cache = FALSE, ...) {
    result <- get_dataframe(daf, axis, columns, cache)
    if (!(is.null(columns) && has_name(result, "name"))) {
        result$name <- axis_entries(daf, axis)
    }
    result <- result |>
        tidyr::pivot_longer(-name, names_to = "key", values_to = "value", ...) |>
        as_tibble()
    return(result)
}

#' Create a read-only wrapper for a Daf object
#'
#' Creates a read-only view of a Daf object to protect it against accidental modification.
#'
#' @param daf A Daf object
#' @param name Optional name for the read-only wrapper (defaults to the original name)
#' @return A read-only Daf object
#' @details This function wraps a Daf object with a read-only interface to protect against
#'   accidental modification. Any attempt to modify the data will result in an error.
#'   The read-only wrapper can be efficiently created as it shares data with the original object.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/read_only.html#DataAxesFormats.ReadOnly.read_only) for details.
#' @export
read_only <- function(daf, name = NULL) {
    validate_daf_object(daf)
    name <- name %||% name(daf)
    readonly_obj <- julia_call("DataAxesFormats.read_only", daf$jl_obj, name = name)
    return(Daf(readonly_obj))
}

#' Get axis version counter
#'
#' Returns the version counter for an axis, which is incremented when the axis is modified.
#' This is useful for cache invalidation.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @return An integer version counter
#' @details The version counter is incremented whenever the axis entries are modified.
#'   This can be used to detect changes and invalidate caches.
#' @export
axis_version_counter <- function(daf, axis) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.axis_version_counter", daf$jl_obj, axis)
}

#' Get vector version counter
#'
#' Returns the version counter for a vector property, which is incremented when the vector is modified.
#' This is useful for cache invalidation.
#'
#' @param daf A Daf object
#' @param axis Name of the axis
#' @param name Name of the vector property
#' @return An integer version counter
#' @details The version counter is incremented whenever the vector data is modified.
#'   This can be used to detect changes and invalidate caches.
#' @export
vector_version_counter <- function(daf, axis, name) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.vector_version_counter", daf$jl_obj, axis, name)
}

#' Get matrix version counter
#'
#' Returns the version counter for a matrix property, which is incremented when the matrix is modified.
#' This is useful for cache invalidation.
#'
#' @param daf A Daf object
#' @param rows_axis Name of the rows axis
#' @param columns_axis Name of the columns axis
#' @param name Name of the matrix property
#' @return An integer version counter
#' @details The version counter is incremented whenever the matrix data is modified.
#'   This can be used to detect changes and invalidate caches.
#' @export
matrix_version_counter <- function(daf, rows_axis, columns_axis, name) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.matrix_version_counter", daf$jl_obj, rows_axis, columns_axis, name)
}
