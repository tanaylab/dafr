#' Use default or custom Julia environment
#'
#' Force JuliaCall to use your specified Julia environment instead of creating a new one.
#' By default JuliaCall creates its own separate independent environment, which means
#' that it re-downloads and re-installs all the dependency packages there.
#' This function allows using the default Julia environment or a custom one.
#'
#' @param env_path Either "default" to use the default Julia environment, or a path to a custom environment
#' @return No return value, called for side effects.
#' @export
use_default_julia_environment <- function(env_path = "default") {
    if (env_path == "default") {
        default_env <- JuliaCall::julia_eval('joinpath(DEPOT_PATH[1], "environments", "v$(VERSION.major).$(VERSION.minor)")')
        JuliaCall::julia_eval(paste0('using Pkg; Pkg.activate("', default_env, '")'))
    } else {
        # Use the custom environment path provided
        JuliaCall::julia_eval(paste0('using Pkg; Pkg.activate("', env_path, '")'))
    }
}

import_julia_packages <- function() {
    julia_eval("using Pkg")

    julia_eval("using DataAxesFormats")
    julia_eval("using TanayLabUtilities")

    julia_eval("import DataFrames")
    julia_eval("import HDF5")
    julia_eval("import LinearAlgebra")
    julia_eval("import Logging")
    julia_eval("import Muon")
    julia_eval("import NamedArrays")
    julia_eval("import SparseArrays")
}


define_julia_functions <- function() {
    julia_eval("
    function _to_daf_readers(readers::AbstractVector)::Vector{DafReader}
        return Vector{DafReader}(readers)
    end
    ")

    julia_eval("
    function _pairify_columns(items::Maybe{AbstractVector})::Maybe{DataAxesFormats.FrameColumns}
        if items == nothing
            return nothing
        else
            return [name => query for (name, query) in items]
        end
    end")

    julia_eval("
    function _pairify_axes(items::Maybe{AbstractVector})::Maybe{DataAxesFormats.ViewAxes}
        if items == nothing
            return nothing
        else
            return [key => query for (key, query) in items]
        end
    end")

    julia_eval("
    function _pairify_data(items::Maybe{AbstractVector})::Maybe{DataAxesFormats.ViewData}
    if items == nothing
        return nothing
    else
        return [typeof(key) <: AbstractVector ? Tuple(key) => query : key => query for (key, query) in items]
    end
    end")

    julia_eval("
    function _pairify_merge(items::Maybe{AbstractVector})::Maybe{DataAxesFormats.MergeData}
        if items == nothing
            return nothing
        else
            return [key => query for (key, query) in items]
        end
    end")

    julia_eval("_DafReadersVector = Vector{DafReader}")

    # Add function to convert R strings to Julia AbstractString
    julia_eval("
    function _to_abstract_string(str::String)::AbstractString
        return str
    end
    ")

    # Add function to get field from Julia object
    julia_eval("
    function get_object_field(obj, field_name)
        return getfield(obj, Symbol(field_name))
    end
    ")

    julia_eval("
    function _construct_pairs(keys, values)
        result = []
        for i in 1:length(keys)
            key = keys[i]
            value = values[i]
            if isa(key, Vector) && length(key) > 1
                # Convert vector to tuple for matrix keys
                tuple_key = Tuple(key)
                push!(result, tuple_key => value)
            else
                push!(result, key => value)
            end
        end
        return result
    end")

    julia_eval("
    function _inefficient_action_handler(new_handler::AbnormalHandler)::AbnormalHandler
        old_handler = TanayLabUtilities.MatrixLayouts.GLOBAL_INEFFICIENT_ACTION_HANDLER
        TanayLabUtilities.MatrixLayouts.GLOBAL_INEFFICIENT_ACTION_HANDLER = new_handler
        return old_handler
    end
    ")
}

get_julia_field <- function(julia_object, field_name, need_return = "Julia") {
    return(julia_call("get_object_field", julia_object, field_name, need_return = need_return))
}

is_julia_type <- function(julia_object, type_name, need_return = "R") {
    return(julia_call("isa", julia_object, julia_eval(type_name), need_return = need_return))
}

jl_pair <- function(x) {
    pair <- list()
    for (i in seq_along(x)) {
        pair[[i]] <- list(names(x)[i], x[[i]])
    }
    return(pair)
}

jl_pairify_axes <- function(axes) {
    axes <- jl_pair(axes)
    return(julia_call("_pairify_axes", axes, need_return = "Julia"))
}

jl_pairify_data <- function(data) {
    data_list <- list()
    for (i in seq_along(data)) {
        key_name <- names(data)[i]

        # Handle matrix tuple keys that use comma notation (e.g., "var,obs,X")
        if (grepl(",", key_name)) {
            parts <- strsplit(key_name, ",")[[1]]
            parts <- trimws(parts)
            data_list <- c(data_list, list(list(parts, data[[i]])))
        } else {
            data_list <- c(data_list, list(list(key_name, data[[i]])))
        }
    }
    return(julia_call("_pairify_data", data_list, need_return = "Julia"))
}



#' Convert R types to Julia types
#'
#' This function converts R types or values to their appropriate Julia type equivalents.
#' It maps R data types to Julia data types using a lookup table.
#'
#' @param value An R value or type to convert to a Julia type
#' @return The equivalent Julia type or the value unchanged if no conversion is needed
#' @noRd
jl_R_to_julia_type <- function(value) {
    # Define mapping from R types to Julia types
    JULIA_TYPE_OF_R_TYPE <- list(
        "logical" = julia_eval("Bool"),
        "integer" = julia_eval("Int64"),
        "double" = julia_eval("Float64"),
        "int8" = julia_eval("Int8"),
        "int16" = julia_eval("Int16"),
        "int32" = julia_eval("Int32"),
        "int64" = julia_eval("Int64"),
        "uint8" = julia_eval("UInt8"),
        "uint16" = julia_eval("UInt16"),
        "uint32" = julia_eval("UInt32"),
        "uint64" = julia_eval("UInt64"),
        "float32" = julia_eval("Float32"),
        "float64" = julia_eval("Float64")
    )

    # If value is a string that represents a type name
    if (is.character(value) && length(value) == 1) {
        if (value %in% names(JULIA_TYPE_OF_R_TYPE)) {
            return(JULIA_TYPE_OF_R_TYPE[[value]])
        }
    }

    # If value is a class name or type
    if (is.character(value) && length(value) == 1) {
        type_name <- value
        if (type_name %in% names(JULIA_TYPE_OF_R_TYPE)) {
            return(JULIA_TYPE_OF_R_TYPE[[type_name]])
        }
    }

    # If the value is the actual R type (class) - not commonly used in R this way,
    # but included for completeness
    if (is.function(value) && inherits(value, "class")) {
        class_name <- attr(value, "class")
        if (class_name %in% names(JULIA_TYPE_OF_R_TYPE)) {
            return(JULIA_TYPE_OF_R_TYPE[[class_name]])
        }
    }

    # Determine the type of the actual value
    if (!is.null(value) && !is.function(value)) {
        r_type <- typeof(value)
        if (r_type %in% names(JULIA_TYPE_OF_R_TYPE)) {
            return(JULIA_TYPE_OF_R_TYPE[[r_type]])
        }
    }

    # Return the value unchanged if no conversion is needed or possible
    return(julia_eval(value, need_return = "Julia"))
}

#' Convert a Julia object to an appropriate R object
#'
#' @param julia_object A Julia object
#' @return An R object of appropriate type
#' @noRd
from_julia_object <- function(julia_object) {
    # Handle NULL
    if (is.null(julia_object)) {
        return(NULL)
    }

    # Handle primitive types
    if (is.character(julia_object) || is.numeric(julia_object) || is.logical(julia_object)) {
        return(julia_object)
    }

    # Check for specific Julia types
    if (inherits(julia_object, "JuliaObject")) {
        # Check for KeySet or other set types
        is_keyset <- is_julia_type(julia_object, "Base.KeySet")
        is_set <- is_julia_type(julia_object, "AbstractSet")

        if (is_keyset || is_set) {
            return(as.character(julia_call("collect", julia_object)))
        }

        # Check for arrays
        is_array <- is_julia_type(julia_object, "AbstractArray")

        if (is_array) {
            return(from_julia_array(julia_object))
        }

        # Check for DataFrames
        is_dataframe <- is_julia_type(julia_object, "DataFrames.AbstractDataFrame")

        if (is_dataframe) {
            return(as.data.frame(julia_object))
        }
    }

    # Default fallback - return as is
    return(julia_object)
}

create_julia_sparse_matrix <- function(sparse_matrix) {
    if (!inherits(sparse_matrix, "dgCMatrix")) {
        sparse_matrix <- as(sparse_matrix, "CsparseMatrix")
    }

    # Column-oriented sparse matrix (CSC format)
    # Extract components of sparse matrix
    # For dgCMatrix, Matrix package uses 0-based indexing internally, but Julia uses 1-based
    colptr <- sparse_matrix@p + 1 # Convert to 1-indexed for Julia
    rowval <- sparse_matrix@i + 1 # Convert to 1-indexed for Julia
    nzval <- sparse_matrix@x

    # Create vectors in Julia
    jl_colptr <- julia_call("Vector", colptr)
    jl_rowval <- julia_call("Vector", rowval)
    jl_nzval <- julia_call("Vector", nzval)

    nrows <- sparse_matrix@Dim[1]
    ncols <- sparse_matrix@Dim[2]

    # Create Julia sparse matrix
    return(julia_call(
        "SparseArrays.SparseMatrixCSC",
        as.integer(nrows), as.integer(ncols),
        as.integer(jl_colptr), as.integer(jl_rowval), jl_nzval,
        need_return = "Julia"
    ))
}



#' Convert R arrays, vectors and sparse matrices to Julia
#'
#' @param value An R object to convert to a Julia array
#' @return A Julia array or the value unchanged if no conversion is needed
#' @noRd
to_julia_array <- function(value) {
    # Handle strings
    if (is.character(value) && length(value) == 1) {
        return(julia_call("_to_abstract_string", value))
    }

    # Handle sparse matrices
    if (inherits(value, "sparseMatrix")) {
        return(create_julia_sparse_matrix(value))
    }

    # Handle vectors and convert to appropriate type
    if (is.vector(value) && !is.list(value)) {
        return(julia_call("Vector", value))
    }

    # Handle matrices and arrays
    if (is.matrix(value) || is.array(value)) {
        return(julia_call("Array", value))
    }

    # Return value unchanged if no conversion needed
    return(value)
}

#' Convert Julia arrays to R arrays, vectors or sparse matrices
#'
#' @param julia_array A Julia array object
#' @return An R array, vector or sparse matrix
#' @noRd
from_julia_array <- function(julia_array) {
    orig_array <- julia_array
    julia_array <- get_julia_field(orig_array, "array")

    if (is_julia_type(julia_array, "SparseArrays.SparseMatrixCSC")) {
        # Extract components from Julia SparseMatrixCSC
        colptr <- get_julia_field(julia_array, "colptr", need_return = "R")
        rowval <- get_julia_field(julia_array, "rowval", need_return = "R")
        nzval <- get_julia_field(julia_array, "nzval", need_return = "R")

        # Adjust indexing (Julia is 1-indexed, R's Matrix package uses 0-indexed storage)
        colptr <- colptr - 1

        # Get dimensions
        dims <- julia_call("size", julia_array)
        dims <- do.call(c, dims)

        sp <- Matrix::sparseMatrix(
            i = rowval,
            p = colptr,
            x = nzval,
            dims = dims,
            repr = "C"
        )

        row_names <- julia_call("NamedArrays.names", orig_array, as.integer(1), need_return = "R")
        col_names <- julia_call("NamedArrays.names", orig_array, as.integer(2), need_return = "R")

        # Set the row and column names
        rownames(sp) <- row_names
        colnames(sp) <- col_names

        return(sp)
    }

    if (is_julia_type(orig_array, "NamedArrays.NamedVector")) {
        # Extract the array values
        r_array <- get_julia_field(orig_array, "array", need_return = "R")

        # Make sure it's a vector
        r_array <- as.vector(r_array)

        # Extract the names using NamedArrays.names
        names_vector <- julia_call("NamedArrays.names", orig_array, as.integer(1), need_return = "R")

        # Set the names on the R vector
        names(r_array) <- names_vector

        return(r_array)
    }

    if (is_julia_type(orig_array, "NamedArrays.NamedMatrix")) {
        # Extract the array values
        r_array <- get_julia_field(orig_array, "array", need_return = "R")

        # Make sure it's a matrix
        r_array <- as.matrix(r_array)

        row_names <- julia_call("NamedArrays.names", orig_array, as.integer(1), need_return = "R")
        col_names <- julia_call("NamedArrays.names", orig_array, as.integer(2), need_return = "R")

        # Set the row and column names
        rownames(r_array) <- row_names
        colnames(r_array) <- col_names

        return(r_array)
    }

    # Handle regular arrays/vectors
    r_array <- get_julia_field(orig_array, "array", need_return = "R")

    # If the array is 1-dimensional, convert to vector
    if (length(dim(r_array)) == 1 || (length(dim(r_array)) == 2 && any(dim(r_array) == 1))) {
        r_array <- as.vector(r_array)
    }

    return(r_array)
}

# Add function to process frame columns for get_frame
#' Process column specifications for get_frame
#'
#' This function transforms R column specifications (lists, named vectors, etc.)
#' into a format suitable for Julia's DataAxesFormats.Queries.get_frame function.
#'
#' @param columns List or vector of column specifications
#' @return A Julia array of pairs suitable for DataAxesFormats.Queries.get_frame
#' @noRd
process_frame_columns <- function(columns) {
    # Return NULL for NULL input
    if (is.null(columns)) {
        return(NULL)
    }

    # If columns is a single string, convert it to a list with one element
    if (is.character(columns) && length(columns) == 1 && is.null(names(columns))) {
        columns <- list(columns)
    }

    processed_columns <- list()

    for (i in seq_along(columns)) {
        item <- columns[[i]]

        # If the item is a named list/vector, convert it to a pair
        if (is.list(item) && !is.null(names(item))) {
            for (j in seq_along(item)) {
                processed_columns <- c(processed_columns, list(list(names(item)[j], item[[j]])))
            }
        } else if (!is.null(names(columns)) && names(columns)[i] != "") {
            # If the columns list itself has names, use those
            processed_columns <- c(processed_columns, list(list(names(columns)[i], item)))
        } else {
            # Pass through simple items
            processed_columns <- c(processed_columns, list(item))
        }
    }

    # Convert to Julia format
    return(julia_call("_pairify_columns", processed_columns, need_return = "Julia"))
}
