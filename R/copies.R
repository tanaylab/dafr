#' Copy a scalar from source to destination
#'
#' @param destination A Daf object to copy to
#' @param source A Daf object to copy from
#' @param name Name of the scalar to copy
#' @param rename Optional new name for the scalar in the destination
#' @param default Default value if scalar doesn't exist. If not provided (the default),
#'   an error will be raised if the scalar is missing in the source. If explicitly set to NULL,
#'   the copy will silently skip missing scalars.
#' @param overwrite Whether to overwrite if scalar already exists
#' @param type Optional type to convert the scalar to (e.g., "Float64", "Int32"). If NULL, the original type is preserved.
#' @param insist Whether to skip if the destination already has the scalar (FALSE) or to
#'   attempt the copy regardless (TRUE, the default). Only relevant when overwrite=FALSE.
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_scalar!) for details.
#' @export
copy_scalar <- function(
    destination,
    source,
    name,
    rename = NULL,
    default,
    overwrite = FALSE,
    type = NULL,
    insist = TRUE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    kwargs <- list(
        destination = destination$jl_obj,
        source = source$jl_obj,
        name = name,
        overwrite = overwrite,
        insist = insist
    )

    if (!is.null(rename)) {
        kwargs[["rename"]] <- rename
    }
    if (!missing(default)) {
        if (is.null(default)) {
            # Use list(NULL) to keep NULL in the list (kwargs[["x"]] <- NULL removes the element)
            # JuliaCall converts R NULL to Julia nothing, causing missing source data to be silently skipped
            kwargs["default"] <- list(NULL)
        } else {
            kwargs[["default"]] <- to_julia_array(default)
        }
    }
    if (!is.null(type)) {
        kwargs[["type"]] <- jl_R_to_julia_type(type)
    }

    do.call(julia_call, c(list("DataAxesFormats.copy_scalar!"), kwargs))

    invisible(destination)
}

#' Copy an axis from source to destination
#'
#' @param destination A Daf object to copy to
#' @param source A Daf object to copy from
#' @param axis Name of the axis to copy
#' @param rename Optional new name for the axis in the destination
#' @param default Default value if axis doesn't exist
#' @param overwrite Whether to overwrite if axis already exists
#' @param insist Whether to fail if the axis doesn't exist
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_axis!) for details.
#' @export
copy_axis <- function(
    destination,
    source,
    axis,
    rename = NULL,
    default,
    overwrite = FALSE,
    insist = TRUE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    kwargs <- list(
        destination = destination$jl_obj,
        source = source$jl_obj,
        axis = axis,
        overwrite = overwrite,
        insist = insist
    )

    if (!is.null(rename)) {
        kwargs[["rename"]] <- rename
    }
    if (!missing(default)) {
        if (is.null(default)) {
            kwargs["default"] <- list(NULL)
        } else {
            kwargs[["default"]] <- to_julia_array(default)
        }
    }

    do.call(julia_call, c(list("DataAxesFormats.copy_axis!"), kwargs))

    invisible(destination)
}

#' Copy a vector from source to destination
#'
#' @param destination A Daf object to copy to
#' @param source A Daf object to copy from
#' @param axis Name of the axis
#' @param name Name of the vector to copy
#' @param reaxis Optional different axis name in the destination
#' @param rename Optional new name for the vector in the destination
#' @param default Default value if vector doesn't exist
#' @param empty Value to use for filling in missing data
#' @param overwrite Whether to overwrite if vector already exists
#' @param eltype Optional element type to convert to (e.g., "Float64", "Int32"). If NULL, the original type is preserved.
#' @param bestify Whether to bestify the vector storage (FALSE by default)
#' @param min_sparse_saving_fraction Optional minimum sparse saving fraction. If NULL, the default is used.
#' @param insist Whether to fail if the vector doesn't exist (TRUE by default)
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_vector!) for details.
#' @export
copy_vector <- function(
    destination,
    source,
    axis,
    name,
    reaxis = NULL,
    rename = NULL,
    default,
    empty = NULL,
    overwrite = FALSE,
    eltype = NULL,
    bestify = FALSE,
    min_sparse_saving_fraction = NULL,
    insist = TRUE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    kwargs <- list(
        destination = destination$jl_obj,
        source = source$jl_obj,
        axis = axis,
        name = name,
        overwrite = overwrite,
        bestify = bestify,
        insist = insist
    )

    if (!is.null(reaxis)) {
        kwargs[["reaxis"]] <- reaxis
    }
    if (!is.null(rename)) {
        kwargs[["rename"]] <- rename
    }
    if (!missing(default)) {
        if (is.null(default)) {
            kwargs["default"] <- list(NULL)
        } else {
            kwargs[["default"]] <- to_julia_array(default)
        }
    }
    if (!is.null(empty)) {
        kwargs[["empty"]] <- empty
    }
    if (!is.null(eltype)) {
        kwargs[["eltype"]] <- jl_R_to_julia_type(eltype)
    }
    if (!is.null(min_sparse_saving_fraction)) {
        kwargs[["min_sparse_saving_fraction"]] <- min_sparse_saving_fraction
    }

    do.call(julia_call, c(list("DataAxesFormats.copy_vector!"), kwargs))

    invisible(destination)
}

#' Copy a matrix from source to destination
#'
#' @param destination A Daf object to copy to
#' @param source A Daf object to copy from
#' @param rows_axis Name of rows axis
#' @param columns_axis Name of columns axis
#' @param name Name of the matrix to copy
#' @param rows_reaxis Optional different rows axis name in the destination
#' @param columns_reaxis Optional different columns axis name in the destination
#' @param rename Optional new name for the matrix in the destination
#' @param default Default value if matrix doesn't exist
#' @param empty Value to use for filling in missing data
#' @param relayout Whether to allow relayout
#' @param overwrite Whether to overwrite if matrix already exists
#' @param eltype Optional element type to convert to (e.g., "Float64", "Int32"). If NULL, the original type is preserved.
#' @param bestify Whether to bestify the matrix storage (FALSE by default)
#' @param min_sparse_saving_fraction Optional minimum sparse saving fraction. If NULL, the default is used.
#' @param insist Whether to fail if the matrix doesn't exist (TRUE by default)
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_matrix!) for details.
#' @export
copy_matrix <- function(
    destination,
    source,
    rows_axis,
    columns_axis,
    name,
    rows_reaxis = NULL,
    columns_reaxis = NULL,
    rename = NULL,
    default,
    empty = NULL,
    relayout = TRUE,
    overwrite = FALSE,
    eltype = NULL,
    bestify = FALSE,
    min_sparse_saving_fraction = NULL,
    insist = TRUE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    kwargs <- list(
        destination = destination$jl_obj,
        source = source$jl_obj,
        rows_axis = rows_axis,
        columns_axis = columns_axis,
        name = name,
        relayout = relayout,
        overwrite = overwrite,
        bestify = bestify,
        insist = insist
    )

    if (!is.null(rows_reaxis)) {
        kwargs[["rows_reaxis"]] <- rows_reaxis
    }
    if (!is.null(columns_reaxis)) {
        kwargs[["columns_reaxis"]] <- columns_reaxis
    }
    if (!is.null(rename)) {
        kwargs[["rename"]] <- rename
    }
    if (!missing(default)) {
        if (is.null(default)) {
            kwargs["default"] <- list(NULL)
        } else {
            kwargs[["default"]] <- to_julia_array(default)
        }
    }
    if (!is.null(empty)) {
        kwargs[["empty"]] <- empty
    }
    if (!is.null(eltype)) {
        kwargs[["eltype"]] <- jl_R_to_julia_type(eltype)
    }
    if (!is.null(min_sparse_saving_fraction)) {
        kwargs[["min_sparse_saving_fraction"]] <- min_sparse_saving_fraction
    }

    do.call(julia_call, c(list("DataAxesFormats.copy_matrix!"), kwargs))

    invisible(destination)
}

#' Copy a tensor from source to destination
#'
#' @param destination A Daf object to copy to
#' @param source A Daf object to copy from
#' @param main_axis Name of main axis
#' @param rows_axis Name of rows axis
#' @param columns_axis Name of columns axis
#' @param name Name of the tensor
#' @param rows_reaxis Optional different rows axis name in the destination
#' @param columns_reaxis Optional different columns axis name in the destination
#' @param rename Optional new name for the tensor in the destination
#' @param empty Value to use for filling in missing data
#' @param relayout Whether to allow relayout
#' @param overwrite Whether to overwrite if tensor already exists
#' @param eltype Optional element type to convert to (e.g., "Float64", "Int32"). If NULL, the original type is preserved.
#' @param bestify Whether to bestify the tensor storage (FALSE by default)
#' @param min_sparse_saving_fraction Optional minimum sparse saving fraction. If NULL, the default is used.
#' @param insist Whether to fail if the tensor doesn't exist (TRUE by default)
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_tensor!) for details.
#' @export
copy_tensor <- function(
    destination,
    source,
    main_axis,
    rows_axis,
    columns_axis,
    name,
    rows_reaxis = NULL,
    columns_reaxis = NULL,
    rename = NULL,
    empty = NULL,
    relayout = TRUE,
    overwrite = FALSE,
    eltype = NULL,
    bestify = FALSE,
    min_sparse_saving_fraction = NULL,
    insist = TRUE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    kwargs <- list(
        destination = destination$jl_obj,
        source = source$jl_obj,
        main_axis = main_axis,
        rows_axis = rows_axis,
        columns_axis = columns_axis,
        name = name,
        relayout = relayout,
        overwrite = overwrite,
        bestify = bestify,
        insist = insist
    )

    if (!is.null(rows_reaxis)) {
        kwargs[["rows_reaxis"]] <- rows_reaxis
    }
    if (!is.null(columns_reaxis)) {
        kwargs[["columns_reaxis"]] <- columns_reaxis
    }
    if (!is.null(rename)) {
        kwargs[["rename"]] <- rename
    }
    if (!is.null(empty)) {
        kwargs[["empty"]] <- empty
    }
    if (!is.null(eltype)) {
        kwargs[["eltype"]] <- jl_R_to_julia_type(eltype)
    }
    if (!is.null(min_sparse_saving_fraction)) {
        kwargs[["min_sparse_saving_fraction"]] <- min_sparse_saving_fraction
    }

    do.call(julia_call, c(list("DataAxesFormats.copy_tensor!"), kwargs))

    invisible(destination)
}

#' Copy all content from source to destination
#'
#' @param destination A Daf object to copy to
#' @param source A Daf object to copy from
#' @param empty A named list mapping data keys to values for filling missing data
#' @param overwrite Whether to overwrite if data already exists
#' @param relayout Whether to allow relayout
#' @param types Optional named list mapping data keys to types for conversion. If NULL, the original types are preserved.
#' @param insist Whether to fail if data doesn't exist (TRUE by default)
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_all!) for details.
#' @export
copy_all <- function(
    destination,
    source,
    empty = NULL,
    overwrite = FALSE,
    relayout = TRUE,
    types = NULL,
    insist = TRUE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    # Process empty parameter if provided
    if (!is.null(empty)) {
        # Convert to Julia-compatible format
        empty <- jl_pair(empty)
    }

    kwargs <- list(
        destination = destination$jl_obj,
        source = source$jl_obj,
        overwrite = overwrite,
        relayout = relayout,
        insist = insist
    )

    if (!is.null(empty)) {
        kwargs[["empty"]] <- empty
    }
    if (!is.null(types)) {
        kwargs[["types"]] <- jl_pair(types)
    }

    do.call(julia_call, c(list("DataAxesFormats.copy_all!"), kwargs))

    invisible(destination)
}
