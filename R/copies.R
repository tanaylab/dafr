#' Copy a scalar from source to destination
#'
#' @param destination A Daf object to copy to
#' @param source A Daf object to copy from
#' @param name Name of the scalar to copy
#' @param rename Optional new name for the scalar in the destination
#' @param default Default value if scalar doesn't exist
#' @param overwrite Whether to overwrite if scalar already exists
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_scalar!) for details.
#' @export
copy_scalar <- function(
    destination,
    source,
    name,
    rename = NULL,
    default = NULL,
    overwrite = FALSE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    julia_call(
        "DataAxesFormats.copy_scalar!",
        destination = destination$jl_obj,
        source = source$jl_obj,
        name = name,
        rename = rename,
        default = to_julia_array(default),
        overwrite = overwrite
    )

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
    default = NULL,
    overwrite = FALSE,
    insist = TRUE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    julia_call(
        "DataAxesFormats.copy_axis!",
        destination = destination$jl_obj,
        source = source$jl_obj,
        axis = axis,
        rename = rename,
        default = to_julia_array(default),
        overwrite = overwrite,
        insist = insist
    )

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
    default = NULL,
    empty = NULL,
    overwrite = FALSE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    julia_call(
        "DataAxesFormats.copy_vector!",
        destination = destination$jl_obj,
        source = source$jl_obj,
        axis = axis,
        name = name,
        reaxis = reaxis,
        rename = rename,
        default = to_julia_array(default),
        empty = empty,
        overwrite = overwrite
    )

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
    default = NULL,
    empty = NULL,
    relayout = TRUE,
    overwrite = FALSE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    julia_call(
        "DataAxesFormats.copy_matrix!",
        destination = destination$jl_obj,
        source = source$jl_obj,
        rows_axis = rows_axis,
        columns_axis = columns_axis,
        name = name,
        rows_reaxis = rows_reaxis,
        columns_reaxis = columns_reaxis,
        rename = rename,
        default = to_julia_array(default),
        empty = empty,
        relayout = relayout,
        overwrite = overwrite
    )

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
    overwrite = FALSE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    julia_call(
        "DataAxesFormats.copy_tensor!",
        destination = destination$jl_obj,
        source = source$jl_obj,
        main_axis = main_axis,
        rows_axis = rows_axis,
        columns_axis = columns_axis,
        name = name,
        rows_reaxis = rows_reaxis,
        columns_reaxis = columns_reaxis,
        rename = rename,
        empty = empty,
        relayout = relayout,
        overwrite = overwrite
    )

    invisible(destination)
}

#' Copy all content from source to destination
#'
#' @param destination A Daf object to copy to
#' @param source A Daf object to copy from
#' @param empty A named list mapping data keys to values for filling missing data
#' @param overwrite Whether to overwrite if data already exists
#' @param relayout Whether to allow relayout
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/copies.html#DataAxesFormats.Copies.copy_all!) for details.
#' @export
copy_all <- function(
    destination,
    source,
    empty = NULL,
    overwrite = FALSE,
    relayout = TRUE) {
    validate_daf_object(destination)
    validate_daf_object(source)

    # Process empty parameter if provided
    if (!is.null(empty)) {
        # Convert to Julia-compatible format
        empty <- jl_pair(empty)
    }

    julia_call(
        "DataAxesFormats.copy_all!",
        destination = destination$jl_obj,
        source = source$jl_obj,
        empty = empty,
        overwrite = overwrite,
        relayout = relayout
    )

    invisible(destination)
}
