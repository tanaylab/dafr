#' Reconstruct implicit axes
#'
#' Given an existing axis with a property representing an implicit axis,
#' create a new axis. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/reconstruction.html) for details.
#'
#' @param daf A Daf object
#' @param existing_axis Name of the existing axis containing implicit axis data
#' @param implicit_axis Name of the property in existing_axis that contains implicit axis values
#' @param rename_axis Optional new name for the implicit axis (default: NULL, uses implicit_axis name)
#' @param empty_implicit Value to consider as empty/NA in the implicit axis values (default: NULL)
#' @param implicit_properties Optional set of properties to copy (default: NULL for all properties)
#' @param skipped_properties Optional set of properties to skip (default: NULL for none)
#' @return A named list of properties and their associated values
#' @export
reconstruct_axis <- function(daf,
                             existing_axis,
                             implicit_axis,
                             rename_axis = NULL,
                             empty_implicit = NULL,
                             implicit_properties = NULL,
                             skipped_properties = NULL) {
    validate_daf_object(daf)

    # Call the Julia function with the appropriate parameters
    julia_call("DataAxesFormats.reconstruct_axis!",
        daf$jl_obj,
        existing_axis = existing_axis,
        implicit_axis = implicit_axis,
        rename_axis = rename_axis,
        empty_implicit = empty_implicit,
        implicit_properties = implicit_properties,
        skipped_properties = skipped_properties
    )
}
