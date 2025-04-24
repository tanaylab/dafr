#' Invoke a computation on a view of some data set and return the result
#'
#' This function creates an adapter pattern that allows running a computation on a view of the data
#' and copying the results back to the original dataset.
#'
#' @param daf A Daf object to adapt
#' @param computation A function that takes a DafWriter and performs computations on it
#' @param input_axes Optional named list specifying axes to expose as input
#' @param input_data Optional named list specifying data to expose as input
#' @param capture Function to create a capture Daf (default: memory_daf)
#' @param output_axes Optional named list specifying axes to expose as output
#' @param output_data Optional named list specifying data to expose as output
#' @param empty Optional named list mapping data keys to values for filling missing data
#' @param relayout Whether to allow relayout when copying results (default: TRUE)
#' @param overwrite Whether to overwrite existing data when copying results (default: FALSE)
#' @return The result of the computation function
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/adapters.html#DataAxesFormats.Adapters.adapter) for details.
#' @export
adapter <- function(
    daf,
    computation,
    input_axes = NULL,
    input_data = NULL,
    capture = memory_daf,
    output_axes = NULL,
    output_data = NULL,
    empty = NULL,
    relayout = TRUE,
    overwrite = FALSE) {
    validate_daf_object(daf)

    # Set up base name for the components
    base_name <- julia_call("get_object_field", daf$jl_obj, "name", need_return = "R")

    # Create input view
    input_daf <- viewer(
        daf,
        name = paste0(base_name, ".input"),
        axes = input_axes,
        data = input_data
    )

    # Create captured daf
    captured_daf <- capture(name = paste0(base_name, ".capture"))

    # Chain input and captured daf
    adapted_daf <- chain_writer(
        list(input_daf, captured_daf),
        name = paste0(base_name, ".adapted")
    )

    # Run the computation on the adapted data
    result <- computation(adapted_daf)

    # Create output view
    output_daf <- viewer(
        adapted_daf,
        name = paste0(base_name, ".output"),
        axes = output_axes,
        data = output_data
    )

    # Copy the results back to the original daf
    copy_all(
        destination = daf,
        source = output_daf,
        empty = empty,
        relayout = relayout,
        overwrite = overwrite
    )

    return(result)
}

#' Get the name of a Daf object
#'
#' @param daf A Daf object
#' @return The name of the Daf object
#' @noRd
jl_get_name <- function(daf) {
    validate_daf_object(daf)
    return(julia_call("get_object_field", daf$jl_obj, "name", need_return = "R"))
}
