#' Concatenate multiple Daf data sets along some axis
#'
#' @param destination A Daf object to write the concatenated result to
#' @param axis Name of the axis or axes to concatenate along
#' @param sources A list of Daf objects to concatenate
#' @param names Optional vector of names for the sources
#' @param dataset_axis Optional name for the dataset axis
#' @param dataset_property Whether to add a dataset property
#' @param prefix Whether to prefix the names of properties in the source data sets
#' @param prefixed Optional set of names that are already prefixed and should be left alone
#' @param empty Value to use for filling in missing data
#' @param sparse_if_saves_storage_fraction Fraction of storage savings required to use sparse matrices
#' @param merge Optional named list mapping property keys to merge actions ("SkipProperty", "LastValue", or "CollectAxis")
#' @param overwrite Whether to overwrite existing data
#' @return The destination Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/concat.html) for details.
#' @export
concatenate <- function(
    destination,
    axis,
    sources,
    names = NULL,
    dataset_axis = "dataset",
    dataset_property = TRUE,
    prefix = FALSE,
    prefixed = NULL,
    empty = NULL,
    sparse_if_saves_storage_fraction = 0.1,
    merge = NULL,
    overwrite = FALSE) {
    validate_daf_object(destination)

    # Validate sources is a list of Daf objects
    if (!is.list(sources)) {
        cli::cli_abort("{.field sources} must be a list of Daf objects")
    }

    for (source in sources) {
        validate_daf_object(source)
    }

    # Convert sources to Julia vector of DafReader objects
    sources_jl <- lapply(sources, function(src) src$jl_obj)
    # Convert to a proper Julia Vector{DafReader}
    sources_jl <- julia_call("_to_daf_readers", sources_jl, need_return = "Julia")

    # Convert merge parameter if provided
    if (!is.null(merge)) {
        # Valid merge actions
        valid_actions <- c("SkipProperty", "LastValue", "CollectAxis")

        # Check that all values are valid merge actions
        invalid_actions <- setdiff(unlist(merge), valid_actions)
        if (length(invalid_actions) > 0) {
            cli::cli_abort("Invalid merge actions: {.val {invalid_actions}}. Must be one of: {.val {valid_actions}}")
        }

        # Convert to Julia format
        merge_pairs <- list()
        for (i in seq_along(merge)) {
            key <- names(merge)[i]
            action <- merge[[i]]
            action_jl <- julia_eval(paste0("DataAxesFormats.", action))
            merge_pairs[[i]] <- list(key, action_jl)
        }

        merge_data <- julia_call("_pairify_merge", merge_pairs, need_return = "Julia")
    } else {
        merge_data <- NULL
    }

    # Call the Julia function
    # Note: Pass the first three arguments as positional arguments, not named arguments
    julia_call(
        "DataAxesFormats.concatenate!",
        destination$jl_obj,
        to_julia_array(axis),
        sources_jl,
        names = if (is.null(names)) NULL else to_julia_array(names),
        dataset_axis = dataset_axis,
        dataset_property = dataset_property,
        prefix = prefix, # Don't convert boolean to array
        prefixed = if (is.null(prefixed)) NULL else to_julia_array(prefixed),
        empty = empty,
        sparse_if_saves_storage_fraction = sparse_if_saves_storage_fraction,
        merge = merge_data,
        overwrite = overwrite
    )

    invisible(destination)
}
