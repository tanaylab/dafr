#' Convert h5ad file to a Daf object
#'
#' View AnnData as a Daf data set, specifically using a MemoryDaf
#'
#' @param h5ad Path to the h5ad file
#' @param name Optional name for the Daf object
#' @param obs_is Optional name for the observation axis
#' @param var_is Optional name for the variable axis
#' @param X_is Optional name for the main matrix
#' @param unsupported_handler How to handle unsupported features (one of IGNORE_HANDLER, WARN_HANDLER, or ERROR_HANDLER)
#' @return A Daf object
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/anndata_format.html#anndata_as_daf) for details.
#' Note that this function works with h5ad file paths, as the Julia AnnData object comes from the Muon.jl package
#' and is not compatible with R/python anndata implementations.
#' @export
h5ad_as_daf <- function(h5ad, name = NULL, obs_is = NULL, var_is = NULL, X_is = NULL,
                        unsupported_handler = WARN_HANDLER) {
    h5ad <- normalizePath(h5ad)

    # Get Julia handler object
    jl_handler <- get_julia_handler(unsupported_handler)

    # Call Julia function
    result <- julia_call("DataAxesFormats.anndata_as_daf",
        h5ad,
        name = name,
        obs_is = obs_is,
        var_is = var_is,
        X_is = X_is,
        unsupported_handler = jl_handler
    )

    # Wrap in Daf object
    return(Daf(result))
}

#' Convert Daf object to h5ad file
#'
#' View the Daf data set as AnnData and save it to an h5ad file
#'
#' @param daf A Daf object
#' @param h5ad Path where the h5ad file will be written
#' @param obs_is Optional name for the observation axis
#' @param var_is Optional name for the variable axis
#' @param X_is Optional name for the main matrix
#' @return Invisibly returns the input Daf object
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/anndata_format.html#DataAxesFormats.AnnDataFormat.daf_as_anndata) for details.
#' Note this just creates the h5ad file. The Julia (Muon.jl) AnnData object is not returned or exposed to R.
#' @export
daf_as_h5ad <- function(daf, h5ad, obs_is = NULL, var_is = NULL, X_is = NULL) {
    validate_daf_object(daf)

    h5ad <- normalizePath(h5ad)

    # Call Julia function
    julia_call("DataAxesFormats.daf_as_anndata",
        daf$jl_obj,
        obs_is = obs_is,
        var_is = var_is,
        X_is = X_is,
        h5ad = h5ad
    )

    invisible(daf)
}
