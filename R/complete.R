#' Complete chain of Daf repositories
#'
#' Open a complete chain of Daf repositories by tracing back through the `base_daf_repository` property.
#' Each repository in a chain contains a scalar property called `base_daf_repository` which identifies
#' its parent repository (if any).
#'
#' @param leaf Path to the leaf repository, which will be traced back through its ancestors
#' @param mode Mode to open the repositories ("r" for read-only, "r+" for read-write)
#' @param name Optional name for the complete Daf object
#' @return A Daf object combining the leaf repository with all its ancestors
#' @details If mode is "r+", only the first (leaf) repository is opened in write mode. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.CompleteDaf.complete_daf) for details.
#' @export
complete_daf <- function(leaf, mode = "r", name = NULL) {
    # Validate mode parameter
    if (!mode %in% c("r", "r+")) {
        cli::cli_abort("Mode must be one of 'r' or 'r+'")
    }

    # Call the Julia implementation directly
    jl_obj <- julia_call("DataAxesFormats.CompleteDaf.complete_daf", leaf, mode, name = name)

    return(Daf(jl_obj))
}

#' Open a Daf repository based on path
#'
#' This function determines whether to open a files-based Daf or an HDF5-based Daf
#' based on the file path.
#'
#' @param path Path to the Daf repository
#' @param mode Mode to open the storage ("r" for read-only, "r+" for read-write)
#' @param name Optional name for the Daf object
#' @return A Daf object (either files_daf or h5df)
#' @export
open_daf <- function(path, mode = "r", name = NULL) {
    if (endsWith(path, ".h5df") || grepl("\\.h5df//", path)) {
        return(h5df(path, mode, name = name))
    } else {
        return(files_daf(path, mode, name = name))
    }
}
