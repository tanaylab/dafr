#' Create a Daf object with in-memory storage
#'
#' This function creates a Daf object that stores data in memory. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/memory_format.html) for details.
#'
#' @param name The name of the Daf object (default: "memory")
#' @return A Daf object with in-memory storage
#' @export
memory_daf <- function(name = "memory") {
    jl_obj <- julia_call("DataAxesFormats.MemoryDaf", name = name)
    return(Daf(jl_obj))
}

#' Create a Daf object with file-based storage
#'
#' This function creates a Daf object that stores data in disk files. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/files_format.html) for details.
#'
#' @param path Path to the files storage location
#' @param mode Mode to open the storage ("r" for read-only, "r+" for read-write)
#' @param name Optional name for the Daf object
#' @return A Daf object with file-based storage
#' @export
files_daf <- function(path, mode = "r", name = NULL) {
    path <- normalizePath(path, mustWork = FALSE)
    jl_obj <- julia_call("DataAxesFormats.FilesDaf", path, mode, name = name)
    return(Daf(jl_obj))
}

#' Create a Daf object with HDF5-based storage
#'
#' This function creates a Daf object that stores data in an HDF5 disk file. See the Julia
#' [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/h5df_format.html) for details.
#'
#' @param root Path to the HDF5 file, or a Julia HDF5 File or Group object
#' @param mode Mode to open the storage ("r" for read-only, "r+" for read-write)
#' @param name Optional name for the Daf object
#' @return A Daf object with HDF5-based storage
#' @export
h5df <- function(root, mode = "r", name = NULL) {
    root <- normalizePath(root, mustWork = FALSE)
    jl_obj <- julia_call("DataAxesFormats.H5df", root, mode, name = name)
    return(Daf(jl_obj))
}

#' Create a read-only chain wrapper of DafReader objects
#'
#' This function creates a read-only chain wrapper of DafReader objects, presenting them as a single DafReader.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/chains.html#DataAxesFormats.Chains.chain_reader) for details.
#'
#' @param dsets List of Daf objects to chain
#' @param name Optional name for the chained Daf object
#' @return A read-only Daf object chaining the input Daf objects
#' @export
chain_reader <- function(dsets, name = NULL) {
    jl_dsets <- lapply(dsets, function(dset) dset$jl_obj)
    jl_obj <- julia_call("DataAxesFormats.chain_reader",
        julia_call("_to_daf_readers", jl_dsets),
        name = name
    )
    return(Daf(jl_obj))
}

#' Create a writable chain wrapper of DafReader objects
#'
#' This function creates a chain wrapper for a chain of DafReader data, presenting them as a single DafWriter.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/chains.html#DataAxesFormats.Chains.chain_writer) for details.
#'
#' @param dsets List of Daf objects to chain
#' @param name Optional name for the chained Daf object
#' @return A writable Daf object chaining the input Daf objects
#' @export
chain_writer <- function(dsets, name = NULL) {
    jl_dsets <- lapply(dsets, function(dset) dset$jl_obj)
    jl_obj <- julia_call("DataAxesFormats.chain_writer",
        julia_call("_to_daf_readers", jl_dsets),
        name = name
    )
    return(Daf(jl_obj))
}
