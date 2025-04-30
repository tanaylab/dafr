#' Daf (Data Axes Format) S3 object
#'
#' An S3 object that wraps a Julia DataAxesFormats object and provides access to its methods.
#'
#' @param jl_obj The Julia DafReader object to wrap
#' @return A Daf S3 object
#' @details See the Julia documentation [here](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/index.html),
#' [here](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/formats.html#Read-API)
#' and [here](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/formats.html#Write-API) for details.
#' @export
Daf <- function(jl_obj) {
    obj <- structure(list(jl_obj = jl_obj), class = "Daf")
    return(obj)
}

#' Print method for Daf objects
#'
#' Prints a description of the Daf object using Julia's description function
#'
#' @param x The Daf object to print
#' @param ... Additional arguments passed to print
#' @return The Daf object (invisibly)
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.description) for details.
#' @export
print.Daf <- function(x, ...) {
    description <- julia_call("DataAxesFormats.description", x$jl_obj)
    cat(description)
    invisible(x)
}

#' Get description of a Daf object
#'
#' @param daf A Daf object
#' @param cache Whether to include cache information
#' @param deep Whether to include detailed information about nested data
#' @return A string description of the Daf object
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/readers.html#DataAxesFormats.Readers.description) for details.
#' @export
description <- function(daf, cache = FALSE, deep = FALSE) {
    validate_daf_object(daf)
    julia_call("DataAxesFormats.description", daf$jl_obj, cache = cache, deep = deep)
}

#' Check if object is a Daf
#'
#' @param x Object to check
#' @return TRUE if x is a Daf object, FALSE otherwise
#' @export
is_daf <- function(x) {
    inherits(x, "Daf")
}

#' Validate that an object is a Daf object
#'
#' @param daf Object to check
#' @param call The calling environment
#' @return NULL, invisibly. Throws an error if object is not a Daf object.
#' @noRd
validate_daf_object <- function(daf, call = parent.frame()) {
    if (!is_daf(daf)) {
        cli::cli_abort("Expected a Daf object but got {class(daf)[1]}", call = call)
    }
    invisible(NULL)
}

#' Extract results from a Daf object using a query
#'
#' @param x A Daf object
#' @param i A query string or object
#' @param ... Ignored. Present for compatibility with the `[` generic.
#' @return The result of the query
#' @details The expression `daf[query]` is equivalent to `get_query(daf, query, cache = FALSE)`.
#' See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Operations.get_query) for details.
#' @export
`[.Daf` <- function(x, i, ...) {
    get_query(x, i, cache = FALSE)
}
