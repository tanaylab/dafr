#' Load example cells data into a Daf object
#'
#' @param name Name for the Daf object (default: "cells!")
#' @return A Daf object containing example cells data
#' @details This function loads example cells data from the DataAxesFormats.jl package.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/example_data.html) for details.
#' @export
example_cells_daf <- function(name = "cells!") {
    if (!is.character(name)) {
        cli::cli_abort("{.field name} must be a character string")
    }

    jl_obj <- julia_call("DataAxesFormats.ExampleData.example_cells_daf", name = name)
    Daf(jl_obj)
}

#' Load example metacells data into a Daf object
#'
#' @param name Name for the Daf object (default: "metacells!")
#' @return A Daf object containing example metacells data
#' @details This function loads example metacells data from the DataAxesFormats.jl package.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/example_data.html) for details.
#' @export
example_metacells_daf <- function(name = "metacells!") {
    if (!is.character(name)) {
        cli::cli_abort("{.field name} must be a character string")
    }

    jl_obj <- julia_call("DataAxesFormats.ExampleData.example_metacells_daf", name = name)
    Daf(jl_obj)
}

#' Load example chain data into a Daf object
#'
#' @param name Name for the Daf object (default: "chain!")
#' @return A Daf object containing chained example cells and metacells data
#' @details This function creates a chain of example cells and metacells data.
#'   See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/example_data.html) for details.
#' @export
example_chain_daf <- function(name = "chain!") {
    if (!is.character(name)) {
        cli::cli_abort("{.field name} must be a character string")
    }

    jl_obj <- julia_call("DataAxesFormats.ExampleData.example_chain_daf", name = name)
    Daf(jl_obj)
}
