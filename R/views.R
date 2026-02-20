#' All scalars specifier
#'
#' A key to use in the `data` parameter of `viewer` to specify all the base data scalars.
#' See the Julia documentation for details.
#'
#' @format Character string
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.ALL_SCALARS}
#' @export
ALL_SCALARS <- "*"

#' All axes specifier
#'
#' A pair to use in the `axes` parameter of `viewer` to specify all the base data axes.
#' See the Julia documentation for details.
#'
#' @format Character string
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.ALL_AXES}
#' @export
ALL_AXES <- "*"

#' All vectors specifier
#'
#' A key to use in the `data` parameter of `viewer` to specify all the vectors of the exposed axes.
#' See the Julia documentation for details.
#'
#' @format Character vector of length 2
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.ALL_VECTORS}
#' @export
ALL_VECTORS <- c("*", "*")

#' All matrices specifier
#'
#' A key to use in the `data` parameter of `viewer` to specify all the matrices of the exposed axes.
#' See the Julia documentation for details.
#'
#' @format Character vector of length 3
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.ALL_MATRICES}
#' @export
ALL_MATRICES <- c("*", "*", "*")

#' View all axes specifier
#'
#' A pair to use in the `axes` parameter of `viewer` to expose all base axes with their original names.
#' See the Julia documentation for details.
#'
#' @format A named list representing the pair ALL_AXES => "="
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.VIEW_ALL_AXES}
#' @export
VIEW_ALL_AXES <- stats::setNames(list("="), ALL_AXES)

#' View all scalars specifier
#'
#' A pair to use in the `data` parameter of `viewer` to expose all base scalars with their original names.
#' See the Julia documentation for details.
#'
#' @format A named list representing the pair ALL_SCALARS => "="
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.VIEW_ALL_SCALARS}
#' @export
VIEW_ALL_SCALARS <- stats::setNames(list("="), ALL_SCALARS)

#' View all vectors specifier
#'
#' A pair to use in the `data` parameter of `viewer` to expose all base vectors with their original names.
#' See the Julia documentation for details.
#'
#' @format A named list representing the pair ALL_VECTORS => "="
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.VIEW_ALL_VECTORS}
#' @export
VIEW_ALL_VECTORS <- stats::setNames(list("="), paste(ALL_VECTORS, collapse = ","))

#' View all matrices specifier
#'
#' A pair to use in the `data` parameter of `viewer` to expose all base matrices with their original names.
#' See the Julia documentation for details.
#'
#' @format A named list representing the pair ALL_MATRICES => "="
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.VIEW_ALL_MATRICES}
#' @export
VIEW_ALL_MATRICES <- stats::setNames(list("="), paste(ALL_MATRICES, collapse = ","))

#' View all data specifier
#'
#' A list of pairs to use in the `data` parameter of `viewer` to expose all base data
#' (scalars, vectors, and matrices) with their original names.
#' See the Julia documentation for details.
#'
#' @format A list containing VIEW_ALL_SCALARS, VIEW_ALL_VECTORS, and VIEW_ALL_MATRICES
#' @references \url{https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.VIEW_ALL_DATA}
#' @export
VIEW_ALL_DATA <- list(VIEW_ALL_SCALARS, VIEW_ALL_VECTORS, VIEW_ALL_MATRICES)

#' @title Create a read-only view of a Daf data set
#'
#' @description Wrap a Daf data set with a read-only DafView
#'
#' @param daf A Daf object
#' @param name Optional name for the view
#' @param axes Named list specifying axes to expose
#' @param data Named list specifying data to expose
#'
#' @return A read-only Daf object
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/views.html#DataAxesFormats.Views.viewer) for details.
#' @export
viewer <- function(daf, name = NULL, axes = NULL, data = NULL) {
    validate_daf_object(daf)

    # Process axes
    if (!is.null(axes)) {
        axes <- jl_pairify_axes(axes)
    }

    # Process data
    if (!is.null(data)) {
        data <- jl_pairify_data(data)
    }


    # Call the Julia viewer function
    daf_view <- julia_call("DataAxesFormats.viewer",
        daf$jl_obj,
        name = name,
        axes = axes,
        data = data,
        need_return = "Julia"
    )

    # Return a proper Daf object
    return(Daf(daf_view))
}
