#' @include classes.R format_api.R handlers.R
#' @importFrom R6 R6Class
NULL

.read_only_error <- function() {
    stop(
        "DafAnnData facade is read-only. Use the underlying Daf object to modify data.",
        call. = FALSE
    )
}

.auto_obs_axis <- function(daf) {
    for (candidate in c("cell", "metacell")) {
        if (format_has_axis(daf, candidate)) {
            return(candidate)
        }
    }
    stop("no obs_axis could be auto-detected (looked for 'cell', 'metacell'); pass obs_axis explicitly", call. = FALSE)
}

.auto_var_axis <- function(daf) {
    if (format_has_axis(daf, "gene")) {
        return("gene")
    }
    stop("no var_axis could be auto-detected (looked for 'gene'); pass var_axis explicitly", call. = FALSE)
}

#' Read-only AnnData-shaped facade over a Daf.
#'
#' Exposes a `DafReader` through the property names familiar to
#' `anndata` / `scanpy` / `Seurat` users: `X`, `obs`, `var`, `layers`,
#' `uns`, `obs_names`, `var_names`, `n_obs`, `n_vars`, `shape`. All
#' bindings are read-only; modifying data requires writing to the
#' underlying `Daf` directly.
#'
#' @param daf A [DafReader].
#' @param obs_axis Axis name for observations. Defaults auto-detect:
#'   `"cell"` then `"metacell"`.
#' @param var_axis Axis name for variables. Defaults to `"gene"`.
#' @param x_name Matrix name for `X`. Default `"UMIs"`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' add_axis(d, "gene", c("g1", "g2", "g3"))
#' set_matrix(d, "cell", "gene", "UMIs", matrix(1:6, 2, 3))
#' ann <- as_anndata(d)
#' ann$X
#' ann$obs_names
#' @seealso [as_anndata()]
#' @export
DafAnnData <- R6::R6Class(
    classname = "DafAnnData",
    public = list(
        #' @field daf The underlying Daf object.
        daf = NULL,
        #' @field obs_axis Name of the observations axis.
        obs_axis = NULL,
        #' @field var_axis Name of the variables axis.
        var_axis = NULL,
        #' @field x_name Matrix name for `X`.
        x_name = NULL,
        #' @description Create a `DafAnnData` facade.
        #' @param daf A [DafReader].
        #' @param obs_axis Axis name for observations. Defaults auto-detect:
        #'   `"cell"` then `"metacell"`.
        #' @param var_axis Axis name for variables. Defaults to `"gene"`.
        #' @param x_name Matrix name for `X`. Default `"UMIs"`.
        initialize = function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
            if (!is_daf(daf)) {
                stop("`daf` must be a DafReader", call. = FALSE)
            }
            self$daf <- daf
            self$obs_axis <- obs_axis %||% .auto_obs_axis(daf)
            self$var_axis <- var_axis %||% .auto_var_axis(daf)
            self$x_name <- x_name
        }
    ),
    active = list(
        #' @field X The primary matrix (`obs_axis` x `var_axis`).
        X = function(value) {
            if (!missing(value)) .read_only_error()
            get_matrix(self$daf, self$obs_axis, self$var_axis, self$x_name)
        },
        #' @field obs Data frame of observation vectors.
        obs = function(value) {
            if (!missing(value)) .read_only_error()
            get_dataframe(self$daf, self$obs_axis)
        },
        #' @field var Data frame of variable vectors.
        var = function(value) {
            if (!missing(value)) .read_only_error()
            get_dataframe(self$daf, self$var_axis)
        },
        #' @field obs_names Character vector of observation names.
        obs_names = function(value) {
            if (!missing(value)) .read_only_error()
            format_axis_array(self$daf, self$obs_axis)
        },
        #' @field var_names Character vector of variable names.
        var_names = function(value) {
            if (!missing(value)) .read_only_error()
            format_axis_array(self$daf, self$var_axis)
        },
        #' @field n_obs Number of observations.
        n_obs = function(value) {
            if (!missing(value)) .read_only_error()
            as.integer(length(format_axis_array(self$daf, self$obs_axis)))
        },
        #' @field n_vars Number of variables.
        n_vars = function(value) {
            if (!missing(value)) .read_only_error()
            as.integer(length(format_axis_array(self$daf, self$var_axis)))
        },
        #' @field shape Dimensions `c(n_obs, n_vars)`.
        shape = function(value) {
            if (!missing(value)) .read_only_error()
            c(self$n_obs, self$n_vars)
        },
        #' @field layers Named list of additional matrices (excluding `X`).
        layers = function(value) {
            if (!missing(value)) .read_only_error()
            mats <- format_matrices_set(self$daf, self$obs_axis, self$var_axis)
            mats <- setdiff(mats, self$x_name)
            stats::setNames(
                lapply(mats, function(nm) {
                    get_matrix(self$daf, self$obs_axis, self$var_axis, nm)
                }),
                mats
            )
        },
        #' @field uns Named list of scalars.
        uns = function(value) {
            if (!missing(value)) .read_only_error()
            scalars <- format_scalars_set(self$daf)
            stats::setNames(
                lapply(scalars, function(nm) get_scalar(self$daf, nm)),
                scalars
            )
        }
    )
)

#' One-shot factory for a [DafAnnData] facade.
#'
#' @param daf A [DafReader].
#' @param obs_axis Axis name for observations. Defaults auto-detect:
#'   `"cell"` then `"metacell"`.
#' @param var_axis Axis name for variables. Defaults to `"gene"`.
#' @param x_name Matrix name for `X`. Default `"UMIs"`.
#' @return A [DafAnnData] instance.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' add_axis(d, "gene", c("g1", "g2"))
#' set_matrix(d, "cell", "gene", "UMIs", matrix(1:4, 2, 2))
#' ann <- as_anndata(d)
#' @seealso [DafAnnData]
#' @export
as_anndata <- function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
    DafAnnData$new(daf, obs_axis = obs_axis, var_axis = var_axis, x_name = x_name)
}
