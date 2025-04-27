#' Set a seed both in Julia and R
#'
#' @param seed seed to be used
#'
#' @return No return value, called for side effects.
#' @examples
#' \dontrun{
#' ## Needs previous call to `setup_daf` which is time
#' ## consuming and requires Julia
#' setup_daf(installJulia = TRUE, seed = 123)
#' set_seed(123)
#' }
#' @export
set_seed <- function(seed) {
    JuliaCall::julia_command(paste0("Random.seed!(", seed, ");"))
    set.seed(seed)
    cli::cli_alert_info("Set the seed of Julia and R to {.val {seed}}")
}

#' Null coalescing operator from R 4.1+, backported for compatibility
#'
#' @param x First value
#' @param y Second value (used if x is NULL)
#' @return x if not NULL, otherwise y
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
    if (is.null(x)) y else x
}

# Generic handler ---------------------------------------------------------
# Works for required and optional arguments and for parameterless ops.
# ╔══════════════════════════════════════════════════════════════════════════════════════╗
# ║ Pattern (user call) │ What arrives in the R function │ Result of ║
# ║ │ (first formal + …) │ extract… ║
# ╠══════════════════════════════════════════════════════════════════════════════════════╣
# ║ 1. No pipe ║
# ║ Convert("Float32") type = "Float32", … = ∅ ⇒ query =NULL ║
# ║ value="Float32"│║
# ║ provided=TRUE ║
# ╠══════════════════════════════════════════════════════════════════════════════════════╣
# ║ 2. Pipe with explicit value ║
# ║ query |> Convert("Float32") type = "Float32", … = query ⇒ query=query║
# ║ value="Float32"│║
# ║ provided=TRUE ║
# ╠══════════════════════════════════════════════════════════════════════════════════════╣
# ║ 3. Pipe relying on default value ║
# ║ query |> Names() kind = query, … = ∅ ⇒ query=query║
# ║ value=NULL ║
# ║ provided=FALSE║
# ╠══════════════════════════════════════════════════════════════════════════════════════╣
# ║ 4. Pipe into parameter-less op ║
# ║ query |> Abs() arg_val = NULL, … = query ⇒ query=query║
# ║ value=NULL ║
# ║ provided=FALSE║
# ╠══════════════════════════════════════════════════════════════════════════════════════╣
# ║ 5. Query handed in “…” (no pipe) ║
# ║ Convert("Float32", query) type = "Float32", … = query ⇒ query=query║
# ║ value="Float32"│║
# ║ provided=TRUE ║
# ╚══════════════════════════════════════════════════════════════════════════════════════╣
extract_query_and_value <- function(arg_val,
                                    arg_missing,
                                    dots,
                                    required = FALSE,
                                    default = NULL) {
    # identify where the query object is (if at all)
    if (!arg_missing && inherits(arg_val, "JuliaObject")) {
        query <- arg_val
        dots <- dots # possibly holds the value
        value <- if (length(dots) && !inherits(dots[[1]], "JuliaObject")) {
            dots[[1]]
        } else {
            default
        }
    } else if (length(dots) && inherits(dots[[1]], "JuliaObject")) {
        query <- dots[[1]]
        value <- if (arg_missing) default else arg_val
    } else {
        query <- NULL
        value <- if (arg_missing) default else arg_val
    }

    provided <- !is.null(value) || (!arg_missing && !inherits(arg_val, "JuliaObject"))

    list(
        query = query,
        value = value,
        provided = provided
    )
}
