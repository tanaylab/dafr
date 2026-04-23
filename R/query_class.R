#' @include classes.R
NULL

#' Pipe-composable query object.
#'
#' Produced by the query builders (`Axis()`, `LookupVector()`, ...) and
#' their composition via `|>`. Carries both the parsed AST (a list in
#' the shape `parse_query()` returns) and its canonical string form.
#'
#' @param ast List of AST nodes.
#' @param canonical Character scalar; the canonical query string for
#'   `ast`.
#' @return A `DafrQuery` instance.
#' @examples
#' DafrQuery(
#'     ast = list(list(op = "Axis", axis_name = "cell")),
#'     canonical = "@ cell"
#' )
#' @seealso [get_query()]
#' @export
DafrQuery <- S7::new_class(
    name = "DafrQuery",
    package = "dafr",
    properties = list(
        ast       = S7::class_list,
        canonical = S7::class_character
    ),
    validator = function(self) {
        if (length(self@canonical) != 1L || is.na(self@canonical)) {
            return("`canonical` must be a non-NA character scalar")
        }
        NULL
    }
)

# S7 registers these against the base generics so ordinary
# `format(q)` / `print(q)` / `length(q)` / `as.character(q)` dispatch
# correctly. S3 methods on classes whose names contain "::" (S7's
# package-qualified form) can't be registered cleanly via NAMESPACE,
# so S7 method registration is the right hook here.
S7::method(format, DafrQuery) <- function(x, ...) x@canonical
S7::method(as.character, DafrQuery) <- function(x, ...) x@canonical
S7::method(print, DafrQuery) <- function(x, ...) {
    cat("<DafrQuery>", x@canonical, "\n")
    invisible(x)
}
S7::method(length, DafrQuery) <- function(x) length(x@ast)
