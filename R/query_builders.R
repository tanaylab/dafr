#' @include classes.R query_class.R query_ast.R query_parse.R
NULL

# Detect a DafrQuery as the pipe target in a builder's arguments.
# Mirrors wrapper R/utils.R:65.
.extract_query_and_value <- function(arg_val, arg_missing, dots,
                                     required = FALSE, default = NULL) {
    if (!arg_missing && inherits_dafr_query(arg_val)) {
        query <- arg_val
        value <- if (length(dots) && !inherits_dafr_query(dots[[1L]])) {
            dots[[1L]]
        } else {
            default
        }
    } else if (length(dots) && inherits_dafr_query(dots[[1L]])) {
        query <- dots[[1L]]
        value <- if (arg_missing) default else arg_val
    } else {
        query <- NULL
        value <- if (arg_missing) default else arg_val
    }
    provided <- !is.null(value) ||
        (!arg_missing && !inherits_dafr_query(arg_val))
    list(query = query, value = value, provided = provided)
}

inherits_dafr_query <- function(x) S7::S7_inherits(x, DafrQuery)

# Compose a DafrQuery from a prior pipe-target and a newly-built node.
.compose_query <- function(prior, new_ast_frag, new_canonical_frag) {
    if (is.null(prior)) {
        DafrQuery(ast = new_ast_frag, canonical = new_canonical_frag)
    } else {
        DafrQuery(
            ast = c(prior@ast, new_ast_frag),
            canonical = paste(prior@canonical, new_canonical_frag)
        )
    }
}

# Build a one-node AST fragment + its canonical string.
# `qop_builder` must be an existing `.qop_*` function in R/query_ast.R.
# Returns list(ast = list(node), canonical = character_scalar).
.build_fragment <- function(qop_builder, ...) {
    node <- qop_builder(...)
    frag_canonical <- .canonicalise_ast(list(node))
    list(ast = list(node), canonical = frag_canonical)
}

# ---- Factory helpers ------------------------------------------------------

.make_nullary <- function(op_name, qop_builder) {
    force(op_name)
    force(qop_builder)
    function(...) {
        dots <- list(...)
        non_query <- Filter(function(x) !inherits_dafr_query(x), dots)
        if (length(non_query) > 0L) {
            cli::cli_abort(
                "{.code {op_name}} expects zero arguments or one query object"
            )
        }
        res <- .extract_query_and_value(NULL, TRUE, dots, required = FALSE)
        frag <- .build_fragment(qop_builder)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}

.make_string_op <- function(op_name, qop_builder, param_name = "property") {
    force(op_name)
    force(qop_builder)
    force(param_name)
    function(value, ...) {
        res <- .extract_query_and_value(value, missing(value), list(...), required = TRUE)
        if (!res$provided) {
            cli::cli_abort("`{param_name}` is missing with no default")
        }
        if (!is.character(res$value) || length(res$value) != 1L) {
            cli::cli_abort("`{param_name}` must be a character scalar")
        }
        frag <- .build_fragment(qop_builder, res$value)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}

.make_value_op <- function(op_name, qop_builder, param_name = "value") {
    force(op_name)
    force(qop_builder)
    force(param_name)
    function(value, ...) {
        res <- .extract_query_and_value(value, missing(value), list(...), required = TRUE)
        if (!res$provided) {
            cli::cli_abort("`{param_name}` is missing with no default")
        }
        frag <- .build_fragment(qop_builder, res$value)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}

.make_optional_string_op <- function(op_name, qop_builder, param_name = "value") {
    force(op_name)
    force(qop_builder)
    force(param_name)
    function(value = NULL, ...) {
        res <- .extract_query_and_value(value, missing(value), list(...), required = FALSE, default = NULL)
        if (!is.null(res$value) && (!is.character(res$value) || length(res$value) != 1L)) {
            cli::cli_abort("`{param_name}` must be a character scalar or NULL")
        }
        frag <- .build_fragment(qop_builder, res$value)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}

# Factory for `ReduceToColumn` / `ReduceToRow`, which consume a prior
# reduction (e.g. `Sum()`, `Quantile(p = 0.5)`) and rewrap its last AST
# node as a `ReduceToColumn` / `ReduceToRow` node with the same
# reduction name and params. Supports chaining: `prior |> ReduceToColumn(Sum())`.
.make_reduce_to <- function(op_name, qop_builder) {
    force(op_name)
    force(qop_builder)
    function(reduction, ...) {
        dots <- list(...)
        # Dispatch: `reduction` may be the pipe target (DafrQuery that is
        # a full query so far) with the real reduction in dots, OR the
        # actual reduction (single-node DafrQuery).
        if (missing(reduction)) {
            cli::cli_abort("{.field reduction} is missing with no default")
        }
        query <- NULL
        actual_reduction <- reduction
        if (inherits_dafr_query(reduction) && length(dots) >= 1L &&
            inherits_dafr_query(dots[[1L]])) {
            query <- reduction
            actual_reduction <- dots[[1L]]
        }
        if (!inherits_dafr_query(actual_reduction)) {
            cli::cli_abort(
                "{.field reduction} must be a {.cls DafrQuery}"
            )
        }
        # The reduction DafrQuery should have a trailing Eltwise node
        # (e.g. built by `Sum()`, `Quantile(p = 0.5)`). Extract its name
        # and params for the ReduceToColumn/ReduceToRow node.
        last_node <- actual_reduction@ast[[length(actual_reduction@ast)]]
        if (is.null(last_node) || last_node$op != "Eltwise") {
            cli::cli_abort(
                "{.code {op_name}} requires a reduction query (e.g. {.code Sum()}), got a query whose trailing node is {.val {last_node$op}}"
            )
        }
        frag <- .build_fragment(
            qop_builder,
            last_node$name,
            last_node$params
        )
        .compose_query(query, frag$ast, frag$canonical)
    }
}

.make_typed_reduction <- function(op_name, qop_builder) {
    force(op_name)
    force(qop_builder)
    function(type = NULL, ...) {
        dots <- list(...)
        # Detect the pipe target: it can land in `type` (when the only
        # pipe-target is positional) or in the first unnamed entry of
        # `...`. Named params (e.g. `base = 2`) are never a pipe target.
        query <- NULL
        type_value <- NULL
        if (!missing(type) && inherits_dafr_query(type)) {
            query <- type
        } else if (!missing(type) && !is.null(type)) {
            type_value <- type
        }
        # Scan dots for a DafrQuery (only one allowed) and strip it.
        if (length(dots)) {
            is_q <- vapply(dots, inherits_dafr_query, logical(1L))
            if (any(is_q)) {
                if (!is.null(query)) {
                    cli::cli_abort(
                        "{.code {op_name}} received more than one {.cls DafrQuery}"
                    )
                }
                query <- dots[[which(is_q)[1L]]]
                dots <- dots[!is_q]
            }
        }
        # If `type` was omitted positionally but passed via a named
        # entry in dots, pick it up.
        if (is.null(type_value) && !is.null(dots[["type"]])) {
            type_value <- dots[["type"]]
            dots[["type"]] <- NULL
        }
        if (!is.null(type_value) &&
            (!is.character(type_value) || length(type_value) != 1L)) {
            cli::cli_abort("`type` must be a character scalar or NULL")
        }
        params <- dots
        frag <- .build_fragment(qop_builder, type_value, params)
        .compose_query(query, frag$ast, frag$canonical)
    }
}
