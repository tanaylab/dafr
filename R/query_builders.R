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

.make_typed_reduction <- function(op_name, qop_builder) {
    force(op_name)
    force(qop_builder)
    function(type = NULL, ...) {
        dots <- list(...)
        res <- .extract_query_and_value(type, missing(type), dots, required = FALSE)
        if (!is.null(res$value) && (!is.character(res$value) || length(res$value) != 1L)) {
            cli::cli_abort("`type` must be a character scalar or NULL")
        }
        # Separate the type from other dots (params like eps, p, na_rm).
        params <- dots[vapply(dots, function(x) !inherits_dafr_query(x), logical(1L))]
        # If the type was in dots (first non-query arg), drop it.
        if (length(params) && identical(params[[1L]], res$value)) {
            params <- params[-1L]
        }
        frag <- .build_fragment(qop_builder, res$value, params)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}
