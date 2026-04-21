#' @include query_ast.R query_tokens.R
NULL

# Parser: tokens -> AST. Hand-rolled state machine.
# Reference: DataAxesFormats.jl queries.jl:2108 (parse_query).

#' Parse a query string into an AST (list of `qop` nodes).
#'
#' @param query_string A character scalar.
#' @return A list of AST node records.
#' @export
parse_query <- function(query_string) {
    stopifnot(
        is.character(query_string), length(query_string) == 1L,
        !is.na(query_string)
    )
    tokens <- .tokenize_query(query_string)
    .parse_tokens(tokens, query_string)
}

.parse_tokens <- function(tokens, src) {
    ast <- list()
    i <- 1L
    n <- length(tokens)
    while (i <= n) {
        step <- .parse_next(tokens, i, src)
        ast[[length(ast) + 1L]] <- step$node
        i <- step$next_index
    }
    ast
}

.parse_next <- function(tokens, i, src) {
    tok <- tokens[[i]]
    if (tok$type == "operator") {
        switch(tok$value,
            "?" = list(node = .qop_names(), next_index = i + 1L),
            "@" = .parse_axis(tokens, i, src),
            "." = .parse_lookup(tokens, i, src, ".", .qop_lookup_scalar),
            ":" = .parse_lookup(tokens, i, src, ":", .qop_lookup_vector),
            "::" = .parse_lookup(tokens, i, src, "::", .qop_lookup_matrix),
            "[" = .parse_begin_mask(tokens, i, src, negated = FALSE),
            "]" = list(node = .qop_end_mask(), next_index = i + 1L),
            "&" = .parse_logical(tokens, i, src, .qop_and_mask),
            "|" = .parse_logical(tokens, i, src, .qop_or_mask),
            "^" = .parse_logical(tokens, i, src, .qop_xor_mask),
            "<" = .parse_cmp(tokens, i, src, .qop_is_less),
            "<=" = .parse_cmp(tokens, i, src, .qop_is_less_equal),
            "=" = .parse_cmp(tokens, i, src, .qop_is_equal),
            "!=" = .parse_cmp(tokens, i, src, .qop_is_not_equal),
            ">" = .parse_cmp(tokens, i, src, .qop_is_greater),
            ">=" = .parse_cmp(tokens, i, src, .qop_is_greater_equal),
            "~" = .parse_match_cmp(tokens, i, src, .qop_is_match),
            "!~" = .parse_match_cmp(tokens, i, src, .qop_is_not_match),
            "@-" = .parse_cmp(tokens, i, src, .qop_square_row_is),
            "@|" = .parse_cmp(tokens, i, src, .qop_square_column_is),
            ">|" = .parse_reduction(tokens, i, src, .qop_reduce_to_column),
            ">-" = .parse_reduction(tokens, i, src, .qop_reduce_to_row),
            "/" = .parse_lookup_like(tokens, i, src, .qop_group_by),
            "-/" = .parse_lookup_like(tokens, i, src, .qop_group_rows_by),
            "|/" = .parse_lookup_like(tokens, i, src, .qop_group_columns_by),
            "*" = .parse_lookup_like(tokens, i, src, .qop_count_by),
            "%" = .parse_eltwise(tokens, i, src),
            "||" = .parse_if_missing(tokens, i, src),
            "??" = .parse_if_not(tokens, i, src),
            "=@" = .parse_lookup_like(tokens, i, src, .qop_as_axis),
            stop(sprintf(
                "unexpected operator %s at position %d in query %s",
                sQuote(tok$value), tok$pos, sQuote(src)
            ), call. = FALSE)
        )
    } else {
        stop(sprintf(
            "expected operator, got value %s at position %d in query %s",
            sQuote(tok$value), tok$pos, sQuote(src)
        ), call. = FALSE)
    }
}

.parse_axis <- function(tokens, i, src) {
    # `@ ?` means "list all axis names" — emit a bare Names node
    if (i + 1L <= length(tokens) &&
        tokens[[i + 1L]]$type == "operator" &&
        tokens[[i + 1L]]$value == "?") {
        return(list(node = .qop_names(), next_index = i + 2L))
    }
    if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
        bad_pos <- if (i + 1L <= length(tokens)) {
            tokens[[i + 1L]]$pos
        } else {
            tokens[[i]]$pos + nchar(tokens[[i]]$value)
        }
        stop(sprintf(
            "expected axis name after '@' at position %d in query %s",
            bad_pos, sQuote(src)
        ), call. = FALSE)
    }
    list(node = .qop_axis(tokens[[i + 1L]]$value), next_index = i + 2L)
}

.parse_lookup <- function(tokens, i, src, tok_val, ctor) {
    if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
        # bare lookup (no name) -- allowed by Julia grammar
        list(node = ctor(NULL), next_index = i + 1L)
    } else {
        list(node = ctor(tokens[[i + 1L]]$value), next_index = i + 2L)
    }
}

.parse_begin_mask <- function(tokens, i, src, negated) {
    if (i + 1L > length(tokens)) {
        stop(sprintf(
            "expected property after '[' at position %d in query %s",
            tokens[[i]]$pos, sQuote(src)
        ), call. = FALSE)
    }
    nxt <- tokens[[i + 1L]]
    if (nxt$type == "operator" && nxt$value == "!") {
        if (i + 2L > length(tokens) || tokens[[i + 2L]]$type != "value") {
            stop(sprintf(
                "expected property name after '[ !' at position %d in query %s",
                nxt$pos, sQuote(src)
            ), call. = FALSE)
        }
        list(
            node = .qop_begin_mask(tokens[[i + 2L]]$value, negated = TRUE),
            next_index = i + 3L
        )
    } else if (nxt$type == "value") {
        list(
            node = .qop_begin_mask(nxt$value, negated = FALSE),
            next_index = i + 2L
        )
    } else {
        stop(sprintf(
            "expected property after '[' at position %d in query %s",
            tokens[[i]]$pos, sQuote(src)
        ), call. = FALSE)
    }
}

.parse_logical <- function(tokens, i, src, ctor) {
    if (i + 1L > length(tokens)) {
        stop(sprintf(
            "expected property after logical operator at position %d in query %s",
            tokens[[i]]$pos, sQuote(src)
        ), call. = FALSE)
    }
    nxt <- tokens[[i + 1L]]
    if (nxt$type == "operator" && nxt$value == "!") {
        if (i + 2L > length(tokens) || tokens[[i + 2L]]$type != "value") {
            stop(sprintf(
                "expected property after '<op> !' at position %d in query %s",
                nxt$pos, sQuote(src)
            ), call. = FALSE)
        }
        list(
            node = ctor(tokens[[i + 2L]]$value, negated = TRUE),
            next_index = i + 3L
        )
    } else if (nxt$type == "value") {
        list(
            node = ctor(nxt$value, negated = FALSE),
            next_index = i + 2L
        )
    } else {
        stop(sprintf(
            "expected property after logical operator at position %d in query %s",
            tokens[[i]]$pos, sQuote(src)
        ), call. = FALSE)
    }
}

.parse_cmp <- function(tokens, i, src, ctor) {
    if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
        stop(sprintf(
            "expected value after comparator at position %d in query %s",
            tokens[[i]]$pos, sQuote(src)
        ), call. = FALSE)
    }
    list(node = ctor(tokens[[i + 1L]]$value), next_index = i + 2L)
}

.parse_lookup_like <- function(tokens, i, src, ctor) {
    if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
        stop(sprintf(
            "expected name after %s at position %d in query %s",
            sQuote(tokens[[i]]$value), tokens[[i]]$pos,
            sQuote(src)
        ), call. = FALSE)
    }
    list(node = ctor(tokens[[i + 1L]]$value), next_index = i + 2L)
}

.parse_reduction <- function(tokens, i, src, ctor) {
    if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
        stop(sprintf(
            "expected reduction name after %s at position %d in query %s",
            sQuote(tokens[[i]]$value), tokens[[i]]$pos,
            sQuote(src)
        ), call. = FALSE)
    }
    nxt <- tokens[[i + 1L]]
    params <- list()
    j <- i + 2L
    while (j + 1L <= length(tokens) && tokens[[j]]$type == "value") {
        k <- tokens[[j]]$value
        # Optional ':' between key and value; accept both "key: value" and "key value"
        val_idx <- if (j + 1L <= length(tokens) &&
            tokens[[j + 1L]]$type == "operator" &&
            tokens[[j + 1L]]$value == ":") {
            j + 2L
        } else {
            j + 1L
        }
        if (val_idx > length(tokens) || tokens[[val_idx]]$type != "value") break
        params[[k]] <- tokens[[val_idx]]$value
        j <- val_idx + 1L
    }
    list(node = ctor(nxt$value, params = params), next_index = j)
}

.parse_eltwise <- function(tokens, i, src) {
    if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
        stop(sprintf(
            "expected eltwise op name after '%%' at position %d in query %s",
            tokens[[i]]$pos, sQuote(src)
        ), call. = FALSE)
    }
    nxt <- tokens[[i + 1L]]
    params <- list()
    j <- i + 2L
    while (j + 1L <= length(tokens) && tokens[[j]]$type == "value") {
        k <- tokens[[j]]$value
        # Optional ':' between key and value; accept both "key: value" and "key value"
        val_idx <- if (j + 1L <= length(tokens) &&
            tokens[[j + 1L]]$type == "operator" &&
            tokens[[j + 1L]]$value == ":") {
            j + 2L
        } else {
            j + 1L
        }
        if (val_idx > length(tokens) || tokens[[val_idx]]$type != "value") break
        params[[k]] <- tokens[[val_idx]]$value
        j <- val_idx + 1L
    }
    list(node = .qop_eltwise(nxt$value, params = params), next_index = j)
}

.parse_if_missing <- function(tokens, i, src) {
    if (i + 1L <= length(tokens) && tokens[[i + 1L]]$type == "value") {
        list(node = .qop_if_missing(tokens[[i + 1L]]$value), next_index = i + 2L)
    } else {
        list(node = .qop_if_missing(NULL), next_index = i + 1L)
    }
}

.parse_if_not <- function(tokens, i, src) {
    if (i + 1L <= length(tokens) && tokens[[i + 1L]]$type == "value") {
        list(node = .qop_if_not(tokens[[i + 1L]]$value), next_index = i + 2L)
    } else {
        list(node = .qop_if_not(NULL), next_index = i + 1L)
    }
}

# Match comparators (~, !~) take a regex pattern which may contain operator
# characters (e.g. "^a").  Collect all consecutive tokens until "]" or end.
.parse_match_cmp <- function(tokens, i, src, ctor) {
    if (i + 1L > length(tokens)) {
        stop(sprintf(
            "expected pattern after match operator at position %d in query %s",
            tokens[[i]]$pos, sQuote(src)
        ), call. = FALSE)
    }
    j <- i + 1L
    parts <- character(0)
    n <- length(tokens)
    while (j <= n) {
        tok <- tokens[[j]]
        if (tok$type == "operator" && tok$value == "]") break
        parts <- c(parts, tok$value)
        j <- j + 1L
    }
    if (length(parts) == 0L) {
        stop(sprintf(
            "expected pattern after match operator at position %d in query %s",
            tokens[[i]]$pos, sQuote(src)
        ), call. = FALSE)
    }
    list(node = ctor(paste(parts, collapse = "")), next_index = j)
}
