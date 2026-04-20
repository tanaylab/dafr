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
  stopifnot(is.character(query_string), length(query_string) == 1L,
            !is.na(query_string))
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
      "?"  = list(node = .qop_names(), next_index = i + 1L),
      "@"  = .parse_axis(tokens, i, src),
      "."  = .parse_lookup(tokens, i, src, ".", .qop_lookup_scalar),
      ":"  = .parse_lookup(tokens, i, src, ":", .qop_lookup_vector),
      "::" = .parse_lookup(tokens, i, src, "::", .qop_lookup_matrix),
      stop(sprintf("unexpected operator %s at position %d in query %s",
                   sQuote(tok$value), tok$pos, sQuote(src)), call. = FALSE)
    )
  } else {
    stop(sprintf("expected operator, got value %s at position %d in query %s",
                 sQuote(tok$value), tok$pos, sQuote(src)), call. = FALSE)
  }
}

.parse_axis <- function(tokens, i, src) {
  if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
    bad_pos <- if (i + 1L <= length(tokens)) tokens[[i + 1L]]$pos
               else tokens[[i]]$pos + nchar(tokens[[i]]$value)
    stop(sprintf("expected axis name after '@' at position %d in query %s",
                 bad_pos, sQuote(src)), call. = FALSE)
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
