#' @include utils.R
NULL

# Tokenizer — see dev/plans/2026-04-20-slice-3-queries-views.md task Q1.
# Reference: DataAxesFormats.jl tokens.jl + queries.jl:2780.

.QUERY_OP_REGEX <- "^(?:[!<>]=|!~|[\\|-]/|[&^\\|]!|\\?\\?|@[-\\|]|=@|::|>[->\\|]|\\|\\||[!&*%./:<=>?@\\[\\]^\\|~])"

.tokenize_query <- function(s) {
    stopifnot(is.character(s), length(s) == 1L, !is.na(s))
    tokens <- list()
    i <- 1L
    n <- nchar(s)
    while (i <= n) {
        ch <- substr(s, i, i)
        if (grepl("\\s", ch, perl = TRUE)) {
            i <- i + 1L
            next
        }
        op <- regmatches(
            substr(s, i, n),
            regexpr(.QUERY_OP_REGEX, substr(s, i, n), perl = TRUE)
        )
        if (length(op) == 1L && nzchar(op)) {
            tokens[[length(tokens) + 1L]] <- list(
                type = "operator",
                value = op, pos = i
            )
            i <- i + nchar(op)
            next
        }
        val_info <- .scan_value(s, i)
        if (is.null(val_info)) {
            stop(sprintf(
                "unexpected character %s at position %d in query %s",
                sQuote(ch), i, sQuote(s)
            ), call. = FALSE)
        }
        tokens[[length(tokens) + 1L]] <- list(
            type = "value",
            value = val_info$value,
            pos = i
        )
        i <- val_info$next_pos
    }
    tokens
}

.scan_value <- function(s, start) {
    n <- nchar(s)
    if (substr(s, start, start) == "\"") {
        j <- start + 1L
        out <- character(0)
        while (j <= n) {
            ch <- substr(s, j, j)
            if (ch == "\\" && j < n) {
                out <- c(out, substr(s, j + 1L, j + 1L))
                j <- j + 2L
            } else if (ch == "\"") {
                return(list(value = paste0(out, collapse = ""), next_pos = j + 1L))
            } else {
                out <- c(out, ch)
                j <- j + 1L
            }
        }
        stop(sprintf(
            "unterminated quoted value at position %d in query %s",
            start, sQuote(s)
        ), call. = FALSE)
    }
    # Decimal numeric literal (e.g. "2.0", "1.5").  Must be tried before the
    # general value regex because '.' is otherwise treated as an operator token.
    nm <- regmatches(
        substr(s, start, n),
        regexpr("^\\d+\\.\\d+(?:[eE][+-]?\\d+)?",
            substr(s, start, n),
            perl = TRUE
        )
    )
    if (length(nm) == 1L && nzchar(nm)) {
        return(list(value = nm, next_pos = start + nchar(nm)))
    }
    m <- regmatches(
        substr(s, start, n),
        regexpr("^[^\\s!&*%./:<=>?@\\[\\]^\\|~\"$#`]+",
            substr(s, start, n),
            perl = TRUE
        )
    )
    if (length(m) == 1L && nzchar(m)) {
        list(value = m, next_pos = start + nchar(m))
    } else {
        NULL
    }
}
