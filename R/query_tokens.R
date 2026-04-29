#' @include utils.R
NULL

# Tokenizer.
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
        # `# ... <eol>` line comment, per Julia tokens.jl SPACE_REGEX. Skip
        # to the next \r / \n or end of string.
        if (ch == "#") {
            j <- i + 1L
            while (j <= n && !(substr(s, j, j) %in% c("\n", "\r"))) {
                j <- j + 1L
            }
            i <- j
            next
        }
        # `''` is the canonical empty-string value token (round-trip with
        # Julia's escape_value("") == "''"). Must beat the operator regex
        # so the leading `'` isn't treated as an unexpected character.
        if (ch == "'" && i < n && substr(s, i + 1L, i + 1L) == "'") {
            tokens[[length(tokens) + 1L]] <- list(
                type = "value", value = "", pos = i
            )
            i <- i + 2L
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
    # Unquoted value: any run of value-safe characters (matching Julia's
    # is_value_char in tokens.jl - letters, digits, '_', '.', '+', '-', plus
    # any non-ASCII Unicode), with `\X` consuming the next character
    # literally so axis-entry names containing otherwise-special characters
    # can be embedded inline (e.g. `gene = AL627309\.1`).
    out <- character(0)
    j <- start
    while (j <= n) {
        ch <- substr(s, j, j)
        if (ch == "\\" && j < n) {
            out <- c(out, substr(s, j + 1L, j + 1L))
            j <- j + 2L
            next
        }
        if (grepl("^(?:[0-9a-zA-Z_.+\\-]|[^\\x00-\\x7F])$", ch, perl = TRUE)) {
            out <- c(out, ch)
            j <- j + 1L
            next
        }
        break
    }
    if (length(out) == 0L) {
        return(NULL)
    }
    list(value = paste0(out, collapse = ""), next_pos = j)
}
