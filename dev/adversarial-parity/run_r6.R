suppressMessages(devtools::load_all("/home/aviezerl/src/dafr-native"))
suppressMessages(library(jsonlite))

FIX   <- "/home/aviezerl/src/dafr-native/dev/adversarial-parity/fixture.daf"
QFILE <- "/home/aviezerl/src/dafr-native/dev/adversarial-parity/queries6.txt"
OUT   <- "/home/aviezerl/src/dafr-native/dev/adversarial-parity/r_out6.jsonl"

daf <- files_daf(FIX, mode = "r")

fmt_num <- function(x) {
    if (inherits(x, "integer64")) x <- as.double(x)
    nms_save <- names(x); names(x) <- NULL
    if (is.numeric(x)) {
        out <- vector("list", length(x))
        for (i in seq_along(x)) {
            v <- x[[i]]
            if (is.na(v) || is.nan(v) || is.infinite(v)) {
                out[[i]] <- as.character(v)
            } else {
                out[[i]] <- v
            }
        }
        out
    } else if (is.logical(x)) {
        out <- vector("list", length(x))
        for (i in seq_along(x)) out[[i]] <- if (is.na(x[[i]])) "NA" else as.logical(x[[i]])
        out
    } else {
        out <- vector("list", length(x))
        for (i in seq_along(x)) out[[i]] <- as.character(x[[i]])
        out
    }
}

is_scalar_class <- function(x) {
    inherits(x, c("integer64")) && length(x) == 1 ||
    (is.atomic(x) && length(x) == 1 && is.null(names(x)) && !is.matrix(x))
}

serialize_result <- function(x) {
    if (is.null(x)) return(list(kind = "nothing"))
    if (is.matrix(x) || inherits(x, "Matrix")) {
        m <- as.matrix(x)
        if (inherits(m, "integer64")) {
            # Drop integer64 attribute, convert to double
            dm <- dim(m); dn <- dimnames(m)
            m <- as.double(m); dim(m) <- dm; dimnames(m) <- dn
        }
        rn <- rownames(m); cn <- colnames(m)
        vals <- vector("list", nrow(m))
        for (i in seq_len(nrow(m))) vals[[i]] <- fmt_num(m[i, ])
        return(list(
            kind = "matrix",
            type = paste(class(x), collapse = "/"),
            dim = dim(m),
            rownames = if (is.null(rn) && nrow(m) > 0L) NA
                       else if (is.null(rn)) list() else rn,
            colnames = if (is.null(cn) && ncol(m) > 0L) NA
                       else if (is.null(cn)) list() else cn,
            values = vals
        ))
    }
    if (inherits(x, "integer64")) {
        nms <- names(x)
        x <- as.double(x)
        names(x) <- nms
    }
    if (is_scalar_class(x)) {
        v <- x
        if (is.numeric(v) && (is.na(v) || is.nan(v) || is.infinite(v))) v <- as.character(v)
        return(list(kind = "scalar", type = paste(class(x), collapse = "/"), value = v))
    }
    if (is.vector(x) || is.factor(x)) {
        nms <- names(x)
        return(list(
            kind = "vector",
            type = paste(class(x), collapse = "/"),
            length = length(x),
            names = if (is.null(nms)) NA else nms,
            values = fmt_num(x)
        ))
    }
    list(kind = "other", type = paste(class(x), collapse = "/"), value = as.character(x))
}

lines <- readLines(QFILE)
con <- file(OUT, "w")
for (i in seq_along(lines)) {
    raw <- lines[i]
    line <- trimws(raw)
    if (line == "" || substr(line, 1, 1) == "#") next
    rec <- list(idx = i, query = line)
    out <- tryCatch({
        r <- get_query(daf, line)
        rec$status <- "ok"
        rec$result <- serialize_result(r)
        rec
    }, error = function(e) {
        rec$status <- "error"
        rec$error <- conditionMessage(e)
        rec$error_type <- class(e)[1]
        rec
    })
    writeLines(jsonlite::toJSON(out, auto_unbox = TRUE, null = "null", na = "string", digits = 15), con)
}
close(con)
cat("WROTE:", OUT, "\n")
