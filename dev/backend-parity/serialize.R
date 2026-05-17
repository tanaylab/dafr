# dev/backend-parity/serialize.R
#
# Normalised JSON record for a single read from a backend. Shared
# between round_trip.R and cross_format.R so the diff tool sees one
# schema across runs.
#
# Record fields (all keys always emitted; absent ones are NA so the
# downstream diff tool can detect "missing"):
#
#   backend     : "memory" | "files" | "zarr" | ...
#   key         : "<kind>|<axis>|[<cols_axis>|]<name>" — joins to manifest
#   kind        : "scalar" | "vector" | "matrix" | "axis"
#   status      : "ok" | "error"
#   error       : NA or message string
#   error_type  : NA or condition class
#   r_class     : R class of the value (paste of class())
#   dtype       : eltype canonical string (Bool/Int32/Int64/Float64/String)
#   storage     : "dense" | "sparse" | NA (sparse is reserved for matrices)
#   shape       : integer vector (length 0 for scalar)
#   names_hash  : hash of dimnames / names attribute (NA if unnamed)
#   names       : full names (vector) or row+col names (matrix); inlined
#                 only for short outputs to keep JSONL readable
#   value_hash  : content hash (post-canonicalisation)
#   value       : full canonicalised value (NaN/Inf -> strings,
#                 integer64 -> strings)
#   sparse_p    : column pointers (sparse matrix only)
#   sparse_i    : row indices (sparse matrix only)
#   sparse_x    : nonzero values (sparse matrix only)

suppressMessages(library(jsonlite))
suppressMessages(library(digest))
suppressMessages(library(Matrix))
suppressMessages(library(bit64))

# Map an R value to a canonical eltype string. Same mapping as
# .dtype_for_r_vector but exposed for the harness.
.bp_dtype <- function(v) {
    if (is.null(v)) return(NA_character_)
    if (is.logical(v)) return("Bool")
    if (inherits(v, "integer64")) return("Int64")
    if (is.integer(v)) return("Int32")
    if (is.double(v)) return("Float64")
    if (is.character(v)) return("String")
    NA_character_
}

# Convert any atomic vector into a JSON-safe list (strings for special
# float / int64 values, plain values otherwise).
.bp_canonicalise <- function(v) {
    if (length(v) == 0L) return(list())
    if (inherits(v, "integer64")) {
        # bit64 stringify is lossless
        return(as.list(as.character(v)))
    }
    if (is.logical(v)) {
        out <- vector("list", length(v))
        for (i in seq_along(v)) {
            out[[i]] <- if (is.na(v[[i]])) "NA" else as.logical(v[[i]])
        }
        return(out)
    }
    if (is.numeric(v)) {
        out <- vector("list", length(v))
        for (i in seq_along(v)) {
            x <- v[[i]]
            if (is.na(x) && !is.nan(x)) {
                out[[i]] <- "NA"
            } else if (is.nan(x)) {
                out[[i]] <- "NaN"
            } else if (is.infinite(x)) {
                out[[i]] <- if (x > 0) "Inf" else "-Inf"
            } else {
                out[[i]] <- x
            }
        }
        return(out)
    }
    if (is.character(v)) {
        out <- vector("list", length(v))
        for (i in seq_along(v)) {
            # enc2utf8 normalises the Encoding() tag so backends that
            # return bytes-equal but tag-different strings (FilesDaf
            # "unknown" vs Memory "UTF-8") hash the same.
            out[[i]] <- if (is.na(v[[i]])) "NA" else enc2utf8(v[[i]])
        }
        return(out)
    }
    # Fallback: stringify
    as.list(as.character(v))
}

# A content-hash that's stable across runs and ignores R-side
# attribute noise. Operates on the already-canonicalised list.
.bp_hash <- function(canonical_list) {
    digest::digest(canonical_list, algo = "xxhash64", serialize = TRUE)
}

# names-hash (NA when unnamed). enc2utf8 normalises Encoding() tags so
# backends that return identical bytes but different tags do not
# false-flag (FilesDaf historically returned "unknown" for scalar
# strings via the regex fast path).
.bp_names_hash <- function(nms) {
    if (is.null(nms)) return(NA_character_)
    digest::digest(enc2utf8(as.character(nms)),
                   algo = "xxhash64", serialize = TRUE)
}

# Serialize the result of one read into the standard record.
# `manifest` is the matching manifest entry (so we can copy key
# fields without recomputing them).
serialize_read <- function(backend, manifest, value, status = "ok",
                           cond = NULL) {
    base <- list(
        backend    = backend,
        key        = manifest$key,
        kind       = manifest$kind,
        manifest_dtype = manifest$dtype,
        manifest_shape = manifest$shape,
        status     = status,
        error      = NA,
        error_type = NA,
        r_class    = NA,
        dtype      = NA,
        storage    = NA,
        shape      = integer(0),
        names_hash = NA,
        names      = NULL,
        rownames   = NULL,
        colnames   = NULL,
        value_hash = NA,
        value      = NULL,
        sparse_p   = NULL,
        sparse_i   = NULL,
        sparse_x   = NULL
    )
    if (status == "error") {
        base$error      <- if (!is.null(cond)) conditionMessage(cond) else "unknown"
        base$error_type <- if (!is.null(cond)) paste(class(cond), collapse = "/") else NA
        return(base)
    }

    base$r_class <- paste(class(value), collapse = "/")

    if (manifest$kind == "scalar") {
        base$dtype <- .bp_dtype(value)
        base$shape <- integer(0)
        canon <- .bp_canonicalise(value)
        base$value      <- canon
        base$value_hash <- .bp_hash(canon)
        return(base)
    }

    if (manifest$kind == "axis") {
        # An axis read returns the entries vector (character).
        base$dtype <- "String"
        base$shape <- length(value)
        canon <- .bp_canonicalise(value)
        base$value      <- canon
        base$value_hash <- .bp_hash(canon)
        return(base)
    }

    if (manifest$kind == "vector") {
        base$dtype      <- .bp_dtype(value)
        base$shape      <- length(value)
        base$names      <- if (!is.null(names(value))) as.list(names(value))
                           else NULL
        base$names_hash <- .bp_names_hash(names(value))
        canon <- .bp_canonicalise(unname(value))
        base$value      <- canon
        base$value_hash <- .bp_hash(canon)
        return(base)
    }

    # ---- matrix ----
    if (inherits(value, "Matrix") || methods::is(value, "sparseMatrix")) {
        # Sparse — keep the CSC triple form.
        sv <- as(value, "CsparseMatrix")
        base$storage    <- "sparse"
        base$dtype      <- .bp_dtype(sv@x)
        base$shape      <- dim(sv)
        base$rownames   <- if (!is.null(rownames(sv))) as.list(rownames(sv)) else NULL
        base$colnames   <- if (!is.null(colnames(sv))) as.list(colnames(sv)) else NULL
        base$names_hash <- digest::digest(
            list(
                if (is.null(rownames(sv))) NULL else enc2utf8(rownames(sv)),
                if (is.null(colnames(sv))) NULL else enc2utf8(colnames(sv))
            ),
            algo = "xxhash64", serialize = TRUE
        )
        # 1-based indices for diff readability.
        base$sparse_p <- as.list(as.integer(sv@p))
        base$sparse_i <- as.list(as.integer(sv@i) + 1L)
        canon_x <- .bp_canonicalise(sv@x)
        base$sparse_x   <- canon_x
        base$value_hash <- .bp_hash(list(base$shape, base$sparse_p,
                                         base$sparse_i, canon_x))
        return(base)
    }
    # Dense matrix.
    if (!is.matrix(value)) {
        # Should not happen — but guard so the runner survives.
        base$storage    <- "unknown"
        base$dtype      <- .bp_dtype(value)
        base$shape      <- if (is.null(dim(value))) length(value) else dim(value)
        canon <- .bp_canonicalise(as.vector(value))
        base$value      <- canon
        base$value_hash <- .bp_hash(canon)
        return(base)
    }
    base$storage <- "dense"
    base$dtype   <- .bp_dtype(as.vector(value))
    base$shape   <- dim(value)
    base$rownames <- if (!is.null(rownames(value))) as.list(rownames(value)) else NULL
    base$colnames <- if (!is.null(colnames(value))) as.list(colnames(value)) else NULL
    base$names_hash <- digest::digest(
        list(
            if (is.null(rownames(value))) NULL else enc2utf8(rownames(value)),
            if (is.null(colnames(value))) NULL else enc2utf8(colnames(value))
        ),
        algo = "xxhash64", serialize = TRUE
    )
    # Column-major flatten.
    flat <- as.vector(value)
    canon <- .bp_canonicalise(flat)
    base$value      <- canon
    base$value_hash <- .bp_hash(list(base$shape, canon))
    base
}

write_record_jsonl <- function(con, rec) {
    writeLines(
        jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null",
                         na = "string", digits = 17),
        con
    )
}
