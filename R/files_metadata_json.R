#' @include files_io.R
NULL

# R/files_metadata_json.R
# Julia-compatible root metadata.json: a single JSON object mapping each relative
# property path to its descriptor (DataAxesFormats FilesFormat). Replaces the old
# metadata.zip bundle. dafr's per-property .json descriptors already match Julia's
# inlined values, so rebuild inlines them verbatim; only the axis descriptor
# (n_entries) is computed. Local FilesDaf reads tree-walk and do not need this;
# it exists for HttpDaf enumeration and Julia interop.

.METADATA_JSON <- "metadata.json"

# Read a per-property descriptor file's raw JSON text (already Julia-shaped),
# collapsed to one line.
.metadata_json_inline <- function(json_path) {
    paste(readLines(json_path, warn = FALSE), collapse = "")
}

# Collect (rel_key -> raw descriptor JSON) for the whole store, sorted by key.
.metadata_json_entries <- function(root) {
    ent <- list()
    adir <- file.path(root, "axes")
    if (dir.exists(adir)) {
        for (f in sort(list.files(adir, pattern = "\\.txt$"))) {
            axis <- sub("\\.txt$", "", f)
            n <- length(readLines(file.path(adir, f), warn = FALSE))
            ent[[paste0("axes/", axis)]] <-
                sprintf('{"format":"axis","n_entries":%d}', n)
        }
    }
    for (sub in c("scalars", "vectors", "matrices")) {
        sdir <- file.path(root, sub)
        if (!dir.exists(sdir)) next
        for (jf in sort(list.files(sdir, pattern = "\\.json$", recursive = TRUE))) {
            if (basename(jf) == "metadata.json") next   # skip any legacy file
            key <- paste0(sub, "/", sub("\\.json$", "", jf))
            ent[[key]] <- .metadata_json_inline(file.path(sdir, jf))
        }
    }
    if (length(ent) == 0L) return(ent)
    ent[order(names(ent))]
}

# Assemble the JSON object text from the (key -> raw descriptor) map.
.metadata_json_assemble <- function(entries) {
    if (length(entries) == 0L) return("{}")
    body <- paste0('"', names(entries), '":', unlist(entries, use.names = FALSE),
                   collapse = ",")
    paste0("{", body, "}")
}

# Rebuild <root>/metadata.json from the tree (atomic via .new + rename).
.metadata_json_rebuild <- function(root) {
    text <- .metadata_json_assemble(.metadata_json_entries(root))
    tmp <- file.path(root, paste0(.METADATA_JSON, ".new"))
    writeLines(text, tmp, useBytes = TRUE)
    if (!file.rename(tmp, file.path(root, .METADATA_JSON))) {
        stop(sprintf(".metadata_json_rebuild: failed to rename %s", tmp),
             call. = FALSE)
    }
    invisible()
}

# Append "<key>":<descriptor> to an existing metadata.json (read, insert before
# the trailing "}"). Rebuilds from tree if the file is missing. On a key
# collision (overwrite), rebuilds from the tree so that per-property .json
# descriptors are inlined verbatim - byte-preserving, matching Julia behaviour.
# In production the caller writes the new per-property .json to disk BEFORE
# calling append, so the rebuild reads the updated descriptor automatically.
.metadata_json_append <- function(root, key, descriptor) {
    p <- file.path(root, .METADATA_JSON)
    if (!file.exists(p)) return(.metadata_json_rebuild(root))
    cur <- paste(readLines(p, warn = FALSE), collapse = "")
    if (grepl(paste0('"', key, '":'), cur, fixed = TRUE)) {
        return(.metadata_json_rebuild(root))   # collision -> rebuild from tree
    }
    inner <- sub("\\}\\s*$", "", cur)
    sep <- if (identical(trimws(inner), "{")) "" else ","
    entry <- paste0('"', key, '":', descriptor)
    writeLines(paste0(inner, sep, entry, "}"), p, useBytes = TRUE)
    invisible()
}

# Rebuild metadata.json if absent (writable-open seed).
.metadata_json_ensure <- function(root) {
    if (!file.exists(file.path(root, .METADATA_JSON))) .metadata_json_rebuild(root)
    invisible()
}

#' Rebuild a FilesDaf store's root metadata.json index.
#'
#' Writes the DataAxesFormats-compatible `metadata.json` consolidated index from
#' the on-disk tree. Use to migrate a store written by an older dafr (which used
#' `metadata.zip`) or modified outside dafr, so it can be served over HTTP and
#' read by DataAxesFormats.jl.
#'
#' @param path FilesDaf store root directory.
#' @return `path`, invisibly.
#' @export
pack_files_daf_metadata <- function(path) {
    if (!dir.exists(path)) {
        stop(sprintf("not a directory: %s", sQuote(path)), call. = FALSE)
    }
    if (!file.exists(file.path(path, "daf.json"))) {
        stop(sprintf("not a FilesDaf store (no daf.json): %s", sQuote(path)),
             call. = FALSE)
    }
    .metadata_json_rebuild(path)
    invisible(path)
}
