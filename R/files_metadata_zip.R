#' @include files_daf.R
NULL

# Write axes/metadata.json: sorted JSON array of axis names currently present
# in axes/*.txt. Mirrors upstream files_format.jl::write_axes_metadata!.
.write_axes_metadata <- function(path) {
    axes_dir <- file.path(path, "axes")
    axes <- character(0)
    if (dir.exists(axes_dir)) {
        files <- list.files(axes_dir, pattern = "\\.txt$", full.names = FALSE)
        axes <- sort(sub("\\.txt$", "", files))
    }
    writeLines(jsonlite::toJSON(axes, auto_unbox = FALSE),
               con = file.path(axes_dir, "metadata.json"))
    invisible()
}

# Rebuild metadata.zip from scratch by walking the tree. Atomic via
# metadata.zip.new + rename. Mirrors files_format.jl::metadata_zip_rebuild!.
.metadata_zip_rebuild <- function(path) {
    .write_axes_metadata(path)
    zip_path <- file.path(path, "metadata.zip")
    staging <- paste0(zip_path, ".new")
    if (file.exists(staging)) unlink(staging, force = TRUE)

    # Build entry list deterministically (sorted) so byte-identical archives
    # come out of the rebuild path on identical trees.
    entries <- character(0)
    if (file.exists(file.path(path, "daf.json"))) {
        entries <- c(entries, "daf.json")
    }
    if (file.exists(file.path(path, "axes", "metadata.json"))) {
        entries <- c(entries, "axes/metadata.json")
    }
    scalars_dir <- file.path(path, "scalars")
    if (dir.exists(scalars_dir)) {
        files <- sort(list.files(scalars_dir, pattern = "\\.json$"))
        if (length(files) > 0L) {
            entries <- c(entries, paste0("scalars/", files))
        }
    }
    vec_dir <- file.path(path, "vectors")
    if (dir.exists(vec_dir)) {
        for (axis in sort(list.files(vec_dir, full.names = FALSE))) {
            d <- file.path(vec_dir, axis)
            if (!dir.exists(d)) next
            files <- sort(list.files(d, pattern = "\\.json$"))
            if (length(files) > 0L) {
                entries <- c(entries, paste0("vectors/", axis, "/", files))
            }
        }
    }
    mat_dir <- file.path(path, "matrices")
    if (dir.exists(mat_dir)) {
        for (rows in sort(list.files(mat_dir, full.names = FALSE))) {
            rows_d <- file.path(mat_dir, rows)
            if (!dir.exists(rows_d)) next
            for (cols in sort(list.files(rows_d, full.names = FALSE))) {
                cols_d <- file.path(rows_d, cols)
                if (!dir.exists(cols_d)) next
                files <- sort(list.files(cols_d, pattern = "\\.json$"))
                if (length(files) > 0L) {
                    entries <- c(entries,
                                 paste0("matrices/", rows, "/", cols, "/", files))
                }
            }
        }
    }

    # Use slice-17 MmapZipStore for the staging file. Close via the cpp11
    # entry to release the file before rename.
    store <- new_mmap_zip_store(staging, mode = "w+")
    closed <- FALSE
    on.exit({
        if (!closed) try(dafr_mmap_zip_close(S7::prop(store, "xptr")), silent = TRUE)
        if (file.exists(staging)) unlink(staging, force = TRUE)
    }, add = TRUE)

    for (rel in entries) {
        bytes <- readBin(file.path(path, rel),
                         what = "raw", n = file.size(file.path(path, rel)))
        store_set_bytes(store, rel, bytes)
    }
    dafr_mmap_zip_close(S7::prop(store, "xptr"))
    closed <- TRUE
    if (!file.rename(staging, zip_path)) {
        stop(sprintf(".metadata_zip_rebuild: failed to rename %s to %s",
                     sQuote(staging), sQuote(zip_path)), call. = FALSE)
    }
    on.exit()  # disarm: rename consumed staging; nothing left to clean up
    invisible(zip_path)
}

# Append one already-on-disk JSON file as a new entry into metadata.zip.
# The entry name is its relative path under `path`. The slice-17
# MmapZipStore is append-only (zip archives don't support in-place
# rewrite), so on a collision (a `set_*(..., overwrite = TRUE)` rewriting
# an existing JSON descriptor in place — dafr does NOT route overwrites
# through delete + create) we fall back to a full rebuild. Rebuild is
# also used when metadata.zip is missing entirely. Mirrors
# files_format.jl::metadata_zip_append! plus its ensure_metadata_zip
# precondition.
.metadata_zip_append <- function(path, relative_path) {
    full <- file.path(path, relative_path)
    bytes <- readBin(full, what = "raw", n = file.size(full))
    zip_path <- file.path(path, "metadata.zip")
    if (!file.exists(zip_path)) {
        # Defensive: if metadata.zip is missing (e.g., user deleted it,
        # or this is the first set_* on a freshly-initialised store),
        # rebuild from scratch — append on a missing target would be a
        # hard error. Matches upstream ensure_metadata_zip semantics.
        .metadata_zip_rebuild(path)
        return(invisible())
    }
    store <- new_mmap_zip_store(zip_path, mode = "r+")
    closed <- FALSE
    on.exit(if (!closed) try(dafr_mmap_zip_close(S7::prop(store, "xptr")), silent = TRUE), add = TRUE)
    existing <- store_list(store, "")
    if (relative_path %in% existing) {
        # Overwrite case: close the live store, then rebuild from the
        # tree. The on-disk JSON has already been rewritten with the new
        # bytes by the caller, so the rebuild picks up the new content.
        dafr_mmap_zip_close(S7::prop(store, "xptr"))
        closed <- TRUE
        .metadata_zip_rebuild(path)
        return(invisible())
    }
    store_set_bytes(store, relative_path, bytes)
    dafr_mmap_zip_close(S7::prop(store, "xptr"))
    closed <- TRUE
    invisible()
}

#' Pack a FilesDaf directory's JSON metadata into `metadata.zip`.
#'
#' Walks `path` and bundles `daf.json`, `axes/metadata.json`, every
#' `scalars/*.json`, every `vectors/<axis>/*.json`, and every
#' `matrices/<rows>/<cols>/*.json` into a single `path/metadata.zip` archive,
#' written atomically via `metadata.zip.new` + rename. Required for serving a
#' FilesDaf over HTTP via [http_daf()]; from dafr 0.2.0 onward, FilesDaf
#' writes maintain the bundle automatically — call this only to repack a
#' tree that was built by an older dafr (pre-0.2.0) or modified outside
#' dafr.
#'
#' @param path Directory path to a FilesDaf root.
#' @return The absolute path to the written `metadata.zip`, invisibly.
#' @examples
#' p <- tempfile("daf-")
#' files_daf(p, "w+")
#' pack_files_daf_metadata(p)
#' @export
pack_files_daf_metadata <- function(path) {
    stopifnot(is.character(path), length(path) == 1L, !is.na(path))
    if (!file.exists(file.path(path, "daf.json"))) {
        stop(sprintf("pack_files_daf_metadata: %s is not a FilesDaf directory (no daf.json)",
                     sQuote(path)), call. = FALSE)
    }
    invisible(.metadata_zip_rebuild(path))
}
