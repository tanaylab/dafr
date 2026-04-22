#' @include classes.R files_daf.R memory_daf.R chain_daf.R view_daf.R readers.R writers.R
NULL

#' Open a daf storage path in a given mode.
#'
#' Dispatches on path extension. Directory paths open a `FilesDaf`; paths
#' ending in `.h5df` or containing `.h5dfs#<group>` are reserved for an
#' H5df backend (not implemented this slice).
#'
#' @param path Filesystem path.
#' @param mode One of `"r"` (read-only) or `"r+"` (read-write).
#' @param name Optional daf name. Default derived from the path basename.
#' @return A `DafReader` or `DafWriter`.
#' @export
#' @examples
#' tmp <- tempfile(); dir.create(tmp)
#' files_daf(tmp, name = "tmp", mode = "w+")
#' d <- open_daf(tmp, "r")
open_daf <- function(path, mode = "r", name = NULL) {
    if (endsWith(path, ".h5df") || grepl(".h5dfs#", path, fixed = TRUE)) {
        stop("H5df backend not supported yet", call. = FALSE)
    }
    if (is.null(name)) name <- basename(path)
    if (!mode %in% c("r", "r+")) {
        stop("`mode` must be \"r\" or \"r+\"", call. = FALSE)
    }
    files_daf(path, name = name, mode = mode)
}

#' Create a persistent chain by linking `new_daf` to a `base_daf`.
#'
#' Writes a `base_daf_repository` scalar on `new_daf` that points at
#' `base_daf`'s filesystem path. If `axes` and/or `data` are specified, the
#' chain reads through a `viewer()` of `base_daf` and the spec is stored as
#' JSON under `base_daf_view`. The returned chain is `chain_writer(list(
#' viewer_or_base, new_daf))`.
#'
#' Call [complete_daf()] later to reopen the chain from disk using the
#' stored scalars.
#'
#' @param base_daf A `DafReader` on disk (its path is stored).
#' @param new_daf A `DafWriter` on disk (receives the pointer scalar).
#' @param name Optional name for the returned chain.
#' @param axes,data Optional `viewer()` axes / data spec applied on top of
#'   `base_daf`.
#' @param absolute If `TRUE`, store the absolute base path (default is
#'   relative).
#' @return The write chain.
#' @export
#' @examples
#' base_dir <- tempfile(); dir.create(base_dir)
#' new_dir <- tempfile(); dir.create(new_dir)
#' base <- files_daf(base_dir, name = "base", mode = "w+")
#' new <- files_daf(new_dir, name = "new", mode = "w+")
#' ch <- complete_chain(base_daf = base, new_daf = new, absolute = TRUE)
complete_chain <- function(base_daf, new_daf, name = NULL,
                           axes = NULL, data = NULL, absolute = FALSE) {
    base_path <- .complete_path(base_daf)
    new_path <- .complete_path(new_daf)
    stored_path <- if (isTRUE(absolute)) {
        normalizePath(base_path)
    } else {
        # Manual relative path construction (fs not in Imports).
        norm_base <- normalizePath(base_path)
        norm_new_parent <- normalizePath(dirname(new_path))
        if (identical(norm_base, norm_new_parent)) {
            "."
        } else if (startsWith(norm_base, paste0(norm_new_parent, "/"))) {
            substring(norm_base, nchar(norm_new_parent) + 2L)
        } else {
            norm_base
        }
    }
    format_set_scalar(new_daf, "base_daf_repository",
                      as.character(stored_path), overwrite = TRUE)
    reader <- if (is.null(axes) && is.null(data)) {
        base_daf
    } else {
        spec <- list(axes = axes, data = data)
        format_set_scalar(new_daf, "base_daf_view",
                          jsonlite::toJSON(spec, auto_unbox = TRUE),
                          overwrite = TRUE)
        viewer(base_daf, axes = axes, data = data,
               name = paste0(S7::prop(base_daf, "name"), ".view"))
    }
    chain_writer(list(reader, new_daf),
                 name = name %||% S7::prop(new_daf, "name"))
}

# Resolve a daf's on-disk path. FilesDaf stores `path` in its internal env.
.complete_path <- function(daf) {
    internal <- tryCatch(S7::prop(daf, "internal"),
        error = function(e) stop(
            "daf has no filesystem path -- only FilesDaf supported by complete_*",
            call. = FALSE))
    if (is.null(internal$path)) {
        stop("daf has no filesystem path -- only FilesDaf supported by complete_*",
             call. = FALSE)
    }
    internal$path
}

#' Reopen a persistent chain from disk.
#'
#' Walks the `base_daf_repository` scalar chain rooted at `leaf`, opening
#' each level with [open_daf()]. Returns a `chain_reader` (`mode = "r"`) or
#' `chain_writer` (`mode = "r+"`, only the leaf is writable).
#'
#' @param leaf Filesystem path to the leaf daf.
#' @param mode `"r"` or `"r+"`.
#' @param name Optional name.
#' @return A `DafReader` or `DafWriter`.
#' @export
#' @examples
#' tmp_root <- tempfile(); dir.create(tmp_root)
#' base_dir <- file.path(tmp_root, "base")
#' new_dir <- file.path(tmp_root, "new")
#' files_daf(base_dir, name = "base", mode = "w+")
#' new <- files_daf(new_dir, name = "new", mode = "w+")
#' complete_chain(
#'     base_daf = open_daf(base_dir, "r"),
#'     new_daf = new, absolute = TRUE
#' )
#' chain <- complete_daf(new_dir, "r")
complete_daf <- function(leaf, mode = "r", name = NULL) {
    if (!mode %in% c("r", "r+")) {
        stop("`mode` must be \"r\" or \"r+\"", call. = FALSE)
    }
    stack <- list()
    path <- leaf
    while (!is.null(path)) {
        is_leaf <- length(stack) == 0L
        open_mode <- if (is_leaf) mode else "r"
        d <- open_daf(path, open_mode)
        stack <- c(stack, list(d))
        next_path <- if (format_has_scalar(d, "base_daf_repository")) {
            base <- format_get_scalar(d, "base_daf_repository")
            if (!startsWith(base, "/")) {
                base <- normalizePath(file.path(dirname(path), base),
                                      mustWork = FALSE)
            }
            base
        } else NULL
        path <- next_path
    }
    # Stack is leaf-first; chain wants base-first (root to leaf, reads fall
    # through to last).
    leaf_daf <- stack[[1L]]
    readers <- rev(stack)
    chain_name <- name %||% basename(leaf)
    chain <- if (length(readers) == 1L) {
        readers[[1L]]
    } else if (identical(mode, "r")) {
        chain_reader(readers, name = chain_name)
    } else {
        chain_writer(readers, name = chain_name)
    }
    if (format_has_scalar(leaf_daf, "base_daf_view")) {
        spec <- jsonlite::fromJSON(
            format_get_scalar(leaf_daf, "base_daf_view"),
            simplifyVector = FALSE
        )
        chain <- viewer(chain, name = chain_name,
            axes = .normalise_json_spec(spec$axes),
            data = .normalise_json_spec(spec$data))
    }
    chain
}

# fromJSON with simplifyVector = FALSE returns JSON arrays of strings as R
# lists rather than character vectors. viewer() (via .parse_view_item) expects
# the key of a matrix item to be a character vector, not a list. This helper
# converts all-character inner lists to vectors so the spec is viewer-ready.
.normalise_json_spec <- function(x) {
    if (is.null(x) || length(x) == 0L) return(x)
    lapply(x, function(item) {
        if (!is.list(item)) return(item)
        # key-value pair: list(key, value) where key may be a char-list
        if (length(item) == 2L && is.null(names(item)) &&
            (is.character(item[[2L]]) || is.null(item[[2L]]))) {
            key <- item[[1L]]
            if (is.list(key) && all(vapply(key, is.character, logical(1L)))) {
                key <- unlist(key, use.names = FALSE)
            }
            return(list(key, item[[2L]]))
        }
        item
    })
}
