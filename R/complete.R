#' @include classes.R files_daf.R memory_daf.R chain_daf.R view_daf.R readers.R writers.R
NULL

# Julia data-key for a view-data entry key: a scalar name stays a plain string;
# a 2/3-element vector/matrix key becomes the stringified Julia tuple, e.g.
# c("cell","age") -> '("cell", "age")' (matches DataAxesFormats JSON of a
# Tuple dict key; the reader maps ()->[] and JSON-parses it back).
.view_data_key <- function(key) {
    if (length(key) == 1L) return(as.character(key))
    paste0("(", paste0('"', key, '"', collapse = ", "), ")")
}

# Serialize viewer axes/data (each a list of list(key,value) or list(name=value)
# items) to Julia's base_daf_view object JSON: {"axes":{name:query},
# "data":{datakey:query}}. Empty axes/data are omitted.
.view_spec_to_julia_json <- function(axes, data) {
    to_obj <- function(items, is_data) {
        if (is.null(items) || length(items) == 0L) return(NULL)
        parsed <- lapply(items, .parse_view_item)
        keys <- vapply(parsed, function(p)
            if (is_data) .view_data_key(p$key) else as.character(p$key),
            character(1L))
        vals <- lapply(parsed, function(p) jsonlite::unbox(as.character(p$value)))
        stats::setNames(vals, keys)
    }
    spec <- list()
    a <- to_obj(axes, FALSE); if (!is.null(a)) spec$axes <- a
    d <- to_obj(data, TRUE);  if (!is.null(d)) spec$data <- d
    as.character(jsonlite::toJSON(spec, auto_unbox = TRUE))
}

# Decode a Julia data-key back to a dafr view key: a tuple-encoded string
# '("cell", "age")' -> c("cell","age"); a plain name stays a string. Mirrors
# Julia's parse: map ()->[] and JSON-parse.
.view_decode_key <- function(key) {
    if (startsWith(key, "(") && endsWith(key, ")")) {
        bracketed <- paste0("[", substr(key, 2L, nchar(key) - 1L), "]")
        return(unlist(jsonlite::fromJSON(bracketed), use.names = FALSE))
    }
    key
}

# Parse a Julia base_daf_view object (axes or data) into dafr's viewer spec form:
# a list of list(key, query). `spec_obj` is the parsed named list (from
# fromJSON(simplifyVector=FALSE)); names are the keys, values the query strings.
.view_spec_from_julia_json <- function(spec_obj, is_data) {
    if (is.null(spec_obj) || length(spec_obj) == 0L) return(NULL)
    keys <- names(spec_obj)
    lapply(seq_along(spec_obj), function(i) {
        k <- if (is_data) .view_decode_key(keys[[i]]) else keys[[i]]
        list(k, as.character(spec_obj[[i]]))
    })
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
    if (is.null(base_path)) {
        stop("base_daf has no filesystem path -- only FilesDaf supported by complete_chain", call. = FALSE)
    }
    if (is.null(new_path)) {
        stop("new_daf has no filesystem path -- only FilesDaf supported by complete_chain", call. = FALSE)
    }
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

# Cross-platform absolute-path check. Unix: leading /. Windows: drive-letter
# form (C:/ or C:\) or UNC (leading \\).
.is_absolute_path <- function(path) {
    startsWith(path, "/") ||
        grepl("^[A-Za-z]:[/\\\\]", path) ||
        startsWith(path, "\\\\")
}

# Resolve a daf's on-disk path. FilesDaf stores `path` in its internal env.
# Returns NULL for memory-backed dafs (no on-disk location). Mirrors Julia's
# complete_path which returns nothing for non-Files dafs.
.complete_path <- function(daf) {
    internal <- tryCatch(S7::prop(daf, "internal"),
        error = function(e) NULL)
    if (is.null(internal) || is.null(internal$path)) return(NULL)
    internal$path
}

#' Canonical disk path of a (possibly chained) daf.
#'
#' Public alias of the internal `.complete_path`. For a `FilesDaf`,
#' returns the root directory on disk. For a chain whose last writer
#' is a `FilesDaf`, returns that directory. Errors on dafs with no
#' on-disk location.
#'
#' @param daf A [DafReader].
#' @return Character scalar (absolute path).
#' @examples
#' tmp <- tempfile("dafr-")
#' dir.create(tmp)
#' fd <- files_daf(tmp, mode = "w+", name = "fd")
#' complete_path(fd)
#' unlink(tmp, recursive = TRUE)
#' @export
complete_path <- function(daf) .complete_path(daf)

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
            base <- format_get_scalar(d, "base_daf_repository")$value
            if (!.is_absolute_path(base)) {
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

    # A `base_daf_view` (stored on the leaf) applies to the BASE only - the
    # viewer wraps the base sub-chain and the leaf is chained ON TOP, so
    # leaf-local data stays visible and overrides the viewed base (matching the
    # write side `chain(list(viewer(base), new))` and Julia's collect_dafs,
    # complete.jl:106-122). Wrapping the WHOLE chain in the viewer instead would
    # reinterpret leaf data through the view and hide it.
    has_view <- format_has_scalar(leaf_daf, "base_daf_view") &&
        length(readers) >= 2L
    if (has_view) {
        spec <- jsonlite::fromJSON(
            format_get_scalar(leaf_daf, "base_daf_view")$value,
            simplifyVector = FALSE
        )
        base_readers <- readers[-length(readers)]
        base_chain <- if (length(base_readers) == 1L) {
            base_readers[[1L]]
        } else {
            chain_reader(base_readers, name = paste0(chain_name, ".base"))
        }
        viewed_base <- viewer(base_chain, name = paste0(chain_name, ".view"),
            axes = .normalise_json_spec(spec$axes),
            data = .normalise_json_spec(spec$data))
        readers <- list(viewed_base, leaf_daf)
    }

    if (length(readers) == 1L) {
        readers[[1L]]
    } else if (identical(mode, "r")) {
        chain_reader(readers, name = chain_name)
    } else {
        chain_writer(readers, name = chain_name)
    }
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
