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

# Serialize a viewer axes/data spec to the JSON DataAxesFormats uses: an array
# of single-key objects ({name:query} / {datakey:query}). It is an array rather
# than an object because the order of the patterns matters and JSON objects are
# unordered.
.view_items_to_json <- function(items, is_data) {
    if (is.null(items) || length(items) == 0L) return(NULL)
    parsed <- lapply(items, .parse_view_item)
    lapply(parsed, function(p) {
        k <- if (is_data) .view_data_key(p$key) else as.character(p$key)
        stats::setNames(list(jsonlite::unbox(as.character(p$value))), k)
    })
}

.view_spec_to_julia_json <- function(axes, data) {
    spec <- list()
    a <- .view_items_to_json(axes, FALSE); if (!is.null(a)) spec$axes <- a
    d <- .view_items_to_json(data, TRUE);  if (!is.null(d)) spec$data <- d
    as.character(jsonlite::toJSON(spec))
}

# Decode a Julia data-key back to a dafr view key: a tuple-encoded string
# '("cell", "age")' -> c("cell","age"); a plain name (incl. one that merely
# starts/ends with parens but is not a valid tuple) stays a string. Mirrors
# Julia's parse: map ()->[] and JSON-parse, falling back to the literal key.
.view_decode_key <- function(key) {
    if (startsWith(key, "(") && endsWith(key, ")")) {
        bracketed <- paste0("[", substr(key, 2L, nchar(key) - 1L), "]")
        result <- tryCatch(
            unlist(jsonlite::fromJSON(bracketed), use.names = FALSE),
            error = function(e) NULL
        )
        if (!is.null(result)) return(result)
    }
    key
}

# Parse a recorded axes/data value into dafr's viewer spec form: a list of
# list(key, query). Accepts both the array-of-single-key-objects form (what both
# dafr and DataAxesFormats write) and a plain object, which is what someone
# writing the property by hand would do. `spec_obj` is the parsed value from
# fromJSON(simplifyVector = FALSE).
.view_spec_from_julia_json <- function(spec_obj, is_data) {
    if (is.null(spec_obj) || length(spec_obj) == 0L) return(NULL)
    pairs <- if (!is.null(names(spec_obj))) {
        # object form: names are the keys
        Map(function(k, v) list(k, as.character(v)), names(spec_obj), spec_obj)
    } else {
        # array form: each element is a single-key named list
        lapply(spec_obj, function(el) list(names(el)[[1L]], as.character(el[[1L]])))
    }
    pairs <- unname(pairs)
    lapply(pairs, function(p) {
        k <- if (is_data) .view_decode_key(p[[1L]]) else p[[1L]]
        list(k, p[[2L]])
    })
}


# Cross-platform absolute-path check. Unix: leading /. Windows: drive-letter
# form (C:/ or C:\) or UNC (leading \\).
.is_absolute_path <- function(path) {
    startsWith(path, "/") ||
        grepl("^[A-Za-z]:[/\\\\]", path) ||
        startsWith(path, "\\\\")
}

# Always with forward slashes, on Windows too: a recorded base path is part of
# an on-disk format that is read on other platforms, and `/` is the spelling
# every platform accepts. It also makes "is this base under the new
# repository's directory?" one comparison rather than one per separator.
.norm_path <- function(path) {
    if (is.null(path)) NULL else normalizePath(path, winslash = "/", mustWork = FALSE)
}

# Resolve a daf's on-disk path. FilesDaf and friends store `path` in their
# internal env; a memory-backed daf has none. Mirrors Julia's `complete_path`.
.complete_path <- function(daf) {
    if (S7::S7_inherits(daf, ReadOnlyChainDaf) ||
        S7::S7_inherits(daf, WriteChainDaf)) {
        return(.chain_complete_path(daf))
    }
    internal <- tryCatch(S7::prop(daf, "internal"),
        error = function(e) NULL)
    if (is.null(internal) || is.null(internal$path)) return(NULL)
    internal$path
}

# A chain has the path of its last repository only when it holds exactly what
# reopening that path would give: every repository the records lead to, and
# nothing besides. Since a repository records only its own immediate bases, this
# follows them outwards from the last repository rather than comparing the chain
# link by link, which a repository resting on several bases would not survive.
.chain_complete_path <- function(chain) {
    dafs <- .chain_dafs(chain)
    path <- .norm_path(.complete_path(dafs[[length(dafs)]]))
    if (is.null(path)) return(NULL)

    daf_of_path <- list()
    for (d in dafs) {
        daf_path <- .norm_path(.complete_path(d))
        if (is.null(daf_path)) return(NULL)
        daf_of_path[[daf_path]] <- d
    }

    reached <- character(0)
    unvisited <- path
    while (length(unvisited) > 0L) {
        daf_path <- unvisited[[length(unvisited)]]
        unvisited <- unvisited[-length(unvisited)]
        reached <- c(reached, daf_path)
        d <- daf_of_path[[daf_path]]
        if (format_has_scalar(d, "base_daf_repository")) {
            spec <- format_get_scalar(d, "base_daf_repository")$value
            for (base in .recorded_bases(spec)) {
                base_path <- .resolve_base_path(daf_path, base$path)
                if (is.null(daf_of_path[[base_path]])) return(NULL)
                if (!(base_path %in% reached)) {
                    unvisited <- c(unvisited, base_path)
                }
            }
        }
    }

    # A repository the records never lead to is one the caller chained in by
    # hand, so reopening would not give this.
    if (length(unique(reached)) != length(daf_of_path)) return(NULL)
    path
}

#' Canonical disk path of a (possibly chained) daf.
#'
#' Public alias of the internal `.complete_path`. For a `FilesDaf`,
#' returns the root directory on disk. For a chain, returns the path of
#' its last repository, but only when the chain holds exactly what
#' reopening that path with [complete_daf()] would give; otherwise
#' `NULL`. Returns `NULL` for a daf with no on-disk location.
#'
#' @param daf A [DafReader].
#' @return Character scalar, or `NULL`.
#' @examples
#' tmp <- tempfile("dafr-")
#' dir.create(tmp)
#' fd <- files_daf(tmp, mode = "w+", name = "fd")
#' complete_path(fd)
#' unlink(tmp, recursive = TRUE)
#' @export
complete_path <- function(daf) .complete_path(daf)

# A recorded base path is relative to the directory holding the repository that
# names it, unless it was stored as an absolute one.
.resolve_base_path <- function(daf_path, base_path) {
    if (.is_absolute_path(base_path)) {
        .norm_path(base_path)
    } else {
        .norm_path(file.path(dirname(daf_path), base_path))
    }
}

# The path to store for a base, relative to the directory holding the new
# repository, for the common case where a group of repositories is stored under
# a common root. Falls back to the absolute path when the base is not under it.
.relative_base_path <- function(base_path, new_directory) {
    if (identical(base_path, new_directory)) {
        "."
    } else if (startsWith(base_path, paste0(new_directory, "/"))) {
        substring(base_path, nchar(new_directory) + 2L)
    } else {
        base_path
    }
}

#' One base repository of a [complete_chain()].
#'
#' Pairs a `DafReader` with the [viewer()] parameters to apply to it,
#' which restrict it to a subset of its data and/or rename that data.
#' Pass a plain `DafReader` to [complete_chain()] instead wherever the
#' whole of it is used, which is the common case.
#'
#' @param daf A `DafReader` on disk.
#' @param axes,data Optional [viewer()] axes / data spec.
#' @return A `dafr_base_daf` spec.
#' @examples
#' tmp <- tempfile()
#' dir.create(tmp)
#' d <- files_daf(tmp, name = "d", mode = "w+")
#' spec <- base_daf(d, axes = list(list("cell", "=")))
#' class(spec)
#' @export
base_daf <- function(daf, axes = NULL, data = NULL) {
    if (!is_daf(daf)) {
        stop("`daf` must be a DafReader", call. = FALSE)
    }
    structure(list(daf = daf, axes = axes, data = data),
              class = "dafr_base_daf")
}

# Whatever the caller gave as `base_daf`, as a list of specs.
.as_base_specs <- function(bases) {
    if (inherits(bases, "dafr_base_daf")) {
        return(list(bases))
    }
    if (is_daf(bases)) {
        return(list(base_daf(bases)))
    }
    if (!is.list(bases) || length(bases) == 0L) {
        stop("`base_daf` must be a DafReader, a base_daf() spec, or a list of them",
             call. = FALSE)
    }
    lapply(bases, function(entry) {
        if (inherits(entry, "dafr_base_daf")) entry else base_daf(entry)
    })
}

# How the immediate bases are described in the `base_daf_repository` scalar. A
# lone unviewed base is stored as its path rather than as JSON, both because
# that is what almost every repository has, and because it is what someone
# looking at the property expects to see.
.base_specification <- function(bases) {
    if (length(bases) == 1L && is.null(bases[[1L]]$axes) &&
        is.null(bases[[1L]]$data)) {
        return(bases[[1L]]$path)
    }
    as.character(jsonlite::toJSON(lapply(bases, .base_json)))
}

.base_json <- function(base) {
    if (is.null(base$axes) && is.null(base$data)) {
        return(jsonlite::unbox(base$path))
    }
    json <- list(path = jsonlite::unbox(base$path))
    a <- .view_items_to_json(base$axes, FALSE); if (!is.null(a)) json$axes <- a
    d <- .view_items_to_json(base$data, TRUE);  if (!is.null(d)) json$data <- d
    json
}

# The immediate bases a repository records - only its own, never theirs. A
# repository resting on several is a JSON array, one resting on a view of a
# single one is a JSON object, and the common case of resting on the whole of a
# single one is the path itself.
.recorded_bases <- function(specification) {
    trimmed <- sub("^[[:space:]]+", "", specification)
    if (!startsWith(trimmed, "[") && !startsWith(trimmed, "{")) {
        return(list(list(path = specification, axes = NULL, data = NULL)))
    }
    json <- jsonlite::fromJSON(trimmed, simplifyVector = FALSE)
    entries <- if (is.null(names(json))) json else list(json)
    lapply(entries, function(entry) {
        if (is.character(entry)) {
            list(path = entry, axes = NULL, data = NULL)
        } else {
            if (is.null(entry$path) || !nzchar(entry$path)) {
                stop(sprintf(
                    "no path in the recorded base: %s",
                    as.character(jsonlite::toJSON(entry, auto_unbox = TRUE))
                ), call. = FALSE)
            }
            list(
                path = as.character(entry$path),
                axes = .view_spec_from_julia_json(entry$axes, is_data = FALSE),
                data = .view_spec_from_julia_json(entry$data, is_data = TRUE)
            )
        }
    })
}

# The bases of one repository, folding in a legacy `base_daf_view` scalar.
# dafr up to 0.9.0 (and DataAxesFormats before the DAG rework) stored a lone
# base path plus a separate `base_daf_view`; nothing writes that any more, but
# repositories written that way still have to open.
.recorded_bases_of <- function(daf, specification) {
    bases <- .recorded_bases(specification)
    if (length(bases) == 1L && is.null(bases[[1L]]$axes) &&
        is.null(bases[[1L]]$data) &&
        format_has_scalar(daf, "base_daf_view")) {
        view <- jsonlite::fromJSON(
            format_get_scalar(daf, "base_daf_view")$value,
            simplifyVector = FALSE
        )
        bases[[1L]]$axes <- .view_spec_from_julia_json(view$axes, is_data = FALSE)
        bases[[1L]]$data <- .view_spec_from_julia_json(view$data, is_data = TRUE)
    }
    bases
}

#' Create a persistent chain by linking `new_daf` to its base repositories.
#'
#' Immediately after creating an empty disk-based `new_daf`, chain it on
#' top of one or more disk-based base repositories and return the new
#' chain. Each base is a `DafReader`, or a [base_daf()] spec when only a
#' view of it is used. Give several when `new_daf` rests on more than one
#' repository - say, a repository of shared computed results and one of
#' the parameters this variant of the analysis uses, both resting in turn
#' on the same raw data. Later bases override earlier ones, as in any
#' chain, and a repository reached more than once is used once, at its
#' earliest position.
#'
#' This sets the `base_daf_repository` scalar of `new_daf` to describe
#' the bases, so the chain can be recreated later by [complete_daf()].
#' By default the stored paths are relative to `new_daf`, for the common
#' case where a group of repositories is stored under a common root; set
#' `absolute` to store absolute paths.
#'
#' @param base_daf A `DafReader` on disk, a [base_daf()] spec, or a list
#'   of either.
#' @param new_daf A `DafWriter` on disk (receives the pointer scalar).
#' @param name Optional name for the returned chain.
#' @param absolute If `TRUE`, store absolute base paths (default is
#'   relative).
#' @return The write chain.
#' @export
#' @examples
#' base_dir <- tempfile(); dir.create(base_dir)
#' new_dir <- tempfile(); dir.create(new_dir)
#' base <- files_daf(base_dir, name = "base", mode = "w+")
#' new <- files_daf(new_dir, name = "new", mode = "w+")
#' ch <- complete_chain(base_daf = base, new_daf = new, absolute = TRUE)
complete_chain <- function(base_daf, new_daf, name = NULL, absolute = FALSE) {
    .assert_flag(absolute, "absolute")
    new_path <- .complete_path(new_daf)
    if (is.null(new_path)) {
        stop("new_daf has no filesystem path -- only on-disk dafs support complete_chain",
             call. = FALSE)
    }
    new_directory <- dirname(.norm_path(new_path))

    recorded <- list()
    readers <- list()
    for (spec in .as_base_specs(base_daf)) {
        path <- .complete_path(spec$daf)
        if (is.null(path)) {
            stop("base_daf has no filesystem path -- only on-disk dafs support complete_chain",
                 call. = FALSE)
        }
        path <- .norm_path(path)
        base <- list(
            path = if (absolute) path else .relative_base_path(path, new_directory),
            axes = spec$axes,
            data = spec$data
        )
        # Two views of one repository are two different bases, so which
        # repository it is of is not enough to tell them apart.
        if (any(vapply(recorded, identical, logical(1), base))) {
            next
        }
        recorded[[length(recorded) + 1L]] <- base
        readers[[length(readers) + 1L]] <-
            if (is.null(spec$axes) && is.null(spec$data)) {
                spec$daf
            } else {
                viewer(spec$daf, axes = spec$axes, data = spec$data,
                       name = paste0(S7::prop(spec$daf, "name"), ".view"))
            }
    }

    format_set_scalar(new_daf, "base_daf_repository",
                      .base_specification(recorded), overwrite = TRUE)
    chain_writer(c(readers, list(new_daf)),
                 name = name %||% S7::prop(new_daf, "name"))
}

#' Reopen a persistent chain from disk.
#'
#' Opens the complete chain of repositories rooted at `leaf` by following
#' the `base_daf_repository` scalar of each. Returns a [chain_reader()]
#' (`mode = "r"`) or [chain_writer()] (`mode = "r+"`, only the leaf is
#' writable).
#'
#' A repository records only its own immediate bases; what each of them
#' rests on is recorded in it. A repository reached through more than one
#' of them appears once, at its earliest position, so that a base never
#' overrides what rests on it.
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
    chain_name <- name %||% basename(leaf)
    index <- 0L

    # A repository comes after everything it is based on. `ancestors` guards
    # against a repository which (directly or not) records itself as a base,
    # which would otherwise be followed for ever.
    collect <- function(path, open_mode, ancestors) {
        norm <- .norm_path(path)
        if (norm %in% ancestors) {
            stop(sprintf(
                "cyclic repository: %s\nis also a base of itself\nin the chain: %s",
                norm, chain_name
            ), call. = FALSE)
        }
        daf <- open_daf(path, open_mode)
        if (!format_has_scalar(daf, "base_daf_repository")) {
            return(list(list(path = norm, daf = daf)))
        }

        collected <- list()
        specification <- format_get_scalar(daf, "base_daf_repository")$value
        for (base in .recorded_bases_of(daf, specification)) {
            sub <- collect(.resolve_base_path(norm, base$path), "r",
                           c(ancestors, norm))
            if (!is.null(base$axes) || !is.null(base$data)) {
                index <<- index + 1L
                inner <- lapply(sub, function(entry) entry$daf)
                base_chain <- if (length(inner) == 1L) {
                    inner[[1L]]
                } else {
                    chain_reader(inner,
                                 name = sprintf("%s.chain_%d", chain_name, index))
                }
                # A view is of the base's own complete chain, so what it exposes
                # is decided before anything is chained on top of it.
                view <- viewer(base_chain,
                               name = sprintf("%s.view_%d", chain_name, index),
                               axes = base$axes, data = base$data)
                view_internal <- S7::prop(view, "internal")
                view_internal$path <- sub[[length(sub)]]$path
                sub <- list(list(path = NULL, daf = view))
            }
            collected <- c(collected, sub)
        }
        c(collected, list(list(path = norm, daf = daf)))
    }

    readers <- lapply(collect(leaf, mode, character(0)),
                      function(entry) entry$daf)
    if (length(readers) == 1L) {
        readers[[1L]]
    } else if (identical(mode, "r")) {
        chain_reader(readers, name = chain_name)
    } else {
        chain_writer(readers, name = chain_name)
    }
}
