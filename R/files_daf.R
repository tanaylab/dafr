#' File-backed Daf store.
#'
#' A `Daf` store backed by a directory of small self-describing files,
#' bidirectionally compatible with Julia's `DataAxesFormats.FilesDaf`.
#' Writes are non-atomic; only one writer may touch a store at a time.
#'
#' @param path Directory path.
#' @param mode One of `"r"` (read-only, store must exist), `"r+"`
#'   (read-write, store must exist), `"w"` (create; fails if store already
#'   exists), `"w+"` (create or open an existing store; if the store
#'   already exists, its `scalars`, `axes`, `vectors`, `matrices`
#'   subdirectories and `daf.json` are truncated, but unrelated files in
#'   the directory are preserved).
#' @param name Human-readable identifier. Defaults to `basename(path)`.
#' @return A `FilesDaf` instance (`DafWriter` under `"r+"`/`"w"`/`"w+"`,
#'   `FilesDafReadOnly`/`DafReadOnly` under `"r"`).
#' @include format_api.R
#' @examples
#' path <- tempfile("dafr-example-")
#' d <- files_daf(path, mode = "w")
#' add_axis(d, "cell", c("c1", "c2"))
#' set_scalar(d, "organism", "human")
#' rm(d)
#' unlink(path, recursive = TRUE)
#' @export
files_daf <- function(path, mode = c("r", "r+", "w", "w+"), name = NULL) {
    stopifnot(is.character(path), length(path) == 1L, !is.na(path))
    mode <- match.arg(mode)
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    has_daf <- file.exists(file.path(path, "daf.json"))
    if (mode %in% c("r", "r+") && !has_daf) {
        stop(sprintf(
            "files_daf(%s, '%s'): not a daf directory (no daf.json)",
            sQuote(path), mode
        ), call. = FALSE)
    }
    if (mode == "w" && has_daf) {
        stop(sprintf(
            "files_daf(%s, 'w'): store already exists; use 'w+' to overwrite",
            sQuote(path)
        ), call. = FALSE)
    }
    if (mode %in% c("w", "w+")) {
        .files_daf_init(path, truncate = (mode == "w+" && has_daf))
    }
    .files_daf_check_version(path)
    if (is.null(name)) name <- basename(path)
    .assert_name(name, "name")
    internal <- new_internal_env()
    internal$path <- path
    internal$mode <- mode
    internal$axes <- new.env(parent = emptyenv())
    ctor <- if (mode == "r") FilesDafReadOnly else FilesDaf
    daf <- ctor(
        name                   = name,
        internal               = internal,
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env()
    )
    # Recovery: a writeable open inherits any leftover .reorder.backup/
    # from a previously-crashed reorder. Roll it back BEFORE returning the
    # daf so the caller never sees a partially-replaced store. Read-only
    # opens leave the backup alone (no permission to mutate).
    if (mode %in% c("r+", "w+")) {
        .files_daf_recover_reorder(daf)
    }
    daf
}

#' File-backed Daf writer class.
#'
#' Concrete `DafWriter` subclass instantiated by [files_daf()] when opened
#' for writing (modes `"r+"`, `"w"`, or `"w+"`). Use [files_daf()] to
#' construct instances.
#'
#' @inheritParams DafReader
#' @examples
#' \dontrun{
#' d <- files_daf(tempfile("dafr-"), mode = "w")
#' inherits(d, "dafr::FilesDaf")
#' }
#' @export
FilesDaf <- S7::new_class(
    name    = "FilesDaf",
    package = "dafr",
    parent  = DafWriter
)

#' File-backed read-only Daf class.
#'
#' Concrete `DafReadOnly` subclass instantiated by [files_daf()] when
#' opened with mode `"r"`. All mutating `format_*` generics reject calls on
#' this class with a clear "store opened read-only" error.
#'
#' @inheritParams DafReader
#' @examples
#' \dontrun{
#' d <- files_daf("/path/to/existing/store", mode = "r")
#' inherits(d, "dafr::FilesDafReadOnly")
#' }
#' @export
FilesDafReadOnly <- S7::new_class(
    name    = "FilesDafReadOnly",
    package = "dafr",
    parent  = DafReadOnly
)

.read_only_guard <- function(verb) {
    stop(sprintf("files_daf: store opened read-only; %s not permitted", verb),
        call. = FALSE
    )
}

S7::method(
    format_set_scalar,
    list(FilesDafReadOnly, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    .read_only_guard("set_scalar")
}
S7::method(
    format_delete_scalar,
    list(FilesDafReadOnly, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    .read_only_guard("delete_scalar")
}
S7::method(
    format_add_axis,
    list(FilesDafReadOnly, S7::class_character, S7::class_character)
) <- function(daf, axis, entries) {
    .read_only_guard("add_axis")
}
S7::method(
    format_delete_axis,
    list(FilesDafReadOnly, S7::class_character, S7::class_logical)
) <- function(daf, axis, must_exist) {
    .read_only_guard("delete_axis")
}
S7::method(
    format_set_vector,
    list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, axis, name, vec, overwrite) {
    .read_only_guard("set_vector")
}
S7::method(
    format_delete_vector,
    list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, axis, name, must_exist) {
    .read_only_guard("delete_vector")
}
S7::method(
    format_set_matrix,
    list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
    .read_only_guard("set_matrix")
}
S7::method(
    format_delete_matrix,
    list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, must_exist) {
    .read_only_guard("delete_matrix")
}
S7::method(
    format_relayout_matrix,
    list(FilesDafReadOnly, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    .read_only_guard("relayout_matrix")
}

.files_daf_init <- function(path, truncate) {
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
    if (truncate) {
        for (sub in c("scalars", "axes", "vectors", "matrices")) {
            sp <- file.path(path, sub)
            if (dir.exists(sp)) unlink(sp, recursive = TRUE, force = TRUE)
        }
        unlink(file.path(path, "daf.json"), force = TRUE)
        unlink(file.path(path, ".reorder.backup"), recursive = TRUE, force = TRUE)
    }
    for (sub in c("scalars", "axes", "vectors", "matrices")) {
        dir.create(file.path(path, sub), recursive = TRUE, showWarnings = FALSE)
    }
    if (!file.exists(file.path(path, "daf.json"))) {
        writeLines('{"version":[1,0]}', con = file.path(path, "daf.json"), sep = "\n")
    }
    invisible()
}

# Fast-path regex for the daf.json dafr emits:
#   {"version":[1,0]}
# Falls back to jsonlite on any mismatch.
.DAF_JSON_RE <- '^\\{"version":\\[([0-9]+),([0-9]+)\\](?:,"name":"[^"\\\\[:cntrl:]]*")?\\}\\s*$'

.files_daf_check_version <- function(path) {
    daf_json_path <- file.path(path, "daf.json")
    raw <- readChar(daf_json_path, file.size(daf_json_path), useBytes = TRUE)
    m <- regmatches(raw, regexec(.DAF_JSON_RE, raw, perl = TRUE))[[1L]]
    if (length(m) == 3L) {
        v1 <- as.integer(m[[2L]])
        v2 <- as.integer(m[[3L]])
    } else {
        # Fallback: full JSON parse
        j <- jsonlite::fromJSON(daf_json_path, simplifyVector = TRUE)
        v <- j$version
        if (is.null(v) || length(v) != 2L) {
            stop(sprintf("files_daf: %s daf.json version is malformed", sQuote(path)),
                call. = FALSE
            )
        }
        v1 <- v[[1L]]
        v2 <- v[[2L]]
    }
    if (v1 != 1L) {
        stop(sprintf(
            "files_daf: %s daf.json major version %d unsupported (expected 1)",
            sQuote(path), v1
        ), call. = FALSE)
    }
    if (v2 > 0L) {
        stop(sprintf(
            "files_daf: %s daf.json minor version %d exceeds supported (0)",
            sQuote(path), v2
        ), call. = FALSE)
    }
    invisible()
}
