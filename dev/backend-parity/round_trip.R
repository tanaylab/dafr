# dev/backend-parity/round_trip.R
#
# Day-3 runner: single-backend write -> close -> reopen -> read for
# every manifest item, on each backend in {memory, files, zarr}.
# Emits one JSONL line per (backend, key) into single_backend.jsonl.
#
# Memory has no "reopen" step (the daf is the storage); we still read
# back through the user-facing get_* API so it serves as the
# in-memory baseline. The diff tool joins on `key` and treats
# `memory` as the reference unless told otherwise.

suppressMessages(devtools::load_all("/home/aviezerl/src/dafr-native"))
source("/home/aviezerl/src/dafr-native/dev/backend-parity/build_fixture_R.R")
source("/home/aviezerl/src/dafr-native/dev/backend-parity/serialize.R")

OUT_PATH <- "/home/aviezerl/src/dafr-native/dev/backend-parity/single_backend.jsonl"
SCRATCH  <- "/home/aviezerl/src/dafr-native/dev/backend-parity/.scratch"

dir.create(SCRATCH, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------
# Backend lifecycle
# ---------------------------------------------------------------------

# Each backend descriptor: open_writer(), close_writer(daf), open_reader(),
# close_reader(daf). For memory, the same instance is used for write/read.

backends <- list(
    memory = list(
        name = "memory",
        open_writer = function() memory_daf("memory_writer"),
        close_writer = function(d) invisible(NULL),
        open_reader  = function(writer) writer,
        close_reader = function(d) invisible(NULL),
        cleanup      = function() invisible(NULL)
    ),
    files = list(
        name = "files",
        path = file.path(SCRATCH, "fixture_files.daf"),
        open_writer = function() {
            p <- file.path(SCRATCH, "fixture_files.daf")
            if (dir.exists(p)) unlink(p, recursive = TRUE)
            files_daf(p, mode = "w")
        },
        close_writer = function(d) invisible(NULL),
        open_reader = function(writer) {
            files_daf(file.path(SCRATCH, "fixture_files.daf"), mode = "r")
        },
        close_reader = function(d) invisible(NULL),
        cleanup = function() {
            p <- file.path(SCRATCH, "fixture_files.daf")
            if (dir.exists(p)) unlink(p, recursive = TRUE)
        }
    ),
    zarr = list(
        name = "zarr",
        path = file.path(SCRATCH, "fixture_zarr.daf.zarr"),
        open_writer = function() {
            p <- file.path(SCRATCH, "fixture_zarr.daf.zarr")
            if (dir.exists(p)) unlink(p, recursive = TRUE)
            zarr_daf(p, mode = "w")
        },
        close_writer = function(d) invisible(NULL),
        open_reader = function(writer) {
            zarr_daf(file.path(SCRATCH, "fixture_zarr.daf.zarr"), mode = "r")
        },
        close_reader = function(d) invisible(NULL),
        cleanup = function() {
            p <- file.path(SCRATCH, "fixture_zarr.daf.zarr")
            if (dir.exists(p)) unlink(p, recursive = TRUE)
        }
    )
)

# ---------------------------------------------------------------------
# Read a single manifest item
# ---------------------------------------------------------------------

.do_read <- function(daf, item) {
    if (item$kind == "scalar")  return(get_scalar(daf, item$name))
    if (item$kind == "axis")    return(axis_vector(daf, item$axis))
    if (item$kind == "vector")  return(get_vector(daf, item$axis, item$name))
    if (item$kind == "matrix")  return(get_matrix(daf, item$axis, item$cols_axis,
                                                  item$name))
    stop("unknown kind: ", item$kind)
}

read_one <- function(daf, backend_name, item) {
    rec <- tryCatch({
        v <- .do_read(daf, item)
        serialize_read(backend_name, item, v, status = "ok")
    }, error = function(e) {
        serialize_read(backend_name, item, NULL,
                       status = "error", cond = e)
    })
    rec
}

# ---------------------------------------------------------------------
# Run one backend
# ---------------------------------------------------------------------

run_backend <- function(backend, manifest, con) {
    cat(sprintf("== %s ==\n", backend$name))
    writer <- backend$open_writer()
    build_fixture(writer)
    backend$close_writer(writer)
    reader <- backend$open_reader(writer)
    nerr <- 0L
    for (item in manifest) {
        rec <- read_one(reader, backend$name, item)
        if (identical(rec$status, "error")) nerr <- nerr + 1L
        write_record_jsonl(con, rec)
    }
    backend$close_reader(reader)
    cat(sprintf("   wrote %d records  (errors=%d)\n",
                length(manifest), nerr))
}

# ---------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------

main <- function() {
    manifest <- fixture_manifest()
    cat(sprintf("manifest: %d items\n", length(manifest)))

    con <- file(OUT_PATH, "w")
    on.exit(close(con), add = TRUE)
    for (bk in backends) {
        on.exit(bk$cleanup(), add = TRUE)
        run_backend(bk, manifest, con)
    }
    cat(sprintf("\nWROTE: %s\n", OUT_PATH))
}

if (!interactive() && identical(sys.nframe(), 0L) &&
    identical(basename(sub("^--file=", "",
        grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)[1])),
        "round_trip.R")) {
    main()
}
