# dev/backend-parity/cross_format.R
#
# Day-4 runner: for each (src_backend, dst_backend) pair, write the
# fixture into src, `copy_all` to dst, read from dst. Each record's
# backend field is set to "src->dst" so the diff tool reports per
# pair. Reference is `memory` (single-backend round-trip output);
# diff joins on `key`.

suppressMessages(devtools::load_all("/home/aviezerl/src/dafr-native"))
source("/home/aviezerl/src/dafr-native/dev/backend-parity/build_fixture_R.R")
source("/home/aviezerl/src/dafr-native/dev/backend-parity/serialize.R")
source("/home/aviezerl/src/dafr-native/dev/backend-parity/round_trip.R")

OUT_PATH <- "/home/aviezerl/src/dafr-native/dev/backend-parity/cross_backend.jsonl"

# Open a fresh writer for a backend name.
.open_writer <- function(bk, suffix) {
    if (bk == "memory") {
        return(memory_daf(sprintf("m_%s", suffix)))
    }
    if (bk == "files") {
        p <- file.path(SCRATCH, sprintf("cross_files_%s.daf", suffix))
        if (dir.exists(p)) unlink(p, recursive = TRUE)
        return(files_daf(p, mode = "w"))
    }
    if (bk == "zarr") {
        p <- file.path(SCRATCH, sprintf("cross_zarr_%s.daf.zarr", suffix))
        if (dir.exists(p)) unlink(p, recursive = TRUE)
        return(zarr_daf(p, mode = "w"))
    }
    stop("unknown backend ", bk)
}

# Reopen a backend in read mode. For memory, return the in-process
# writer (no on-disk medium).
.reopen_reader <- function(bk, writer, suffix) {
    if (bk == "memory") return(writer)
    if (bk == "files")
        return(files_daf(file.path(SCRATCH, sprintf("cross_files_%s.daf", suffix)),
                         mode = "r"))
    if (bk == "zarr")
        return(zarr_daf(file.path(SCRATCH, sprintf("cross_zarr_%s.daf.zarr", suffix)),
                        mode = "r"))
    stop("unknown backend ", bk)
}

.cleanup <- function(bk, suffix) {
    if (bk == "memory") return(invisible(NULL))
    if (bk == "files") {
        p <- file.path(SCRATCH, sprintf("cross_files_%s.daf", suffix))
        if (dir.exists(p)) unlink(p, recursive = TRUE)
    }
    if (bk == "zarr") {
        p <- file.path(SCRATCH, sprintf("cross_zarr_%s.daf.zarr", suffix))
        if (dir.exists(p)) unlink(p, recursive = TRUE)
    }
}

run_pair <- function(src, dst, manifest, con) {
    label <- sprintf("%s->%s", src, dst)
    cat(sprintf("== %s ==\n", label))

    src_writer <- .open_writer(src, sprintf("src_%s_%s", src, dst))
    build_fixture(src_writer)
    src_reader <- .reopen_reader(src, src_writer, sprintf("src_%s_%s", src, dst))

    dst_writer <- .open_writer(dst, sprintf("dst_%s_%s", src, dst))
    copy_all(dst_writer, src_reader, relayout = FALSE, insist = FALSE)
    dst_reader <- .reopen_reader(dst, dst_writer, sprintf("dst_%s_%s", src, dst))

    nerr <- 0L
    for (item in manifest) {
        rec <- tryCatch({
            v <- .do_read(dst_reader, item)
            serialize_read(label, item, v, status = "ok")
        }, error = function(e) {
            serialize_read(label, item, NULL,
                           status = "error", cond = e)
        })
        if (identical(rec$status, "error")) nerr <- nerr + 1L
        write_record_jsonl(con, rec)
    }
    cat(sprintf("   wrote %d records  (errors=%d)\n",
                length(manifest), nerr))
    .cleanup(src, sprintf("src_%s_%s", src, dst))
    .cleanup(dst, sprintf("dst_%s_%s", src, dst))
}

main_cross <- function() {
    manifest <- fixture_manifest()
    # Also emit a "memory" baseline so diff.py can join against it
    # exactly as in the single-backend file.
    cat(sprintf("manifest: %d items\n", length(manifest)))
    con <- file(OUT_PATH, "w")
    on.exit(close(con), add = TRUE)

    baseline_writer <- memory_daf("baseline")
    build_fixture(baseline_writer)
    for (item in manifest) {
        rec <- tryCatch({
            v <- .do_read(baseline_writer, item)
            serialize_read("memory", item, v, status = "ok")
        }, error = function(e) {
            serialize_read("memory", item, NULL,
                           status = "error", cond = e)
        })
        write_record_jsonl(con, rec)
    }

    bks <- c("memory", "files", "zarr")
    for (s in bks) for (d in bks) {
        if (s == d) next  # already covered by Day-3 single-backend
        run_pair(s, d, manifest, con)
    }
    cat(sprintf("\nWROTE: %s\n", OUT_PATH))
}

if (!interactive()) main_cross()
