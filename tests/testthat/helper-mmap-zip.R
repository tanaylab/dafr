# Helpers for slice-17 MmapZipStore tests.
#
# Phase 2 lacks a writer, so test fixtures are produced by an out-of-process
# Python `zipfile` writer. We gate on python3 + zipfile being available; tests
# that need a fixture skip cleanly otherwise.

# Returns TRUE iff `python3` is on PATH and the standard-library `zipfile`
# module loads. Cached across calls in this session.
have_python3_zipfile <- local({
    cached <- NULL
    function() {
        if (!is.null(cached)) return(cached)
        ok <- nzchar(Sys.which("python3")) &&
            tryCatch(
                system2(
                    "python3",
                    c("-c", shQuote("import zipfile,sys; sys.exit(0)")),
                    stdout = FALSE, stderr = FALSE
                ) == 0L,
                error = function(e) FALSE
            )
        cached <<- ok
        ok
    }
})

skip_if_no_python_zipfile <- function() {
    if (!have_python3_zipfile()) {
        testthat::skip("python3 + zipfile not available")
    }
}

# Build a zip file at `path` whose entries are the named-list `entries`
# (names are entry paths inside the archive, values are character or raw).
# `compression` is one of "stored", "deflate", "bzip2", "lzma" (mapped to
# zipfile.ZIP_*). All entries use the same compression; tests that need a
# mix call build_zip twice into the same file (Python's `zipfile` opens in
# "a"ppend mode in that case).
build_zip <- function(path, entries, compression = "stored") {
    skip_if_no_python_zipfile()
    method <- switch(
        compression,
        stored = "zipfile.ZIP_STORED",
        deflate = "zipfile.ZIP_DEFLATED",
        bzip2 = "zipfile.ZIP_BZIP2",
        lzma = "zipfile.ZIP_LZMA",
        stop("unsupported compression: ", compression)
    )
    # Pass entry names + payloads via env vars to avoid quoting hell.
    env <- character()
    for (i in seq_along(entries)) {
        nm <- names(entries)[[i]]
        val <- entries[[i]]
        if (is.raw(val)) {
            # Hex-encode raw payloads for transport through env.
            env <- c(
                env,
                paste0("DAFR_TEST_NAME_", i, "=", nm),
                paste0("DAFR_TEST_HEX_", i, "=", paste(format(val), collapse = ""))
            )
        } else {
            env <- c(
                env,
                paste0("DAFR_TEST_NAME_", i, "=", nm),
                paste0("DAFR_TEST_TXT_", i, "=", val)
            )
        }
    }
    env <- c(
        env,
        paste0("DAFR_TEST_OUT=", path),
        paste0("DAFR_TEST_METHOD=", method),
        paste0("DAFR_TEST_COUNT=", length(entries))
    )
    py <- '
import os, zipfile
out = os.environ["DAFR_TEST_OUT"]
method = eval(os.environ["DAFR_TEST_METHOD"])
n = int(os.environ["DAFR_TEST_COUNT"])
with zipfile.ZipFile(out, "w", compression=method) as zf:
    for i in range(1, n + 1):
        nm = os.environ["DAFR_TEST_NAME_%d" % i]
        hx = os.environ.get("DAFR_TEST_HEX_%d" % i)
        if hx is not None:
            payload = bytes.fromhex(hx)
        else:
            payload = os.environ["DAFR_TEST_TXT_%d" % i].encode("utf-8")
        zf.writestr(nm, payload)
'
    rc <- withr::with_envvar(
        new = setNames(
            sub("^[^=]+=", "", env),
            sub("=.*$", "", env)
        ),
        system2("python3", c("-c", shQuote(py)), stdout = FALSE, stderr = FALSE)
    )
    if (rc != 0L) {
        stop("python3 zipfile writer failed (rc=", rc, ")")
    }
    invisible(path)
}

# Read a zip via Python's `zipfile` and return a named list of raw vectors
# (names = entry paths inside the archive, values = raw contents). Used to
# verify cross-language readability of writes produced by MmapZipStore.
read_zip_via_python <- function(path) {
    skip_if_no_python_zipfile()
    out_dir <- new_tempdir()
    py <- '
import os, zipfile
src = os.environ["DAFR_TEST_IN"]
out = os.environ["DAFR_TEST_OUT_DIR"]
with zipfile.ZipFile(src, "r") as zf:
    names = zf.namelist()
    with open(os.path.join(out, "names.txt"), "w") as f:
        for n in names:
            f.write(n + "\\n")
    for i, n in enumerate(names):
        with open(os.path.join(out, "entry_%d.bin" % i), "wb") as f:
            f.write(zf.read(n))
'
    rc <- withr::with_envvar(
        new = c(DAFR_TEST_IN = path, DAFR_TEST_OUT_DIR = out_dir),
        system2("python3", c("-c", shQuote(py)),
                stdout = FALSE, stderr = FALSE)
    )
    if (rc != 0L) {
        stop("python3 zipfile reader failed (rc=", rc, ")")
    }
    names_path <- file.path(out_dir, "names.txt")
    if (!file.exists(names_path)) return(list())
    names_vec <- readLines(names_path, warn = FALSE)
    out <- vector("list", length(names_vec))
    names(out) <- names_vec
    for (i in seq_along(names_vec)) {
        f <- file.path(out_dir, sprintf("entry_%d.bin", i - 1L))
        out[[i]] <- readBin(f, what = "raw", n = file.info(f)$size)
    }
    out
}
