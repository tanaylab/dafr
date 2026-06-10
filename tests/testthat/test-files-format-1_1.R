# FilesFormat v1.1 read support. DataAxesFormats.jl 0.3.0 bumped the FilesDaf
# format from 1.0 to 1.1: the binary blobs are byte-identical, but a sparse
# property's JSON sidecar moved from the legacy top-level {eltype, indtype}
# keys to per-component descriptors (nzind/nzval for vectors; colptr/rowval/
# nzval for matrices), each shaped like a stand-alone vector descriptor.
# Julia's reader accepts both shapes; dafr (a 1.0-only reader) used to reject
# 1.1 repos outright. These tests pin that dafr opens + reads v1.1 repos.
#
# Fixture strategy: write a normal v1.0 repo with dafr, then rewrite daf.json
# to [1,1] and each sparse JSON to the v1.1 per-component form (matching
# DataAxesFormats.jl 0.3.0 `sparse_{vector,matrix}_json_bytes`). Binary blobs
# are untouched (identical across versions).

.dtype_bytes_v11 <- function(t) {
    switch(t, UInt8 = 1L, Int8 = 1L, UInt16 = 2L, Int16 = 2L,
        UInt32 = 4L, Int32 = 4L, UInt64 = 8L, Int64 = 8L,
        Float32 = 4L, Float64 = 8L, Bool = 1L, 8L)
}

.rewrite_files_repo_to_v11 <- function(path) {
    writeLines('{"version":[1,1]}', file.path(path, "daf.json"))
    jsons <- list.files(path, pattern = "\\.json$", recursive = TRUE,
                        full.names = TRUE)
    for (j in jsons) {
        if (basename(j) %in% c("metadata.json")) next
        d <- jsonlite::fromJSON(j, simplifyVector = TRUE)
        fmt <- d[["format"]]
        if (is.null(fmt) || fmt != "sparse") next
        elt <- d$eltype
        if (is.null(elt)) next   # already v1.1 per-component (dafr now writes 1.1)
        ind <- if (is.null(d$indtype)) "UInt32" else d$indtype
        base <- sub("\\.json$", "", j)
        comp <- function(dtype, file) {
            n <- if (file.exists(file)) {
                as.integer(file.size(file) %/% .dtype_bytes_v11(dtype))
            } else 0L
            list(format = "dense", eltype = dtype, n_elements = n)
        }
        if (file.exists(paste0(base, ".nzind"))) {           # sparse vector
            out <- list(format = "sparse",
                        nzind = comp(ind, paste0(base, ".nzind")))
            if (file.exists(paste0(base, ".nzval"))) {
                out$nzval <- comp(elt, paste0(base, ".nzval"))
            }
        } else {                                             # sparse matrix
            out <- list(format = "sparse",
                        colptr = comp(ind, paste0(base, ".colptr")),
                        rowval = comp(ind, paste0(base, ".rowval")))
            if (file.exists(paste0(base, ".nzval"))) {
                out$nzval <- comp(elt, paste0(base, ".nzval"))
            }
        }
        writeLines(jsonlite::toJSON(out, auto_unbox = TRUE), j)
    }
    invisible(path)
}

test_that("files_daf opens a FilesFormat v1.1 directory (version acceptance)", {
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    d <- files_daf(p, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_scalar(d, "n", 7L)
    .rewrite_files_repo_to_v11(p)

    d2 <- files_daf(p, "r")
    expect_s7_class(d2, DafReader)
    expect_identical(get_scalar(d2, "n"), 7L)
    expect_identical(axis_vector(d2, "cell"), c("c1", "c2", "c3"))
})

test_that("files_daf reads v1.1 per-component sparse vector + matrix descriptors", {
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    d <- files_daf(p, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "sv", c(0, 5, 0))
    sm <- Matrix::Matrix(matrix(c(1, 0, 0, 0, 2, 0), 3, 2), sparse = TRUE)
    set_matrix(d, "cell", "gene", "SM", sm)
    .rewrite_files_repo_to_v11(p)

    d2 <- files_daf(p, "r")
    expect_equal(unname(get_vector(d2, "cell", "sv")), c(0, 5, 0))
    expect_equal(as.matrix(get_matrix(d2, "cell", "gene", "SM")),
                 as.matrix(sm), ignore_attr = TRUE)
})

test_that("files_daf reads a v1.1 all-true Bool sparse vector (no nzval descriptor)", {
    # A v1.1 all-true Bool sparse property omits the nzval component entirely;
    # the reader must derive eltype = Bool from its absence. dafr stores plain
    # R logicals densely, so construct the nzind-only layout directly (as
    # DataAxesFormats.jl 0.3.0 would write it: 1-based UInt32 indices).
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    d <- files_daf(p, "w")
    add_axis(d, "cell", sprintf("c%d", 1:5))
    rm(d)
    writeLines('{"version":[1,1]}', file.path(p, "daf.json"))
    vdir <- file.path(p, "vectors", "cell")
    writeBin(c(2L, 4L), file.path(vdir, "flag.nzind"), size = 4L,
             endian = "little")
    writeLines(paste0('{"format":"sparse",',
                      '"nzind":{"format":"dense","eltype":"UInt32","n_elements":2}}'),
               file.path(vdir, "flag.json"))

    d2 <- files_daf(p, "r")
    expect_identical(unname(get_vector(d2, "cell", "flag")),
                     c(FALSE, TRUE, FALSE, TRUE, FALSE))
})

test_that("files_daf rejects a 'zipped'-only packed component (no Zarr index)", {
    # dafr reads the dual-format "indexed+zipped" shards DataAxesFormats.jl
    # writes (via the start-located Zarr index); a bare "zipped" ZIP-only shard
    # from a foreign producer carries no such index and is rejected with an
    # actionable message before any byte is read. (Real "indexed+zipped" read
    # coverage lives in test-files-packed-read.R against committed fixtures.)
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    d <- files_daf(p, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "sv", c(0, 5, 0))
    .rewrite_files_repo_to_v11(p)
    j <- file.path(p, "vectors/cell/sv.json")
    desc <- jsonlite::fromJSON(j, simplifyVector = TRUE)
    desc$nzind$packed_format <- "zipped"   # ZIP-only, no leading Zarr index
    writeLines(jsonlite::toJSON(desc, auto_unbox = TRUE), j)

    d2 <- files_daf(p, "r")
    expect_error(get_vector(d2, "cell", "sv"), "indexed\\+zipped")
})

test_that("http_daf reads a FilesFormat v1.1 repo served over HTTP", {
    skip_on_cran()
    # http_daf serving needs metadata.zip, which .metadata_zip_rebuild only
    # writes on POSIX (MmapZipStore is POSIX-only); without this guard the test
    # runs on Windows CI whenever python is available and 404s on metadata.zip.
    skip_if_no_mmap_zip()
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    d <- files_daf(p, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_vector(d, "cell", "sv", c(0, 5, 0))
    set_scalar(d, "title", "hi")
    sm <- Matrix::Matrix(matrix(c(1, 0, 0, 0, 2, 0), 3, 2), sparse = TRUE)
    set_matrix(d, "cell", "gene", "SM", sm)
    rm(d)
    .rewrite_files_repo_to_v11(p)
    dafr:::.metadata_zip_rebuild(p)   # repack metadata.zip with the v1.1 JSONs

    srv <- start_http_server(p)
    on.exit(stop_http_server(srv), add = TRUE)
    hd <- http_daf(srv$url, name = "v11")
    expect_identical(get_scalar(hd, "title"), "hi")
    expect_equal(unname(get_vector(hd, "cell", "sv")), c(0, 5, 0))
    expect_equal(as.matrix(get_matrix(hd, "cell", "gene", "SM")),
                 as.matrix(sm), ignore_attr = TRUE)
})

test_that("files_daf reads a real DataAxesFormats.jl 0.3.0-written v1.1 repo", {
    skip_on_cran()
    skip_if_not(.have_julia_env(), "dafr-mcview Julia env not available")
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    out <- run_julia(c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = FilesDaf("%s", "w"; name = "jl")', p),
        'add_axis!(daf, "cell", ["c1", "c2", "c3"])',
        'add_axis!(daf, "gene", ["g1", "g2", "g3"])',
        'set_scalar!(daf, "title", "hi")',
        'set_vector!(daf, "cell", "age", Float64[10, 20, 30])',
        'set_vector!(daf, "cell", "sv", SparseVector(3, [2], [5.0]))',
        'set_matrix!(daf, "cell", "gene", "UMIs", sparse([1.0 0 0; 0 2.0 0; 0 0 3.0]); relayout = false)',
        'println("DONE")'
    ))
    skip_if_not(any(grepl("^DONE$", out)),
                paste0("Julia FilesDaf writer failed:\n", paste(out, collapse = "\n")))
    # This only exercises the v1.1 reader if the writer actually emitted 1.1.
    skip_if_not(any(grepl("\\[1, ?1\\]",
                          readLines(file.path(p, "daf.json"), warn = FALSE))),
                "Julia FilesDaf did not write format v1.1")

    d <- files_daf(p, "r")
    expect_identical(get_scalar(d, "title"), "hi")
    expect_equal(unname(get_vector(d, "cell", "age")), c(10, 20, 30))
    expect_equal(unname(get_vector(d, "cell", "sv")), c(0, 5, 0))
    expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
                 diag(c(1, 2, 3)), ignore_attr = TRUE)
})

test_that("DataAxesFormats.jl 0.3.0 reads a dafr-written v1.1 FilesDaf store", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3(), "dafr-mcview Julia env not DAF >= 0.3.0")
    out <- withr::local_tempdir("dafr-v11-")
    p <- file.path(out, "w.daf")
    d <- files_daf(p, mode = "w+")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    add_axis(d, "gene", c("g1", "g2"))
    set_scalar(d, "name", "from-dafr")
    set_vector(d, "cell", "score", c(1.5, 2.5, 3.5, 4.5))            # dense
    set_vector(d, "cell", "sv", c(0, 5, 0, 7))                       # sparse numeric
    set_matrix(d, "cell", "gene", "dm", matrix(c(1, 2, 3, 4, 5, 6, 7, 8), 4, 2))
    set_matrix(d, "cell", "gene", "sm",
               Matrix::sparseMatrix(i = c(1L, 4L), j = c(1L, 2L),
                                    x = c(1.5, 2.5), dims = c(4L, 2L)))
    rm(d); gc()
    # dafr wrote v1.1; confirm Julia 0.3.0 reads every property kind back.
    expect_true(any(grepl("\\[1, ?1\\]",
                          readLines(file.path(p, "daf.json"), warn = FALSE))))
    res <- run_julia(c(
        "using DataAxesFormats",
        sprintf('d = FilesDaf(raw"%s", "r")', p),
        'println("NAME=", get_scalar(d, "name"))',
        'println("SCORE=", join(get_vector(d, "cell", "score"), ","))',
        'println("SV=", join(get_vector(d, "cell", "sv"), ","))',
        'println("DM=", join(vec(Matrix(get_matrix(d, "cell", "gene", "dm"))), ","))',
        'println("SM=", join(vec(Matrix(get_matrix(d, "cell", "gene", "sm"))), ","))'
    ))
    skip_if_not(any(grepl("^NAME=from-dafr$", res)),
                paste0("Julia FilesDaf read failed:\n", paste(res, collapse = "\n")))
    getline <- function(tag) {
        sub(paste0("^", tag), "", grep(paste0("^", tag), res, value = TRUE)[1])
    }
    expect_equal(getline("SCORE="), "1.5,2.5,3.5,4.5")
    expect_equal(getline("SV="), "0.0,5.0,0.0,7.0")
    expect_equal(getline("DM="), "1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0")
    expect_equal(getline("SM="), "1.5,0.0,0.0,0.0,0.0,0.0,0.0,2.5")
})

test_that("files_daf still rejects a too-new FilesFormat (v1.2)", {
    p <- tempfile(fileext = ".daf")
    on.exit(unlink(p, recursive = TRUE, force = TRUE), add = TRUE)
    d <- files_daf(p, "w")
    add_axis(d, "cell", "c1")
    writeLines('{"version":[1,2]}', file.path(p, "daf.json"))
    expect_error(files_daf(p, "r"), "incompatible format version")
})
