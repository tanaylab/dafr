# Cross-language smoke tests for ZarrDaf <-> Python zarr.
# Gated on python3 + zarr + numcodecs availability; skipped otherwise.
#
# NOTE: tested against zarr-python 3.x (installed on the dev system).
# The zarr.open() call requires a root .zgroup file (written by
# _zarr_daf_init_store) and consolidated .zmetadata with intermediate
# .zgroup entries (synthesised by zarr_v2_consolidated_metadata).
#
# Implementation note: system2() passes args to sh -c, so Python code
# must be wrapped in shQuote() to survive shell metacharacters.

.python3_with_zarr_available <- function() {
    py <- Sys.which("python3")
    if (!nzchar(py)) py <- Sys.which("python")
    if (!nzchar(py)) return(NA_character_)
    code <- "import zarr, numcodecs; print('OK')"
    out <- suppressWarnings(system2(
        py, c("-c", shQuote(code)),
        stdout = TRUE, stderr = TRUE
    ))
    if (isTRUE(attr(out, "status") != 0L)) return(NA_character_)
    if (!any(grepl("^OK$", out))) return(NA_character_)
    py
}

# Helper: run a Python script string via system2; return stdout+stderr lines.
# Wraps code in shQuote() so shell metacharacters survive.
.py_run <- function(py, script) {
    system2(py, c("-c", shQuote(script)), stdout = TRUE, stderr = TRUE)
}

# ---- Python reads dafr-written zarr_daf ---------------------------------

test_that("dafr-written zarr_daf opens cleanly in Python's zarr.open() — scalars + axes", {
    skip_on_cran()
    py <- .python3_with_zarr_available()
    skip_if(is.na(py), "python3 + zarr + numcodecs not available locally")

    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B", "C"))
    set_scalar(d, "n", 42L)
    set_scalar(d, "label", "hello")
    rm(d)

    script <- paste0(
        'import zarr\n',
        'g = zarr.open(', shQuote(tmp), ', mode="r")\n',
        'n = g["scalars/n"][...]\n',
        'label = g["scalars/label"][...]\n',
        'cell = g["axes/cell"][...]\n',
        'print(f"SCALAR_N={int(n[0])}", flush=True)\n',
        'print(f"SCALAR_LABEL={str(label[0])}", flush=True)\n',
        'print(f"AXIS_CELL={list(map(str, cell))}", flush=True)\n'
    )
    out <- .py_run(py, script)
    if (isTRUE(attr(out, "status") != 0L)) {
        fail(paste(out, collapse = "\n"))
    }
    cat_out <- paste(out, collapse = "\n")
    expect_match(cat_out, "SCALAR_N=42")
    expect_match(cat_out, "SCALAR_LABEL=hello")
    expect_match(cat_out, "AXIS_CELL=\\['A', 'B', 'C'\\]")
})

test_that("dafr-written zarr_daf opens cleanly in Python — dense vector + matrix", {
    skip_on_cran()
    py <- .python3_with_zarr_available()
    skip_if(is.na(py), "python3 + zarr + numcodecs not available locally")

    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "x", c(1.0, 2.0, 3.0))
    set_matrix(d, "cell", "gene", "M",
               matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 3))
    rm(d)

    # zarr-python 3.x returns numpy scalars; cast to plain float for printing.
    script <- paste0(
        'import zarr\n',
        'g = zarr.open(', shQuote(tmp), ', mode="r")\n',
        'x = g["vectors/cell/x"][...]\n',
        'M = g["matrices/cell/gene/M"][...]\n',
        'print(f"VECTOR_X={[float(v) for v in x]}", flush=True)\n',
        'print(f"MATRIX_SHAPE={list(M.shape)}", flush=True)\n',
        'print(f"MATRIX_FIRST_ROW={[float(v) for v in M[0]]}", flush=True)\n'
    )
    out <- .py_run(py, script)
    if (isTRUE(attr(out, "status") != 0L)) {
        fail(paste(out, collapse = "\n"))
    }
    cat_out <- paste(out, collapse = "\n")
    expect_match(cat_out, "VECTOR_X=\\[1\\.0, 2\\.0, 3\\.0\\]")
    # Matrix shape from dafr is on-disk [n_cols, n_rows] per Phase 7's
    # upstream-compat layout, so Python sees shape (n_cols, n_rows).
    # When dafr writes a 3x2 matrix (rows=cell=3, cols=gene=2), on-disk
    # shape is [2, 3], so MATRIX_SHAPE should be [2, 3].
    expect_match(cat_out, "MATRIX_SHAPE=\\[2, 3\\]")
    expect_match(cat_out, "MATRIX_FIRST_ROW=\\[1\\.0, 2\\.0, 3\\.0\\]")
})

test_that("dafr-written zarr_daf opens in Python — sparse matrix", {
    skip_on_cran()
    py <- .python3_with_zarr_available()
    skip_if(is.na(py), "python3 + zarr + numcodecs not available locally")

    tmp <- tempfile(fileext = ".daf.zarr")
    d <- zarr_daf(tmp, mode = "w")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    sp <- Matrix::sparseMatrix(i = c(1L, 3L), j = c(1L, 2L),
                               x = c(10.0, 20.0), dims = c(3L, 2L))
    set_matrix(d, "cell", "gene", "S", sp)
    rm(d)

    script <- paste0(
        'import zarr\n',
        'g = zarr.open(', shQuote(tmp), ', mode="r")\n',
        'sp_grp = g["matrices/cell/gene/S"]\n',
        'colptr = sp_grp["colptr"][...]\n',
        'rowval = sp_grp["rowval"][...]\n',
        'nzval  = sp_grp["nzval"][...]\n',
        'print(f"COLPTR={list(map(int, colptr))}", flush=True)\n',
        'print(f"ROWVAL={list(map(int, rowval))}", flush=True)\n',
        'print(f"NZVAL={list(map(float, nzval))}", flush=True)\n'
    )
    out <- .py_run(py, script)
    if (isTRUE(attr(out, "status") != 0L)) {
        fail(paste(out, collapse = "\n"))
    }
    cat_out <- paste(out, collapse = "\n")
    # 1-based on disk: colptr starts at 1; rowval entries are 1-based row ids.
    expect_match(cat_out, "COLPTR=\\[1, 2, 3\\]")
    expect_match(cat_out, "ROWVAL=\\[1, 3\\]")
    expect_match(cat_out, "NZVAL=\\[10\\.0, 20\\.0\\]")
})

# ---- Foreign-codec error on read ----------------------------------------

test_that("dafr rejects foreign Zarr with blosc compressor on read", {
    skip_on_cran()
    py <- .python3_with_zarr_available()
    skip_if(is.na(py), "python3 + zarr + numcodecs not available locally")

    tmp <- tempfile(fileext = ".zarr")
    # Create a Zarr v2 store with blosc-compressed data via Python.
    # Write raw bytes directly rather than using zarr.open() (zarr-python
    # 3.x changed the create API for v2 stores).
    py_setup <- paste0(
        'import os, json, numpy, numcodecs\n',
        'tmp = ', shQuote(tmp), '\n',
        'os.makedirs(os.path.join(tmp, "foo"), exist_ok=True)\n',
        'with open(os.path.join(tmp, ".zgroup"), "w") as f:\n',
        '    json.dump({"zarr_format": 2}, f)\n',
        'data = numpy.zeros(10, dtype="<f8")\n',
        'blosc = numcodecs.Blosc()\n',
        'encoded = blosc.encode(data.tobytes())\n',
        'with open(os.path.join(tmp, "foo", "0"), "wb") as f:\n',
        '    f.write(encoded)\n',
        'zarray = {\n',
        '    "chunks": [10],\n',
        '    "compressor": {"id": "blosc", "cname": "lz4", "clevel": 5,\n',
        '                   "shuffle": 1, "blocksize": 0},\n',
        '    "dtype": "<f8", "fill_value": 0, "filters": None,\n',
        '    "order": "C", "shape": [10], "zarr_format": 2\n',
        '}\n',
        'with open(os.path.join(tmp, "foo", ".zarray"), "w") as f:\n',
        '    json.dump(zarray, f)\n',
        'print("OK", flush=True)\n'
    )
    setup_out <- .py_run(py, py_setup)
    skip_if(
        isTRUE(attr(setup_out, "status") != 0L),
        paste("python setup failed:", paste(setup_out, collapse = "\n"))
    )

    # Open the foreign-blosc array via dafr's low-level zarr reader.
    store  <- new_dir_store(tmp)
    zarray <- dafr:::zarr_v2_read_zarray(store, "foo")
    expect_identical(zarray$compressor$id, "blosc")
    chunk_bytes <- store_get_bytes(store, "foo/0")
    expect_error(
        dafr:::zarr_v2_decode_chunk(chunk_bytes, zarray$dtype, n = 10L,
                                    compressor = zarray$compressor),
        "blosc.*not supported"
    )
})

# ---- Round-trip via Python (write through Python, read in dafr) ---------

test_that("Python-written zarr opens cleanly in dafr if uncompressed", {
    skip_on_cran()
    py <- .python3_with_zarr_available()
    skip_if(is.na(py), "python3 + zarr + numcodecs not available locally")

    tmp <- tempfile(fileext = ".zarr")
    # Create an uncompressed Zarr v2 array manually via Python.
    py_setup <- paste0(
        'import os, json, numpy\n',
        'tmp = ', shQuote(tmp), '\n',
        'os.makedirs(os.path.join(tmp, "foo"), exist_ok=True)\n',
        'with open(os.path.join(tmp, ".zgroup"), "w") as f:\n',
        '    json.dump({"zarr_format": 2}, f)\n',
        'data = numpy.array([1.5, 2.5, 3.5], dtype="<f8")\n',
        'with open(os.path.join(tmp, "foo", "0"), "wb") as f:\n',
        '    f.write(data.tobytes())\n',
        'zarray = {"chunks": [3], "compressor": None, "dtype": "<f8",\n',
        '          "fill_value": 0, "filters": None, "order": "C",\n',
        '          "shape": [3], "zarr_format": 2}\n',
        'with open(os.path.join(tmp, "foo", ".zarray"), "w") as f:\n',
        '    json.dump(zarray, f)\n',
        'print("OK", flush=True)\n'
    )
    setup_out <- .py_run(py, py_setup)
    skip_if(
        isTRUE(attr(setup_out, "status") != 0L),
        paste("python setup failed:", paste(setup_out, collapse = "\n"))
    )

    store   <- new_dir_store(tmp)
    zarray  <- dafr:::zarr_v2_read_zarray(store, "foo")
    chunk   <- store_get_bytes(store, "foo/0")
    decoded <- dafr:::zarr_v2_decode_chunk(chunk, zarray$dtype, n = 3L,
                                            compressor = zarray$compressor)
    expect_equal(decoded, c(1.5, 2.5, 3.5))
})
