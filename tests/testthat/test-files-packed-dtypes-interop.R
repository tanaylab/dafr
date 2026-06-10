# Adversarial dtype coverage for packed FilesFormat read. The committed packed
# fixtures (test-files-packed-read.R) only exercise Float64 / Int64; the Julia
# packed_format.jl tests cover every numeric width. Here we generate a packed
# store with one above-threshold vector per dtype (Int8..UInt64, Float32/64),
# plus an all-true Bool sparse vector, special floats (NaN/Inf/-Inf), and a
# vlen-utf8 string vector with unicode + control chars, and read them all back
# through dafr's packed reader. Live-gated on the Julia env (gzip = no C dep).

test_that("dafr reads packed FilesDaf vectors of every dtype (live, gzip)", {
    skip_on_cran()
    skip_if_not(.daf_jl_uses_zarr_v3(), "DAF >= 0.3.0 not available")
    out <- withr::local_tempdir("daf-dtypes-packed-")
    p <- file.path(out, "p.files")
    n <- 2000L
    res <- run_julia(c(
        "using DataAxesFormats, SparseArrays",
        "import DataAxesFormats.PackedFormat as PF",
        "PF.DAF_PACKED_COMPRESSION = :gzip",
        sprintf('d = FilesDaf(raw"%s", "w"; packed=true)', p),
        sprintf('add_axis!(d, "cell", ["c$(i)" for i in 1:%d])', n),
        'for (nm,T) in [("i8",Int8),("i16",Int16),("i32",Int32),("i64",Int64),',
        '               ("u8",UInt8),("u16",UInt16),("u32",UInt32),("u64",UInt64),',
        '               ("f32",Float32),("f64",Float64)]',
        sprintf('    set_vector!(d, "cell", nm, T[T((i-1)%%100) for i in 1:%d])', n),
        'end',
        sprintf('set_vector!(d, "cell", "boolv", SparseVector(%d, collect(2:2:%d), fill(true, %d)))', n, n, n %/% 2L),
        sprintf('fv = Float64.(1:%d); fv[1]=NaN; fv[2]=Inf; fv[3]=-Inf', n),
        'set_vector!(d, "cell", "special", fv)',
        sprintf('set_vector!(d, "cell", "str", ["s_\\u00e9_$(i)_\\t" for i in 1:%d])', n),
        'println("OK")'
    ))
    skip_if_not(any(grepl("^OK$", res)),
                paste0("julia packed write failed:\n", paste(res, collapse = "\n")))
    daf <- files_daf(p, mode = "r")
    expected <- (seq_len(n) - 1L) %% 100L
    for (nm in c("i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64", "f32", "f64")) {
        expect_equal(as.numeric(get_vector(daf, "cell", nm)), as.numeric(expected),
                     info = nm)
    }
    bv <- get_vector(daf, "cell", "boolv")
    expect_true(all(bv[seq(2L, n, 2L)]))   # all-true Bool sparse: evens TRUE
    expect_false(any(bv[seq(1L, n, 2L)]))  # odds FALSE
    sp <- as.numeric(get_vector(daf, "cell", "special"))
    expect_true(is.nan(sp[1]) && sp[2] == Inf && sp[3] == -Inf)
    st <- unname(get_vector(daf, "cell", "str"))
    expect_identical(st[1], "s_é_1_\t")   # unicode + tab survive vlen-utf8
})
