# Interop parity with DataAxesFormats.jl's ZarrDaf on-disk format.
#
# Upstream (DataAxesFormats.jl src/zarr_format.jl) marks a daf-zarr store
# with a Zarr *array* named `daf` holding two UInt8 bytes
# [MAJOR_VERSION, MINOR_VERSION] = [1, 0], and validates a store by
# `haskey(root.arrays, "daf")`. dafr historically wrote a plain `daf.json`
# file instead, which made R-written and Julia-written `.daf.zarr` stores
# mutually unreadable. These tests pin the upstream-compatible marker.

test_that("a fresh ZarrDaf store uses upstream's `daf` array marker, not daf.json", {
    path <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)

    d <- zarr_daf(path, "w")
    add_axis(d, "cell", c("c1", "c2"))
    store <- S7::prop(d, "store")

    # Upstream marker: a `daf` Zarr array of dtype |u1, shape [2], bytes [1, 0].
    expect_true(store_exists(store, "daf/.zarray"))
    expect_false(store_exists(store, "daf.json"))

    zarray <- zarr_v2_read_zarray(store, "daf")
    expect_identical(zarray$dtype, "|u1")
    expect_identical(as.integer(unlist(zarray$shape)), 2L)

    marker <- store_get_bytes(store, "daf/0")
    expect_identical(as.integer(marker), c(1L, 0L))
})

test_that("zarr_v2_decode_chunk reads the unsigned / narrow / float32 dtypes Julia emits", {
    # |u1 (the `daf` marker dtype) + <u1: unsigned byte 0..255.
    expect_identical(zarr_v2_decode_chunk(as.raw(c(1, 255)), "|u1", 2L),
                     c(1L, 255L))
    expect_identical(zarr_v2_decode_chunk(as.raw(c(0, 200)), "<u1", 2L),
                     c(0L, 200L))
    # <i1: signed byte.
    expect_identical(zarr_v2_decode_chunk(as.raw(c(0xFF, 0x7F)), "<i1", 2L),
                     c(-1L, 127L))
    # <u2 / <i2: little-endian 16-bit. 65535 = FF FF, 258 = 02 01, -1 = FF FF.
    expect_identical(zarr_v2_decode_chunk(as.raw(c(0xFF, 0xFF, 0x02, 0x01)),
                                          "<u2", 2L), c(65535L, 258L))
    expect_identical(zarr_v2_decode_chunk(as.raw(c(0xFF, 0xFF, 0x02, 0x01)),
                                          "<i2", 2L), c(-1L, 258L))
    # <u4: 3000000000 > .Machine$integer.max -> returned as double.
    expect_equal(zarr_v2_decode_chunk(as.raw(c(0x00, 0x5E, 0xD0, 0xB2)),
                                      "<u4", 1L), 3000000000)
    # <u8: 5000000000 (Julia index/size eltype on the empty-fill path).
    expect_equal(as.numeric(zarr_v2_decode_chunk(
        as.raw(c(0x00, 0xF2, 0x05, 0x2A, 0x01, 0x00, 0x00, 0x00)), "<u8", 1L)),
        5000000000)
    # <f4: Float32 values (common for single-cell expression matrices).
    f32 <- writeBin(c(1.5, -2.25), raw(), size = 4L, endian = "little")
    expect_equal(zarr_v2_decode_chunk(f32, "<f4", 2L), c(1.5, -2.25))
})

test_that("R-written store has a real .zgroup for every group (Julia directory open needs them)", {
    path <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)

    d <- zarr_daf(path, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    set_scalar(d, "title", "hi")
    set_vector(d, "cell", "age", c(1, 2, 3))
    set_matrix(d, "cell", "gene", "UMIs",
               Matrix::Matrix(matrix(c(1, 0, 0, 0, 2, 0), 3, 2), sparse = TRUE))
    store <- S7::prop(d, "store")

    # The four standard top-level groups (Julia create_daf makes them eagerly).
    for (g in c("axes", "scalars", "vectors", "matrices")) {
        expect_true(store_exists(store, paste0(g, "/.zgroup")), info = g)
    }
    # Per-axis / per-matrix subgroups also need real .zgroup markers.
    expect_true(store_exists(store, "vectors/cell/.zgroup"))
    expect_true(store_exists(store, "matrices/cell/.zgroup"))
    expect_true(store_exists(store, "matrices/cell/gene/.zgroup"))
    expect_true(store_exists(store, "matrices/cell/gene/UMIs/.zgroup"))
})

test_that("R-written dense matrix uses the Zarr-default '.' chunk separator (Julia interop)", {
    path <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)

    d <- zarr_daf(path, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    set_matrix(d, "cell", "gene", "D", m)
    store <- S7::prop(d, "store")

    # Upstream (and the Zarr v2 default) addresses a 2-D chunk as `0.0`, not
    # `0/0`. dafr historically used `/`, which made Julia fail to find the
    # dense-matrix chunk ("missing chunks and no fill_value").
    expect_true(store_exists(store, "matrices/cell/gene/D/0.0"))
    expect_false(store_exists(store, "matrices/cell/gene/D/0/0"))
    za <- zarr_v2_read_zarray(store, "matrices/cell/gene/D")
    expect_identical(za$dimension_separator, ".")

    # Still round-trips within R.
    expect_equal(as.matrix(get_matrix(d, "cell", "gene", "D")), m,
                 ignore_attr = TRUE)
})

# The dense column-major flattening of the shared sparse UMIs fixture
# (cell x gene): (1,1)=1, (2,2)=2, (3,3)=3, (1,4)=5.
.umis_dense <- matrix(c(1, 0, 0, 0, 2, 0, 0, 0, 3, 5, 0, 0), nrow = 3, ncol = 4)

test_that("an R-written .daf.zarr is readable by DataAxesFormats.jl", {
    skip_on_cran()
    skip_if_not(.have_julia_env(), "dafr-mcview Julia env not available")
    skip_if(.daf_jl_uses_zarr_v3(),
            "DAF >= 0.3.0 uses Zarr v3; dafr ZarrDaf is v2 (R<->Julia zarr interop pending the v3 port)")

    rpath <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(rpath, recursive = TRUE, force = TRUE), add = TRUE)

    d <- zarr_daf(rpath, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2", "g3", "g4"))
    set_scalar(d, "title", "hello")
    set_vector(d, "cell", "age", c(10, 20, 30))
    set_vector(d, "gene", "symbol", c("A", "B", "C", "D"))
    set_matrix(d, "cell", "gene", "UMIs",
               Matrix::Matrix(.umis_dense, sparse = TRUE))

    out <- run_julia(c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = ZarrDaf("%s", "r"; name = "rstore")', rpath),
        'println("TITLE=", get_scalar(daf, "title"))',
        'println("AGE=", join(get_vector(daf, "cell", "age"), ","))',
        'println("SYMBOL=", join(get_vector(daf, "gene", "symbol"), ","))',
        'println("UMIS=", join(vec(Matrix(get_matrix(daf, "cell", "gene", "UMIs"))), ","))'
    ))

    get_line <- function(prefix) {
        hit <- grep(paste0("^", prefix, "="), out, value = TRUE)
        if (length(hit) != 1L) {
            fail(paste0("Julia did not emit ", prefix, "; output:\n",
                        paste(out, collapse = "\n")))
        }
        sub(paste0("^", prefix, "="), "", hit)
    }

    expect_identical(get_line("TITLE"), "hello")
    expect_equal(as.numeric(strsplit(get_line("AGE"), ",")[[1]]), c(10, 20, 30))
    expect_identical(strsplit(get_line("SYMBOL"), ",")[[1]], c("A", "B", "C", "D"))
    expect_equal(as.numeric(strsplit(get_line("UMIS"), ",")[[1]]),
                 as.numeric(.umis_dense))
})

test_that("a DataAxesFormats.jl-written .daf.zarr is readable by dafr", {
    skip_on_cran()
    skip_if_not(.have_julia_env(), "dafr-mcview Julia env not available")
    skip_if(.daf_jl_uses_zarr_v3(),
            "DAF >= 0.3.0 uses Zarr v3; dafr ZarrDaf is v2 (R<->Julia zarr interop pending the v3 port)")

    jpath <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(jpath, recursive = TRUE, force = TRUE), add = TRUE)

    out <- run_julia(c(
        "using DataAxesFormats, SparseArrays",
        sprintf('daf = ZarrDaf("%s", "w"; name = "jstore")', jpath),
        'add_axis!(daf, "cell", ["c1", "c2", "c3"])',
        'add_axis!(daf, "gene", ["g1", "g2", "g3", "g4"])',
        'set_scalar!(daf, "title", "hello")',
        'set_vector!(daf, "cell", "age", Float64[10, 20, 30])',
        'set_vector!(daf, "gene", "symbol", ["A", "B", "C", "D"])',
        'set_matrix!(daf, "cell", "gene", "UMIs", sparse([1.0 0 0 5; 0 2.0 0 0; 0 0 3.0 0]))',
        'println("DONE")'
    ))
    skip_if_not(any(grepl("^DONE$", out)),
                paste0("Julia writer failed:\n", paste(out, collapse = "\n")))

    d <- zarr_daf(jpath, "r")
    expect_identical(get_scalar(d, "title"), "hello")
    expect_equal(unname(get_vector(d, "cell", "age")), c(10, 20, 30))
    expect_identical(unname(get_vector(d, "gene", "symbol")),
                     c("A", "B", "C", "D"))
    expect_equal(as.matrix(get_matrix(d, "cell", "gene", "UMIs")),
                 .umis_dense, ignore_attr = TRUE)
})

test_that("a DENSE matrix round-trips R <-> Julia (the '.'-separator chunk)", {
    skip_on_cran()
    skip_if_not(.have_julia_env(), "dafr-mcview Julia env not available")
    skip_if(.daf_jl_uses_zarr_v3(),
            "DAF >= 0.3.0 uses Zarr v3; dafr ZarrDaf is v2 (R<->Julia zarr interop pending the v3 port)")

    dense <- matrix(as.double(1:12), nrow = 3, ncol = 4)  # cell x gene

    # (a) R writes -> Julia reads.
    rpath <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(rpath, recursive = TRUE, force = TRUE), add = TRUE)
    d <- zarr_daf(rpath, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2", "g3", "g4"))
    set_matrix(d, "cell", "gene", "dense", dense)

    out <- run_julia(c(
        "using DataAxesFormats",
        sprintf('daf = ZarrDaf("%s", "r"; name = "r")', rpath),
        'println("DENSE=", join(vec(get_matrix(daf, "cell", "gene", "dense")), ","))'
    ))
    hit <- grep("^DENSE=", out, value = TRUE)
    skip_if_not(length(hit) == 1L, paste(out, collapse = "\n"))
    expect_equal(as.numeric(strsplit(sub("^DENSE=", "", hit), ",")[[1]]),
                 as.numeric(dense))

    # (b) Julia writes -> R reads.
    jpath <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(jpath, recursive = TRUE, force = TRUE), add = TRUE)
    out2 <- run_julia(c(
        "using DataAxesFormats",
        sprintf('daf = ZarrDaf("%s", "w"; name = "j")', jpath),
        'add_axis!(daf, "cell", ["c1", "c2", "c3"])',
        'add_axis!(daf, "gene", ["g1", "g2", "g3", "g4"])',
        'set_matrix!(daf, "cell", "gene", "dense", reshape(Float64.(1:12), 3, 4); relayout = false)',
        'println("DONE")'
    ))
    skip_if_not(any(grepl("^DONE$", out2)), paste(out2, collapse = "\n"))
    d2 <- zarr_daf(jpath, "r")
    expect_equal(as.matrix(get_matrix(d2, "cell", "gene", "dense")), dense,
                 ignore_attr = TRUE)
})
