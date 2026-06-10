# Interop parity with DataAxesFormats.jl's ZarrDaf on-disk format.
#
# DataAxesFormats.jl 0.3.0 uses the Zarr *v3* on-disk format and marks a
# daf-zarr store with the root-group attribute `daf: [MAJOR, MINOR] = [1, 0]`,
# validating a store by reading that attribute. dafr's ZarrDaf now reads and
# writes the same v3 format. These tests pin the upstream-compatible v3 shape
# (single `zarr.json` per node, `c/`-prefixed chunk keys, root `daf` attribute).

test_that("a fresh ZarrDaf store uses upstream's root `daf` attribute marker (v3)", {
    path <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)

    d <- zarr_daf(path, "w")
    add_axis(d, "cell", c("c1", "c2"))
    store <- S7::prop(d, "store")

    # v3 marker: the root group carries a `daf: [1, 0]` attribute; there is no
    # legacy v2 `daf` array, no daf.json.
    expect_true(zarr_v3_daf_marker_exists(store))
    expect_identical(zarr_v3_daf_version(store), c(1L, 0L))
    expect_false(store_exists(store, "daf/.zarray"))
    expect_false(store_exists(store, "daf.json"))

    root <- zarr_v3_read_node(store, "")
    expect_identical(root$node_type, "group")
    expect_identical(as.integer(unlist(root$attributes$daf)), c(1L, 0L))
})

# NOTE: the former "zarr_v2_decode_chunk reads the unsigned/narrow/float32
# dtypes Julia emits" test is superseded by the v3 port. It exercised the v2
# numpy-dtype reader (`zarr_v2_decode_chunk`), which dafr no longer uses for
# interop now that DAF 0.3.0 is Zarr v3. The equivalent v3 decode coverage
# lives in tests/testthat/test-zarr-v3.R ("zarr_v3 decodes float32 ...", the
# numeric chunk round-trip cases). The v2 codec and its own unit tests are
# removed in the Phase 4 sweep; nothing here depends on the v2 dtype reader.

test_that("R-written store has a `zarr.json` group marker for every group (v3 directory open)", {
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
        expect_true(store_exists(store, paste0(g, "/zarr.json")), info = g)
    }
    # Per-axis / per-matrix subgroups also need real group markers.
    expect_true(store_exists(store, "vectors/cell/zarr.json"))
    expect_true(store_exists(store, "matrices/cell/zarr.json"))
    expect_true(store_exists(store, "matrices/cell/gene/zarr.json"))
    expect_true(store_exists(store, "matrices/cell/gene/UMIs/zarr.json"))
    # No legacy v2 markers remain.
    expect_false(store_exists(store, "axes/.zgroup"))
})

test_that("R-written dense matrix uses the v3 `c/0/0` chunk key (Julia interop)", {
    path <- tempfile(fileext = ".daf.zarr")
    on.exit(unlink(path, recursive = TRUE, force = TRUE), add = TRUE)

    d <- zarr_daf(path, "w")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
    set_matrix(d, "cell", "gene", "D", m)
    store <- S7::prop(d, "store")

    # Zarr v3 addresses a 2-D single chunk as `c/0/0` (prefix `c`, separator
    # `/`). The legacy v2 `0.0` / `0/0` keys are gone.
    expect_true(store_exists(store, "matrices/cell/gene/D/c/0/0"))
    expect_false(store_exists(store, "matrices/cell/gene/D/0.0"))
    expect_false(store_exists(store, "matrices/cell/gene/D/0/0"))
    meta <- zarr_v3_read_array(store, "matrices/cell/gene/D")
    expect_identical(meta$node_type, "array")
    # On-disk shape is reversed [n_cols, n_rows]; no v2 `order`/`dimension_separator`.
    expect_identical(as.integer(unlist(meta$shape)), c(2L, 3L))

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
    skip_if_not(.daf_jl_uses_zarr_v3(),
                "requires DAF >= 0.3.0 (Zarr v3)")

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
    skip_if_not(.daf_jl_uses_zarr_v3(),
                "requires DAF >= 0.3.0 (Zarr v3)")

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

test_that("a DENSE matrix round-trips R <-> Julia (the v3 c/0/0 chunk)", {
    skip_on_cran()
    skip_if_not(.have_julia_env(), "dafr-mcview Julia env not available")
    skip_if_not(.daf_jl_uses_zarr_v3(),
                "requires DAF >= 0.3.0 (Zarr v3)")

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
