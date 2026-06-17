# tests/testthat/test-zarr-packed-write.R
# Tests for zarr_v3_sharded_array_meta() - the zarr.json builder for packed
# (sharding_indexed) Zarr v3 arrays.

test_that("zarr_v3_sharded_array_meta matches the fixture structure", {
    meta <- dafr:::zarr_v3_sharded_array_meta(
        shape = 1200L, dtype = "float64", inner = 1024L,
        codec = "blosc_zstd_bitshuffle", level = 5L)
    expect_equal(meta$codecs[[1]]$name, "sharding_indexed")
    cfg <- meta$codecs[[1]]$configuration
    expect_equal(unlist(cfg$chunk_shape), 1024L)
    expect_equal(cfg$index_location, "start")
    expect_equal(meta$attributes$daf_packed_format, "indexed+zipped")
    expect_equal(unlist(meta$chunk_grid$configuration$chunk_shape), 1200L)
})

test_that("zarr_v3_sharded_array_meta parsed structure matches bz fixture (score vector)", {
    fixture_path <- testthat::test_path(
        "fixtures/zpk/bz.daf.zarr/vectors/cell/score/zarr.json")
    skip_if_not(file.exists(fixture_path), "zpk fixture not available")

    meta <- dafr:::zarr_v3_sharded_array_meta(
        shape = 1200L, dtype = "float64", inner = 1024L,
        codec = "blosc_zstd_bitshuffle", level = 5L)

    # Serialize and re-parse to canonicalize (jsonlite round-trip)
    json_str <- as.character(jsonlite::toJSON(meta, auto_unbox = TRUE,
                                              null = "null", pretty = FALSE))
    got  <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    ref  <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)

    # --- top-level keys present in fixture must be present in dafr output ---
    expect_equal(got$zarr_format,   ref$zarr_format)
    expect_equal(got$node_type,     ref$node_type)
    expect_equal(got$data_type,     ref$data_type)
    expect_equal(got$fill_value,    ref$fill_value)
    expect_equal(got$shape,         ref$shape)
    expect_equal(got$attributes,    ref$attributes)

    # --- chunk_grid ---
    expect_equal(got$chunk_grid$name, ref$chunk_grid$name)
    expect_equal(got$chunk_grid$configuration$chunk_shape,
                 ref$chunk_grid$configuration$chunk_shape)

    # --- chunk_key_encoding ---
    expect_equal(got$chunk_key_encoding$name, ref$chunk_key_encoding$name)
    expect_equal(got$chunk_key_encoding$configuration$separator,
                 ref$chunk_key_encoding$configuration$separator)

    # --- sharding_indexed codec ---
    expect_length(got$codecs, 1L)
    gc <- got$codecs[[1L]]
    rc <- ref$codecs[[1L]]
    expect_equal(gc$name, "sharding_indexed")
    expect_equal(gc$name, rc$name)

    # inner chunk_shape
    expect_equal(gc$configuration$chunk_shape,
                 rc$configuration$chunk_shape)

    # index_location
    expect_equal(gc$configuration$index_location,
                 rc$configuration$index_location)

    # inner codecs pipeline: bytes + blosc
    got_inner <- gc$configuration$codecs
    ref_inner <- rc$configuration$codecs
    expect_length(got_inner, 2L)
    # bytes step
    expect_equal(got_inner[[1L]]$name, ref_inner[[1L]]$name)
    expect_equal(got_inner[[1L]]$configuration$endian,
                 ref_inner[[1L]]$configuration$endian)
    # blosc step
    expect_equal(got_inner[[2L]]$name, ref_inner[[2L]]$name)
    bc_got <- got_inner[[2L]]$configuration
    bc_ref <- ref_inner[[2L]]$configuration
    expect_equal(bc_got$cname,    bc_ref$cname)
    expect_equal(bc_got$clevel,   bc_ref$clevel)
    expect_equal(bc_got$shuffle,  bc_ref$shuffle)
    expect_equal(bc_got$typesize, bc_ref$typesize)
    expect_equal(bc_got$blocksize, bc_ref$blocksize)

    # index_codecs: bytes + crc32c
    got_ic <- gc$configuration$index_codecs
    ref_ic <- rc$configuration$index_codecs
    expect_length(got_ic, 2L)
    expect_equal(got_ic[[1L]]$name, ref_ic[[1L]]$name)
    expect_equal(got_ic[[1L]]$configuration$endian,
                 ref_ic[[1L]]$configuration$endian)
    expect_equal(got_ic[[2L]]$name, ref_ic[[2L]]$name)
    # crc32c must have no configuration key (pinned from fixture)
    expect_null(got_ic[[2L]]$configuration)
    expect_null(ref_ic[[2L]]$configuration)
})

test_that("zarr_v3_sharded_array_meta zstd codec matches fixture", {
    fixture_path <- testthat::test_path(
        "fixtures/zpk/zs.daf.zarr/vectors/cell/score/zarr.json")
    skip_if_not(file.exists(fixture_path), "zpk fixture not available")

    meta <- dafr:::zarr_v3_sharded_array_meta(
        shape = 1200L, dtype = "float64", inner = 1024L,
        codec = "zstd", level = 5L)
    json_str <- as.character(jsonlite::toJSON(meta, auto_unbox = TRUE,
                                              null = "null", pretty = FALSE))
    got <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    ref <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)

    got_inner <- got$codecs[[1L]]$configuration$codecs
    ref_inner <- ref$codecs[[1L]]$configuration$codecs
    expect_equal(got_inner[[2L]]$name, "zstd")
    expect_equal(got_inner[[2L]]$name, ref_inner[[2L]]$name)
    # zstd: only {level}, no checksum key (pinned from fixture)
    expect_equal(names(got_inner[[2L]]$configuration), "level")
    expect_equal(got_inner[[2L]]$configuration$level,
                 ref_inner[[2L]]$configuration$level)
})

test_that("zarr_v3_sharded_array_meta gzip codec matches fixture", {
    fixture_path <- testthat::test_path(
        "fixtures/zpk/gz.daf.zarr/vectors/cell/score/zarr.json")
    skip_if_not(file.exists(fixture_path), "zpk fixture not available")

    meta <- dafr:::zarr_v3_sharded_array_meta(
        shape = 1200L, dtype = "float64", inner = 1024L,
        codec = "gzip", level = 5L)
    json_str <- as.character(jsonlite::toJSON(meta, auto_unbox = TRUE,
                                              null = "null", pretty = FALSE))
    got <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    ref <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)

    got_inner <- got$codecs[[1L]]$configuration$codecs
    ref_inner <- ref$codecs[[1L]]$configuration$codecs
    expect_equal(got_inner[[2L]]$name, "gzip")
    expect_equal(got_inner[[2L]]$name, ref_inner[[2L]]$name)
    expect_equal(got_inner[[2L]]$configuration$level,
                 ref_inner[[2L]]$configuration$level)
})

test_that("zarr_v3_sharded_array_meta matrix shape is on-disk [ncol, nrow]", {
    # Matrix fixture: gene=8 rows, cell=1200 cols -> on-disk shape [8, 1200]
    # (reversed), inner [1, 1024]
    fixture_path <- testthat::test_path(
        "fixtures/zpk/bz.daf.zarr/matrices/cell/gene/dense/zarr.json")
    skip_if_not(file.exists(fixture_path), "zpk fixture not available")

    meta <- dafr:::zarr_v3_sharded_array_meta(
        shape = c(8L, 1200L), dtype = "float64", inner = c(1L, 1024L),
        codec = "blosc_zstd_bitshuffle", level = 5L)

    expect_equal(unlist(meta$shape), c(8L, 1200L))
    expect_equal(unlist(meta$chunk_grid$configuration$chunk_shape), c(8L, 1200L))
    cfg <- meta$codecs[[1L]]$configuration
    expect_equal(unlist(cfg$chunk_shape), c(1L, 1024L))

    json_str <- as.character(jsonlite::toJSON(meta, auto_unbox = TRUE,
                                              null = "null", pretty = FALSE))
    got <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
    ref <- jsonlite::fromJSON(fixture_path, simplifyVector = FALSE)
    expect_equal(got$shape, ref$shape)
    expect_equal(got$chunk_grid$configuration$chunk_shape,
                 ref$chunk_grid$configuration$chunk_shape)
    expect_equal(got$codecs[[1L]]$configuration$chunk_shape,
                 ref$codecs[[1L]]$configuration$chunk_shape)
})

test_that("zarr_v3_sharded_array_meta unknown codec errors cleanly", {
    expect_error(
        dafr:::zarr_v3_sharded_array_meta(
            shape = 100L, dtype = "float64", inner = 64L,
            codec = "lzma", level = 5L),
        "unknown packed codec")
})

test_that("zarr_daf(packed=TRUE) round-trips dense, matrix, sparse, strings", {
    codec <- if (dafr:::dafr_have_blosc_cpp()) "blosc_zstd_bitshuffle" else "gzip"
    withr::local_options(list(dafr.packed_compression = codec))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.daf.zarr")
    daf <- zarr_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200))
    add_axis(daf, "gene", paste0("g", 1:8))
    set_vector(daf, "cell", "score", as.numeric(1:1200))
    set_vector(daf, "cell", "tag", rep("x", 1200))
    set_matrix(daf, "cell", "gene", "dense", matrix(as.numeric(1:(1200*8)), 1200, 8))
    sm <- Matrix::sparseMatrix(i = ((seq_len(2000) - 1) %% 1200) + 1,
                               j = ((seq_len(2000) - 1) %% 8) + 1,
                               x = as.numeric(1:2000), dims = c(1200, 8))
    set_matrix(daf, "cell", "gene", "sparse", sm)

    ro <- zarr_daf(path, "r")
    expect_equal(as.numeric(get_vector(ro, "cell", "score")), as.numeric(1:1200))
    expect_equal(as.numeric(get_matrix(ro, "cell", "gene", "dense")), as.numeric(1:(1200*8)))
    expect_equal(unname(get_vector(ro, "cell", "tag"))[1], "x")
    expect_equal(Matrix::nnzero(get_matrix(ro, "cell", "gene", "sparse")), Matrix::nnzero(sm))
    # score must actually be sharded on disk
    node <- dafr:::zarr_v3_read_array(S7::prop(ro, "store"), "vectors/cell/score")
    expect_true(dafr:::.zarr_is_sharded(node))
    # dense matrix inner chunk_shape must be [1,1024], shape [8,1200] (matches fixture)
    mnode <- dafr:::zarr_v3_read_array(S7::prop(ro, "store"), "matrices/cell/gene/dense")
    expect_equal(unlist(mnode$codecs[[1]]$configuration$chunk_shape), c(1L, 1024L))
    expect_equal(unlist(mnode$shape), c(8L, 1200L))
})

test_that("zarr_daf(packed=FALSE) is unchanged (flat)", {
    dir <- withr::local_tempdir(); path <- file.path(dir, "f.daf.zarr")
    daf <- zarr_daf(path, "w")  # default packed=FALSE
    add_axis(daf, "cell", paste0("c", 1:1200))
    set_vector(daf, "cell", "score", as.numeric(1:1200))
    ro <- zarr_daf(path, "r")
    node <- dafr:::zarr_v3_read_array(S7::prop(ro, "store"), "vectors/cell/score")
    expect_false(dafr:::.zarr_is_sharded(node))   # flat
    expect_equal(as.numeric(get_vector(ro, "cell", "score")), as.numeric(1:1200))
})

test_that("ZarrDaf packed write errors actionably when the zstd lib is absent", {
    if (dafr:::dafr_have_zstd_cpp()) skip("libzstd present")
    withr::local_options(list(dafr.packed_compression = "zstd"))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.daf.zarr")
    daf <- zarr_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200))
    expect_error(set_vector(daf, "cell", "score", as.numeric(1:1200)),
                 "requires libzstd")
})

test_that("ZarrDaf packed write with gzip works without any optional lib", {
    withr::local_options(list(dafr.packed_compression = "gzip"))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.daf.zarr")
    daf <- zarr_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:1200))
    set_vector(daf, "cell", "score", as.numeric(1:1200))
    ro <- zarr_daf(path, "r")
    expect_equal(as.numeric(get_vector(ro, "cell", "score")), as.numeric(1:1200))
})

test_that("zarr_daf(packed=TRUE) keeps string vectors flat", {
    codec <- if (dafr:::dafr_have_blosc_cpp()) "blosc_zstd_bitshuffle" else "gzip"
    withr::local_options(list(dafr.packed_compression = codec))
    dir <- withr::local_tempdir(); path <- file.path(dir, "p.daf.zarr")
    daf <- zarr_daf(path, "w", packed = TRUE)
    add_axis(daf, "cell", paste0("c", 1:5000))
    set_vector(daf, "cell", "label", paste0("lab", 1:5000))   # large string vec
    ro <- zarr_daf(path, "r")
    node <- dafr:::zarr_v3_read_array(S7::prop(ro, "store"), "vectors/cell/label")
    expect_false(dafr:::.zarr_is_sharded(node))   # strings stay flat
    expect_equal(unname(get_vector(ro, "cell", "label"))[c(1, 5000)], c("lab1", "lab5000"))
})
