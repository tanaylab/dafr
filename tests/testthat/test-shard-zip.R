test_that("a dual-format blosc shard is a legal ZIP and round-trips", {
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    skip_if_not_installed("zip")
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble(vals, "float64", shape = 1200L, inner = 1024L,
                                   codec = "blosc_zstd_bitshuffle", level = 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "blosc_zstd_bitshuffle",
             chunk_shape = list(1024L)), shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
    tmp <- tempfile(fileext = ".zip"); writeBin(blob, tmp)
    z <- zip::zip_list(tmp)
    expect_true(all(c("c/0", "c/1", "codec.json") %in% z$filename))
})

test_that(".shard_chunk_name uses C-order matching the 2-D Julia matrix fixture", {
    # On-disk grid for the dense matrix fixture: shape [8,1200] / inner [1,1024]
    # -> per_dim [8,2]. Julia writes inner chunks in C-order (last axis fastest)
    # with the path components in grid-axis order (NOT reversed).
    fixture_names <- c("c/0/0", "c/0/1", "c/1/0", "c/1/1", "c/2/0", "c/2/1",
                       "c/3/0", "c/3/1", "c/4/0", "c/4/1", "c/5/0", "c/5/1",
                       "c/6/0", "c/6/1", "c/7/0", "c/7/1")
    got <- vapply(1:16, function(i) dafr:::.shard_chunk_name(i, c(8L, 2L)), "")
    expect_equal(got, fixture_names)
    # 1-D behavior is unchanged.
    expect_equal(vapply(1:2, function(i) dafr:::.shard_chunk_name(i, 2L), ""),
                 c("c/0", "c/1"))
    # Multi-digit zero-padding (per_dim 12 -> width 2).
    expect_equal(dafr:::.shard_chunk_name(11L, 12L), "c/10")
})

test_that("dual-format framing is byte-identical to the 2-D Julia matrix fixture", {
    skip_if_not(dafr:::dafr_have_blosc_cpp(), "c-blosc not built in")
    skip_if_not_installed("zip")
    fx <- testthat::test_path("fixtures/zpk/bz.daf.zarr/matrices/cell/gene/dense")
    fixture <- readBin(file.path(fx, "c/0/0"), "raw", n = 1e7)
    # On-disk shape + inner chunk_shape from the dense matrix zarr.json.
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "blosc_zstd_bitshuffle",
             chunk_shape = list(1L, 1024L)),
        shape = c(8L, 1200L), chunk_shape = c(1L, 1024L))
    cfg <- dafr:::.zarr_sharding_config(node)
    idx <- dafr:::.zarr_shard_index(fixture, node, cfg)
    n <- nrow(idx)
    # Each chunk's compressed payload, lifted straight out of the fixture via the
    # shard index (so blosc nondeterminism is irrelevant - we re-use Julia's
    # exact STORED bytes and only re-run dafr's ZIP framing around them).
    comp <- lapply(seq_len(n), function(k)
        fixture[(idx$offset[[k]] + 1):(idx$offset[[k]] + idx$nbytes[[k]])])

    # Reproduce .shard_assemble's framing with the fixture payloads.
    per_dim <- as.integer(ceiling(c(8L, 1200L) / c(1L, 1024L)))
    method <- 0L
    idx_size <- as.numeric(n) * 16 + 4
    bodies <- vector("list", n); offsets <- numeric(n); nbytes <- numeric(n)
    centrals <- vector("list", n); cursor <- idx_size
    for (k in seq_len(n)) {
        c_k <- comp[[k]]
        name <- dafr:::.shard_chunk_name(k, per_dim)
        crc <- dafr:::dafr_crc32_cpp(c_k) %% 2^32
        lfh <- dafr:::.shard_zip_local_header(name, crc, length(c_k),
                                              length(c_k), method)
        offsets[[k]] <- cursor + length(lfh); nbytes[[k]] <- length(c_k)
        centrals[[k]] <- dafr:::.shard_zip_central_entry(name, crc, length(c_k),
                                                         length(c_k), cursor, method)
        bodies[[k]] <- c(lfh, c_k)
        cursor <- cursor + length(lfh) + length(c_k)
    }
    cfg2 <- list(codecs = list(list(name = "bytes"), list(name = "blosc")),
                 .blosc_cname = "zstd", .clevel = 5L)
    cj <- dafr:::.shard_codec_json_entry(cfg2, typesize = 8L, cursor)
    bodies <- c(bodies, list(cj$body))
    centrals <- c(centrals, list(cj$central)); cursor <- cursor + length(cj$body)
    cd <- do.call(c, centrals); cd_off <- cursor
    eocd <- dafr:::.shard_zip_eocd(length(centrals), cd_off, length(cd))
    index <- dafr:::.shard_build_index(offsets, nbytes)
    reconstructed <- c(index, do.call(c, bodies), cd, eocd)

    expect_identical(reconstructed, fixture)

    # And the framed blob is a legal ZIP whose entry order matches the fixture.
    tmp <- tempfile(fileext = ".zip"); writeBin(reconstructed, tmp)
    expect_equal(zip::zip_list(tmp)$filename,
                 c(vapply(1:16, function(i) dafr:::.shard_chunk_name(i, c(8L, 2L)), ""),
                   "codec.json"))
})

test_that("a dual-format zstd shard round-trips and is a legal ZIP", {
    skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
    skip_if_not_installed("zip")
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble(vals, "float64", 1200L, 1024L, "zstd", 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "zstd", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
    tmp <- tempfile(fileext = ".zip"); writeBin(blob, tmp)
    z <- zip::zip_list(tmp)
    expect_true(all(c("c/0", "c/1") %in% z$filename))
    expect_false("codec.json" %in% z$filename)   # zstd self-describes
})

test_that("a dual-format gzip shard round-trips and is a legal ZIP", {
    skip_if_not_installed("zip")
    vals <- as.numeric(1:1200)
    blob <- dafr:::.shard_assemble(vals, "float64", 1200L, 1024L, "gzip", 5L)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "gzip", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    expect_equal(dafr:::.shard_decode_vector(blob, node), vals)
    tmp <- tempfile(fileext = ".zip"); writeBin(blob, tmp)
    z <- zip::zip_list(tmp)
    expect_equal(nrow(z), 2L)            # 2 DEFLATE entries, no codec.json
    expect_false("codec.json" %in% z$filename)
})

test_that(".shard_gzip_name encodes the chunk index per the gz fixture", {
    # b0..b3 = base64 of the big-endian 3-byte (k-1) chunk index; the rest is the
    # fixed gzip header `1f 8b 08 01 ... 02 ff`.
    expect_identical(dafr:::.shard_gzip_name(1L),
                     as.raw(c(0x1f, 0x8b, 0x08, 0x01, 0x41, 0x41, 0x41, 0x41,
                              0x02, 0xff)))               # chunk 0 -> "AAAA"
    expect_identical(dafr:::.shard_gzip_name(2L),
                     as.raw(c(0x1f, 0x8b, 0x08, 0x01, 0x41, 0x41, 0x41, 0x42,
                              0x02, 0xff)))               # chunk 1 -> "AAAB"
})

test_that("gzip framing is byte-identical to the 1-D Julia gzip fixture", {
    # The gz fixture is a gzip dual-format shard: score vector, 1200 float64,
    # inner 1024 -> 2 chunks. The 10-byte gzip header is relocated into the ZIP
    # filename so each chunk's Zarr range [name(10) | deflate | trailer(8)] is a
    # valid gzip stream.
    fx <- testthat::test_path("fixtures/zpk/gz.daf.zarr/vectors/cell/score/c/0")
    fixture <- readBin(fx, "raw", n = 1e6)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "gzip", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    cfg <- dafr:::.zarr_sharding_config(node)
    idx <- dafr:::.zarr_shard_index(fixture, node, cfg)
    n <- nrow(idx)
    # Lift each chunk's OWN deflate payload (strip the 10-byte gzip-header name
    # and the 8-byte gzip trailer) straight out of the fixture, then re-run only
    # dafr's gzip framing around it. This isolates the FRAMING (LFH/CDE/index
    # offsets, name encoding, trailer) from any DEFLATE-encoder difference
    # between R's zlib and Julia's CodecZlib, so the test stays robust to a
    # future zlib that emits different deflate bytes.
    gz <- lapply(seq_len(n), function(k)
        fixture[(idx$offset[[k]] + 1):(idx$offset[[k]] + idx$nbytes[[k]])])
    plain <- lapply(gz, function(g) memDecompress(g, type = "gzip"))
    deflate <- lapply(gz, function(g) g[11:(length(g) - 8L)])

    idx_size <- as.numeric(n) * 16 + 4
    bodies <- vector("list", n); offsets <- numeric(n); nbytes <- numeric(n)
    centrals <- vector("list", n); cursor <- idx_size
    for (k in seq_len(n)) {
        p_k <- plain[[k]]; d_k <- deflate[[k]]
        name_raw <- dafr:::.shard_gzip_name(k)
        crc <- dafr:::dafr_crc32_cpp(p_k) %% 2^32
        trailer <- c(dafr:::.shard_u32_raw(crc),
                     dafr:::.shard_u32_raw(length(p_k) %% 2^32))
        lfh <- dafr:::.shard_zip_local_header_raw(name_raw, crc, length(d_k),
                                                  length(p_k), 8L)
        offsets[[k]] <- cursor + (length(lfh) - length(name_raw))
        nbytes[[k]] <- length(name_raw) + length(d_k) + length(trailer)
        centrals[[k]] <- dafr:::.shard_zip_central_entry_raw(name_raw, crc,
            length(d_k), length(p_k), cursor, 8L)
        bodies[[k]] <- c(lfh, d_k, trailer)
        cursor <- cursor + length(lfh) + length(d_k) + length(trailer)
    }
    cd <- do.call(c, centrals); cd_off <- cursor
    eocd <- dafr:::.shard_zip_eocd(length(centrals), cd_off, length(cd))
    index <- dafr:::.shard_build_index(offsets, nbytes)
    reconstructed <- c(index, do.call(c, bodies), cd, eocd)
    expect_identical(reconstructed, fixture)
    # NB: we deliberately do NOT assert a freshly re-compressed dafr blob equals
    # the fixture byte-for-byte. Per the design, only the FRAMING is guaranteed
    # byte-identical to Julia; the compressed DEFLATE payload depends on the
    # platform zlib build (R's zlib vs Julia's CodecZlib) and is not portable.
    # The reconstruction above isolates and pins the framing without that
    # dependency; round-trip + interop tests cover functional correctness.
})

test_that("zstd framing is byte-identical to the 1-D Julia zstd fixture", {
    skip_if_not(dafr:::dafr_have_zstd_cpp(), "libzstd not built in")
    skip_if_not_installed("zip")
    # The zs fixture is a zstd dual-format shard: score vector, 1200 float64,
    # inner 1024 -> 2 chunks. We lift each chunk's compressed zstd frame straight
    # out of the fixture via its shard index (so zstd-encoder nondeterminism is
    # irrelevant - we reuse Julia's exact frames and only re-run dafr's framing).
    fx <- testthat::test_path("fixtures/zpk/zs.daf.zarr/vectors/cell/score/c/0")
    fixture <- readBin(fx, "raw", n = 1e6)
    node <- dafr:::.files_packed_node(
        list(eltype = "Float64", compression = "zstd", chunk_shape = list(1024L)),
        shape = 1200L, chunk_shape = 1024L)
    cfg <- dafr:::.zarr_sharding_config(node)
    idx <- dafr:::.zarr_shard_index(fixture, node, cfg)
    n <- nrow(idx)
    comp <- lapply(seq_len(n), function(k)
        fixture[(idx$offset[[k]] + 1):(idx$offset[[k]] + idx$nbytes[[k]])])
    # Each chunk's uncompressed length is the full inner shape (1024*8=8192) -
    # chunks are fill-padded before compression, so even the last partial chunk
    # decompresses to a full 8192 bytes. Confirm against the fixture's usize.
    z <- zip::zip_list({tmp0 <- tempfile(fileext = ".zip"); writeBin(fixture, tmp0); tmp0})
    expect_equal(z$uncompressed_size, rep(8192, n))   # usize = length(plain)
    expect_equal(z$compressed_size, vapply(comp, length, numeric(1)))  # csize = length(comp)
    plain <- lapply(comp, function(c_k) dafr:::dafr_zstd_decompress_cpp(c_k, 8192))

    # Reproduce .shard_assemble's framing with the fixture frames, NO codec.json,
    # usize=length(plain) and crc-over-plain per the per-method rule (method 93).
    per_dim <- 2L
    method <- 93L
    idx_size <- as.numeric(n) * 16 + 4
    bodies <- vector("list", n); offsets <- numeric(n); nbytes <- numeric(n)
    centrals <- vector("list", n); cursor <- idx_size
    for (k in seq_len(n)) {
        c_k <- comp[[k]]; p_k <- plain[[k]]
        name <- dafr:::.shard_chunk_name(k, per_dim)
        crc <- dafr:::dafr_crc32_cpp(p_k) %% 2^32
        lfh <- dafr:::.shard_zip_local_header(name, crc, length(c_k),
                                              length(p_k), method)
        offsets[[k]] <- cursor + length(lfh); nbytes[[k]] <- length(c_k)
        centrals[[k]] <- dafr:::.shard_zip_central_entry(name, crc, length(c_k),
                                                         length(p_k), cursor, method)
        bodies[[k]] <- c(lfh, c_k)
        cursor <- cursor + length(lfh) + length(c_k)
    }
    cd <- do.call(c, centrals); cd_off <- cursor
    eocd <- dafr:::.shard_zip_eocd(length(centrals), cd_off, length(cd))
    index <- dafr:::.shard_build_index(offsets, nbytes)
    reconstructed <- c(index, do.call(c, bodies), cd, eocd)

    expect_identical(reconstructed, fixture)
})
