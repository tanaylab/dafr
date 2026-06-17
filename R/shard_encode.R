#' @include zarr_sharded.R
NULL

# R/shard_encode.R
# Dual-format packed/sharded WRITE encoder - the exact inverse of the read core
# in R/zarr_sharded.R. Pure R except inner-chunk compression (gzip via base-R
# memCompress; zstd/blosc via the configure-gated wrappers in
# src/shard_codecs.cpp). Storage-agnostic: produces one shard blob (raw vector);
# the ZarrDaf / FilesDaf writers persist it at the single outer-chunk key /
# `<name>.zip`. ZIP framing lives in R/shard_zip.R.

# Estimated element size for chunk-sizing: real width for fixed-width dtypes, a
# 16-byte estimate for strings (matches Julia STRING_SIZEOF_ESTIMATE).
.shard_effective_sizeof <- function(dtype) {
    if (identical(dtype, "string")) return(16L)
    zarr_v3_size_for_dtype(dtype)
}

# TRUE if a component's first-dim byte size meets the pack threshold.
.shard_should_pack <- function(dim1, dtype, target_kb) {
    as.numeric(dim1) * .shard_effective_sizeof(dtype) >= target_kb * 1024
}

# Inner chunk shape: n_chunk_rows over dim1 (column-slab for matrices).
.shard_inner_chunk_shape <- function(shape, dtype, target_kb) {
    target_bytes <- target_kb * 1024L
    esz <- .shard_effective_sizeof(dtype)
    n_rows <- min(target_bytes %/% esz, shape[[1L]])
    if (length(shape) == 1L) as.integer(n_rows) else c(as.integer(n_rows), 1L)
}

# The compressor name in the inner pipeline (skip the array->bytes step).
.shard_inner_compressor <- function(cfg) {
    for (c in cfg$codecs) {
        if (!c$name %in% c("bytes", "vlen-utf8")) return(c$name)
    }
    "none"
}

# Map a DAF compression symbol to (compressor name, blosc cname). The compressor
# name is what the read core's .zarr_inner_compressor() returns.
.SHARD_CODEC_TABLE <- list(
    blosc_zstd_bitshuffle = list(compressor = "blosc", cname = "zstd"),
    blosc_lz4_bitshuffle  = list(compressor = "blosc", cname = "lz4"),
    zstd                  = list(compressor = "zstd",  cname = NA_character_),
    gzip                  = list(compressor = "gzip",  cname = NA_character_)
)

# Compress one inner chunk's raw element bytes per the cfg's inner compressor.
# `typesize` is the element width (for blosc bitshuffle); `level` the clevel.
.shard_inner_compress <- function(raw_bytes, cfg, level, typesize = 1L) {
    comp <- .shard_inner_compressor(cfg)
    switch(comp,
        "none"  = raw_bytes,
        "gzip"  = memCompress(raw_bytes, type = "gzip"),
        "zstd"  = dafr_zstd_compress_cpp(raw_bytes, as.integer(level)),
        "blosc" = {
            cname <- cfg$.blosc_cname %||% "zstd"
            dafr_blosc_compress_cpp(raw_bytes, as.integer(level), cname,
                                    2L, as.integer(typesize))  # 2 = bitshuffle
        },
        stop(sprintf("shard_encode: unsupported compressor %s", sQuote(comp)),
             call. = FALSE))
}

# Split a flat (column-major) element vector into inner chunks in the grid's
# column-major linear order, fill-padding the final partial chunk per dim to the
# full inner shape. Returns a list of per-chunk element vectors.
.shard_split_chunks <- function(values, shape, inner) {
    grid <- list(outer = as.integer(shape), inner = as.integer(inner),
                 per_dim = as.integer(ceiling(shape / inner)))
    fill <- if (is.character(values)) ""
        else if (bit64::is.integer64(values)) bit64::as.integer64(0L)
        else as(0, typeof(values))
    if (length(shape) == 1L) {
        chunks <- vector("list", grid$per_dim[[1L]])
        for (k in seq_len(grid$per_dim[[1L]])) {
            lo <- (k - 1L) * inner[[1L]]
            valid <- min(inner[[1L]], shape[[1L]] - lo)
            piece <- values[(lo + 1L):(lo + valid)]
            if (valid < inner[[1L]]) piece <- c(piece, rep(fill, inner[[1L]] - valid))
            chunks[[k]] <- piece
        }
        return(chunks)
    }
    # 2-D: on-disk column-major buffer dim=[d0,d1]; inner [i0,i1]; C-order over
    # the grid (c1 fastest), matching .shard_decode_matrix, each inner chunk
    # emitted C-order over [i0,i1] to match local (a,b) at a*i1+b.
    d0 <- shape[[1L]]; d1 <- shape[[2L]]; i0 <- inner[[1L]]; i1 <- inner[[2L]]
    n0 <- grid$per_dim[[1L]]; n1 <- grid$per_dim[[2L]]
    buf <- values  # length d0*d1, on-disk C-order over [d0,d1]
    chunks <- vector("list", n0 * n1)
    lin <- 0L
    for (c0 in seq_len(n0)) for (c1 in seq_len(n1)) {  # C-order over the grid (c1 fastest)
        lin <- lin + 1L
        lo0 <- (c0 - 1L) * i0; lo1 <- (c1 - 1L) * i1
        v0 <- min(i0, d0 - lo0); v1 <- min(i1, d1 - lo1)
        piece <- rep(fill, i0 * i1)
        for (a in seq_len(v0)) {
            dst <- (a - 1L) * i1
            src <- (lo0 + a - 1L) * d1 + lo1
            piece[(dst + 1L):(dst + v1)] <- buf[(src + 1L):(src + v1)]
        }
        chunks[[lin]] <- piece
    }
    chunks
}

# Build the start-located shard index from per-chunk (offset, nbytes): N*16 LE
# bytes (offset:u64, nbytes:u64) then crc32c over them. Offsets are absolute
# into the final blob.
.shard_build_index <- function(offsets, nbytes) {
    con <- rawConnection(raw(0L), "wb"); on.exit(close(con))
    for (i in seq_along(offsets)) {
        .shard_write_u64(con, offsets[[i]]); .shard_write_u64(con, nbytes[[i]])
    }
    idx <- rawConnectionValue(con)
    crc <- dafr_crc32c_cpp(idx) %% 2^32
    c(idx, .shard_u32_raw(crc))
}

# Write a u64 (value < 2^53) as 8 LE bytes via lo/hi u32 halves.
.shard_write_u64 <- function(con, x) {
    lo <- x %% 2^32; hi <- (x - lo) / 2^32
    writeBin(.shard_u32_raw(lo), con); writeBin(.shard_u32_raw(hi), con)
}
.shard_u32_raw <- function(x) {
    as.raw(c(x %% 256, (x %/% 256) %% 256, (x %/% 65536) %% 256,
             (x %/% 16777216) %% 256))
}

# Assemble a PLAIN (no ZIP framing) shard blob: serialize -> chunk -> compress ->
# [index][chunk bytes...]. Used as the Phase-2 correctness pin; Task 6 swaps the
# layout for the ZIP dual-format one.
.shard_assemble_plain <- function(values, dtype, shape, inner, codec, level,
                                  cname = NULL) {
    cfg <- list(codecs = list(list(name = "bytes"),
                              list(name = .SHARD_CODEC_TABLE[[codec]]$compressor)),
                .blosc_cname = cname %||% .SHARD_CODEC_TABLE[[codec]]$cname)
    typesize <- if (identical(dtype, "string")) 1L else zarr_v3_size_for_dtype(dtype)
    chunks <- .shard_split_chunks(values, shape, inner)
    comp <- lapply(chunks, function(ch) {
        raw_bytes <- if (identical(dtype, "string"))
            zarr_v3_encode_strings(ch) else zarr_v3_encode_chunk(ch, dtype)
        .shard_inner_compress(raw_bytes, cfg, level, typesize)
    })
    n <- length(comp)
    nbytes <- as.numeric(vapply(comp, length, integer(1L)))
    idx_size <- as.numeric(n) * 16 + 4
    offsets <- idx_size + c(0, cumsum(nbytes)[-n])
    c(.shard_build_index(offsets, nbytes), do.call(c, comp))
}
