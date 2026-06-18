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

# Rows per column-slab inner chunk: as many dim-1 rows as fit in the target
# chunk size, capped at the actual dim. Shared by the vector + both matrix paths.
.shard_slab_rows <- function(dtype, target_kb, n_rows) {
    as.integer(min(target_kb * 1024L %/% .shard_effective_sizeof(dtype), n_rows))
}

# Inner chunk shape: n_chunk_rows over dim1 (1-D vectors only).
.shard_inner_chunk_shape <- function(shape, dtype, target_kb) {
    as.integer(.shard_slab_rows(dtype, target_kb, shape[[1L]]))
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

# Plain (no ZIP framing) shard blob: [index][chunk bytes]. Retained as a
# ZIP-framing-independent regression harness for the index+chunk+compress core
# (see test-shard-encode.R); production writes use .shard_assemble.
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

# Assemble a ZIP dual-format shard blob ("indexed+zipped"): the start-located
# Zarr shard index at byte 0, then each inner chunk wrapped in a ZIP local file
# header (the index offset points at the entry data AFTER its header), then a
# STORED `codec.json` entry (STORED-method codecs only), then the ZIP central
# directory + ZIP64 EOCD region. The Zarr read path uses the index; the ZIP
# framing makes the blob a legal archive for generic tools.
#
# The ZIP CRC32 / "uncompressed size" convention is METHOD-DEPENDENT, pinned
# against the Julia fixtures:
#   - STORED (method 0, blosc): the ZIP entry data IS the compressor output
#     stored verbatim (an opaque blob), so usize == csize == length(comp) and
#     the crc32 is over those stored bytes.
#   - zstd (method 93): the ZIP entry data is a real zstd frame, so csize =
#     length(comp) and usize = length(plain) (they DIFFER), and the crc32 is
#     over the UNCOMPRESSED bytes (`plain`) per the ZIP spec.
# Returns the (crc32, usize) pair to stamp into the LFH / central entry; the
# entry CRC always describes what a ZIP reader treats as the entry's content.
.shard_zip_entry_meta <- function(method, plain, comp) {
    if (method == 0L) {            # STORED: usize == csize, crc over comp
        list(crc = dafr_crc32_cpp(comp) %% 2^32, usize = length(comp))
    } else {                       # method 93 (zstd) / 8 (gzip): crc over plain
        list(crc = dafr_crc32_cpp(plain) %% 2^32, usize = length(plain))
    }
}

# For a STORED inner codec (blosc) the ZIP entry is the compressor output stored
# verbatim: csize == usize == length(compressed) and the ZIP crc32 is over the
# compressed bytes. zstd uses ZIP method 93 (Task 7) - a real compressed frame,
# so usize = length(plain) and crc over plain; gzip method 8 is Task 8. The
# Zarr shard index offset/nbytes is method-independent: offset at the entry data
# (after the LFH), nbytes = length(comp).
.shard_assemble <- function(values, dtype, shape, inner, codec, level, cname = NULL) {
    cfg <- list(codecs = list(list(name = "bytes"),
                              list(name = .SHARD_CODEC_TABLE[[codec]]$compressor)),
                .blosc_cname = cname %||% .SHARD_CODEC_TABLE[[codec]]$cname,
                .clevel = as.integer(level))
    typesize <- if (identical(dtype, "string")) 1L else zarr_v3_size_for_dtype(dtype)
    per_dim <- as.integer(ceiling(shape / inner))
    chunks <- .shard_split_chunks(values, shape, inner)
    n <- length(chunks)
    idx_size <- as.numeric(n) * 16 + 4
    method <- .shard_zip_method(codec)
    bodies <- vector("list", n); offsets <- numeric(n); nbytes <- numeric(n)
    centrals <- vector("list", n); cursor <- idx_size
    for (k in seq_len(n)) {
        plain <- if (identical(dtype, "string"))
            zarr_v3_encode_strings(chunks[[k]]) else
            zarr_v3_encode_chunk(chunks[[k]], dtype)
        if (method == 8L) {
            # gzip dual-format: relocate the 10-byte gzip header into the ZIP
            # filename so the Zarr range [name | deflate | trailer] is a valid
            # gzip stream. The entry data is the RAW deflate payload; the index
            # points at the name field (inside the LFH), nbytes = whole gzip
            # stream. See R/shard_zip.R for the byte layout pinned against the gz
            # fixture. NOTE: R's memCompress(type="gzip") actually emits ZLIB
            # framing (2-byte 0x78 0x9c header + raw deflate + 4-byte Adler-32),
            # NOT a gzip stream - so strip 2 leading + 4 trailing bytes to get the
            # raw deflate (this matches Julia/CodecZlib's deflate byte-for-byte).
            full <- memCompress(plain, type = "gzip")
            deflate <- full[3:(length(full) - 4L)]
            name_raw <- .shard_gzip_name(k)
            crc <- dafr_crc32_cpp(plain) %% 2^32
            trailer <- c(.shard_u32_raw(crc),
                         .shard_u32_raw(length(plain) %% 2^32))
            lfh <- .shard_zip_local_header_raw(name_raw, crc, length(deflate),
                                               length(plain), 8L)
            name_off <- cursor + (length(lfh) - length(name_raw))
            offsets[[k]] <- name_off
            nbytes[[k]] <- length(name_raw) + length(deflate) + length(trailer)
            centrals[[k]] <- .shard_zip_central_entry_raw(name_raw, crc,
                length(deflate), length(plain), cursor, 8L)
            bodies[[k]] <- c(lfh, deflate, trailer)
            cursor <- cursor + length(lfh) + length(deflate) + length(trailer)
            next
        }
        comp <- .shard_inner_compress(plain, cfg, level, typesize)
        name <- .shard_chunk_name(k, per_dim)
        meta <- .shard_zip_entry_meta(method, plain, comp)
        crc <- meta$crc; usize <- meta$usize; csize <- length(comp)
        lfh <- .shard_zip_local_header(name, crc, csize, usize, method)
        data_off <- cursor + length(lfh)
        offsets[[k]] <- data_off; nbytes[[k]] <- length(comp)
        centrals[[k]] <- .shard_zip_central_entry(name, crc, csize, usize,
                                                  cursor, method)
        bodies[[k]] <- c(lfh, comp)
        cursor <- cursor + length(lfh) + length(comp)
    }
    if (method == 0L) {
        cj <- .shard_codec_json_entry(cfg, typesize, cursor)
        bodies <- c(bodies, list(cj$body))
        centrals <- c(centrals, list(cj$central)); cursor <- cursor + length(cj$body)
    }
    cd <- do.call(c, centrals); cd_off <- cursor
    eocd <- .shard_zip_eocd(length(centrals), cd_off, length(cd))
    index <- .shard_build_index(offsets, nbytes)
    c(index, do.call(c, bodies), cd, eocd)
}

# Write one array component to a zarr store, sharded if `packed` and over
# threshold, else flat. `shape`/`values` are in ON-DISK order (the caller
# reverses matrices). For a 2-D (matrix) component the on-disk shape is
# [ncol, nrow] and the inner chunk tiles the fast nrow dim as [1, nrow_chunk]
# (do NOT use .shard_inner_chunk_shape there - it chunks dim1). The flat branch
# reproduces exactly what .zarr_write_dense_array did before packed support.
# Strings are always written flat regardless of `packed` (matches FilesDaf and
# the documented contract that strings are never sharded).
.shard_write_zarr_component <- function(store, base, values, shape, dtype,
                                        packed) {
    if (identical(dtype, "string")) packed <- FALSE
    opts <- .packed_opts()
    shape <- as.integer(shape)
    if (length(shape) == 1L) {
        do_pack <- packed && .shard_should_pack(shape[[1L]], dtype, opts$target_kb)
        inner <- if (do_pack)
            .shard_inner_chunk_shape(shape, dtype, opts$target_kb) else NULL
    } else {  # 2-D matrix, on-disk reversed [ncol, nrow]; tile nrow (fast dim)
        nrow <- shape[[2L]]
        do_pack <- packed && .shard_should_pack(nrow, dtype, opts$target_kb)
        if (do_pack) {
            inner <- c(1L, .shard_slab_rows(dtype, opts$target_kb, nrow))
        } else {
            inner <- NULL
        }
    }
    if (do_pack) {
        .packed_validate_codec(opts$compression)
        zarr_v3_write_array(store, base, zarr_v3_sharded_array_meta(
            shape = shape, dtype = dtype, inner = inner,
            codec = opts$compression, level = opts$level))
        blob <- .shard_assemble(values, dtype, shape, inner,
                                opts$compression, opts$level)
        store_set_bytes(store, zarr_v3_chunk_path(base, length(shape)), blob)
    } else {
        zarr_v3_write_array(store, base,
                            zarr_v3_array_meta(shape = shape, dtype = dtype))
        chunk <- if (dtype == "string") zarr_v3_encode_strings(values) else
            zarr_v3_encode_chunk(values, dtype)
        store_set_bytes(store, zarr_v3_chunk_path(base, length(shape)), chunk)
    }
    invisible()
}
