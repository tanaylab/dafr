#' @include zarr_v3.R
NULL

# R/zarr_sharded.R
# Read-only Zarr v3 packed/sharded arrays (ZEP-0002 "sharding_indexed" codec,
# written by DataAxesFormats.jl `packed=true`). Pure R except for inner-chunk
# decompression: gzip via base-R memDecompress (no dependency); blosc and zstd
# via the optional configure-gated C wrappers in src/shard_codecs.cpp. Flat
# arrays never reach here - R/zarr_format.R routes here only when an array
# node's first codec is the sharding codec (.zarr_is_sharded()).
#
# On-disk shard layout (verified against real DAF 0.3.0 fixtures):
#   - The shard blob lives at the single outer-chunk key (c/0 for 1-D, c/0/0
#     for 2-D); the outer chunk grid equals the full shape.
#   - It is a "dual-format" blob (also a ZIP archive), but the Zarr read path
#     ignores the ZIP framing: a start-located shard index maps each inner
#     chunk to (offset:u64 LE, nbytes:u64 LE) within the blob, pointing
#     directly at the inner-codec output. The index is N*16 bytes followed by a
#     4-byte crc32c over those bytes.
#   - Each inner chunk decodes via the inner codec; partial last chunks are
#     fill-padded to the inner shape before compression.

# ---- detection + config --------------------------------------------------

# TRUE if the array node is a sharded (packed) array.
.zarr_is_sharded <- function(node) {
    cs <- node$codecs
    length(cs) >= 1L && identical(cs[[1L]]$name, "sharding_indexed")
}

# The sharding codec configuration list (chunk_shape, codecs, index_*, ...).
.zarr_sharding_config <- function(node) node$codecs[[1L]]$configuration

# Inner-chunk grid: per-dimension chunk counts (C-order) and total N.
.zarr_shard_grid <- function(node, cfg) {
    outer <- as.integer(unlist(node$shape))
    inner <- as.integer(unlist(cfg$chunk_shape))
    per_dim <- as.integer(ceiling(outer / inner))
    list(outer = outer, inner = inner, per_dim = per_dim, n = prod(per_dim))
}

# ---- shard index ---------------------------------------------------------

# Parse the start-located shard index: N*(offset:u64, nbytes:u64) LE, then a
# 4-byte crc32c over the N*16 index bytes. Warns (does not stop) on crc
# mismatch - the offsets remain usable. Returns data.frame(offset, nbytes);
# empty-chunk sentinels (0xFFFF..FF) become NA.
.zarr_shard_index <- function(shard, node, cfg) {
    grid <- .zarr_shard_grid(node, cfg)
    n <- grid$n
    idx_bytes <- n * 16L
    if (length(shard) < idx_bytes + 4L) {
        stop("zarr_sharded: shard shorter than its start index", call. = FALSE)
    }
    have <- as.numeric(dafr_crc32c_cpp(shard[seq_len(idx_bytes)])) %% 2^32
    want <- readBin(shard[idx_bytes + 1:4], "integer", 1L, 4L,
                    endian = "little") %% 2^32
    if (!isTRUE(all.equal(have, want))) {
        warning(sprintf(paste0("zarr_sharded: shard index crc32c mismatch ",
                               "(%.0f != %.0f); reading anyway"), have, want),
                call. = FALSE)
    }
    # R has no native u64: read u32 low/high pairs and recombine in doubles
    # (shard offsets/sizes are < 2^53 here, so this is exact).
    con <- rawConnection(shard[seq_len(idx_bytes)], "rb")
    on.exit(close(con))
    u32 <- readBin(con, "integer", n * 4L, 4L, endian = "little")
    u32 <- ifelse(u32 < 0L, as.double(u32) + 2^32, as.double(u32))
    u64 <- u32[c(TRUE, FALSE)] + u32[c(FALSE, TRUE)] * 2^32   # n*2 values
    off <- u64[c(TRUE, FALSE)]
    nb  <- u64[c(FALSE, TRUE)]
    is_empty <- off >= 2^63    # 0xFFFF..FF sentinel -> all-fill chunk
    off[is_empty] <- NA_real_
    nb[is_empty]  <- NA_real_
    data.frame(offset = off, nbytes = nb)
}

# ---- inner-codec decode --------------------------------------------------

# Name of the compression codec in the inner pipeline. The array->bytes
# serialization step (`bytes` for fixed-width, `vlen-utf8` for strings) is
# skipped; "none" if there is no compressor.
.zarr_inner_compressor <- function(cfg) {
    for (c in cfg$codecs) {
        if (!c$name %in% c("bytes", "vlen-utf8")) return(c$name)
    }
    "none"
}

# Decompress one inner chunk's raw bytes. `out_nbytes` is the known
# uncompressed size for fixed-width dtypes; 0 for variable-length (string)
# chunks, where blosc/zstd read the size from their own header.
.zarr_inner_decompress <- function(raw_bytes, cfg, out_nbytes) {
    comp <- .zarr_inner_compressor(cfg)
    switch(comp,
        "none"  = raw_bytes,
        "gzip"  = memDecompress(raw_bytes, type = "gzip"),
        "zstd"  = dafr_zstd_decompress_cpp(raw_bytes, out_nbytes),
        "blosc" = dafr_blosc_decompress_cpp(raw_bytes, out_nbytes),
        stop(sprintf("zarr_sharded: unsupported inner compressor %s",
                     sQuote(comp)), call. = FALSE))
}

# Decode one inner chunk's raw bytes to a vector of dtype. Fixed-width dtypes
# yield n_elem values; strings yield however many the chunk's vlen-utf8 block
# encodes (the count is in the block header, padded chunks included).
.zarr_inner_decode <- function(raw_bytes, cfg, n_elem, dtype) {
    if (identical(dtype, "string")) {
        plain <- .zarr_inner_decompress(raw_bytes, cfg, out_nbytes = 0)
        return(zarr_v3_decode_strings(plain))
    }
    out_nbytes <- n_elem * zarr_v3_size_for_dtype(dtype)
    plain <- .zarr_inner_decompress(raw_bytes, cfg, out_nbytes)
    zarr_v3_decode_chunk(plain, dtype, n = n_elem)
}

# An n-length zero/empty vector of the R type for a v3 dtype (handles
# integer64, which base vector() cannot allocate).
.zarr_zero_vector <- function(dtype, n) {
    k <- zarr_v3_r_kind_for_dtype(dtype)
    if (k == "integer64") return(bit64::as.integer64(rep(0L, n)))
    if (k == "character") return(character(n))
    vector(k, n)   # logical/integer/double -> FALSE/0L/0
}

# Slice the inner-chunk bytes for index entry `i` out of the shard blob
# (offset is 0-based; R slice is 1-based).
.zarr_shard_chunk_bytes <- function(shard, idx, i) {
    shard[idx$offset[[i]] + seq_len(idx$nbytes[[i]])]
}

# ---- reassembly ----------------------------------------------------------
#
# The reassembly core takes the raw shard bytes plus a node descriptor and is
# storage-agnostic: ZarrDaf passes bytes fetched from its store at the single
# outer-chunk key; FilesDaf/ZipDaf pass the whole dual-format `.zip` payload
# (the start-located Zarr shard index sits at byte 0 of that file). The thin
# `.zarr_read_sharded_*` wrappers below fetch the bytes from a zarr store; the
# `.shard_decode_*` cores do the actual index parse + inner decode + tiling.

# Decode a sharded 1-D array (dense vector or sparse component) from shard
# bytes to an R vector.
.shard_decode_vector <- function(shard, node) {
    cfg <- .zarr_sharding_config(node)
    idx <- .zarr_shard_index(shard, node, cfg)
    grid <- .zarr_shard_grid(node, cfg)
    len <- grid$outer[[1L]]; inner <- grid$inner[[1L]]
    dtype <- node$data_type
    out <- .zarr_zero_vector(dtype, len)
    for (k in seq_len(grid$per_dim[[1L]])) {
        lo <- (k - 1L) * inner
        valid <- min(inner, len - lo)
        if (is.na(idx$offset[[k]])) next            # empty chunk -> fill (0)
        chunk <- .zarr_inner_decode(.zarr_shard_chunk_bytes(shard, idx, k),
                                    cfg, n_elem = inner, dtype = dtype)
        out[(lo + 1L):(lo + valid)] <- chunk[seq_len(valid)]
    }
    out
}

# Read a sharded 1-D array (dense vector or a sparse component) to an R vector.
.zarr_read_sharded_vector <- function(store, base, node) {
    shard <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    if (is.null(shard)) {
        stop(sprintf("sharded array %s missing chunk", sQuote(base)),
             call. = FALSE)
    }
    .shard_decode_vector(shard, node)
}

# Decode a sharded 2-D dense array from shard bytes to an R matrix. The node's
# `shape` is in on-disk (C-order) axis order; the matrix is filled column-major
# so that C-order over `shape` equals column-major over (nr, nc). For ZarrDaf
# the on-disk shape is reversed [n_cols, n_rows]; FilesDaf presents the same
# reversed convention (see R/files_packed.R) so this core is shared verbatim.
.shard_decode_matrix <- function(shard, node) {
    cfg <- .zarr_sharding_config(node)
    idx <- .zarr_shard_index(shard, node, cfg)
    grid <- .zarr_shard_grid(node, cfg)
    d0 <- grid$outer[[1L]]; d1 <- grid$outer[[2L]]   # on-disk dims (C-order)
    i0 <- grid$inner[[1L]]; i1 <- grid$inner[[2L]]
    n0 <- grid$per_dim[[1L]]; n1 <- grid$per_dim[[2L]]
    dtype <- node$data_type
    flat <- .zarr_zero_vector(dtype, d0 * d1)        # on-disk C-order buffer
    lin <- 0L
    for (c0 in seq_len(n0)) for (c1 in seq_len(n1)) {  # C-order over the grid
        lin <- lin + 1L
        lo0 <- (c0 - 1L) * i0; lo1 <- (c1 - 1L) * i1
        v0 <- min(i0, d0 - lo0); v1 <- min(i1, d1 - lo1)
        if (is.na(idx$offset[[lin]])) next
        chunk <- .zarr_inner_decode(.zarr_shard_chunk_bytes(shard, idx, lin),
                                    cfg, n_elem = i0 * i1, dtype = dtype)
        # chunk is C-order over [i0, i1]: local (a, b) at a*i1 + b.
        for (a in seq_len(v0)) {
            dst <- (lo0 + a - 1L) * d1 + lo1
            src <- (a - 1L) * i1
            flat[(dst + 1L):(dst + v1)] <- chunk[(src + 1L):(src + v1)]
        }
    }
    dim(flat) <- c(d1, d0)   # nr=d1, nc=d0; column-major == on-disk C-order
    flat
}

# Read a sharded 2-D dense array to an R matrix (ZarrDaf store wrapper).
.zarr_read_sharded_matrix <- function(store, base, node) {
    shard <- store_get_bytes(store, zarr_v3_chunk_path(base, 2L))
    if (is.null(shard)) {
        stop(sprintf("sharded matrix %s missing chunk", sQuote(base)),
             call. = FALSE)
    }
    .shard_decode_matrix(shard, node)
}

# Decode a 1-D component array (sharded or flat) to an R vector. Sparse
# nzind/nzval/colptr/rowval shard independently above the 8 KiB threshold.
.zarr_read_component_vector <- function(store, base, node) {
    if (.zarr_is_sharded(node)) {
        return(.zarr_read_sharded_vector(store, base, node))
    }
    n <- as.integer(node$shape[[1L]])
    if (n == 0L) return(.zarr_zero_vector(node$data_type, 0L))   # no chunk file
    chunk <- store_get_bytes(store, zarr_v3_chunk_path(base, 1L))
    zarr_v3_decode_chunk(chunk, node$data_type, n = n)
}
