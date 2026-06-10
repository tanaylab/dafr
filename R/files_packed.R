#' @include zarr_sharded.R
NULL

# R/files_packed.R
# Read-only support for packed (chunked + compressed) FilesDaf / ZipDaf /
# HttpDaf properties (DataAxesFormats.jl 0.3.0 "packed" format). A packed
# property is stored as a single dual-format shard file: `<name>.zip` for a
# dense vector/matrix, or `<name>.<component>.zip` for an independently-packed
# sparse component. The shard is simultaneously a ZIP archive and a Zarr v3
# ZEP-0002 sharded array with a start-located shard index at byte 0 (descriptor
# key "packed_format":"indexed+zipped"). dafr reads it through the Zarr index,
# reusing the shard-decode primitives in R/zarr_sharded.R; the ZIP framing is
# ignored. A "zipped"-only shard (ZIP central directory, no leading Zarr index)
# from a foreign producer is rejected with an actionable message.
#
# Axis convention: the FilesFormat descriptor's `chunk_shape` is in natural
# Julia [rows, cols] order and the on-disk element stream is column-major, so a
# matrix's inner-chunk grid is C-order over [n_row_chunks, n_col_chunks]
# (verified against real DAF 0.3.0 fixtures). This differs from ZarrDaf, whose
# zarr.json stores the reversed [n_cols, n_rows] shape; hence the matrix
# reassembly here is FilesFormat-specific rather than calling
# .shard_decode_matrix (which assumes the reversed convention).

# Map a Julia eltype name to a Zarr v3 dtype name. Lowercasing is exact:
# Float64->float64, Int64->int64, UInt32->uint32, Bool->bool, String->string.
.files_packed_dtype <- function(eltype) tolower(eltype)

# Map a FilesFormat `compression` value to the inner-codec name the shard-decode
# core understands. The blosc_* family (blosc_zstd_bitshuffle / blosc_lz4_*)
# all decode through one blosc1 call.
.files_packed_codec <- function(compression) {
    if (is.null(compression)) return("none")
    if (startsWith(compression, "blosc")) return("blosc")
    compression   # "gzip" / "zstd" pass through
}

# TRUE if a property/component descriptor is a packed shard.
.files_is_packed <- function(desc) {
    !is.null(desc$packed_format) ||
        (!is.null(desc$format) && identical(desc$format, "packed"))
}

# Reject a shard we cannot read through the start-located Zarr index (only the
# "indexed+zipped" dual-format shard carries it; a bare "zipped" archive does
# not).
.files_packed_require_indexed <- function(desc, name) {
    if (!identical(desc$packed_format, "indexed+zipped")) {
        pf <- if (is.null(desc$packed_format)) "NULL" else desc$packed_format
        stop(sprintf(paste0(
            "files_daf: packed property %s has packed_format=%s; dafr can read ",
            "only 'indexed+zipped' shards (re-save with the DataAxesFormats.jl ",
            "writer, or copy to flat components)"), sQuote(name), sQuote(pf)),
            call. = FALSE)
    }
}

# A Zarr-style pseudo-node for the shard-decode primitives, built from a
# FilesFormat packed descriptor. `shape` / `chunk_shape` are passed in the axis
# order the caller wants the grid + index parsed in.
.files_packed_node <- function(desc, shape, chunk_shape) {
    list(
        shape = as.list(as.integer(shape)),
        data_type = .files_packed_dtype(desc$eltype),
        codecs = list(list(
            name = "sharding_indexed",
            configuration = list(
                chunk_shape = as.list(as.integer(chunk_shape)),
                codecs = list(list(name = "bytes"),
                              list(name = .files_packed_codec(desc$compression))),
                index_location = "start"
            )
        ))
    )
}

# Read the whole shard file (the start-located Zarr index sits at byte 0).
.files_packed_read_shard <- function(zip_path) {
    readBin(zip_path, "raw", n = file.size(zip_path))
}

# Decode a packed 1-D array from its shard bytes to an R vector of length n.
# 1-D tiling is identical to the Zarr case, so reuse the core. (Bytes-based so
# HttpDaf, which fetches the whole .zip over HTTP, shares this path.)
.files_packed_decode_vector <- function(shard, desc, n, name) {
    .files_packed_require_indexed(desc, name)
    node <- .files_packed_node(desc, shape = n,
                               chunk_shape = unlist(desc$chunk_shape))
    .shard_decode_vector(shard, node)
}

# Read a packed 1-D array (dense vector or a sparse component) from its shard
# file to an R vector of length n.
.files_read_packed_vector <- function(zip_path, desc, n, name = zip_path) {
    .files_packed_decode_vector(.files_packed_read_shard(zip_path), desc, n, name)
}

# Decode a packed dense matrix from its shard bytes to an R nr x nc matrix
# (column-major). The inner chunk grid is C-order over the natural
# [n_row_chunks, n_col_chunks]; scatter each chunk's column-major slice into the
# column-major output buffer.
.files_packed_decode_matrix <- function(shard, desc, nr, nc, name) {
    .files_packed_require_indexed(desc, name)
    cs <- as.integer(unlist(desc$chunk_shape))            # [i_rows, i_cols]
    node <- .files_packed_node(desc, shape = c(nr, nc), chunk_shape = cs)
    cfg <- .zarr_sharding_config(node)
    idx <- .zarr_shard_index(shard, node, cfg)
    grid <- .zarr_shard_grid(node, cfg)
    i0 <- grid$inner[[1L]]; i1 <- grid$inner[[2L]]        # chunk rows, cols
    n0 <- grid$per_dim[[1L]]; n1 <- grid$per_dim[[2L]]    # row-chunks, col-chunks
    dtype <- node$data_type
    out <- .zarr_zero_vector(dtype, nr * nc)              # column-major buffer
    # The writer emits inner chunks column-major over the grid (row-block
    # fastest, matching the column-major data stream): all row-blocks of
    # column-chunk 1, then column-chunk 2, ... So iterate col-chunks outer,
    # row-chunks inner.
    lin <- 0L
    for (c1 in seq_len(n1)) for (c0 in seq_len(n0)) {     # column-major grid order
        lin <- lin + 1L
        lo0 <- (c0 - 1L) * i0; lo1 <- (c1 - 1L) * i1
        v0 <- min(i0, nr - lo0); v1 <- min(i1, nc - lo1)
        if (is.na(idx$offset[[lin]])) next                # empty chunk -> fill
        chunk <- .zarr_inner_decode(.zarr_shard_chunk_bytes(shard, idx, lin),
                                    cfg, n_elem = i0 * i1, dtype = dtype)
        # chunk is C-order over [i0, i1]: local (a, b) at a*i1 + b. Scatter to
        # the column-major buffer at column (lo1+b), rows (lo0+1 .. lo0+v0).
        rows <- lo0 + seq_len(v0)
        for (b in seq_len(v1)) {
            col <- lo1 + b - 1L
            out[col * nr + rows] <- chunk[(seq_len(v0) - 1L) * i1 + b]
        }
    }
    dim(out) <- c(nr, nc)
    out
}

# Read a packed dense matrix from its shard file to an R nr x nc matrix.
.files_read_packed_matrix <- function(zip_path, desc, nr, nc, name = zip_path) {
    .files_packed_decode_matrix(.files_packed_read_shard(zip_path), desc, nr, nc, name)
}
