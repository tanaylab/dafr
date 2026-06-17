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
