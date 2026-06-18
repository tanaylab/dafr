#' @include shard_encode.R
NULL

# R/packed_opts.R
# Resolve packed-write tuning from R options (mirrors Julia's DAF_PACKED_*
# module globals) and validate that the requested codec's optional library is
# compiled in.

.packed_opts <- function() {
    list(
        compression = getOption("dafr.packed_compression", "blosc_zstd_bitshuffle"),
        level       = as.integer(getOption("dafr.packed_compression_level", 5L)),
        target_kb   = as.integer(getOption("dafr.packed_target_chunk_kb", 8L))
    )
}

# Stop with an actionable message if `codec` needs an optional lib not built in.
.packed_validate_codec <- function(codec) {
    if (!codec %in% names(.SHARD_CODEC_TABLE)) {
        stop(sprintf("dafr packed write: unknown compression %s (supported: %s)",
                     sQuote(codec),
                     paste(names(.SHARD_CODEC_TABLE), collapse = ", ")),
             call. = FALSE)
    }
    comp <- .SHARD_CODEC_TABLE[[codec]]$compressor
    if (comp == "zstd" && !dafr_have_zstd_cpp()) {
        stop("dafr packed write: compression 'zstd' requires libzstd; install it ",
             "and reinstall dafr, or use options(dafr.packed_compression='gzip').",
             call. = FALSE)
    }
    if (comp == "blosc" && !dafr_have_blosc_cpp()) {
        stop("dafr packed write: blosc compression requires c-blosc; install it ",
             "and reinstall dafr, or use options(dafr.packed_compression='gzip').",
             call. = FALSE)
    }
    invisible(TRUE)
}
