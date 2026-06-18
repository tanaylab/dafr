#' @include shard_encode.R
NULL

# R/shard_zip.R
# ZIP dual-format framing for packed shards (the byte-exact ZIP half of the
# "indexed+zipped" blob). The Zarr shard index (R/shard_encode.R) sits at byte 0;
# this module interleaves ZIP local file headers before each chunk, appends a
# fixed-width-named central directory + ZIP64 end-of-central-directory region, and
# (for STORED codecs) a `codec.json` entry recording the inner pipeline.
#
# Byte layout pinned against the Julia fixture
#   tests/testthat/fixtures/zpk/bz.daf.zarr/vectors/cell/score/c/0
# (blosc/zstd/bitshuffle, 1200 float64, inner 1024 -> 2 chunks). Key facts:
#   - LFH: version-needed=45, flags=0x0800 (UTF-8), method=0 (STORED), mtime=0,
#     mdate=0x0021, crc32 over the STORED bytes, csize/usize ZIP64 sentinels,
#     ZIP64 extra (tag 0x0001, size 16) = [usize:u64, csize:u64].
#   - For a STORED inner codec the ZIP entry data IS the compressor output (an
#     opaque STORED blob), so csize == usize == length(compressed) and the
#     crc32 is over those compressed bytes - NOT over the uncompressed plain
#     element bytes. The Zarr shard index offset for each chunk points at this
#     entry data (i.e. AFTER the chunk's local file header).
#   - CDE: version-made-by=0x031e, version-needed=45, flags=0x0800, method=0,
#     external attrs = 0o100644<<16, LFH-offset sentinel, ZIP64 extra (tag
#     0x0001, size 24) = [usize:u64, csize:u64, lfh_off:u64].
#   - Tail: ZIP64 EOCD (size 44, ver-made-by 0x031e, ver-needed 45) + ZIP64
#     locator + a sentinel base EOCD.
#   - codec.json STORED entry holds the full inner pipeline as JSON3-style JSON
#     (keys sorted: configuration before name; blosc config keys alphabetical).

.ZIP_LFH_SIG <- 0x04034b50; .ZIP_CDE_SIG <- 0x02014b50
.ZIP64_EOCD_SIG <- 0x06064b50; .ZIP64_LOC_SIG <- 0x07064b50; .ZIP_EOCD_SIG <- 0x06054b50

# ZIP compression method per inner codec: STORED (0) for blosc (and any codec
# whose output is an opaque blob the ZIP just stores), 93 for zstd, 8 for gzip.
.shard_zip_method <- function(codec) {
    switch(codec, zstd = 93L, gzip = 8L, 0L)  # blosc* / default -> STORED
}

# Fixed-width inner-chunk entry name ("c/<i>" 1-D, "c/<i>/<j>" 2-D). `lin` is the
# 1-based linear chunk index in .shard_split_chunks emission order, which is
# C-ORDER over per_dim (last grid axis fastest): for per_dim=[8,2] the order is
# c/0/0, c/0/1, c/1/0, c/1/1, ... Decode lin in C-order and emit the path
# components in grid-axis order (NOT reversed) so name(lin) lines up with both
# the shard index and the payload at lin. Pinned against the 2-D Julia fixture
# tests/testthat/fixtures/zpk/bz.daf.zarr/matrices/cell/gene/dense (on-disk
# shape [8,1200], inner [1,1024] -> per_dim [8,2]; 16 chunks c/0/0 .. c/7/1).
.shard_chunk_name <- function(lin, per_dim) {
    nd <- length(per_dim)
    idx0 <- lin - 1L
    coords <- integer(nd)
    for (d in rev(seq_len(nd))) {  # C-order decode: last axis fastest
        coords[d] <- idx0 %% per_dim[d]
        idx0 <- idx0 %/% per_dim[d]
    }
    widths <- pmax(1L, nchar(as.character(per_dim - 1L)))
    parts <- mapply(function(v, w) formatC(v, width = w, flag = "0"),
                    coords, widths, USE.NAMES = FALSE)
    paste0("c/", paste(parts, collapse = "/"))
}

.shard_u16_raw <- function(x) as.raw(c(x %% 256, (x %/% 256) %% 256))
.shard_u64_raw <- function(x) {
    lo <- x %% 2^32; hi <- (x - lo) / 2^32
    c(.shard_u32_raw(lo), .shard_u32_raw(hi))
}
.U32MAX <- as.raw(c(0xff, 0xff, 0xff, 0xff)); .U16MAX <- as.raw(c(0xff, 0xff))

# Local file header with a ZIP64 extra carrying the real usize/csize (the base
# record uses 0xFFFFFFFF sentinels). `csize`==`usize` for STORED entries; for a
# compressed method (zstd 93) csize=length(comp) and usize=length(plain).
.shard_zip_local_header <- function(name, crc, csize, usize, method = 0L) {
    nm <- charToRaw(name)
    zip64 <- c(.shard_u16_raw(1L), .shard_u16_raw(16L),
               .shard_u64_raw(usize), .shard_u64_raw(csize))
    c(.shard_u32_raw(.ZIP_LFH_SIG), .shard_u16_raw(45L), .shard_u16_raw(0x0800L),
      .shard_u16_raw(method), .shard_u16_raw(0L), .shard_u16_raw(0x0021L),
      .shard_u32_raw(crc), .U32MAX, .U32MAX,
      .shard_u16_raw(length(nm)), .shard_u16_raw(length(zip64)), nm, zip64)
}

# Central-directory entry; ZIP64 extra carries usize/csize/lfh_off (the base
# record uses sentinels). External attrs encode Unix mode 0o100644.
.shard_zip_central_entry <- function(name, crc, csize, usize, lfh_off, method = 0L) {
    nm <- charToRaw(name)
    zip64 <- c(.shard_u16_raw(1L), .shard_u16_raw(24L),
               .shard_u64_raw(usize), .shard_u64_raw(csize), .shard_u64_raw(lfh_off))
    c(.shard_u32_raw(.ZIP_CDE_SIG), .shard_u16_raw(0x031eL), .shard_u16_raw(45L),
      .shard_u16_raw(0x0800L), .shard_u16_raw(method), .shard_u16_raw(0L),
      .shard_u16_raw(0x0021L), .shard_u32_raw(crc), .U32MAX, .U32MAX,
      .shard_u16_raw(length(nm)), .shard_u16_raw(length(zip64)),
      .shard_u16_raw(0L), .shard_u16_raw(0L), .shard_u16_raw(0L),
      .shard_u32_raw(33188 * 65536),  # 0o100644 << 16 (Unix perms, u32)
      .U32MAX, nm, zip64)
}

# gzip dual-format chunk name: the 10-byte gzip header `1f 8b 08 01 <b0..b3> 02
# ff` used as the ZIP entry filename so that [name | deflate | trailer] is a
# valid gzip stream. b0..b3 = base64 of the big-endian 3-byte (k-1) chunk index
# ("AAAA" for chunk 0, "AAAB" for chunk 1, ...). Pinned against the gz fixture
# tests/testthat/fixtures/zpk/gz.daf.zarr/vectors/cell/score/c/0.
.shard_gzip_name <- function(k) {
    idx <- k - 1L
    three <- as.raw(c((idx %/% 65536L) %% 256L, (idx %/% 256L) %% 256L, idx %% 256L))
    b64 <- charToRaw(jsonlite::base64_enc(three))  # 4 chars (no padding for 3 bytes)
    c(as.raw(c(0x1f, 0x8b, 0x08, 0x01)), b64, as.raw(c(0x02, 0xff)))
}

# Local file header for the gzip dual-format entry (method 8). Unlike the STORED
# / zstd LFH (.shard_zip_local_header, ZIP64 extra + sentinel sizes), this LFH
# carries INLINE u32 csize/usize and extra-len 0, so the raw gzip-header name is
# immediately followed by the deflate payload (no ZIP64 extra interposed) - the
# Zarr index points at the name field, making [name | deflate | trailer] a
# contiguous gzip stream. version-needed=20 (no ZIP64 in the LFH). `name_raw` is
# the 10-byte gzip header; csize=length(deflate), usize=length(plain). Pinned:
# LFH = 50 4b 03 04 | 14 00 | 00 08 | 08 00 | 00 00 | 21 00 | crc | csize:u32 |
#       usize:u32 | name-len(10):u16 | extra-len(0):u16 | name_raw.
.shard_zip_local_header_raw <- function(name_raw, crc, csize, usize, method = 8L) {
    c(.shard_u32_raw(.ZIP_LFH_SIG), .shard_u16_raw(20L), .shard_u16_raw(0x0800L),
      .shard_u16_raw(method), .shard_u16_raw(0L), .shard_u16_raw(0x0021L),
      .shard_u32_raw(crc), .shard_u32_raw(csize), .shard_u32_raw(usize),
      .shard_u16_raw(length(name_raw)), .shard_u16_raw(0L), name_raw)
}

# Central-directory entry for a gzip dual-format chunk. Same ZIP64-extra layout
# as .shard_zip_central_entry (sentinel u32 sizes; ZIP64 extra carries
# usize/csize/lfh_off), but the name is the raw 10-byte gzip header (not a UTF-8
# path) and method=8. Pinned against the gz fixture CDE (ver-need=45,
# ZIP64 extra tag 1 size 24).
.shard_zip_central_entry_raw <- function(name_raw, crc, csize, usize, lfh_off,
                                         method = 8L) {
    zip64 <- c(.shard_u16_raw(1L), .shard_u16_raw(24L),
               .shard_u64_raw(usize), .shard_u64_raw(csize), .shard_u64_raw(lfh_off))
    c(.shard_u32_raw(.ZIP_CDE_SIG), .shard_u16_raw(0x031eL), .shard_u16_raw(45L),
      .shard_u16_raw(0x0800L), .shard_u16_raw(method), .shard_u16_raw(0L),
      .shard_u16_raw(0x0021L), .shard_u32_raw(crc), .U32MAX, .U32MAX,
      .shard_u16_raw(length(name_raw)), .shard_u16_raw(length(zip64)),
      .shard_u16_raw(0L), .shard_u16_raw(0L), .shard_u16_raw(0L),
      .shard_u32_raw(33188 * 65536),  # 0o100644 << 16 (Unix perms, u32)
      .U32MAX, name_raw, zip64)
}

# ZIP64 EOCD record + ZIP64 locator + sentinel base EOCD.
.shard_zip_eocd <- function(n_entries, cd_off, cd_size) {
    z64 <- c(.shard_u32_raw(.ZIP64_EOCD_SIG), .shard_u64_raw(44),
             .shard_u16_raw(0x031eL), .shard_u16_raw(45L),
             .shard_u32_raw(0), .shard_u32_raw(0),
             .shard_u64_raw(n_entries), .shard_u64_raw(n_entries),
             .shard_u64_raw(cd_size), .shard_u64_raw(cd_off))
    loc <- c(.shard_u32_raw(.ZIP64_LOC_SIG), .shard_u32_raw(0),
             .shard_u64_raw(cd_off + cd_size), .shard_u32_raw(1))
    eocd <- c(.shard_u32_raw(.ZIP_EOCD_SIG), .shard_u16_raw(0L),
              .shard_u16_raw(0L), .U16MAX, .U16MAX, .U32MAX, .U32MAX, .shard_u16_raw(0L))
    c(z64, loc, eocd)
}

# Full JSON3-style inner pipeline for the codec.json entry. Keys are emitted in
# the order Julia's JSON3 sorts them (configuration before name; blosc config
# keys alphabetical) so the bytes match the writer. jsonlite preserves the
# list's insertion order, so the order is encoded here.
.shard_codec_json <- function(cfg, typesize) {
    comp <- .shard_inner_compressor(cfg)
    bytes_codec <- list(configuration = list(endian = "little"), name = "bytes")
    if (identical(comp, "blosc")) {
        cname <- cfg$.blosc_cname %||% "zstd"
        clevel <- cfg$.clevel %||% 5L
        comp_codec <- list(
            configuration = list(blocksize = 0L, clevel = as.integer(clevel),
                                 cname = cname, shuffle = "bitshuffle",
                                 typesize = as.integer(typesize)),
            name = "blosc")
    } else if (identical(comp, "zstd")) {
        clevel <- cfg$.clevel %||% 5L
        comp_codec <- list(
            configuration = list(checksum = FALSE, level = as.integer(clevel)),
            name = "zstd")
    } else if (identical(comp, "gzip")) {
        clevel <- cfg$.clevel %||% 5L
        comp_codec <- list(configuration = list(level = as.integer(clevel)),
                           name = "gzip")
    } else {
        return(list(bytes_codec))
    }
    list(bytes_codec, comp_codec)
}

# Build the STORED codec.json ZIP entry (LFH + JSON body + central entry). The
# JSON is stored uncompressed, so csize == usize == length(json) and the crc32
# is over the JSON bytes.
.shard_codec_json_entry <- function(cfg, typesize, lfh_off) {
    pipeline <- .shard_codec_json(cfg, typesize)
    json <- charToRaw(as.character(jsonlite::toJSON(pipeline, auto_unbox = TRUE)))
    crc <- dafr_crc32_cpp(json) %% 2^32
    lfh <- .shard_zip_local_header("codec.json", crc, length(json), length(json))
    list(body = c(lfh, json),
         central = .shard_zip_central_entry("codec.json", crc, length(json),
                                            length(json), lfh_off))
}
