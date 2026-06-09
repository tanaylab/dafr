# R/zarr_v3.R
# Zarr v3 metadata + chunk I/O layer. Operates against any ZarrStore from
# R/zarr_store.R; does no I/O of its own beyond the Store interface.
# Replaces the deleted R/zarr_v2.R. On-disk format matches DataAxesFormats.jl
# 0.3.0 (Zarr v3): single zarr.json per node, lowercase dtype names, c/-prefixed
# chunk keys, daf marker as a root-group attribute, inline consolidated metadata.

#' @importFrom methods as
NULL

# ---- dtype mapping (R type <-> Zarr v3 data_type name) -------------------

# Zarr v3 data_type string for an R value. Always little-endian.
zarr_v3_dtype_for_r <- function(value) {
    if (is.character(value)) return("string")
    if (is.logical(value)) return("bool")
    if (inherits(value, "integer64")) return("int64")
    if (is.integer(value)) return("int32")
    if (is.numeric(value)) return("float64")
    stop(sprintf("zarr_v3: cannot map R type %s to a Zarr v3 data_type",
                 class(value)[1L]), call. = FALSE)
}

# R "kind" (readBin target) for a Zarr v3 data_type name.
zarr_v3_r_kind_for_dtype <- function(dtype) {
    switch(dtype,
        "float64" = "double", "float32" = "double",
        "int8" = "integer", "int16" = "integer", "int32" = "integer",
        "uint8" = "integer", "uint16" = "integer", "uint32" = "integer",
        "int64" = "integer64", "uint64" = "integer64",
        "bool" = "logical",
        "string" = "character",
        stop(sprintf("zarr_v3: unsupported data_type %s", sQuote(dtype)),
             call. = FALSE))
}

# Element byte size for a fixed-width v3 dtype; NA for variable-length (string).
zarr_v3_size_for_dtype <- function(dtype) {
    switch(dtype,
        "float64" = 8L, "float32" = 4L,
        "int64" = 8L, "uint64" = 8L,
        "int32" = 4L, "uint32" = 4L,
        "int16" = 2L, "uint16" = 2L,
        "int8" = 1L, "uint8" = 1L, "bool" = 1L,
        "string" = NA_integer_,
        stop(sprintf("zarr_v3: unsupported data_type %s", sQuote(dtype)),
             call. = FALSE))
}

# ---- chunk keys (v3 default encoding: prefix "c", separator "/") ---------

# Single-chunk key for an ndim-dimensional array: "c/0" (1-D), "c/0/0" (2-D).
zarr_v3_chunk_key <- function(ndim) {
    paste(c("c", rep("0", ndim)), collapse = "/")
}

# Full store key for the single chunk of the array at `base`.
zarr_v3_chunk_path <- function(base, ndim) {
    paste0(base, "/", zarr_v3_chunk_key(ndim))
}
