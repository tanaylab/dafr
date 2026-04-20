# Low-level I/O helpers shared by FilesDaf read/write methods.
# - dtype table maps R-side types <-> on-disk Julia-style type strings.
# - path helpers translate (root, axis, name) triples to absolute paths.

.DTYPE_SIZES <- c(
  Bool    = 1L,
  Int8    = 1L,  UInt8  = 1L,
  Int16   = 2L,  UInt16 = 2L,
  Int32   = 4L,  UInt32 = 4L,
  Int64   = 8L,  UInt64 = 8L,
  Float32 = 4L, Float64 = 8L
)

.dtype_canonical <- function(x) {
  stopifnot(is.character(x), length(x) == 1L, !is.na(x))
  lower <- tolower(x)
  mapping <- c(
    bool    = "Bool",
    int8    = "Int8",  uint8   = "UInt8",
    int16   = "Int16", uint16  = "UInt16",
    int32   = "Int32", uint32  = "UInt32",
    int64   = "Int64", uint64  = "UInt64",
    float32 = "Float32", float64 = "Float64",
    string  = "String",
    int     = "Int64"
  )
  out <- mapping[lower]
  if (is.na(out)) {
    stop(sprintf("files_daf: unsupported type %s", sQuote(x)), call. = FALSE)
  }
  unname(out)
}

.dtype_size <- function(dtype) {
  dtype <- .dtype_canonical(dtype)
  if (dtype == "String") {
    stop("files_daf: String has no fixed byte size", call. = FALSE)
  }
  unname(.DTYPE_SIZES[[dtype]])
}

.dtype_for_r_vector <- function(v) {
  if (is.logical(v))             return("Bool")
  if (inherits(v, "integer64"))  return("Int64")
  if (is.integer(v))             return("Int32")
  if (is.double(v))              return("Float64")
  if (is.character(v))           return("String")
  stop(sprintf("files_daf: cannot map R type %s to on-disk dtype",
               sQuote(typeof(v))), call. = FALSE)
}

.path_scalar     <- function(root, name)   file.path(root, "scalars", paste0(name, ".json"))
.path_axis       <- function(root, axis)   file.path(root, "axes",    paste0(axis, ".txt"))
.path_vector_dir <- function(root, axis)   file.path(root, "vectors", axis)
.path_matrix_dir <- function(root, rows_axis, columns_axis) {
  file.path(root, "matrices", rows_axis, columns_axis)
}
