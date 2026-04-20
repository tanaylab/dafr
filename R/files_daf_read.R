#' @include files_daf.R files_io.R format_api.R
NULL

.files_root <- function(daf) S7::prop(daf, "internal")$path

# ---- scalars: query ----
S7::method(format_has_scalar,
           list(FilesDaf, S7::class_character)) <- function(daf, name) {
  file.exists(.path_scalar(.files_root(daf), name))
}
S7::method(format_has_scalar,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, name) {
  file.exists(.path_scalar(.files_root(daf), name))
}

.files_get_scalar <- function(daf, name) {
  p <- .path_scalar(.files_root(daf), name)
  if (!file.exists(p)) {
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
  }
  .read_scalar_json(p)
}
S7::method(format_get_scalar,
           list(FilesDaf, S7::class_character)) <- function(daf, name) {
  .files_get_scalar(daf, name)
}
S7::method(format_get_scalar,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, name) {
  .files_get_scalar(daf, name)
}

.files_scalars_set <- function(daf) {
  dir <- file.path(.files_root(daf), "scalars")
  files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
  sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(format_scalars_set, FilesDaf)         <- function(daf) {
  .files_scalars_set(daf)
}
S7::method(format_scalars_set, FilesDafReadOnly) <- function(daf) {
  .files_scalars_set(daf)
}

# ---- axes: query ----

.files_axis_parsed <- function(daf, axis) {
  cache <- S7::prop(daf, "internal")$axes
  if (exists(axis, envir = cache, inherits = FALSE)) {
    return(get(axis, envir = cache, inherits = FALSE))
  }
  p <- .path_axis(.files_root(daf), axis)
  if (!file.exists(p)) return(NULL)
  entries <- readLines(p, encoding = "UTF-8", warn = FALSE)
  if (anyNA(entries) || any(!nzchar(entries))) {
    stop(sprintf("files_daf: axis %s contains empty entries", sQuote(axis)),
         call. = FALSE)
  }
  if (anyDuplicated(entries)) {
    dup <- entries[duplicated(entries)][1L]
    stop(sprintf("files_daf: axis %s has duplicate entry %s",
                 sQuote(axis), sQuote(dup)), call. = FALSE)
  }
  dict <- new.env(parent = emptyenv(), size = length(entries))
  for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
  parsed <- list(entries = entries, dict = dict)
  assign(axis, parsed, envir = cache)
  parsed
}

.files_has_axis <- function(daf, axis) {
  file.exists(.path_axis(.files_root(daf), axis))
}

S7::method(format_has_axis,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  .files_has_axis(daf, axis)
}
S7::method(format_has_axis,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  .files_has_axis(daf, axis)
}

.files_axes_set <- function(daf) {
  dir <- file.path(.files_root(daf), "axes")
  files <- list.files(dir, pattern = "\\.txt$", full.names = FALSE)
  sort(sub("\\.txt$", "", files), method = "radix")
}
S7::method(format_axes_set, FilesDaf)         <- function(daf) .files_axes_set(daf)
S7::method(format_axes_set, FilesDafReadOnly) <- function(daf) .files_axes_set(daf)

.files_axis_require <- function(daf, axis) {
  parsed <- .files_axis_parsed(daf, axis)
  if (is.null(parsed)) {
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
  }
  parsed
}

S7::method(format_axis_length,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  length(.files_axis_require(daf, axis)$entries)
}
S7::method(format_axis_length,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  length(.files_axis_require(daf, axis)$entries)
}

S7::method(format_axis_array,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  .files_axis_require(daf, axis)$entries
}
S7::method(format_axis_array,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  .files_axis_require(daf, axis)$entries
}

S7::method(format_axis_dict,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  .files_axis_require(daf, axis)$dict
}
S7::method(format_axis_dict,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  .files_axis_require(daf, axis)$dict
}

# ---- vectors: query ----

.files_vector_desc_path <- function(root, axis, name) {
  file.path(.path_vector_dir(root, axis), paste0(name, ".json"))
}

.files_has_vector <- function(daf, axis, name) {
  if (!format_has_axis(daf, axis)) return(FALSE)
  file.exists(.files_vector_desc_path(.files_root(daf), axis, name))
}
S7::method(format_has_vector,
           list(FilesDaf, S7::class_character, S7::class_character)) <- function(daf, axis, name) {
  .files_has_vector(daf, axis, name)
}
S7::method(format_has_vector,
           list(FilesDafReadOnly, S7::class_character, S7::class_character)) <- function(daf, axis, name) {
  .files_has_vector(daf, axis, name)
}

.files_vectors_set <- function(daf, axis) {
  if (!format_has_axis(daf, axis)) return(character(0L))
  dir <- .path_vector_dir(.files_root(daf), axis)
  if (!dir.exists(dir)) return(character(0L))
  files <- list.files(dir, pattern = "\\.json$", full.names = FALSE)
  sort(sub("\\.json$", "", files), method = "radix")
}
S7::method(format_vectors_set,
           list(FilesDaf, S7::class_character)) <- function(daf, axis) {
  .files_vectors_set(daf, axis)
}
S7::method(format_vectors_set,
           list(FilesDafReadOnly, S7::class_character)) <- function(daf, axis) {
  .files_vectors_set(daf, axis)
}

# ---- vectors: read ----

.files_get_vector_dense_string <- function(daf, axis, name, n) {
  dir <- .path_vector_dir(.files_root(daf), axis)
  txt <- file.path(dir, paste0(name, ".txt"))
  if (!file.exists(txt)) {
    stop(sprintf("files_daf: missing payload %s", sQuote(txt)), call. = FALSE)
  }
  out <- readLines(txt, encoding = "UTF-8", warn = FALSE)
  if (length(out) != n) {
    stop(sprintf("files_daf: string vector %s has %d entries (expected %d)",
                 sQuote(name), length(out), n), call. = FALSE)
  }
  out
}

.files_get_vector_dense <- function(daf, axis, name, desc, n) {
  root <- .files_root(daf)
  dir  <- .path_vector_dir(root, axis)
  elt  <- desc$eltype
  if (elt == "String") {
    return(.files_get_vector_dense_string(daf, axis, name, n))
  }
  data_path <- file.path(dir, paste0(name, ".data"))
  if (!file.exists(data_path)) {
    stop(sprintf("files_daf: missing payload %s", sQuote(data_path)),
         call. = FALSE)
  }
  expected <- n * .dtype_size(elt)
  actual   <- file.size(data_path)
  if (actual < expected) {
    stop(sprintf("files_daf: vector %s payload truncated (%d < %d bytes)",
                 sQuote(name), actual, expected), call. = FALSE)
  }
  use_mmap <- isTRUE(dafr_opt("dafr.mmap"))
  if (use_mmap && elt == "Float64") {
    return(mmap_real(data_path, n))
  }
  if (use_mmap && elt == "Int32") {
    return(mmap_int(data_path, n))
  }
  # Fallback: eager read for all other types and when mmap disabled.
  .read_bin_dense(data_path, n, elt)
}

.files_get_vector_sparse <- function(daf, axis, name, desc, n) {
  vdir <- .path_vector_dir(.files_root(daf), axis)
  ind_path <- file.path(vdir, paste0(name, ".nzind"))
  if (!file.exists(ind_path)) {
    stop(sprintf("files_daf: sparse vector %s missing .nzind", sQuote(name)),
         call. = FALSE)
  }
  indtype <- desc$indtype %||% "UInt32"
  nnz <- file.size(ind_path) %/% .dtype_size(indtype)
  idx <- .read_bin_dense(ind_path, nnz, indtype)
  if (desc$eltype == "Bool") {
    val_path <- file.path(vdir, paste0(name, ".nzval"))
    vals <- if (file.exists(val_path)) {
      as.logical(.read_bin_dense(val_path, nnz, "Bool"))
    } else {
      rep(TRUE, nnz)
    }
    out <- logical(n)
    out[as.integer(idx)] <- vals
    return(out)
  }
  if (desc$eltype == "String") {
    nztxt <- file.path(vdir, paste0(name, ".nztxt"))
    vals <- readLines(nztxt, encoding = "UTF-8", warn = FALSE)
    if (length(vals) != nnz) {
      stop(sprintf("files_daf: sparse string vector %s .nztxt has %d lines (expected %d)",
                   sQuote(name), length(vals), nnz), call. = FALSE)
    }
    out <- rep("", n)
    out[as.integer(idx)] <- vals
    return(out)
  }
  val_path <- file.path(vdir, paste0(name, ".nzval"))
  if (!file.exists(val_path)) {
    stop(sprintf("files_daf: sparse vector %s missing .nzval for non-Bool eltype",
                 sQuote(name)), call. = FALSE)
  }
  vals <- .read_bin_dense(val_path, nnz, desc$eltype)
  out <- if (desc$eltype %in% c("Int8","Int16","Int32","UInt8","UInt16","UInt32")) {
    integer(n)
  } else if (desc$eltype %in% c("Int64","UInt64")) {
    bit64::as.integer64(integer(n))
  } else {
    numeric(n)
  }
  out[as.integer(idx)] <- vals
  out
}

.files_get_vector_impl <- function(daf, axis, name) {
  root <- .files_root(daf)
  desc_path <- .files_vector_desc_path(root, axis, name)
  if (!file.exists(desc_path)) {
    stop(sprintf("vector %s does not exist on axis %s",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  desc <- .read_descriptor(desc_path)
  n <- format_axis_length(daf, axis)
  if (desc$format == "dense") {
    return(.files_get_vector_dense(daf, axis, name, desc, n))
  }
  if (desc$format == "sparse") {
    return(.files_get_vector_sparse(daf, axis, name, desc, n))
  }
  stop(sprintf("files_daf: unsupported vector format %s", desc$format),
       call. = FALSE)
}

S7::method(format_get_vector,
           list(FilesDaf, S7::class_character, S7::class_character)) <- function(daf, axis, name) {
  .files_get_vector_impl(daf, axis, name)
}
S7::method(format_get_vector,
           list(FilesDafReadOnly, S7::class_character, S7::class_character)) <- function(daf, axis, name) {
  .files_get_vector_impl(daf, axis, name)
}
