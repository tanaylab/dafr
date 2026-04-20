#' @include files_daf.R files_io.R utils.R format_api.R
NULL

S7::method(format_set_scalar,
           list(FilesDaf, S7::class_character, S7::class_any, S7::class_logical)) <- function(daf, name, value, overwrite) {
  .assert_scalar_value(name, value)
  p <- .path_scalar(.files_root(daf), name)
  if (file.exists(p) && !overwrite) {
    stop(sprintf("scalar %s already exists; use overwrite = TRUE",
                 sQuote(name)), call. = FALSE)
  }
  .write_scalar_json(p, value)
  invisible()
}

S7::method(format_delete_scalar,
           list(FilesDaf, S7::class_character, S7::class_logical)) <- function(daf, name, must_exist) {
  p <- .path_scalar(.files_root(daf), name)
  if (!file.exists(p)) {
    if (must_exist) {
      stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
    }
    return(invisible())
  }
  unlink(p, force = TRUE)
  invisible()
}

.write_axis_file <- function(path, entries) {
  con <- file(path, open = "wb", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(entries, con, useBytes = FALSE)
}

S7::method(format_add_axis,
           list(FilesDaf, S7::class_character, S7::class_character)) <- function(daf, axis, entries) {
  if (!is.character(entries)) {
    stop(sprintf("axis %s entries must be a character vector", sQuote(axis)), call. = FALSE)
  }
  if (anyNA(entries)) {
    stop(sprintf("axis %s entries contain NA", sQuote(axis)), call. = FALSE)
  }
  if (any(!nzchar(entries))) {
    stop(sprintf("axis %s entries contain empty strings", sQuote(axis)), call. = FALSE)
  }
  if (any(grepl("[\n\r]", entries))) {
    stop(sprintf("axis %s entries contain newline characters", sQuote(axis)), call. = FALSE)
  }
  if (anyDuplicated(entries)) {
    dup <- entries[duplicated(entries)][1L]
    stop(sprintf("axis %s has duplicate entry %s",
                 sQuote(axis), sQuote(dup)), call. = FALSE)
  }
  if (length(entries) > .Machine$integer.max) {
    stop(sprintf("axis %s length exceeds R integer capacity", sQuote(axis)),
         call. = FALSE)
  }
  root <- .files_root(daf)
  p <- .path_axis(root, axis)
  if (file.exists(p)) {
    stop(sprintf("axis %s already exists", sQuote(axis)), call. = FALSE)
  }
  .write_axis_file(p, entries)
  dict <- new.env(parent = emptyenv(), size = length(entries))
  for (i in seq_along(entries)) assign(entries[[i]], i, envir = dict)
  assign(axis, list(entries = entries, dict = dict),
         envir = S7::prop(daf, "internal")$axes)
  bump_axis_counter(daf, axis)
  invisible()
}

S7::method(format_delete_axis,
           list(FilesDaf, S7::class_character, S7::class_logical)) <- function(daf, axis, must_exist) {
  root <- .files_root(daf)
  p <- .path_axis(root, axis)
  if (!file.exists(p)) {
    if (must_exist) {
      stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
    }
    return(invisible())
  }
  unlink(p, force = TRUE)
  # Cascade: drop vectors/<axis> and any matrices involving <axis>.
  vdir <- .path_vector_dir(root, axis)
  if (dir.exists(vdir)) unlink(vdir, recursive = TRUE, force = TRUE)
  mroot <- file.path(root, "matrices")
  mrow <- file.path(mroot, axis)
  if (dir.exists(mrow)) unlink(mrow, recursive = TRUE, force = TRUE)
  # Remove <axis> used as columns_axis under other rows_axis buckets.
  for (rows in list.files(mroot, full.names = FALSE)) {
    mcol <- file.path(mroot, rows, axis)
    if (dir.exists(mcol)) unlink(mcol, recursive = TRUE, force = TRUE)
  }
  cache <- S7::prop(daf, "internal")$axes
  if (exists(axis, envir = cache, inherits = FALSE)) {
    rm(list = axis, envir = cache)
  }
  bump_axis_counter(daf, axis)
  invisible()
}

.files_write_vector_dense <- function(vdir, name, vec) {
  dtype <- .dtype_for_r_vector(vec)
  if (dtype == "String") {
    con <- file(file.path(vdir, paste0(name, ".txt")), open = "wb",
                encoding = "UTF-8")
    writeLines(vec, con, useBytes = FALSE)
    close(con)
  } else {
    .write_bin_dense(file.path(vdir, paste0(name, ".data")), vec, dtype)
  }
  .write_descriptor_dense(file.path(vdir, paste0(name, ".json")), dtype)
  invisible()
}

.files_vector_unlink_payload <- function(vdir, name) {
  for (ext in c(".data", ".txt", ".nzind", ".nzval", ".nztxt")) {
    p <- file.path(vdir, paste0(name, ext))
    if (file.exists(p)) unlink(p, force = TRUE)
  }
}

.files_write_vector_sparse_numeric <- function(vdir, name, nzind, nzval,
                                               eltype, indtype) {
  .write_bin_dense(file.path(vdir, paste0(name, ".nzind")),
                   as.integer(nzind), indtype)
  if (eltype == "Bool") {
    if (!all(nzval)) {
      .write_bin_dense(file.path(vdir, paste0(name, ".nzval")),
                       as.logical(nzval), "Bool")
    }
  } else {
    .write_bin_dense(file.path(vdir, paste0(name, ".nzval")), nzval, eltype)
  }
  .write_descriptor_sparse(file.path(vdir, paste0(name, ".json")),
                           dtype = eltype, indtype = indtype)
  invisible()
}

.files_write_vector_sparse_string <- function(vdir, name, nzind, nzval, indtype) {
  .write_bin_dense(file.path(vdir, paste0(name, ".nzind")),
                   as.integer(nzind), indtype)
  con <- file(file.path(vdir, paste0(name, ".nztxt")), open = "wb",
              encoding = "UTF-8")
  writeLines(nzval, con, useBytes = FALSE)
  close(con)
  .write_descriptor_sparse(file.path(vdir, paste0(name, ".json")),
                           dtype = "String", indtype = indtype)
  invisible()
}

# Adaptive dispatcher: picks dense vs sparse on-disk layout based on the
# heuristics in files_io.R. sparseVector input is always written sparse.
S7::method(format_set_vector,
           list(FilesDaf, S7::class_character, S7::class_character,
                S7::class_any, S7::class_logical)) <- function(daf, axis, name, vec, overwrite) {
  if (methods::is(vec, "sparseVector")) {
    return(.files_set_vector_sparse_input(daf, axis, name, vec, overwrite))
  }
  vec <- .validate_vector_value(daf, axis, name, vec)
  root <- .files_root(daf)
  vdir <- .path_vector_dir(root, axis)
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  desc_path <- file.path(vdir, paste0(name, ".json"))
  if (file.exists(desc_path) && !overwrite) {
    stop(sprintf("vector %s already exists on axis %s; use overwrite = TRUE",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  .files_vector_unlink_payload(vdir, name)
  eltype  <- .dtype_for_r_vector(vec)
  n       <- length(vec)
  indtype <- .indtype_for_size(n)
  go_sparse <- if (eltype == "String") {
    .should_sparsify_string(vec, indtype)
  } else {
    .should_sparsify_numeric(vec, eltype, indtype)
  }
  if (!go_sparse) {
    .files_write_vector_dense(vdir, name, vec)
  } else if (eltype == "String") {
    nz <- which(nzchar(vec))
    .files_write_vector_sparse_string(vdir, name, nz, vec[nz], indtype)
  } else {
    nz <- if (is.logical(vec)) which(vec) else which(vec != 0)
    .files_write_vector_sparse_numeric(vdir, name, nz, vec[nz], eltype, indtype)
  }
  bump_vector_counter(daf, axis, name)
  invisible()
}

.files_set_vector_sparse_input <- function(daf, axis, name, sv, overwrite) {
  n <- format_axis_length(daf, axis)
  if (sv@length != n) {
    stop(sprintf("sparseVector %s length %d (expected %d) on axis %s",
                 sQuote(name), sv@length, n, sQuote(axis)), call. = FALSE)
  }
  root <- .files_root(daf)
  vdir <- .path_vector_dir(root, axis)
  dir.create(vdir, recursive = TRUE, showWarnings = FALSE)
  desc_path <- file.path(vdir, paste0(name, ".json"))
  if (file.exists(desc_path) && !overwrite) {
    stop(sprintf("vector %s already exists on axis %s; use overwrite = TRUE",
                 sQuote(name), sQuote(axis)), call. = FALSE)
  }
  .files_vector_unlink_payload(vdir, name)
  eltype  <- .dtype_for_r_vector(sv@x)
  indtype <- .indtype_for_size(n)
  .files_write_vector_sparse_numeric(vdir, name,
                                     as.integer(sv@i), sv@x, eltype, indtype)
  bump_vector_counter(daf, axis, name)
  invisible()
}

S7::method(format_delete_vector,
           list(FilesDaf, S7::class_character, S7::class_character, S7::class_logical)) <- function(daf, axis, name, must_exist) {
  vdir <- .path_vector_dir(.files_root(daf), axis)
  desc_path <- file.path(vdir, paste0(name, ".json"))
  if (!file.exists(desc_path)) {
    if (must_exist) {
      stop(sprintf("vector %s does not exist on axis %s",
                   sQuote(name), sQuote(axis)), call. = FALSE)
    }
    return(invisible())
  }
  unlink(desc_path, force = TRUE)
  .files_vector_unlink_payload(vdir, name)
  bump_vector_counter(daf, axis, name)
  invisible()
}
