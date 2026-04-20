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
