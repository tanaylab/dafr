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
