new_tempfile <- function(ext = "bin", envir = parent.frame()) {
  f <- tempfile(fileext = paste0(".", ext))
  withr::defer(unlink(f, force = TRUE), envir = envir)
  f
}

new_tempdir <- function(envir = parent.frame()) {
  d <- tempfile()
  dir.create(d)
  withr::defer(unlink(d, recursive = TRUE, force = TRUE), envir = envir)
  d
}
