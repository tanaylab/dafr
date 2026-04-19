new_tempfile <- function(ext = "bin") {
  f <- tempfile(fileext = paste0(".", ext))
  withr::defer_parent(unlink(f, force = TRUE))
  f
}

new_tempdir <- function() {
  d <- tempfile()
  dir.create(d)
  withr::defer_parent(unlink(d, recursive = TRUE, force = TRUE))
  d
}
