.onLoad <- function(libname, pkgname) {
  set_default_options()
  invisible()
}

.onUnload <- function(libpath) {
  library.dynam.unload("dafr", libpath)
}
