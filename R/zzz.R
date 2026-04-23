.onLoad <- function(libname, pkgname) {
    set_default_options()
    .register_default_ops()
    S7::methods_register()
    invisible()
}

.onUnload <- function(libpath) {
    library.dynam.unload("dafr", libpath)
}
