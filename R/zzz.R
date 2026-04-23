.onLoad <- function(libname, pkgname) {
    set_default_options()
    .register_default_ops()
    S7::methods_register()
    .register_dplyr_methods()
    invisible()
}

# Conditionally bind S3 methods for dplyr's generics so the dplyr
# backend works when dplyr is loaded, without making dplyr a hard
# dependency of dafr.
.register_dplyr_methods <- function() {
    if (!requireNamespace("dplyr", quietly = TRUE)) return(invisible())
    ns <- asNamespace("dafr")
    dplyr_ns <- asNamespace("dplyr")
    registerS3method("tbl", "dafr::DafReader", ns$tbl_DafReader_axis, envir = dplyr_ns)
    registerS3method("collect", "daf_axis_tbl", ns$collect_daf_axis_tbl, envir = dplyr_ns)
    if (requireNamespace("tibble", quietly = TRUE)) {
        registerS3method(
            "as_tibble", "daf_axis_tbl",
            ns$as_tibble_daf_axis_tbl,
            envir = asNamespace("tibble")
        )
    }
    registerS3method("print", "daf_axis_tbl", ns$print_daf_axis_tbl)
}

.onUnload <- function(libpath) {
    library.dynam.unload("dafr", libpath)
}
