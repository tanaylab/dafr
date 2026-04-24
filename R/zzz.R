.onLoad <- function(libname, pkgname) {
    set_default_options()
    .register_default_ops()
    S7::methods_register()
    .register_dplyr_methods()
    .dafr_apply_cran_thread_cap()
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
    registerS3method("tbl_vars", "daf_axis_tbl", ns$tbl_vars_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("group_vars", "daf_axis_tbl", ns$group_vars_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("select", "daf_axis_tbl", ns$select_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("pull", "daf_axis_tbl", ns$pull_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("filter", "daf_axis_tbl", ns$filter_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("slice", "daf_axis_tbl", ns$slice_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("slice_head", "daf_axis_tbl", ns$slice_head_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("slice_tail", "daf_axis_tbl", ns$slice_tail_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("slice_min", "daf_axis_tbl", ns$slice_min_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("slice_max", "daf_axis_tbl", ns$slice_max_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("slice_sample", "daf_axis_tbl", ns$slice_sample_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("rename", "daf_axis_tbl", ns$rename_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("relocate", "daf_axis_tbl", ns$relocate_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("count", "daf_axis_tbl", ns$count_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("tally", "daf_axis_tbl", ns$tally_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("add_count", "daf_axis_tbl", ns$add_count_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("add_tally", "daf_axis_tbl", ns$add_tally_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("transmute", "daf_axis_tbl", ns$transmute_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("reframe", "daf_axis_tbl", ns$reframe_daf_axis_tbl, envir = dplyr_ns)
    # Unsupported-verb stubs — give a helpful error instead of
    # R's generic "no applicable method".
    registerS3method("inner_join", "daf_axis_tbl", ns$inner_join_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("left_join",  "daf_axis_tbl", ns$left_join_daf_axis_tbl,  envir = dplyr_ns)
    registerS3method("right_join", "daf_axis_tbl", ns$right_join_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("full_join",  "daf_axis_tbl", ns$full_join_daf_axis_tbl,  envir = dplyr_ns)
    registerS3method("semi_join",  "daf_axis_tbl", ns$semi_join_daf_axis_tbl,  envir = dplyr_ns)
    registerS3method("anti_join",  "daf_axis_tbl", ns$anti_join_daf_axis_tbl,  envir = dplyr_ns)
    registerS3method("cross_join", "daf_axis_tbl", ns$cross_join_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("nest_join",  "daf_axis_tbl", ns$nest_join_daf_axis_tbl,  envir = dplyr_ns)
    registerS3method("union",      "daf_axis_tbl", ns$union_daf_axis_tbl,      envir = dplyr_ns)
    registerS3method("union_all",  "daf_axis_tbl", ns$union_all_daf_axis_tbl,  envir = dplyr_ns)
    registerS3method("intersect",  "daf_axis_tbl", ns$intersect_daf_axis_tbl,  envir = dplyr_ns)
    registerS3method("setdiff",    "daf_axis_tbl", ns$setdiff_daf_axis_tbl,    envir = dplyr_ns)
    registerS3method("rowwise",    "daf_axis_tbl", ns$rowwise_daf_axis_tbl,    envir = dplyr_ns)
    registerS3method("mutate", "daf_axis_tbl", ns$mutate_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("arrange", "daf_axis_tbl", ns$arrange_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("distinct", "daf_axis_tbl", ns$distinct_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("group_by", "daf_axis_tbl", ns$group_by_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("ungroup", "daf_axis_tbl", ns$ungroup_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("summarise", "daf_axis_tbl", ns$summarise_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("summarize", "daf_axis_tbl", ns$summarise_daf_axis_tbl, envir = dplyr_ns)
    registerS3method("compute", "daf_axis_tbl", ns$compute_daf_axis_tbl, envir = dplyr_ns)
}

.onUnload <- function(libpath) {
    library.dynam.unload("dafr", libpath)
}
