#!/usr/bin/env Rscript
# Run the @examples block of every Rd we just touched, to confirm the Julia
# jldoctest mirrors execute cleanly under the loaded package.

suppressMessages(pkgload::load_all(quiet = TRUE))

targets <- c(
    # example_data
    "example_cells_daf", "example_metacells_daf", "example_chain_daf",
    # readers
    "has_axis", "axes_set", "axis_length", "axis_vector",
    "axis_entries", "axis_indices", "axis_dict",
    "has_scalar", "scalars_set", "get_scalar",
    "has_vector", "vectors_set", "get_vector",
    "has_matrix", "matrices_set", "get_matrix",
    "description",
    # cache (version counters)
    "axis_version_counter", "vector_version_counter", "matrix_version_counter",
    # writers
    "add_axis", "delete_axis",
    "set_scalar", "delete_scalar",
    "set_vector", "delete_vector",
    "set_matrix", "delete_matrix"
)

extract_example <- function(rd_path) {
    rd <- tools::parse_Rd(rd_path)
    out <- character()
    walk <- function(node) {
        if (inherits(node, "Rd_tag") && attr(node, "Rd_tag") == "\\examples") {
            for (child in node) out <<- c(out, as.character(child))
        }
        if (is.list(node)) for (n in node) walk(n)
    }
    walk(rd)
    paste(out, collapse = "")
}

n_pass <- 0L; n_fail <- 0L
for (fn in targets) {
    rd <- file.path("man", paste0(fn, ".Rd"))
    if (!file.exists(rd)) {
        cat(sprintf("  SKIP  %s (Rd not found)\n", fn)); next
    }
    code <- extract_example(rd)
    res <- tryCatch(
        eval(parse(text = code), envir = new.env(parent = globalenv())),
        error = function(e) e
    )
    if (inherits(res, "error")) {
        n_fail <- n_fail + 1L
        cat(sprintf("  FAIL  %s: %s\n", fn, conditionMessage(res)))
    } else {
        n_pass <- n_pass + 1L
        cat(sprintf("  PASS  %s\n", fn))
    }
}

cat(sprintf("\nSummary: %d PASS, %d FAIL (of %d)\n",
            n_pass, n_fail, length(targets)))
