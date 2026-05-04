#' @include classes.R memory_daf.R writers.R chain_daf.R
NULL

.example_data_dir <- function() {
    path <- system.file("extdata", "example_data", package = "dafr")
    if (!nzchar(path)) {
        # devtools::load_all path
        path <- file.path(
            rprojroot::find_package_root_file(),
            "inst", "extdata", "example_data"
        )
    }
    path
}

.cast_vector <- function(strs) {
    if (all(tolower(strs) %in% c("true", "false"))) {
        return(tolower(strs) == "true")
    }
    num <- suppressWarnings(as.numeric(strs))
    if (anyNA(num)) {
        return(strs)
    }
    int_v <- suppressWarnings(as.integer(num))
    if (all(!is.na(int_v) & num == int_v)) {
        return(int_v)
    }
    num
}

.cast_matrix <- function(m) {
    if (!is.double(m)) return(m)
    if (all(m == floor(m)) && all(m >= 0) && all(m <= 65535)) {
        storage.mode(m) <- "integer"
    }
    m
}

.load_axis_file <- function(daf, which, file_path) {
    file <- basename(file_path)
    parts <- strsplit(file, ".", fixed = TRUE)[[1L]]
    stopifnot(length(parts) == 3L)
    kind <- parts[[1L]]
    axis <- parts[[2L]]
    # suffix == parts[[3L]] == "rds"

    if (grepl(which, kind, fixed = TRUE)) {
        entries <- readRDS(file_path)
        add_axis(daf, axis, entries)
    }
    invisible(daf)
}

.load_vector_file <- function(daf, which, file_path) {
    file <- basename(file_path)
    parts <- strsplit(file, ".", fixed = TRUE)[[1L]]
    stopifnot(length(parts) == 4L)
    kind <- parts[[1L]]
    axis <- parts[[2L]]
    prop <- parts[[3L]]
    # suffix == parts[[4L]] == "rds"

    if (grepl(which, kind, fixed = TRUE)) {
        strs <- readRDS(file_path)
        vec <- .cast_vector(strs)
        names(vec) <- axis_vector(daf, axis)
        set_vector(daf, axis, prop, vec)
    }
    invisible(daf)
}

.load_matrix_file <- function(daf, which, file_path) {
    file <- basename(file_path)
    parts <- strsplit(file, ".", fixed = TRUE)[[1L]]
    stopifnot(length(parts) == 5L)
    kind <- parts[[1L]]
    lines_axis <- parts[[2L]]
    vals_axis <- parts[[3L]]
    prop <- parts[[4L]]
    # suffix == parts[[5L]] == "rds"

    if (grepl(which, kind, fixed = TRUE)) {
        m <- readRDS(file_path)
        m <- .cast_matrix(m)

        set_matrix(daf, vals_axis, lines_axis, prop, m)
        if (is.integer(m)) {
            relayout_matrix(daf, vals_axis, lines_axis, prop)
        }
    }
    invisible(daf)
}

.example_daf <- function(which, name) {
    daf <- memory_daf(name = name)

    if (which == "c") {
        set_scalar(daf, "organism", "human")
        set_scalar(daf, "reference", "test")
    }

    dir <- .example_data_dir()

    axes_files <- sort(list.files(file.path(dir, "axes"), full.names = TRUE), method = "radix")
    for (f in axes_files) {
        .load_axis_file(daf, which, f)
    }

    vectors_files <- sort(list.files(file.path(dir, "vectors"), full.names = TRUE), method = "radix")
    for (f in vectors_files) {
        .load_vector_file(daf, which, f)
    }

    matrices_files <- sort(list.files(file.path(dir, "matrices"), full.names = TRUE), method = "radix")
    for (f in matrices_files) {
        .load_matrix_file(daf, which, f)
    }

    daf
}

#' Load cells example data into a MemoryDaf.
#'
#' Replicates the Julia `ExampleData.example_cells_daf()` dataset.  Data are
#' loaded from the bundled `inst/extdata/example_data/` files and type-promoted
#' using the same lattice as Julia (Bool → UInt32/Int32 → Float32 for vectors;
#' UInt8 → UInt16 → Float32 for matrices).
#'
#' @param name Name for the returned `MemoryDaf`.
#' @return A [`MemoryDaf`] pre-populated with 856 cells, 683 genes, 95 donors,
#'   and 23 experiments.
#' @examples
#' # Mirrors example_data.jl jldoctest at line 22.
#' d <- example_cells_daf()
#' cat(description(d))
#' @export
example_cells_daf <- function(name = "cells!") {
    .example_daf("c", name)
}

#' Load metacells example data into a MemoryDaf.
#'
#' Replicates the Julia `ExampleData.example_metacells_daf()` dataset.
#'
#' @param name Name for the returned `MemoryDaf`.
#' @return A [`MemoryDaf`] pre-populated with 856 cells, 683 genes, 7
#'   metacells, and 4 types.
#' @examples
#' # Mirrors example_data.jl jldoctest at line 62.
#' m <- example_metacells_daf()
#' cat(description(m))
#' @export
example_metacells_daf <- function(name = "metacells!") {
    .example_daf("m", name)
}

#' Load a chain of cells and metacells example data.
#'
#' Replicates the Julia `ExampleData.example_chain_daf()` dataset.
#'
#' @param name Name for the returned `WriteChainDaf`.
#' @return A [`WriteChainDaf`] combining `example_cells_daf()` and
#'   `example_metacells_daf()`.
#' @examples
#' # Mirrors example_data.jl jldoctest at line 205.
#' ch <- example_chain_daf()
#' cat(description(ch))
#' @export
example_chain_daf <- function(name = "chain!") {
    chain_writer(
        list(example_cells_daf(), example_metacells_daf()),
        name = name
    )
}
