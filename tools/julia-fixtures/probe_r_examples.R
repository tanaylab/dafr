#!/usr/bin/env Rscript
# Probe every Julia jldoctest from example_data.jl/readers.jl/writers.jl in R.
# Runs each example, captures the value, and prints a pass/fail line per case.

suppressMessages(pkgload::load_all(quiet = TRUE))
suppressMessages(library(jsonlite))

FX_DIR <- file.path("tests", "testthat", "fixtures", "julia-readers-writers")

read_fx <- function(name) fromJSON(file.path(FX_DIR, paste0(name, ".json")), simplifyVector = TRUE)

cells <- function() example_cells_daf()
metacells <- function() example_metacells_daf()
chain <- function() example_chain_daf()

cases <- list()
add <- function(name, fn) cases[[name]] <<- fn

# ---- example_data.jl ------------------------------------------------------

.vectors_per_axis <- function(daf) {
    out <- lapply(sort(axes_set(daf)), function(a) sort(vectors_set(daf, a)))
    names(out) <- sort(axes_set(daf))
    out[lengths(out) > 0L]
}

add("example_data_cells_description", function() {
    fx <- read_fx("example_data_cells_description")
    d <- cells()
    list(
        scalars  = list(want = fx$structure$scalars, got = list(organism = get_scalar(d, "organism"), reference = get_scalar(d, "reference"))),
        axes     = list(want = fx$structure$axes, got = sapply(axes_set(d), function(a) axis_length(d, a))),
        vectors  = list(want = fx$structure$vectors, got = .vectors_per_axis(d))
    )
})

add("example_data_metacells_description", function() {
    fx <- read_fx("example_data_metacells_description")
    d <- metacells()
    list(
        axes    = list(want = fx$structure$axes, got = sapply(axes_set(d), function(a) axis_length(d, a))),
        vectors = list(want = fx$structure$vectors, got = .vectors_per_axis(d))
    )
})

add("example_data_chain_description", function() {
    fx <- read_fx("example_data_chain_description")
    d <- chain()
    list(
        scalars = list(want = fx$structure$scalars, got = list(organism = get_scalar(d, "organism"), reference = get_scalar(d, "reference"))),
        axes    = list(want = fx$structure$axes, got = sapply(axes_set(d), function(a) axis_length(d, a)))
    )
})

# ---- readers.jl -----------------------------------------------------------

add("readers_has_scalar_cells_organism",   function() list(want = read_fx("readers_has_scalar_cells_organism")$value,   got = has_scalar(cells(), "organism")))
add("readers_has_scalar_metacells_organism", function() list(want = read_fx("readers_has_scalar_metacells_organism")$value, got = has_scalar(metacells(), "organism")))
add("readers_scalars_set_cells", function() list(want = read_fx("readers_scalars_set_cells")$values, got = sort(scalars_set(cells()))))
add("readers_get_scalar_cells_organism", function() list(want = read_fx("readers_get_scalar_cells_organism")$value, got = get_scalar(cells(), "organism")))
add("readers_get_scalar_metacells_default_nothing", function() {
    fx <- read_fx("readers_get_scalar_metacells_default_nothing")
    list(want = fx$value, got = get_scalar(metacells(), "organism", default = NULL))
})
add("readers_has_axis_cells_metacell",   function() list(want = read_fx("readers_has_axis_cells_metacell")$value,   got = has_axis(cells(), "metacell")))
add("readers_has_axis_metacells_metacell", function() list(want = read_fx("readers_has_axis_metacells_metacell")$value, got = has_axis(metacells(), "metacell")))
add("readers_axis_version_counter_type", function() {
    fx <- read_fx("readers_axis_version_counter_type")
    m <- metacells()
    before <- axis_version_counter(m, "type")
    delete_axis(m, "type")
    add_axis(m, "type", c("Foo", "Bar", "Baz"))
    after <- axis_version_counter(m, "type")
    list(before = list(want = fx$before, got = before),
         after  = list(want = fx$after,  got = after))
})
add("readers_axes_set_cells", function() list(want = read_fx("readers_axes_set_cells")$values, got = sort(axes_set(cells()))))
add("readers_axis_vector_metacells_type", function() list(want = read_fx("readers_axis_vector_metacells_type")$values, got = axis_vector(metacells(), "type")))
add("readers_axis_dict_metacells_type", function() {
    fx <- read_fx("readers_axis_dict_metacells_type")
    d <- axis_dict(metacells(), "type")
    list(want_keys = fx$keys, got_keys = ls(d), want_values = fx$values,
         got_values = sapply(fx$keys, function(k) d[[k]], USE.NAMES = FALSE))
})
add("readers_axis_indices_allow_empty", function() {
    fx <- read_fx("readers_axis_indices_allow_empty")
    out <- axis_indices(metacells(), "type", c("MPP", ""), allow_empty = TRUE)
    list(want = fx$values, got = out)
})
add("readers_axis_entries_allow_empty", function() {
    fx <- read_fx("readers_axis_entries_allow_empty")
    out <- axis_entries(metacells(), "type", indices = c(3L, 0L), allow_empty = TRUE)
    list(want = fx$values, got = out)
})
add("readers_axis_length_metacells_type", function() list(want = read_fx("readers_axis_length_metacells_type")$value, got = axis_length(metacells(), "type")))
add("readers_has_vector_cells_cell_type", function() list(want = read_fx("readers_has_vector_cells_cell_type")$value, got = has_vector(cells(), "cell", "type")))
add("readers_has_vector_metacells_metacell_type", function() list(want = read_fx("readers_has_vector_metacells_metacell_type")$value, got = has_vector(metacells(), "metacell", "type")))
add("readers_vector_version_counter_color", function() {
    fx <- read_fx("readers_vector_version_counter_color")
    m <- metacells()
    before <- vector_version_counter(m, "type", "color")
    set_vector(m, "type", "color", as.character(1:4), overwrite = TRUE)
    after <- vector_version_counter(m, "type", "color")
    list(before = list(want = fx$before, got = before),
         after  = list(want = fx$after,  got = after))
})
add("readers_vectors_set_cells_cell", function() list(want = read_fx("readers_vectors_set_cells_cell")$values, got = sort(vectors_set(cells(), "cell"))))
add("readers_get_vector_metacells_type_color", function() {
    fx <- read_fx("readers_get_vector_metacells_type_color")
    v <- get_vector(metacells(), "type", "color")
    list(want_names = fx$names, got_names = names(v), want_values = fx$values, got_values = unname(v))
})
add("readers_has_matrix_cells_gene_cell_UMIs", function() list(want = read_fx("readers_has_matrix_cells_gene_cell_UMIs")$value, got = has_matrix(cells(), "gene", "cell", "UMIs")))
add("readers_matrices_set_cells_gene_cell", function() list(want = read_fx("readers_matrices_set_cells_gene_cell")$values, got = sort(matrices_set(cells(), "gene", "cell"))))
add("readers_get_matrix_metacells_gene_metacell_fraction", function() {
    fx <- read_fx("readers_get_matrix_metacells_gene_metacell_fraction")
    m <- get_matrix(metacells(), "gene", "metacell", "fraction")
    head_want <- matrix(fx$head, nrow = 8L, ncol = 7L)
    list(
        rows = list(want = fx$rows, got = nrow(m)),
        cols = list(want = fx$cols, got = ncol(m)),
        row_names_head = list(want = head(fx$row_names, 8L), got = head(rownames(m), 8L)),
        col_names      = list(want = fx$col_names, got = colnames(m)),
        head           = list(want = head_want, got = as.matrix(m)[1:8, 1:7]),
        checksum       = list(want = fx$checksum, got = sum(as.matrix(m)))
    )
})
add("readers_matrix_version_counter_fraction", function() {
    fx <- read_fx("readers_matrix_version_counter_fraction")
    m <- metacells()
    before <- matrix_version_counter(m, "gene", "metacell", "fraction")
    set_matrix(m, "gene", "metacell", "fraction",
               matrix(stats::runif(683L * 7L), 683L, 7L), overwrite = TRUE)
    after <- matrix_version_counter(m, "gene", "metacell", "fraction")
    list(before = list(want = fx$before, got = before),
         after  = list(want = fx$after,  got = after))
})
add("readers_description_chain_deep", function() {
    fx <- read_fx("readers_description_chain_deep")
    d <- chain()
    list(axes = list(want = fx$structure$axes, got = sapply(axes_set(d), function(a) axis_length(d, a))))
})

# ---- writers.jl -----------------------------------------------------------

add("writers_set_scalar_overwrite", function() {
    fx <- read_fx("writers_set_scalar_overwrite")
    d <- cells()
    set_scalar(d, "version", 1.0)
    a <- get_scalar(d, "version")
    set_scalar(d, "version", 2.0, overwrite = TRUE)
    b <- get_scalar(d, "version")
    list(first = list(want = fx$first, got = a), second = list(want = fx$second, got = b))
})
add("writers_delete_scalar_organism", function() {
    fx <- read_fx("writers_delete_scalar_organism")
    d <- cells()
    a <- has_scalar(d, "organism")
    delete_scalar(d, "organism")
    b <- has_scalar(d, "organism")
    list(before = list(want = fx$before, got = a), after = list(want = fx$after, got = b))
})
add("writers_add_axis_block", function() {
    fx <- read_fx("writers_add_axis_block")
    d <- cells()
    a <- has_axis(d, "block")
    add_axis(d, "block", c("B1", "B2"))
    b <- has_axis(d, "block")
    list(before = list(want = fx$before, got = a), after = list(want = fx$after, got = b))
})
add("writers_delete_axis_type", function() {
    fx <- read_fx("writers_delete_axis_type")
    d <- metacells()
    a <- has_axis(d, "type")
    delete_axis(d, "type")
    b <- has_axis(d, "type")
    list(before = list(want = fx$before, got = a), after = list(want = fx$after, got = b))
})
add("writers_set_vector_is_mebemp", function() {
    fx <- read_fx("writers_set_vector_is_mebemp")
    d <- metacells()
    s0 <- has_vector(d, "type", "is_mebemp")
    set_vector(d, "type", "is_mebemp", c(TRUE, TRUE, FALSE, FALSE))
    s1 <- has_vector(d, "type", "is_mebemp")
    set_vector(d, "type", "is_mebemp", c(TRUE, TRUE, TRUE, FALSE), overwrite = TRUE)
    s2 <- has_vector(d, "type", "is_mebemp")
    final <- unname(get_vector(d, "type", "is_mebemp"))
    list(stages = list(want = fx$stages, got = c(s0, s1, s2)),
         final = list(want = fx$final, got = final))
})
add("writers_delete_vector_color", function() {
    fx <- read_fx("writers_delete_vector_color")
    d <- metacells()
    a <- has_vector(d, "type", "color")
    delete_vector(d, "type", "color")
    b <- has_vector(d, "type", "color")
    list(before = list(want = fx$before, got = a), after = list(want = fx$after, got = b))
})
add("writers_set_matrix_confidence", function() {
    fx <- read_fx("writers_set_matrix_confidence")
    d <- metacells()
    snap <- function(daf) c(
        has_matrix(daf, "gene", "metacell", "confidence"),                       # relayout=TRUE default
        has_matrix(daf, "gene", "metacell", "confidence", relayout = FALSE),
        has_matrix(daf, "metacell", "gene", "confidence", relayout = FALSE)
    )
    s0 <- snap(d)
    set_matrix(d, "metacell", "gene", "confidence",
               matrix(stats::runif(7L * 683L), 7L, 683L), relayout = FALSE)
    s1 <- snap(d)
    set_matrix(d, "metacell", "gene", "confidence",
               matrix(stats::runif(7L * 683L), 7L, 683L),
               overwrite = TRUE, relayout = TRUE)
    s2 <- snap(d)
    list(stage0 = list(want = fx$stage0, got = s0),
         stage1 = list(want = fx$stage1, got = s1),
         stage2 = list(want = fx$stage2, got = s2))
})
add("writers_delete_matrix_UMIs", function() {
    fx <- read_fx("writers_delete_matrix_UMIs")
    d <- cells()
    snap <- function(daf) c(
        has_matrix(daf, "gene", "cell", "UMIs"),                                 # relayout=TRUE default
        has_matrix(daf, "gene", "cell", "UMIs", relayout = FALSE),
        has_matrix(daf, "cell", "gene", "UMIs", relayout = FALSE)
    )
    s0 <- snap(d)
    delete_matrix(d, "gene", "cell", "UMIs", relayout = FALSE)
    s1 <- snap(d)
    delete_matrix(d, "gene", "cell", "UMIs", must_exist = FALSE)
    s2 <- snap(d)
    list(stage0 = list(want = fx$stage0, got = s0),
         stage1 = list(want = fx$stage1, got = s1),
         stage2 = list(want = fx$stage2, got = s2))
})

# ---- run probe ------------------------------------------------------------

cat(sprintf("Running %d probes ...\n\n", length(cases)))
results <- list()
for (nm in names(cases)) {
    res <- tryCatch(cases[[nm]](), error = function(e) list(error = conditionMessage(e)))
    results[[nm]] <- res
}

format_status <- function(res) {
    if (!is.null(res$error)) return(list(status = "ERROR", detail = res$error))
    diffs <- character(0)
    walk <- function(x, path = "") {
        if (is.list(x) && all(c("want", "got") %in% names(x))) {
            ok <- tryCatch(isTRUE(all.equal(x$want, x$got, check.attributes = FALSE, tolerance = 1e-6)),
                           error = function(e) FALSE)
            if (!ok) {
                diffs <<- c(diffs, sprintf("%s: want=%s got=%s",
                    path, paste(deparse(x$want, control = NULL), collapse = " "),
                    paste(deparse(x$got,  control = NULL), collapse = " ")))
            }
            return(invisible())
        }
        if (is.list(x)) for (k in names(x)) walk(x[[k]], if (nzchar(path)) paste0(path, ".", k) else k)
    }
    walk(res)
    if (length(diffs) == 0L) list(status = "PASS")
    else list(status = "FAIL", detail = paste(diffs, collapse = "\n  "))
}

n_pass <- 0L; n_fail <- 0L; n_err <- 0L
for (nm in names(results)) {
    s <- format_status(results[[nm]])
    if (s$status == "PASS") {
        n_pass <- n_pass + 1L
        cat(sprintf("  PASS  %s\n", nm))
    } else if (s$status == "FAIL") {
        n_fail <- n_fail + 1L
        cat(sprintf("  FAIL  %s\n  %s\n", nm, s$detail))
    } else {
        n_err <- n_err + 1L
        cat(sprintf("  ERROR %s\n  %s\n", nm, s$detail))
    }
}

cat(sprintf("\nSummary: %d PASS, %d FAIL, %d ERROR (of %d)\n",
            n_pass, n_fail, n_err, length(cases)))
