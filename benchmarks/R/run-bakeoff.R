#!/usr/bin/env Rscript
# R-side bake-off runner.
#
# Usage:
#   Rscript benchmarks/R/run-bakeoff.R --out OUT.csv [--only id1,id2,...] [--fixture NAME] [--par]
#
# Reads benchmarks/queries.yaml, opens each required fixture once (unless
# reopen=true in the entry, in which case the fixture is reopened inside
# the benched expression), executes each query via bench::mark, emits a
# CSV aligned with the Julia runner.

options(error = NULL)  # user's .Rprofile may set recover()

suppressPackageStartupMessages({
    library(dafr)
    library(bench)
    library(yaml)
    library(digest)
})

args <- commandArgs(trailingOnly = TRUE)
.arg <- function(flag, default = NULL) {
    i <- match(flag, args)
    if (is.na(i) || i == length(args)) return(default)
    args[i + 1L]
}
.has_flag <- function(flag) flag %in% args

out_path    <- .arg("--out", "/tmp/r-times.csv")
only_ids    <- .arg("--only", NULL)
only_fixt   <- .arg("--fixture", NULL)
min_iter    <- as.integer(.arg("--min-iter", "5"))
par_mode    <- .has_flag("--par")

# ---- single-threaded baseline unless --par ----
if (!par_mode) {
    Sys.setenv(OMP_NUM_THREADS = "1")
    options(dafr.kernel_threshold = Inf)   # force scalar code paths
    if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
        RhpcBLASctl::blas_set_num_threads(1)
        RhpcBLASctl::omp_set_num_threads(1)
    }
}

fixture_root <- "benchmarks/fixture/data"
queries <- yaml::read_yaml("benchmarks/queries.yaml")

if (!is.null(only_ids)) {
    ids <- strsplit(only_ids, ",", fixed = TRUE)[[1L]]
    queries <- Filter(function(q) q$id %in% ids, queries)
}
if (!is.null(only_fixt)) {
    queries <- Filter(function(q) q$fixture == only_fixt, queries)
}
cat(sprintf("running %d queries\n", length(queries)))

.open_fixture <- function(name) {
    path <- file.path(fixture_root, name)
    if (name == "chain_triple" || name == "view_renamed") {
        dafr::complete_daf(file.path(path, "leaf"), "r")
    } else {
        dafr::files_daf(path, name = name, mode = "r")
    }
}

fixtures_needed <- unique(vapply(queries, function(q) q$fixture, character(1L)))
opened <- list()
for (name in fixtures_needed) {
    opened[[name]] <- .open_fixture(name)
    cat(sprintf("opened fixture %s\n", name))
}

# ---- fixture checksums (sha256 over sorted file content) ----
.sha256_dir <- function(path) {
    files <- sort(list.files(path, recursive = TRUE, full.names = TRUE))
    hashes <- vapply(files, function(f) digest::digest(file = f, algo = "sha256"),
                     character(1L))
    digest::digest(paste(basename(files), hashes, collapse = "\n"), algo = "sha256", serialize = FALSE)
}
checksums <- vapply(sort(fixtures_needed),
                    function(n) substr(.sha256_dir(file.path(fixture_root, n)), 1, 16),
                    character(1L))

# ---- benchmark loop ----
rows <- vector("list", length(queries))
for (k in seq_along(queries)) {
    q <- queries[[k]]
    reopen <- isTRUE(q$reopen)
    text <- q$text
    fixture_name <- q$fixture

    # Build the benched expression. For reopen=true, re-open the fixture
    # inside the benched expression so I/O is part of what we measure.
    if (reopen) {
        expr <- bquote({
            d <- .open_fixture(.(fixture_name))
            dafr::get_query(d, .(text))
        })
    } else {
        d <- opened[[fixture_name]]
        expr <- bquote(dafr::get_query(.(d), .(text)))
    }

    t0 <- Sys.time()
    b <- tryCatch(
        bench::mark(
            eval(expr),
            min_iterations = min_iter,
            filter_gc = FALSE,
            check = FALSE,
            time_unit = "ns"
        ),
        error = function(e) {
            cat(sprintf("  [%3d/%3d] %-30s %-22s FAIL: %s\n",
                        k, length(queries), q$id, q$category,
                        conditionMessage(e)))
            NULL
        }
    )
    if (is.null(b)) {
        rows[[k]] <- data.frame(
            query_id = q$id, query_text = q$text,
            category = q$category, fixture = q$fixture,
            median_time_ns = NA_real_, min_time_ns = NA_real_,
            gc_count = NA_integer_, allocations = NA_real_,
            n_iter = NA_integer_,
            stringsAsFactors = FALSE
        )
        next
    }
    dt <- as.numeric(Sys.time() - t0, units = "secs")
    cat(sprintf("  [%3d/%3d] %-30s %-22s ok (%.1fs)\n",
                k, length(queries), q$id, q$category, dt))

    rows[[k]] <- data.frame(
        query_id       = q$id,
        query_text     = q$text,
        category       = q$category,
        fixture        = q$fixture,
        median_time_ns = as.numeric(b$median),
        min_time_ns    = as.numeric(b$min),
        gc_count       = b$n_gc,
        allocations    = sum(as.numeric(b$mem_alloc), na.rm = TRUE),
        n_iter         = b$n_itr,
        stringsAsFactors = FALSE
    )
}

df <- do.call(rbind, rows)

# ---- header lines ----
commit <- tryCatch(
    system("git rev-parse HEAD", intern = TRUE)[1L],
    error = function(e) "unknown"
)
header_lines <- c(
    sprintf("# runner: R"),
    sprintf("# dafr_commit: %s", commit),
    sprintf("# R_version: %s", getRversion()),
    sprintf("# platform: %s", R.version$platform),
    sprintf("# OMP_NUM_THREADS: %s", Sys.getenv("OMP_NUM_THREADS", "default")),
    sprintf("# BLAS: %s", extSoftVersion()[["BLAS"]]),
    sprintf("# fixtures: %s", paste(sprintf("%s=%s", names(checksums), checksums), collapse = "; "))
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(header_lines, out_path)
suppressWarnings(
    write.table(df, out_path, sep = ",", row.names = FALSE, col.names = TRUE,
                append = TRUE, qmethod = "double")
)
cat(sprintf("\nwrote %s (%d rows)\n", out_path, nrow(df)))
