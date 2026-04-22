#!/usr/bin/env Rscript
# Join R and Julia bake-off CSVs on query_id, compute ratios, flag breaches.
#
# Usage:
#   Rscript benchmarks/compare.R --r R.csv --julia J.csv --out report.md
#       [--csv comparison.csv]

options(error = NULL)

suppressPackageStartupMessages({
    library(dplyr, warn.conflicts = FALSE)
})

args <- commandArgs(trailingOnly = TRUE)
.arg <- function(flag, default = NULL) {
    i <- match(flag, args); if (is.na(i) || i == length(args)) return(default)
    args[i + 1L]
}
r_path   <- .arg("--r",      stop("--r required"))
j_path   <- .arg("--julia",  stop("--julia required"))
out_path <- .arg("--out",    "/tmp/report.md")
csv_path <- .arg("--csv",    sub("\\.md$", ".csv", out_path))

# ratio = dafr_median / julia_median
# ratio > threshold  →  breach
THRESHOLDS <- c(
    kernel   = 1.2,
    blas     = 1.1,
    mmap     = 1.5,
    light    = 2.0,
    grouped  = 1.2,
    chain    = 2.0,
    complete = 2.0
)

.read_csv <- function(path) {
    lines   <- readLines(path)
    headers <- grep("^#", lines, value = TRUE)
    data    <- grep("^#", lines, invert = TRUE, value = TRUE)
    df      <- read.csv(text = data, stringsAsFactors = FALSE)
    list(df = df, headers = headers)
}

.extract_fixtures <- function(headers) {
    line <- grep("^# fixtures:", headers, value = TRUE)[1L]
    if (is.na(line)) return(character(0))
    sort(trimws(strsplit(sub("^# fixtures:\\s*", "", line), ";", fixed = TRUE)[[1L]]))
}

r <- .read_csv(r_path); j <- .read_csv(j_path)
r_fix <- .extract_fixtures(r$headers); j_fix <- .extract_fixtures(j$headers)
if (!identical(r_fix, j_fix)) {
    stop(sprintf("fixture checksum mismatch:\n  R:     %s\n  Julia: %s",
                 paste(r_fix, collapse = "; "), paste(j_fix, collapse = "; ")))
}

joined <- inner_join(r$df, j$df,
                     by = c("query_id", "query_text", "category", "fixture"),
                     suffix = c("_r", "_j"))
if (nrow(joined) == 0L) stop("no query_ids joined")
if (nrow(joined) != nrow(r$df) || nrow(joined) != nrow(j$df)) {
    warning(sprintf("R had %d, Julia had %d, joined %d",
                    nrow(r$df), nrow(j$df), nrow(joined)))
}

# Fail loudly on categories the threshold table doesn't know about, so
# silently-mis-tiered rows can't slip through as phantom breaches (NA
# thresholds propagate to NA breach which is ambiguous).
unknown_cats <- setdiff(unique(joined$category), names(THRESHOLDS))
if (length(unknown_cats) > 0L) {
    stop(sprintf("unknown categories in query set: %s",
                 paste(unknown_cats, collapse = ", ")))
}
# ratio = R / J (higher = dafr slower = bad)
joined$ratio     <- joined$median_time_ns_r / joined$median_time_ns_j
joined$threshold <- THRESHOLDS[joined$category]
joined$breach    <- !is.na(joined$ratio) & joined$ratio > joined$threshold

write.csv(joined, csv_path, row.names = FALSE)
cat(sprintf("wrote %s\n", csv_path))

# ---- markdown report ----
.fmt_ns <- function(x) {
    if (is.na(x)) return("    NA")
    units <- c("ns","µs","ms","s")
    i <- 1L; while (x >= 1000 && i < 4L) { x <- x / 1000; i <- i + 1L }
    sprintf("%6.2f %s", x, units[i])
}
.fmt_ratio <- function(x) if (is.na(x)) "    NA" else sprintf("%.2f×", x)

md <- c(
    sprintf("# Bake-off: R vs Julia (%s)", Sys.Date()),
    "",
    "Ratio = dafr_median / julia_median. Higher = dafr slower. A breach is",
    "`ratio > threshold` for the query's tier.",
    "",
    "## Headers",
    "",
    "```", r$headers, "```", "",
    "```", j$headers, "```", ""
)

.section <- function(df, heading) {
    if (nrow(df) == 0L) return(c(sprintf("## %s", heading),
                                 "", "_(none)_", ""))
    # sort within section by ratio descending (NAs at bottom)
    df <- df[order(is.na(df$ratio), -df$ratio), ]
    tbl <- c(
        sprintf("## %s (%d)", heading, nrow(df)),
        "",
        "| query_id | category | fixture | R (median) | J (median) | ratio | threshold |",
        "|---|---|---|---|---|---|---|"
    )
    for (i in seq_len(nrow(df))) {
        tbl <- c(tbl,
            sprintf("| %s | %s | %s | %s | %s | %s | %s |",
                    df$query_id[i], df$category[i], df$fixture[i],
                    .fmt_ns(df$median_time_ns_r[i]),
                    .fmt_ns(df$median_time_ns_j[i]),
                    .fmt_ratio(df$ratio[i]),
                    .fmt_ratio(df$threshold[i])))
    }
    c(tbl, "")
}

# NaN ratios (Julia side couldn't measure): a separate section
na_rows <- joined[is.na(joined$ratio), ]
ok_rows <- joined[!is.na(joined$ratio) & !joined$breach, ]
bad_rows <- joined[!is.na(joined$ratio) & joined$breach, ]

md <- c(md,
        .section(bad_rows, "BREACHED"),
        .section(ok_rows, "Within threshold"),
        .section(na_rows, "Julia N/A (R-only or DAF.jl gap)"))

writeLines(md, out_path)
cat(sprintf("wrote %s (%d breached / %d within / %d Julia-NA / %d total)\n",
            out_path, nrow(bad_rows), nrow(ok_rows), nrow(na_rows), nrow(joined)))
