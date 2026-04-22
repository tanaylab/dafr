#!/usr/bin/env Rscript
# Regenerate the <LIGHT-START>...<LIGHT-END> block in benchmarks/queries.yaml
# from tests/testthat/fixtures/julia-queries/fixture.json.
# Idempotent. Safe to re-run whenever the julia-queries fixture changes.

options(error = NULL)

suppressPackageStartupMessages({
    library(jsonlite)
})

fixture_path <- "tests/testthat/fixtures/julia-queries/fixture.json"
yaml_path    <- "benchmarks/queries.yaml"
light_fixture_name <- "cells_daf"

records <- read_json(fixture_path, simplifyVector = FALSE)
cat(sprintf("loaded %d julia-queries records\n", length(records)))

.yaml_str <- function(s) {
    # Emit as a single-quoted YAML scalar, escaping embedded single quotes.
    sprintf("'%s'", gsub("'", "''", s, fixed = TRUE))
}

light_lines <- character(0)
for (i in seq_along(records)) {
    r <- records[[i]]
    light_lines <- c(light_lines,
        sprintf("- id: julia_queries_%03d", i),
        sprintf("  text: %s",         .yaml_str(r$query)),
        sprintf("  category: light"),
        sprintf("  fixture: %s",       light_fixture_name),
        ""
    )
}

txt <- readLines(yaml_path)
start <- grep("^#<LIGHT-START>$", txt)
end   <- grep("^#<LIGHT-END>$",   txt)
stopifnot(length(start) == 1L, length(end) == 1L, start < end)

new_txt <- c(
    txt[seq_len(start)],
    light_lines,
    txt[seq(end, length(txt))]
)
writeLines(new_txt, yaml_path)
cat(sprintf("wrote %d light entries into %s\n", length(records), yaml_path))
