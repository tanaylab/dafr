#!/usr/bin/env Rscript
# CRAN pre-flight script.
#
# Run this from the package root before submitting to CRAN. It runs:
#
#   1. A local `R CMD check --as-cran` on a fresh tarball.
#   2. `devtools::check_win_devel()`   — submits to https://win-builder.r-project.org/
#   3. `devtools::check_win_release()` — same.
#
# The two win-builder checks are asynchronous: win-builder emails the result to
# the `cre` maintainer listed in DESCRIPTION (Authors@R role = "cre") within
# ~20–40 minutes. Watch that inbox; a pass/fail summary plus a link to the
# logs appears in the body.
#
# R-hub v2 runs are triggered from GitHub Actions — see .github/workflows/rhub.yaml —
# not from this script.
#
# Usage:
#   Rscript tools/cran-preflight.R            # local + both win-builders
#   Rscript tools/cran-preflight.R local      # local --as-cran only
#   Rscript tools/cran-preflight.R win        # both win-builder submissions only
#   Rscript tools/cran-preflight.R win-devel  # win-builder r-devel only
#   Rscript tools/cran-preflight.R win-rel    # win-builder r-release only

args <- commandArgs(trailingOnly = TRUE)
mode <- if (length(args) == 0) "all" else args[1]
valid_modes <- c("all", "local", "win", "win-devel", "win-rel")
if (!mode %in% valid_modes) {
    stop(sprintf(
        "Unknown mode '%s'. Valid: %s",
        mode, paste(valid_modes, collapse = ", ")
    ))
}

pkg_root <- rprojroot::find_root(rprojroot::is_r_package)
setwd(pkg_root)
message(sprintf("[cran-preflight] package root: %s", pkg_root))

if (mode %in% c("all", "local")) {
    message("[cran-preflight] running local R CMD check --as-cran ...")
    res <- devtools::check(
        pkg = pkg_root,
        args = c("--as-cran", "--no-manual"),
        error_on = "warning",
        vignettes = TRUE,
        incoming = TRUE,
        remote = TRUE
    )
    message(sprintf(
        "[cran-preflight] local check: %d errors, %d warnings, %d notes.",
        length(res$errors), length(res$warnings), length(res$notes)
    ))
    if (length(res$errors) > 0 || length(res$warnings) > 0) {
        stop("[cran-preflight] local check failed — fix before submitting.")
    }
}

if (mode %in% c("all", "win", "win-devel")) {
    message("[cran-preflight] submitting to win-builder r-devel ...")
    devtools::check_win_devel(pkg = pkg_root, quiet = FALSE)
    message("[cran-preflight] watch email for results (r-devel).")
}

if (mode %in% c("all", "win", "win-rel")) {
    message("[cran-preflight] submitting to win-builder r-release ...")
    devtools::check_win_release(pkg = pkg_root, quiet = FALSE)
    message("[cran-preflight] watch email for results (r-release).")
}

message("[cran-preflight] done.")
