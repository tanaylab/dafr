#!/usr/bin/env Rscript
# Verifies every exported symbol appears in _pkgdown.yml reference sections.

pkg <- "dafr"
yml <- yaml::read_yaml("_pkgdown.yml")

exports <- sort(getNamespaceExports(pkg))
listed <- unlist(lapply(yml$reference, function(r) r$contents), use.names = FALSE)
listed <- sort(unique(listed))

missing <- setdiff(exports, listed)
extra <- setdiff(listed, exports)

if (length(missing) > 0L) {
    cat("Missing from _pkgdown.yml:\n")
    cat(paste0("  ", missing, "\n"), sep = "")
}
if (length(extra) > 0L) {
    cat("Listed in _pkgdown.yml but not exported:\n")
    cat(paste0("  ", extra, "\n"), sep = "")
}
if (length(missing) == 0L && length(extra) == 0L) {
    cat("_pkgdown.yml reference sections cover all exports.\n")
}
