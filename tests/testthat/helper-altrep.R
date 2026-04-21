# Test-only helper: returns TRUE iff `x` is ALTREP.
# Lives here (not in the package namespace) to avoid exposing an
# unexported-but-user-reachable API via `dafr:::is_altrep`.
# Body matches the C-level test in src/altrep_mmap_r.cpp.
is_altrep <- function(x) {
    dafr:::is_altrep_cpp(x)
}
