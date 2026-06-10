fixture_path <- function() {
    testthat::test_path("fixtures", "julia-filesdaf")
}

.have_julia_env <- function() {
    if (is.na(Sys.which("conda")) || Sys.which("conda") == "") {
        return(FALSE)
    }
    out <- tryCatch(
        suppressWarnings(
            system2("conda", c("run", "-n", "dafr-mcview", "julia", "--version"),
                stdout = TRUE, stderr = TRUE
            )
        ),
        error = function(e) character(0L),
        warning = function(w) character(0L)
    )
    length(out) > 0L && any(grepl("^julia", out))
}

run_julia <- function(script_lines) {
    script <- tempfile(fileext = ".jl")
    on.exit(unlink(script), add = TRUE)
    writeLines(script_lines, script)
    system2("conda", c("run", "-n", "dafr-mcview", "julia", script),
        stdout = TRUE, stderr = TRUE
    )
}

# DataAxesFormats.jl 0.3.0 uses the Zarr v3 on-disk format and rejects v2
# stores; dafr's ZarrDaf now speaks Zarr v3 too. So R <-> Julia `.daf.zarr`
# interop holds against DAF >= 0.3.0. The Julia-gated zarr round-trip tests
# RUN when the env's DAF is v3 and skip on older (v2-only) DAF.
.daf_jl_uses_zarr_v3 <- function() {
    if (!.have_julia_env()) return(FALSE)
    out <- run_julia(c(
        "using DataAxesFormats",
        'println(pkgversion(DataAxesFormats) >= v"0.3.0" ? "ZARRV3" : "ZARRV2")'
    ))
    any(grepl("^ZARRV3$", out))
}
