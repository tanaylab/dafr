fixture_path <- function() {
  testthat::test_path("fixtures", "julia-filesdaf")
}

.have_julia_env <- function() {
  out <- suppressWarnings(
    system2("conda", c("run", "-n", "dafr-mcview", "julia", "--version"),
            stdout = TRUE, stderr = TRUE))
  length(out) > 0L && any(grepl("^julia", out))
}

run_julia <- function(script_lines) {
  script <- tempfile(fileext = ".jl")
  on.exit(unlink(script), add = TRUE)
  writeLines(script_lines, script)
  system2("conda", c("run", "-n", "dafr-mcview", "julia", script),
          stdout = TRUE, stderr = TRUE)
}
