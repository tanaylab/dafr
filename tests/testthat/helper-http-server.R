# HTTP test-server helper for HttpDaf / HttpStore live integration tests.
#
# Boots `python -m http.server 0` rooted at `root_dir` in a subprocess.
# Reads the assigned port from the server's stdout first line ("Serving
# HTTP on 127.0.0.1 port 12345 (...)"). Returns a handle with $url and
# $process. Caller passes the handle to stop_http_server() in setup or
# teardown blocks.
#
# Live HTTP tests gate on:
# - Sys.which("python") != "" — Python interpreter available
# - requireNamespace("processx", quietly = TRUE) — subprocess control
# - !on CRAN — CI environments may forbid binding localhost ports

start_http_server <- function(root_dir) {
    if (!nzchar(Sys.which("python"))) {
        testthat::skip("python not available")
    }
    if (!requireNamespace("processx", quietly = TRUE)) {
        testthat::skip("processx not installed")
    }
    py <- Sys.which("python")

    out_file <- tempfile("dafr-http-srv-out-", fileext = ".log")
    err_file <- tempfile("dafr-http-srv-err-", fileext = ".log")
    p <- processx::process$new(
        py, c("-u", "-m", "http.server", "0",
              "--bind", "127.0.0.1",
              "--directory", root_dir),
        stdout = out_file, stderr = err_file
    )

    deadline <- Sys.time() + 5
    port <- NA_integer_
    while (Sys.time() < deadline) {
        # The "Serving HTTP on ... port N" message goes to stdout (Python
        # 3.12); some older versions wrote it to stderr — check both.
        for (f in c(out_file, err_file)) {
            if (!file.exists(f) || file.size(f) == 0L) next
            txt <- tryCatch(
                paste(readLines(f, warn = FALSE), collapse = "\n"),
                error = function(e) ""
            )
            m <- regmatches(txt, regexec("port (\\d+)", txt))[[1L]]
            if (length(m) == 2L) {
                port <- as.integer(m[[2L]])
                break
            }
        }
        if (!is.na(port)) break
        Sys.sleep(0.05)
    }
    if (is.na(port)) {
        try(p$kill(), silent = TRUE)
        unlink(c(out_file, err_file), force = TRUE)
        testthat::skip("could not parse python http.server port")
    }
    list(process = p,
         url = sprintf("http://127.0.0.1:%d", port),
         out_file = out_file,
         err_file = err_file)
}

stop_http_server <- function(handle) {
    if (is.null(handle)) return(invisible())
    if (!is.null(handle$process) && handle$process$is_alive()) {
        try(handle$process$kill(), silent = TRUE)
    }
    unlink(c(handle$out_file, handle$err_file), force = TRUE)
    invisible()
}
