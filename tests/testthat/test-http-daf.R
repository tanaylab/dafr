test_that("http_daf rejects non-HTTP URLs", {
    expect_error(http_daf("file:///tmp"), "not an HTTP\\(S\\) URL")
    expect_error(http_daf("memory://"), "not an HTTP\\(S\\) URL")
    expect_error(http_daf("/some/path"), "not an HTTP\\(S\\) URL")
})

test_that("http_daf surfaces connection failure with upstream-style message", {
    skip_on_cran()
    # localhost:1 is reserved and unreachable on POSIX systems; the
    # connection-refused error path is exercised consistently.
    expect_error(http_daf("http://localhost:1"),
                 "HTTP GET failed for: http://localhost:1/metadata.zip")
})

test_that("http test helper boots and binds a port", {
    skip_if_not_installed("processx")
    skip_if_not(nzchar(Sys.which("python")))
    skip_on_cran()

    root <- withr::local_tempdir("daf-http-smoke-")
    file.create(file.path(root, "hello.txt"))
    h <- start_http_server(root)
    on.exit(stop_http_server(h), add = TRUE)
    bytes <- dafr:::.dafr_http_get(paste0(h$url, "/hello.txt"))
    expect_identical(bytes, raw(0))
})
