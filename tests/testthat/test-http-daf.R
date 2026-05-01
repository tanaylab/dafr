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
