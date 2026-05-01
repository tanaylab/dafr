# Internal httr2 wrapper used by HttpDaf and HttpStore. Applies dafr's
# timeout + no-retry defaults uniformly. Errors are translated to the
# upstream-style message shapes ("HTTP GET failed for: <url>" / "HTTP GET
# returned status N for: <url>") so error-message tests match
# DataAxesFormats.jl::HttpDaf.

.dafr_http_timeout <- function() {
    opt <- getOption("dafr.http_timeout", NULL)
    if (!is.null(opt)) return(as.numeric(opt))
    env <- Sys.getenv("DAFR_HTTP_TIMEOUT", "")
    if (nzchar(env)) return(as.numeric(env))
    30
}

.dafr_http_get <- function(url, allow_404 = FALSE) {
    req <- httr2::request(url) |>
        httr2::req_timeout(.dafr_http_timeout()) |>
        httr2::req_retry(max_tries = 1L) |>
        httr2::req_error(is_error = function(resp) FALSE)

    resp <- tryCatch(
        httr2::req_perform(req),
        error = function(e) {
            stop(sprintf("HTTP GET failed for: %s\nunderlying error: %s",
                         url, conditionMessage(e)),
                 call. = FALSE)
        }
    )
    status <- httr2::resp_status(resp)
    if (status == 404 && allow_404) return(NULL)
    if (status != 200) {
        stop(sprintf("HTTP GET returned status %d for: %s", status, url),
             call. = FALSE)
    }
    httr2::resp_body_raw(resp)
}

.dafr_http_head <- function(url) {
    req <- httr2::request(url) |>
        httr2::req_method("HEAD") |>
        httr2::req_timeout(.dafr_http_timeout()) |>
        httr2::req_retry(max_tries = 1L) |>
        httr2::req_error(is_error = function(resp) FALSE)
    resp <- tryCatch(
        httr2::req_perform(req),
        error = function(e) NULL
    )
    if (is.null(resp)) return(FALSE)
    httr2::resp_status(resp) == 200
}
