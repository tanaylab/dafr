`%||%` <- function(a, b) if (is.null(a)) b else a

.FORBIDDEN_NAME_CHARS <- "[/\\\\:,\n\r\t\\x00]"

# Scalar-character argument guard used by the user-facing wrappers in
# readers.R / writers.R. Kept here (not inlined via stopifnot) so error
# messaging can be tuned in one place.
.assert_name <- function(value, arg) {
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("`%s` must be a non-NA character scalar", arg), call. = FALSE)
  }
  if (!nzchar(value)) {
    stop(sprintf("`%s` may not be empty", arg), call. = FALSE)
  }
  if (value != trimws(value)) {
    stop(sprintf("`%s` may not have leading/trailing whitespace: %s",
                 arg, sQuote(value)), call. = FALSE)
  }
  if (grepl(.FORBIDDEN_NAME_CHARS, value, perl = TRUE)) {
    stop(sprintf("`%s` contains forbidden character(s): %s", arg, sQuote(value)),
         call. = FALSE)
  }
  invisible()
}

# Scalar-logical argument guard — used for overwrite / must_exist / etc.
.assert_flag <- function(value, arg) {
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop(sprintf("`%s` must be a non-NA logical scalar", arg),
         call. = FALSE)
  }
  invisible()
}
