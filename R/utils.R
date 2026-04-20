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

.assert_scalar_value <- function(name, value) {
  if (is.null(value)) {
    stop(sprintf("scalar %s value may not be NULL", sQuote(name)), call. = FALSE)
  }
  if (!is.atomic(value)) {
    stop(sprintf("scalar %s value must be an atomic scalar", sQuote(name)), call. = FALSE)
  }
  if (length(value) != 1L) {
    stop(sprintf("scalar %s value must have length 1 (got %d)", sQuote(name), length(value)), call. = FALSE)
  }
  if (is.na(value)) {
    stop(sprintf("scalar %s value may not be NA", sQuote(name)), call. = FALSE)
  }
  invisible()
}

.validate_vector_value <- function(daf, axis, name, vec) {
  if (is.null(vec) || !is.atomic(vec)) {
    stop(sprintf("vector %s on axis %s must be atomic", sQuote(name), sQuote(axis)),
         call. = FALSE)
  }
  n <- format_axis_length(daf, axis)
  if (!is.null(names(vec))) {
    entries <- format_axis_array(daf, axis)
    missing <- setdiff(names(vec), entries)
    if (length(missing)) {
      stop(sprintf("vector %s has names not in axis %s: %s",
                   sQuote(name), sQuote(axis),
                   paste(sQuote(missing), collapse = ", ")),
           call. = FALSE)
    }
    if (length(vec) != n) {
      stop(sprintf("vector %s has length %d (expected %d) on axis %s",
                   sQuote(name), length(vec), n, sQuote(axis)),
           call. = FALSE)
    }
    # Reorder to axis order; drop names but preserve class (e.g. bit64).
    vec <- vec[entries]
    names(vec) <- NULL
  } else {
    if (length(vec) != n) {
      stop(sprintf("vector %s has length %d (expected %d) on axis %s",
                   sQuote(name), length(vec), n, sQuote(axis)),
           call. = FALSE)
    }
  }
  vec
}
