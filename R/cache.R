# Cache skeleton for dafr.
#
# Three tiers:
# - mapped : ALTREP mmap views (populated by FilesDaf in Slice 2)
# - memory : materialized R vectors
# - query  : query-execution results (populated in Slice 4)
#
# Full LRU + heap-cap + version-stamp invalidation lands in Slice 1/4.
# Slice 0 ships the storage layout and the simple operations.

# ---- Key formatters (canonical cache-key string builders) ----
cache_key_scalar <- function(name)      paste0("scalar:", name)
cache_key_axis   <- function(axis)      paste0("axis:", axis)
cache_key_vector <- function(axis, name) paste0("vector:", axis, ":", name)
cache_key_matrix <- function(rows_axis, cols_axis, name) {
  paste0("matrix:", rows_axis, ":", cols_axis, ":", name)
}
cache_key_query  <- function(canon)     paste0("query:", canon)

# ---- Low-level tier operations ----
cache_get <- function(cache_env, tier, key) {
  bucket <- cache_env[[tier]]
  if (exists(key, envir = bucket, inherits = FALSE)) {
    return(get(key, envir = bucket, inherits = FALSE))
  }
  NULL
}

cache_put <- function(cache_env, tier, key, value) {
  bucket <- cache_env[[tier]]
  assign(key, value, envir = bucket)
  invisible()
}

cache_remove <- function(cache_env, tier, key) {
  bucket <- cache_env[[tier]]
  if (exists(key, envir = bucket, inherits = FALSE)) {
    rm(list = key, envir = bucket)
  }
  invisible()
}

#' Empty caches on a Daf object.
#'
#' @param daf A `DafReader`/`DafWriter` instance.
#' @param group Character vector; any of `"mapped"`, `"memory"`, `"query"`.
#'   Defaults to all three.
#' @return Invisibly the input `daf`.
#' @export
empty_cache <- function(daf, group = c("mapped", "memory", "query")) {
  group <- match.arg(group, choices = c("mapped", "memory", "query"), several.ok = TRUE)
  cache_env <- S7::prop(daf, "cache")
  for (tier in group) {
    bucket <- cache_env[[tier]]
    rm(list = ls(bucket, all.names = TRUE), envir = bucket)
  }
  invisible(daf)
}

# ---- Version counters (monotonic integers, bumped on mutation) ----
bump_axis_counter <- function(daf, axis) {
  counters <- S7::prop(daf, "axis_version_counter")
  counters[[axis]] <- (counters[[axis]] %||% 0L) + 1L
  invisible()
}

bump_vector_counter <- function(daf, axis, name) {
  counters <- S7::prop(daf, "vector_version_counter")
  key <- paste0(axis, ":", name)
  counters[[key]] <- (counters[[key]] %||% 0L) + 1L
  invisible()
}

bump_matrix_counter <- function(daf, rows_axis, cols_axis, name) {
  counters <- S7::prop(daf, "matrix_version_counter")
  key <- paste0(rows_axis, ":", cols_axis, ":", name)
  counters[[key]] <- (counters[[key]] %||% 0L) + 1L
  invisible()
}
