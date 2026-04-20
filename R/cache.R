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
    if (.is_capped_tier(tier)) {
      cache_env$lru <- cache_env$lru[!startsWith(cache_env$lru, paste0(tier, ":"))]
    }
  }
  # Recompute bytes from what's left in the capped tiers.
  total <- 0
  for (t in c("memory", "query")) {
    bucket <- cache_env[[t]]
    for (k in ls(bucket, all.names = TRUE)) {
      total <- total + get(k, envir = bucket, inherits = FALSE)$size
    }
  }
  cache_env$bytes <- total
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

# ---- Version stamps (computed from counters) --------------------------------

axis_stamp <- function(daf, axis) {
  S7::prop(daf, "axis_version_counter")[[axis]] %||% 0L
}

vector_stamp <- function(daf, axis, name) {
  vc <- S7::prop(daf, "vector_version_counter")
  c(axis_stamp(daf, axis),
    vc[[paste0(axis, ":", name)]] %||% 0L)
}

matrix_stamp <- function(daf, rows_axis, cols_axis, name) {
  mc <- S7::prop(daf, "matrix_version_counter")
  c(axis_stamp(daf, rows_axis),
    axis_stamp(daf, cols_axis),
    mc[[paste0(rows_axis, ":", cols_axis, ":", name)]] %||% 0L)
}

# ---- Cache with LRU + memory-cap (applies to memory + query tiers) ----------

.cache_default_cap <- function() {
  mb <- dafr_opt("dafr.cache.memory_mb")
  as.numeric(mb) * 1024 * 1024
}

cache_set_cap <- function(cache_env, bytes) {
  cache_env$cap <- as.numeric(bytes)
  .cache_evict(cache_env)
  invisible()
}

.is_capped_tier <- function(tier) tier %in% c("memory", "query")

.lru_key <- function(tier, key) paste0(tier, ":", key)

.lru_touch <- function(cache_env, tier, key) {
  k <- .lru_key(tier, key)
  cache_env$lru <- c(setdiff(cache_env$lru, k), k)
}

.lru_drop <- function(cache_env, tier, key) {
  cache_env$lru <- setdiff(cache_env$lru, .lru_key(tier, key))
}

.cache_evict <- function(cache_env) {
  # Never evict the MRU entry (tail): if a single oversized entry is all
  # that remains, it must stay — the "oversized entries still stored" contract.
  while (cache_env$bytes > cache_env$cap && length(cache_env$lru) > 1L) {
    victim <- cache_env$lru[[1L]]
    cache_env$lru <- cache_env$lru[-1L]
    # Split "tier:key" — the tier is the literal "memory" or "query";
    # the key itself may contain colons so we only split on the first one.
    sep_pos <- regexpr(":", victim)
    tier    <- substr(victim, 1L, sep_pos - 1L)
    key     <- substr(victim, sep_pos + 1L, nchar(victim))
    bucket  <- cache_env[[tier]]
    if (exists(key, envir = bucket, inherits = FALSE)) {
      entry <- get(key, envir = bucket, inherits = FALSE)
      cache_env$bytes <- cache_env$bytes - entry$size
      rm(list = key, envir = bucket)
    }
  }
}

# ---- Cache entries with version stamps --------------------------------------

cache_lookup <- function(cache_env, tier, key, expected_stamp) {
  bucket <- cache_env[[tier]]
  if (!exists(key, envir = bucket, inherits = FALSE)) return(NULL)
  entry <- get(key, envir = bucket, inherits = FALSE)
  if (!identical(entry$stamp, expected_stamp)) {
    if (.is_capped_tier(tier)) {
      cache_env$bytes <- cache_env$bytes - entry$size
      .lru_drop(cache_env, tier, key)
    }
    rm(list = key, envir = bucket)
    return(NULL)
  }
  if (.is_capped_tier(tier)) .lru_touch(cache_env, tier, key)
  entry$value
}

cache_store <- function(cache_env, tier, key, value, stamp, size_bytes = 0L) {
  size_bytes <- as.numeric(size_bytes)
  bucket <- cache_env[[tier]]
  if (exists(key, envir = bucket, inherits = FALSE) && .is_capped_tier(tier)) {
    old <- get(key, envir = bucket, inherits = FALSE)
    cache_env$bytes <- cache_env$bytes - old$size
  }
  assign(key,
         list(value = value, stamp = stamp, size = size_bytes),
         envir = bucket)
  if (.is_capped_tier(tier)) {
    cache_env$bytes <- cache_env$bytes + size_bytes
    .lru_touch(cache_env, tier, key)
    .cache_evict(cache_env)
  }
  invisible()
}
