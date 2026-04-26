# Cache for dafr. Three tiers:
# - mapped : ALTREP mmap views (populated by FilesDaf)
# - memory : materialized R vectors
# - query  : query-execution results

# ---- Verbose user-facing messages (gated on dafr.verbose) ----
.cli_verbose <- function(msg, ...) {
    if (isTRUE(dafr_opt("dafr.verbose"))) {
        cli::cli_inform(c("i" = sprintf(msg, ...)))
    }
    invisible()
}

# ---- Key formatters (canonical cache-key string builders) ----
cache_key_scalar <- function(name) paste0("scalar:", name)
cache_key_axis <- function(axis) paste0("axis:", axis)
cache_key_vector <- function(axis, name) paste0("vector:", axis, ":", name)
cache_key_matrix <- function(rows_axis, columns_axis, name) {
    paste0("matrix:", rows_axis, ":", columns_axis, ":", name)
}
cache_key_query <- function(canon) paste0("query:", canon)

# Low-level tier storage is handled by `cache_store` / `cache_lookup` below.

#' Empty caches on a Daf object.
#'
#' Specify at most one of `clear` or `keep`. Tier names use the short form
#' (`"mapped"`, `"memory"`, `"query"`) or the Julia-style capitalised form
#' (`"MappedData"`, `"MemoryData"`, `"QueryData"`).
#'
#' @param daf A `DafReader`/`DafWriter` instance.
#' @param clear Character vector of tiers to clear (default: all tiers).
#' @param keep Character vector of tiers to keep; all others are cleared.
#' @return Invisibly the input `daf`.
#' @examples
#' d <- example_cells_daf()
#' get_vector(d, "cell", "donor")  # populates memory cache
#' empty_cache(d, clear = "memory")
#' @export
empty_cache <- function(daf, clear = NULL, keep = NULL) {
    all_tiers <- c("mapped", "memory", "query")
    if (!is.null(clear) && !is.null(keep)) {
        stop("specify at most one of `clear`, `keep`", call. = FALSE)
    }
    chosen <- if (!is.null(clear)) {
        .canonical_tier(clear)
    } else if (!is.null(keep)) {
        setdiff(all_tiers, .canonical_tier(keep))
    } else {
        all_tiers
    }
    .cli_verbose(
        "empty_cache on %s tier(s): %s",
        S7::prop(daf, "name"), paste(chosen, collapse = ", ")
    )
    cache_env <- S7::prop(daf, "cache")
    for (tier in chosen) {
        bucket <- cache_env[[tier]]
        rm(list = ls(bucket, all.names = TRUE), envir = bucket)
        if (.is_capped_tier(tier)) {
            cache_env$lru <- cache_env$lru[!startsWith(cache_env$lru, paste0(tier, ":"))]
        }
    }
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

.canonical_tier <- function(x) {
    map <- c(
        mapped     = "mapped", memory     = "memory", query      = "query",
        MappedData = "mapped", MemoryData = "memory", QueryData  = "query"
    )
    out <- map[x]
    if (any(is.na(out))) {
        stop(
            sprintf(
                "unknown cache tier(s): %s",
                paste(sQuote(x[is.na(out)]), collapse = ", ")
            ),
            call. = FALSE
        )
    }
    unname(out)
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

bump_matrix_counter <- function(daf, rows_axis, columns_axis, name) {
    counters <- S7::prop(daf, "matrix_version_counter")
    key <- paste0(rows_axis, ":", columns_axis, ":", name)
    counters[[key]] <- (counters[[key]] %||% 0L) + 1L
    invisible()
}

# ---- Version stamps (computed from counters) --------------------------------

axis_stamp <- function(daf, axis) {
    S7::prop(daf, "axis_version_counter")[[axis]] %||% 0L
}

vector_stamp <- function(daf, axis, name) {
    vc <- S7::prop(daf, "vector_version_counter")
    c(
        axis_stamp(daf, axis),
        vc[[paste0(axis, ":", name)]] %||% 0L
    )
}

matrix_stamp <- function(daf, rows_axis, columns_axis, name) {
    mc <- S7::prop(daf, "matrix_version_counter")
    c(
        axis_stamp(daf, rows_axis),
        axis_stamp(daf, columns_axis),
        mc[[paste0(rows_axis, ":", columns_axis, ":", name)]] %||% 0L
    )
}

# ---- Public version-counter accessors --------------------------------------

#' Per-axis version counter.
#'
#' Returns the monotonic counter for `axis` on `daf`. Incremented on
#' `add_axis` / `delete_axis`. Returns `0L` if the axis has never been
#' mutated (including non-existent axes, to match wrapper semantics).
#'
#' @param daf A [DafReader].
#' @param axis Axis name (character scalar).
#' @return `integer(1)`.
#' @examples
#' d <- memory_daf()
#' axis_version_counter(d, "cell") # 0L
#' add_axis(d, "cell", c("c1", "c2"))
#' axis_version_counter(d, "cell") # 1L
#' @seealso [vector_version_counter()], [matrix_version_counter()]
#' @export
axis_version_counter <- function(daf, axis) {
    S7::prop(daf, "axis_version_counter")[[axis]] %||% 0L
}

#' Per-vector version counter.
#'
#' Returns the monotonic counter for the `name` vector on `axis`.
#' Incremented on `set_vector` / `delete_vector`. Returns `0L` if the
#' vector has never been mutated.
#'
#' @inheritParams axis_version_counter
#' @param name Vector name (character scalar).
#' @return `integer(1)`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' vector_version_counter(d, "cell", "donor") # 0L
#' set_vector(d, "cell", "donor", c("A", "B"))
#' vector_version_counter(d, "cell", "donor") # 1L
#' @export
vector_version_counter <- function(daf, axis, name) {
    key <- paste0(axis, ":", name)
    S7::prop(daf, "vector_version_counter")[[key]] %||% 0L
}

#' Per-matrix version counter.
#'
#' Returns the monotonic counter for the `name` matrix on
#' `(rows_axis, columns_axis)`. Incremented on `set_matrix` /
#' `delete_matrix` / `relayout_matrix`. Returns `0L` if never mutated.
#'
#' @inheritParams axis_version_counter
#' @param rows_axis,columns_axis Axis names.
#' @param name Matrix name.
#' @return `integer(1)`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' add_axis(d, "gene", c("g1", "g2", "g3"))
#' matrix_version_counter(d, "cell", "gene", "UMIs") # 0L
#' @export
matrix_version_counter <- function(daf, rows_axis, columns_axis, name) {
    key <- paste0(rows_axis, ":", columns_axis, ":", name)
    S7::prop(daf, "matrix_version_counter")[[key]] %||% 0L
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
        tier <- substr(victim, 1L, sep_pos - 1L)
        key <- substr(victim, sep_pos + 1L, nchar(victim))
        bucket <- cache_env[[tier]]
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
    if (!exists(key, envir = bucket, inherits = FALSE)) {
        return(NULL)
    }
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
        envir = bucket
    )
    if (.is_capped_tier(tier)) {
        cache_env$bytes <- cache_env$bytes + size_bytes
        .lru_touch(cache_env, tier, key)
        .cache_evict(cache_env)
    }
    invisible()
}
