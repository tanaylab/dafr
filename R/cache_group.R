# Cache-group classifications returned by every `format_get_*` /
# `format_set_*` method. Mirrors `DataAxesFormats.jl::Formats::CacheGroup`
# (commit 49fbba1). Three constants: where to cache the just-read /
# just-written value.
#
# Internally these map to the existing 3-tier cache in `R/cache.R`
# (`mapped`, `memory`, `query`). Mapping handled by `.canonical_tier()`.

#' Cache-group constants.
#'
#' Returned by `format_get_*` / `format_set_*` backend methods to
#' classify how a value should be cached. Three constants:
#'
#' - `MEMORY_DATA` — value is an in-memory R object; cache at the
#'   `"memory"` tier (subject to LRU + memory cap).
#' - `MAPPED_DATA` — value is an mmap-backed view (zero-copy);
#'   cache at the `"mapped"` tier (uncapped, no LRU).
#' - `QUERY_DATA` — value is a query-evaluation intermediate;
#'   cache at the `"query"` tier (subject to LRU + memory cap).
#'
#' These names mirror upstream `DataAxesFormats.jl`'s `CacheGroup`
#' enum. The runtime tier names (`"mapped"` / `"memory"` /
#' `"query"`) accept both the constant form and the lowercase form
#' via [empty_cache()].
#'
#' @return Character scalar.
#' @examples
#' MEMORY_DATA
#' MAPPED_DATA
#' QUERY_DATA
#' @name cache_group_constants
NULL

#' @rdname cache_group_constants
#' @export
MEMORY_DATA <- "MemoryData"

#' @rdname cache_group_constants
#' @export
MAPPED_DATA <- "MappedData"

#' @rdname cache_group_constants
#' @export
QUERY_DATA <- "QueryData"

# ---- Internal helpers ------------------------------------------------------

# Wrap a value with its cache_group classification. Standard return
# shape from every `format_get_*` method.
.cache_group_value <- function(value, cache_group) {
    list(value = value, cache_group = cache_group)
}

# Validate that `x` is one of the three constants. Used in tests and
# defensive call paths. Returns TRUE on success; throws otherwise.
.is_cache_group <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) &&
        x %in% c(MEMORY_DATA, MAPPED_DATA, QUERY_DATA)
}

.assert_cache_group <- function(x, name = "cache_group") {
    if (!.is_cache_group(x)) {
        stop(sprintf(
            "%s must be one of MEMORY_DATA / MAPPED_DATA / QUERY_DATA; got %s",
            name, sQuote(as.character(x))
        ), call. = FALSE)
    }
    invisible(TRUE)
}
