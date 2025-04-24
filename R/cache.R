#' Empty cache in a Daf object
#'
#' @param daf A Daf object
#' @param clear Cache group to clear. Can be one of "MappedData", "MemoryData", or "QueryData".
#' @param keep Cache group to keep. Can be one of "MappedData", "MemoryData", or "QueryData".
#' @return The Daf object
#' @details See the Julia [documentation](https://tanaylab.github.io/DataAxesFormats.jl/v0.1.2/queries.html#DataAxesFormats.Data.empty_cache!) for details.
#' @export
empty_cache <- function(daf, clear = NULL, keep = NULL) {
    validate_daf_object(daf)
    jl_cache_type <- c(
        "MappedData" = "DataAxesFormats.MappedData",
        "MemoryData" = "DataAxesFormats.MemoryData",
        "QueryData" = "DataAxesFormats.QueryData"
    )

    if (!is.null(clear)) {
        clear <- julia_eval(jl_cache_type[clear])
    }
    if (!is.null(keep)) {
        keep <- julia_eval(jl_cache_type[keep])
    }

    julia_call(
        "DataAxesFormats.empty_cache!",
        daf$jl_obj,
        clear = clear,
        keep = keep
    )
    invisible(daf)
}
