#' @include files_daf.R
NULL

# Write axes/metadata.json: sorted JSON array of axis names currently present
# in axes/*.txt. Mirrors upstream files_format.jl::write_axes_metadata!.
.write_axes_metadata <- function(path) {
    axes_dir <- file.path(path, "axes")
    axes <- character(0)
    if (dir.exists(axes_dir)) {
        files <- list.files(axes_dir, pattern = "\\.txt$", full.names = FALSE)
        axes <- sort(sub("\\.txt$", "", files))
    }
    writeLines(jsonlite::toJSON(axes, auto_unbox = FALSE),
               con = file.path(axes_dir, "metadata.json"))
    invisible()
}
