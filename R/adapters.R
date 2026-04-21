#' @include classes.R chain_daf.R view_daf.R format_api.R
NULL

#' Apply a computation to a renaming view of a DafWriter.
#'
#' Mirrors Julia `adapter(computation, daf; input_axes, input_data, capture,
#' output_axes, output_data, empty, relayout, overwrite, name)`. The typical
#' use is to run a `@computation` (R: `computation()`-wrapped) function whose
#' expected property names differ from the names stored in `daf`.
#'
#' Flow:
#' 1. `input = viewer(daf, axes = input_axes, data = input_data)` exposes the
#'    subset the computation consumes, possibly under renamed axes / names.
#' 2. `capture = capture_factory(name = "<base>.capture")` is a fresh writable.
#' 3. `adapted = chain_writer(list(input, capture))` — reads fall through to
#'    `input`, writes go to `capture`.
#' 4. `result = fn(adapted)` — the computation's return value.
#' 5. `output = viewer(adapted, axes = output_axes, data = output_data)` —
#'    selects + renames the outputs.
#' 6. Copy `output` into `daf` via an internal helper. Honors `overwrite`,
#'    `relayout`, and `empty`.
#' 7. Return `result`.
#'
#' @param daf A `DafWriter` — the base data to read from and write into.
#' @param fn A function taking a single `DafWriter` argument (the `adapted`
#'   chain). Return value passes through.
#' @param input_axes,input_data Passed through to [viewer()] for the input
#'   view. At least one of these or `output_axes` / `output_data` must be
#'   non-NULL (otherwise `adapter()` degenerates to `fn(daf)`).
#' @param output_axes,output_data Passed through to [viewer()] for the
#'   output view.
#' @param capture Factory function returning a fresh `DafWriter`. Default
#'   `memory_daf`.
#' @param empty Named list `list("<axis>|<vector>" = default, "<r>|<c>|<m>" =
#'   default)` supplying default values for entries present in `daf`'s axis
#'   but absent from the source view's. NULL (default) disables the feature.
#' @param relayout If `TRUE` (default), matrix copies also write the
#'   transposed layout.
#' @param overwrite If `TRUE`, pre-existing destination entries are replaced.
#' @param name Human-readable name for the input/capture/adapted dafs. Default
#'   `".adapter"`.
#' @return The return value of `fn(adapted)`.
#' @seealso [viewer()], [chain_writer()], [computation()].
#' @export
adapter <- function(daf, fn,
                    input_axes = NULL, input_data = NULL,
                    output_axes = NULL, output_data = NULL,
                    capture = memory_daf,
                    empty = NULL, relayout = TRUE, overwrite = FALSE,
                    name = ".adapter") {
    if (!S7::S7_inherits(daf, DafWriter)) {
        stop("`daf` must be a DafWriter", call. = FALSE)
    }
    if (!is.function(fn)) stop("`fn` must be a function", call. = FALSE)
    if (is.null(input_axes) && is.null(input_data) &&
        is.null(output_axes) && is.null(output_data)) {
        stop("no-op adapter: at least one of input_axes/input_data/output_axes/output_data required",
             call. = FALSE)
    }

    base_name <- S7::prop(daf, "name")
    input <- viewer(daf,
        name = paste0(base_name, name, ".input"),
        axes = input_axes, data = input_data
    )
    captured <- capture(name = paste0(base_name, name, ".capture"))
    adapted <- chain_writer(
        list(input, captured),
        name = paste0(base_name, name, ".adapted")
    )

    result <- fn(adapted)

    output <- viewer(adapted,
        name = paste0(base_name, name, ".output"),
        axes = output_axes, data = output_data
    )
    .copy_view_to_daf(
        source_view = output, dest = daf,
        empty = empty, relayout = relayout, overwrite = overwrite
    )
    result
}

.copy_view_to_daf <- function(source_view, dest, empty = NULL,
                              relayout = TRUE, overwrite = FALSE) {
    for (nm in format_scalars_set(source_view)) {
        format_set_scalar(dest, nm, format_get_scalar(source_view, nm),
                          overwrite)
    }

    axis_mode <- list()
    for (ax in format_axes_set(source_view)) {
        src_entries <- format_axis_array(source_view, ax)
        if (!format_has_axis(dest, ax)) {
            format_add_axis(dest, ax, src_entries)
            axis_mode[[ax]] <- "new"
            next
        }
        dest_entries <- format_axis_array(dest, ax)
        if (length(src_entries) == length(dest_entries) &&
            identical(src_entries, dest_entries)) {
            axis_mode[[ax]] <- "replace"
            next
        }
        if (all(src_entries %in% dest_entries)) {
            axis_mode[[ax]] <- "pad"
            next
        }
        if (!overwrite) {
            stop(sprintf(
                "axis %s already exists in destination (not a superset of the source)",
                sQuote(ax)
            ), call. = FALSE)
        }
        format_delete_axis(dest, ax, must_exist = TRUE)
        format_add_axis(dest, ax, src_entries)
        axis_mode[[ax]] <- "new"
    }

    for (ax in format_axes_set(source_view)) {
        src_entries <- format_axis_array(source_view, ax)
        dest_entries <- format_axis_array(dest, ax)
        for (vn in format_vectors_set(source_view, ax)) {
            val <- format_get_vector(source_view, ax, vn)
            mode_ <- axis_mode[[ax]]
            if (identical(mode_, "pad")) {
                key <- paste(ax, vn, sep = "|")
                default <- if (is.null(empty)) NULL else empty[[key]]
                if (is.null(default)) {
                    stop(sprintf(
                        "missing empty value for pad-mode vector: %s",
                        key
                    ), call. = FALSE)
                }
                full <- rep(default, length(dest_entries))
                names(full) <- dest_entries
                idx <- match(src_entries, dest_entries)
                full[idx] <- val
                format_set_vector(dest, ax, vn, full, overwrite)
            } else {
                format_set_vector(dest, ax, vn, val, overwrite)
            }
        }
    }

    axes <- format_axes_set(source_view)
    for (ra in axes) {
        for (ca in axes) {
            for (mn in format_matrices_set(source_view, ra, ca)) {
                val <- format_get_matrix(source_view, ra, ca, mn)
                mode_ra <- axis_mode[[ra]]
                if (is.null(mode_ra)) mode_ra <- "new"
                mode_ca <- axis_mode[[ca]]
                if (is.null(mode_ca)) mode_ca <- "new"
                if (identical(mode_ra, "pad") || identical(mode_ca, "pad")) {
                    key <- paste(ra, ca, mn, sep = "|")
                    default <- if (is.null(empty)) NULL else empty[[key]]
                    if (is.null(default)) {
                        stop(sprintf(
                            "missing empty value for pad-mode matrix: %s",
                            key
                        ), call. = FALSE)
                    }
                    dest_ra_entries <- format_axis_array(dest, ra)
                    dest_ca_entries <- format_axis_array(dest, ca)
                    full <- matrix(default,
                        nrow = length(dest_ra_entries),
                        ncol = length(dest_ca_entries),
                        dimnames = list(dest_ra_entries, dest_ca_entries)
                    )
                    idx_r <- match(
                        format_axis_array(source_view, ra),
                        dest_ra_entries
                    )
                    idx_c <- match(
                        format_axis_array(source_view, ca),
                        dest_ca_entries
                    )
                    full[idx_r, idx_c] <- as.matrix(val)
                    format_set_matrix(dest, ra, ca, mn, full, overwrite)
                } else {
                    format_set_matrix(dest, ra, ca, mn, val, overwrite)
                }
                if (relayout && ra != ca) {
                    format_relayout_matrix(dest, ra, ca, mn)
                }
            }
        }
    }
    invisible()
}
