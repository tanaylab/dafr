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
#' @examples
#' d <- memory_daf(name = "base")
#' add_axis(d, "cell", c("c1", "c2", "c3"))
#' set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
#' adapter(d,
#'     function(adapted) {
#'         entries <- axis_vector(adapted, "obs")
#'         set_vector(adapted, "obs", "rank", seq_along(entries))
#'     },
#'     input_axes  = list(list("obs", "@ cell"), list("cell", NULL)),
#'     input_data  = VIEW_ALL_VECTORS,
#'     output_axes = list(list("cell", "@ obs"), list("obs", NULL)),
#'     output_data = list(list(ALL_VECTORS, NULL), list(c("cell", "rank"), "="))
#' )
#' get_vector(d, "cell", "rank")
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
    copy_all(
        destination = daf, source = output,
        empty = empty, relayout = relayout, overwrite = overwrite,
        insist = FALSE
    )
    result
}
