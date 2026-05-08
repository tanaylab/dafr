# Format API — unexported S7 generics mirroring the Julia Formats module.
# A backend implements these; user-facing Readers/Writers call through.
#
# Naming: format_<verb>[_<object>]. Each argument order matches the
# corresponding Julia function signature.

# ---- Scalars (Julia: Formats.format_has_scalar, ..) ----
format_has_scalar <- S7::new_generic("format_has_scalar", c("daf", "name"))
format_get_scalar <- S7::new_generic("format_get_scalar", c("daf", "name"))
format_set_scalar <- S7::new_generic("format_set_scalar", c("daf", "name", "value", "overwrite"))
format_delete_scalar <- S7::new_generic("format_delete_scalar", c("daf", "name", "must_exist"))
format_scalars_set <- S7::new_generic("format_scalars_set", "daf")

# ---- Axes ----
format_has_axis <- S7::new_generic("format_has_axis", c("daf", "axis"))
format_add_axis <- S7::new_generic("format_add_axis", c("daf", "axis", "entries"))
format_delete_axis <- S7::new_generic("format_delete_axis", c("daf", "axis", "must_exist"))
format_axes_set <- S7::new_generic("format_axes_set", "daf")
format_axis_array <- S7::new_generic("format_axis_array", c("daf", "axis"))
format_axis_length <- S7::new_generic("format_axis_length", c("daf", "axis"))
format_axis_dict <- S7::new_generic("format_axis_dict", c("daf", "axis"))

# ---- Vectors (per-axis namespace) ----
format_has_vector <- S7::new_generic("format_has_vector", c("daf", "axis", "name"))
format_get_vector <- S7::new_generic("format_get_vector", c("daf", "axis", "name"))
format_set_vector <- S7::new_generic("format_set_vector", c("daf", "axis", "name", "vec", "overwrite"))
format_delete_vector <- S7::new_generic("format_delete_vector", c("daf", "axis", "name", "must_exist"))
format_vectors_set <- S7::new_generic("format_vectors_set", c("daf", "axis"))

# ---- Matrices (ordered-pair-of-axes namespace, CSC canonical) ----
format_has_matrix <- S7::new_generic(
    "format_has_matrix",
    c("daf", "rows_axis", "columns_axis", "name")
)
format_get_matrix <- S7::new_generic(
    "format_get_matrix",
    c("daf", "rows_axis", "columns_axis", "name")
)
format_set_matrix <- S7::new_generic(
    "format_set_matrix",
    c("daf", "rows_axis", "columns_axis", "name", "mat", "overwrite")
)
format_delete_matrix <- S7::new_generic(
    "format_delete_matrix",
    c("daf", "rows_axis", "columns_axis", "name", "must_exist")
)
format_matrices_set <- S7::new_generic(
    "format_matrices_set",
    c("daf", "rows_axis", "columns_axis")
)
format_relayout_matrix <- S7::new_generic(
    "format_relayout_matrix",
    c("daf", "rows_axis", "columns_axis", "name")
)

# ---- Reorder (Julia: Reorder.format_replace_reorder!, etc.) ----
format_replace_reorder <- S7::new_generic(
    "format_replace_reorder",
    c("daf", "plan"),
    function(daf, plan, crash_counter = NULL) {
        S7::S7_dispatch()
    }
)
format_cleanup_reorder <- S7::new_generic(
    "format_cleanup_reorder",
    c("daf", "plan"),
    function(daf, plan, crash_counter = NULL) {
        S7::S7_dispatch()
    }
)
format_reset_reorder <- S7::new_generic(
    "format_reset_reorder",
    c("daf"),
    function(daf, crash_counter = NULL) {
        S7::S7_dispatch()
    }
)

# ---- Description header (Julia: Formats.format_description_header) ----
# Returns a character vector of header lines (each prefixed with `indent`)
# that `description()` emits between the `name:` line and the structural
# sections. Default emits just `<indent>type: <ClassName>`; per-format
# overrides add storage-specific identifiers (path/mode/url).
format_description_header <- S7::new_generic(
    "format_description_header",
    "daf",
    function(daf, indent = "", deep = FALSE) {
        S7::S7_dispatch()
    }
)

#' Is `daf` a leaf storage format?
#'
#' Returns `TRUE` for storage classes that own their on-disk or in-memory
#' state directly (`MemoryDaf`, `FilesDaf` / `FilesDafReadOnly`, `ZarrDaf`
#' / `ZarrDafReadOnly`, `HttpDaf`) and `FALSE` for wrappers
#' (`ReadOnlyChainDaf`, `WriteChainDaf`, `ContractDaf`, `ViewDaf`). Used
#' by [reorder_axes()] to reject non-leaf inputs because permuting
#' indices is only meaningful on the underlying storage. Mirrors
#' upstream Julia `Readers.is_leaf`.
#'
#' @param daf A [DafReader].
#' @param ... Reserved for method-specific extensions.
#' @return Logical scalar.
#' @examples
#' is_leaf(memory_daf())
#' @export
.is_leaf_dispatch <- S7::new_generic(".is_leaf_dispatch", "daf")

# Names of concrete leaf daf classes. Abstract classes (DafReader,
# DafWriter, DafReadOnly) are non-leaf; everything that has a
# `.is_leaf_dispatch` method on its instance is a leaf.
.LEAF_DAF_CLASS_NAMES <- c(
    "MemoryDaf",
    "FilesDaf", "FilesDafReadOnly",
    "ZarrDaf", "ZarrDafReadOnly",
    "HttpDaf"
)

is_leaf <- function(daf) {
    # Class-level dispatch (Julia parity: `is_leaf(MemoryDaf)`):
    # accept an S7 class object directly. A leaf class is any concrete
    # subclass of DafReader; abstract classes are non-leaf.
    if (inherits(daf, "S7_class")) {
        return(attr(daf, "name") %in% .LEAF_DAF_CLASS_NAMES)
    }
    .is_leaf_dispatch(daf)
}
