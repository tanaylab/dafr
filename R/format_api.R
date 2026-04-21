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
