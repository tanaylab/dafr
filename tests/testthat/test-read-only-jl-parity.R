# Literal port of read_only.jl into R.
#
# Julia's read_only.jl tests SparseArrays.ReadOnly, NamedArray,
# permute_dims, transpose, and adjoint - all Julia AbstractArray
# wrappers, plus is_read_only_array / read_only_array / brief / copy
# introspection. R has none of these as first-class concepts:
#   - R matrices are not wrapped types; there is no `ReadOnly`
#   - R uses dimnames on matrices, not NamedArray wrappers
#   - R has t() for transpose; no permute_dims / adjoint generics
#   - R has no is_read_only_array equivalent
#
# Skipping the entire file with CR_ARRAY divergence. dafr's daf-level
# `read_only(daf)` (a different API - the "read-only daf wrapper")
# is exercised in test-read-only-daf.R and the chain/views slices.

test_that("read_only / array internals skipped", {
    skip("R divergence CR_ARRAY: read_only.jl tests Julia AbstractArray internals (SparseArrays.ReadOnly, NamedArray, permute_dims, transpose, adjoint, is_read_only_array). R has no analogous wrappers; the substantive 'read-only daf' API is covered separately. The 86 nested_test leaves of read_only.jl all reduce to this single divergence.")
})
