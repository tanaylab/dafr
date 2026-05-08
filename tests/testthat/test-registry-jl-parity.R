# Literal port of registry.jl. 2 leaves, both skipped.
#
# Julia tests register_query_operation()'s conflicting-registration error
# path. dafr's query operations are baked into the parser at package
# load time and are not user-extensible; there is no register_query_operation
# function. Skipped with C-class divergence.

test_that("registry / conflicting / eltwise", {
    skip("R divergence CR1: dafr's query operations are not user-extensible. There is no register_query_operation function; operations like Abs/Sum are added to the parser at package load. The Julia conflicting-registration error has no analogue.")
})

test_that("registry / conflicting / reduction", {
    skip("R divergence CR1: dafr's query operations are not user-extensible.")
})
