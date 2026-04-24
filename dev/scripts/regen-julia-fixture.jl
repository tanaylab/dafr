using DataAxesFormats, SparseArrays

root = joinpath(@__DIR__, "..", "..", "tests", "testthat",
                "fixtures", "julia-filesdaf")
isdir(root) && rm(root; recursive = true)
mkpath(dirname(root))

daf = FilesDaf(root, "w")

add_axis!(daf, "cell", ["A", "B", "C", "D"])
add_axis!(daf, "gene", ["X", "Y"])

set_scalar!(daf, "pi",    3.14)
set_scalar!(daf, "cells", Int64(100))
set_scalar!(daf, "note",  "hello")

set_vector!(daf, "cell", "donor", Int32[1, 2, 3, 4])

set_vector!(daf, "cell", "sparse_x",
            SparseVector{Float64, UInt32}(4, UInt32[2, 4],
                                          Float64[10.0, 30.0]))

# All-true sparse Bool (.nzval omitted on disk)
set_vector!(daf, "cell", "flags",
            SparseVector{Bool, UInt32}(4, UInt32[1, 3], Bool[true, true]))

set_matrix!(daf, "cell", "gene", "dense_m",
            Float64[1 5;
                    2 6;
                    3 7;
                    4 8])

set_matrix!(daf, "cell", "gene", "sparse_m",
            sparse(Int32[1, 3, 2],
                   Int32[1, 1, 2],
                   Float64[10.0, 20.0, 30.0], 4, 2))

set_matrix!(daf, "cell", "gene", "mask",
            sparse(Int32[1, 2],
                   Int32[1, 2],
                   Bool[true, true], 4, 2))

println("wrote fixture to $root")
