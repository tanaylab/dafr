#!/usr/bin/env julia
# Drill into the 652ms get_query path: where is the time actually going?
# Self-time hot frames in the existing flat profile point at sort.jl
# searchsortedfirst + Base/int.jl < (a binary search). That smells like
# scalar SparseMatrixCSC getindex. This script re-profiles with a deeper
# tree and prints both flat and tree views at higher resolution so we
# can see what *DAF.jl line* is calling A[i,j].

using DataAxesFormats, SparseArrays, Statistics, BenchmarkTools, LinearAlgebra, Printf
using Profile

BLAS.set_num_threads(1)
const FIXTURE = "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/benchmarks/fixture/data/big_sparse"
const QUERY   = "@ row @ col :: value >- Sum"
const OUTDIR  = joinpath(@__DIR__, "out")
mkpath(OUTDIR)

daf = FilesDaf(FIXTURE, "r"; name = "big_sparse")

# warmup (compile everything)
empty_cache!(daf); get_query(daf, QUERY)
empty_cache!(daf); get_query(daf, QUERY)

# profile a long sample (~10s, 0.5ms interval → ~20k samples)
Profile.clear()
Profile.init(n = 20_000_000, delay = 0.0005)
let t0 = time()
    while time() - t0 < 10.0
        @profile begin
            empty_cache!(daf)
            get_query(daf, QUERY)
        end
    end
end

# Top self-time frames — what's actually burning CPU
open(joinpath(OUTDIR, "deep-flat.txt"), "w") do io
    Profile.print(io; format = :flat, sortedby = :overhead, mincount = 50)
end

# Deep tree, no maxdepth so we see all the way down
open(joinpath(OUTDIR, "deep-tree.txt"), "w") do io
    Profile.print(io; format = :tree, mincount = 50)
end

# Filter view: only frames in DataAxesFormats.jl source
open(joinpath(OUTDIR, "deep-daf-only.txt"), "w") do io
    redirect_stdout(io) do
        Profile.print(format = :tree, mincount = 50)
    end
end

println("wrote dev/scripts/out/deep-{flat,tree,daf-only}.txt")

# Quick visual: what's each of the SparseArrays.getindex sites called from?
println("\n--- top self-time frames (overhead descending) ---")
Profile.print(format = :flat, sortedby = :overhead, mincount = 100)
