#!/usr/bin/env julia
# Profile the Julia side of `kernel_sum_col` to break the 4× gap
# (vs. dafr-native, OMP_NUM_THREADS=1) into:
#
#   (a) raw kernel:    `sum(A; dims=1)` on the underlying SparseMatrixCSC
#   (b) full query:    `get_query(daf, "@ row @ col :: value >- Sum")`
#       framework cost = (b) − (a)
#
# Then dumps a sampling profile of (b) so we can see where the framework
# time actually lives (parser, axis walk, NamedArray wrap, empty_cache!,
# `mapreducedim!`, etc.).
#
# Usage:
#   julia --project=benchmarks/julia dev/scripts/profile-kernel-sum-col.jl
#
# Outputs (to dev/scripts/out/):
#   profile-flat.txt   — top frames by sample count (descending)
#   profile-tree.txt   — call tree (collapsed)
#   summary.txt        — kernel-only vs. full-query timings + allocs

using DataAxesFormats
using BenchmarkTools
using LinearAlgebra
using SparseArrays
using Profile
using Printf

BLAS.set_num_threads(1)

const FIXTURE = joinpath(@__DIR__, "..", "..", "benchmarks", "fixture", "data", "big_sparse")
const QUERY   = "@ row @ col :: value >- Sum"
const OUTDIR  = joinpath(@__DIR__, "out")
mkpath(OUTDIR)

println("opening fixture: $FIXTURE")
daf = FilesDaf(FIXTURE, "r"; name = "big_sparse")

# Pull the raw matrix once so we can time the kernel without any DAF
# plumbing. `get_matrix` returns DAF.jl's wrapped form; `parent()` peels
# any NamedArray / view layers down to the underlying SparseMatrixCSC.
println("fetching raw matrix...")
function unwrap_sparse(x)
    cur = x
    while !isa(cur, SparseMatrixCSC)
        p = parent(cur)
        p === cur && break
        cur = p
    end
    return cur
end
named = get_matrix(daf, "row", "col", "value")
println("  get_matrix returned $(typeof(named))")
A = unwrap_sparse(named)
@assert isa(A, SparseMatrixCSC) "expected SparseMatrixCSC, got $(typeof(A))"
@printf "matrix: %d × %d, nnz = %d (%.2f%% density)\n" size(A,1) size(A,2) nnz(A) 100*nnz(A)/(size(A,1)*size(A,2))

# ---- warmup (JIT compile both paths) ----
println("\nwarmup...")
sum(A; dims = 1)
empty_cache!(daf)
get_query(daf, QUERY)
empty_cache!(daf)
get_query(daf, QUERY)   # second pass: parser cache populated if any

# ---- (a) raw kernel ----
println("\n(a) raw kernel: sum(A; dims=1)")
b_kernel = @benchmark sum($A; dims = 1) samples = 50 seconds = 5 evals = 1
show(stdout, MIME"text/plain"(), b_kernel); println()

# ---- (b) full query path ----
println("\n(b) full query: empty_cache! + get_query(...)")
b_query = @benchmark begin
    empty_cache!($daf)
    get_query($daf, $QUERY)
end samples = 50 seconds = 5 evals = 1
show(stdout, MIME"text/plain"(), b_query); println()

kernel_ms = median(b_kernel).time / 1e6
query_ms  = median(b_query).time  / 1e6
overhead_ms = query_ms - kernel_ms

println()
@printf "kernel-only (median):  %8.2f ms\n" kernel_ms
@printf "full query  (median):  %8.2f ms\n" query_ms
@printf "framework overhead:    %8.2f ms  (%.1f%% of full query)\n" overhead_ms 100*overhead_ms/query_ms

# ---- (c) sampling profile of the full query ----
# Aim for ~thousands of samples so flat counts are meaningful.
# Default sample interval is 1ms; for a ~700ms query we need many calls.
println("\n(c) sampling profile of full query (~5s of samples)...")
Profile.clear()
Profile.init(n = 10_000_000, delay = 0.0005)   # 0.5ms → ~10k samples in 5s
let t0 = time()
    while time() - t0 < 5.0
        @profile begin
            empty_cache!(daf)
            get_query(daf, QUERY)
        end
    end
end

flat_path = joinpath(OUTDIR, "profile-flat.txt")
tree_path = joinpath(OUTDIR, "profile-tree.txt")
println("writing $flat_path")
open(flat_path, "w") do io
    Profile.print(io; format = :flat, sortedby = :count, mincount = 5)
end
println("writing $tree_path")
open(tree_path, "w") do io
    Profile.print(io; format = :tree, mincount = 10, maxdepth = 25)
end

summary_path = joinpath(OUTDIR, "summary.txt")
open(summary_path, "w") do io
    println(io, "Profile of `kernel_sum_col` Julia path")
    println(io, "fixture: $FIXTURE")
    println(io, "query:   $QUERY")
    println(io, "matrix:  $(size(A,1)) × $(size(A,2)), nnz = $(nnz(A))")
    println(io, "Julia:   $(VERSION)")
    println(io, "BLAS:    $(BLAS.get_config())")
    println(io)
    @printf io "kernel-only (median):  %8.2f ms\n" kernel_ms
    @printf io "full query  (median):  %8.2f ms\n" query_ms
    @printf io "framework overhead:    %8.2f ms  (%.1f%% of full query)\n" overhead_ms 100*overhead_ms/query_ms
end
println("\nwrote $summary_path")
println("done.")
