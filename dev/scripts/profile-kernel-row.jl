#!/usr/bin/env julia
# Companion to profile-kernel-sum-col.jl: probe the *row* direction so we
# can tell whether dafr's 15-37× row-reduction wins come from a faster
# kernel or just a leaner framework (as we found for the column direction).
#
# For each of {sum_row, var_row, median_row}:
#   (a) raw Julia kernel on the underlying SparseMatrixCSC
#       (uses SparseArrays' specialized path where it exists)
#   (b) full DAF.jl query
#
# Usage:
#   julia --project=benchmarks/julia dev/scripts/profile-kernel-row.jl

using DataAxesFormats
using BenchmarkTools
using LinearAlgebra
using SparseArrays
using Statistics
using Printf

BLAS.set_num_threads(1)

const FIXTURE = joinpath(@__DIR__, "..", "..", "benchmarks", "fixture", "data", "big_sparse")
const OUTDIR  = joinpath(@__DIR__, "out")
mkpath(OUTDIR)

println("opening fixture: $FIXTURE")
daf = FilesDaf(FIXTURE, "r"; name = "big_sparse")

function unwrap_sparse(x)
    cur = x
    while !isa(cur, SparseMatrixCSC)
        p = parent(cur)
        p === cur && break
        cur = p
    end
    return cur
end
A = unwrap_sparse(get_matrix(daf, "row", "col", "value"))
@assert isa(A, SparseMatrixCSC)
@printf "matrix: %d × %d, nnz = %d (%.2f%% density)\n\n" size(A,1) size(A,2) nnz(A) 100*nnz(A)/(size(A,1)*size(A,2))

# Pure-Julia row-reductions on a SparseMatrixCSC.
# - sum/maximum have specialized fast paths in SparseArrays for dims=2.
# - var via Statistics over a sparse matrix goes through generic mapreduce.
# - median per row has no fast path; mapslices densifies each row.
row_sum(M)    = vec(sum(M; dims = 2))
row_var(M)    = vec(var(M; dims = 2))
row_median(M) = [median(view(M, i, :)) for i in 1:size(M,1)]   # pessimal but typical

queries = [
    ("sum_row",    "@ row @ col :: value >| Sum",        row_sum),
    ("var_row",    "@ row @ col :: value >| Var",        row_var),
    ("median_row", "@ row @ col :: value >| Median",     row_median),
]

# warmup all paths
println("warmup...")
for (_, q, fn) in queries
    fn(A)
    empty_cache!(daf); get_query(daf, q)
end

results = Vector{NamedTuple}()
for (name, q, fn) in queries
    println("\n--- $name ---")
    bk = @benchmark $fn($A) samples = 20 seconds = 5 evals = 1
    println("raw kernel:")
    show(stdout, MIME"text/plain"(), bk); println()

    bq = @benchmark begin
        empty_cache!($daf)
        get_query($daf, $q)
    end samples = 20 seconds = 5 evals = 1
    println("full query:")
    show(stdout, MIME"text/plain"(), bq); println()

    push!(results, (
        name        = name,
        kernel_ms   = median(bk).time / 1e6,
        query_ms    = median(bq).time / 1e6,
        ratio       = median(bq).time / median(bk).time,
        kernel_alloc = bk.allocs,
        query_alloc  = bq.allocs,
    ))
end

println("\n\n========== SUMMARY ==========")
@printf "%-12s %12s %12s %10s %10s %10s\n" "query" "kernel(ms)" "query(ms)" "framework×" "k_allocs" "q_allocs"
for r in results
    @printf "%-12s %12.2f %12.2f %10.1f %10d %10d\n" r.name r.kernel_ms r.query_ms r.ratio r.kernel_alloc r.query_alloc
end

open(joinpath(OUTDIR, "row-summary.txt"), "w") do io
    @printf io "%-12s %12s %12s %10s %10s %10s\n" "query" "kernel(ms)" "query(ms)" "framework×" "k_allocs" "q_allocs"
    for r in results
        @printf io "%-12s %12.2f %12.2f %10.1f %10d %10d\n" r.name r.kernel_ms r.query_ms r.ratio r.kernel_alloc r.query_alloc
    end
end
println("\nwrote $(joinpath(OUTDIR, "row-summary.txt"))")
