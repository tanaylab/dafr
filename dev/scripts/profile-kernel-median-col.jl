#!/usr/bin/env julia
# Self-contained reproducer for the DataAxesFormats.jl column-median path.
# Compares:
#   (a) raw `vec(median.(eachcol(A)))` on the underlying SparseMatrixCSC
#       (the obvious idiomatic Julia path; densifies each column)
#   (b) DAF.jl `get_query(daf, "@ row @ col :: value >- Median")`
#
# Usage:
#   julia --project=benchmarks/julia dev/scripts/profile-kernel-median-col.jl

using DataAxesFormats
using BenchmarkTools
using LinearAlgebra
using SparseArrays
using Statistics
using Printf

BLAS.set_num_threads(1)

const FIXTURE = joinpath(@__DIR__, "..", "..", "benchmarks", "fixture", "data", "big_sparse")
const QUERY   = "@ row @ col :: value >- Median"

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
@printf "matrix: %d × %d, nnz = %d (%.2f%% density)\n\n" size(A,1) size(A,2) nnz(A) 100*nnz(A)/(size(A,1)*size(A,2))

# warmup
println("warmup...")
vec(median.(eachcol(A)))
empty_cache!(daf); get_query(daf, QUERY)

# Sparse-aware column median: avoids densifying. For each column, sort the
# (small) nonzero buffer and use index arithmetic to skip over the implicit
# zeros. Allocates O(nnz_per_col) per column instead of O(nrows).
function sparse_col_median(A::SparseMatrixCSC{Tv}, j::Integer) where Tv
    n   = size(A, 1)
    rng = nzrange(A, j)
    vals = sort(A.nzval[rng])          # only the nonzeros for this column
    nz_count = length(vals)
    z_count  = n - nz_count
    n_neg    = searchsortedfirst(vals, zero(Tv)) - 1
    @inline val_at(k) = k <= n_neg ? vals[k] :
                        k <= n_neg + z_count ? zero(Tv) :
                                                vals[k - z_count]
    if isodd(n)
        return val_at((n + 1) >>> 1)
    else
        a = val_at(n >>> 1); b = val_at((n >>> 1) + 1)
        return (a + b) / 2
    end
end
sparse_col_medians(A) = [sparse_col_median(A, j) for j in 1:size(A, 2)]

# Validate against the naive path before benchmarking.
ref  = vec(median.(eachcol(A)))
fast = sparse_col_medians(A)
@assert isapprox(ref, fast; atol = 0, rtol = 0) "sparse_col_medians disagrees with naive"
println("validated sparse_col_medians == naive median.(eachcol)\n")

println("\n(a) naive kernel: vec(median.(eachcol(A)))")
b_naive = @benchmark vec(median.(eachcol($A))) samples = 20 seconds = 5 evals = 1
show(stdout, MIME"text/plain"(), b_naive); println()

println("\n(b) sparse-aware kernel: sparse_col_medians(A)")
b_smart = @benchmark sparse_col_medians($A) samples = 20 seconds = 5 evals = 1
show(stdout, MIME"text/plain"(), b_smart); println()

println("\n(c) full DAF.jl query: empty_cache! + get_query(...)")
b_query = @benchmark begin
    empty_cache!($daf)
    get_query($daf, $QUERY)
end samples = 20 seconds = 5 evals = 1
show(stdout, MIME"text/plain"(), b_query); println()

@printf "\nnaive kernel       (median): %8.2f ms,  %d allocs,  %.2f MiB\n"  median(b_naive).time/1e6 b_naive.allocs b_naive.memory/1024^2
@printf "sparse-aware kernel (median): %8.2f ms,  %d allocs,  %.2f MiB\n" median(b_smart).time/1e6 b_smart.allocs b_smart.memory/1024^2
@printf "full DAF.jl query  (median): %8.2f ms,  %d allocs,  %.2f MiB\n"   median(b_query).time/1e6 b_query.allocs b_query.memory/1024^2
@printf "\nDAF.jl / naive    : %.2f×\n" median(b_query).time / median(b_naive).time
@printf "naive / sparse    : %.2f×\n"   median(b_naive).time / median(b_smart).time
@printf "DAF.jl / sparse   : %.2f×\n"   median(b_query).time / median(b_smart).time
