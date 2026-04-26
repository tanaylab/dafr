#!/usr/bin/env julia
# Per-reason evidence for the perf gap between DataAxesFormats.jl and dafr.
# For each claimed reason, run the smallest possible measurement that
# proves or disproves it. Outputs a table summarizing each.
#
#   julia --project=benchmarks/julia dev/scripts/profile-reasons-evidence.jl

using DataAxesFormats
using BenchmarkTools
using LinearAlgebra
using SparseArrays
using Statistics
using Printf

BLAS.set_num_threads(1)

const FIXTURE = joinpath(@__DIR__, "..", "..", "benchmarks", "fixture", "data", "big_sparse")
daf = FilesDaf(FIXTURE, "r"; name = "big_sparse")
function unwrap_sparse(x)
    cur = x
    while !isa(cur, SparseMatrixCSC)
        p = parent(cur); p === cur && break
        cur = p
    end
    return cur
end
A = unwrap_sparse(get_matrix(daf, "row", "col", "value"))
@printf "matrix: %d × %d, nnz = %d (%.2f%%)\n\n" size(A,1) size(A,2) nnz(A) 100*nnz(A)/(size(A,1)*size(A,2))

# warmup all paths
println("warmup...")
sum(A; dims = 1)
var(A; dims = 1)
empty_cache!(daf); get_query(daf, "@ row @ col :: value >- Sum")
empty_cache!(daf); get_query(daf, "@ row @ col :: value >- Var")

# === Reason 1: SparseArrays has a specialized dims=1 sum that DAF.jl doesn't dispatch to ===
println("\n=== R1: SparseArrays specialized sum vs generic-per-column reduce ===")
b1_specialized = @benchmark sum($A; dims = 1) samples = 30 evals = 1
println("(a) sum(A; dims=1)  [SparseArrays specialized]:")
show(stdout, MIME"text/plain"(), b1_specialized); println()

# Generic per-column reduce — the kind of loop DAF.jl's compute_reduction
# falls back to. Iterates eachcol → SparseColumnView → folds with +.
b1_generic = @benchmark [sum(view($A, :, j)) for j in 1:size($A, 2)] samples = 30 evals = 1
println("(b) [sum(view(A,:,j)) for j in 1:n]  [generic per-column]:")
show(stdout, MIME"text/plain"(), b1_generic); println()

# === Reason 2: DAF.jl wraps the kernel in 99.9% framework overhead ===
println("\n=== R2: DAF.jl framework cost vs raw kernel ===")
b2_query = @benchmark begin
    empty_cache!($daf)
    get_query($daf, "@ row @ col :: value >- Sum")
end samples = 20 evals = 1
println("(c) DAF.jl get_query(... >- Sum):")
show(stdout, MIME"text/plain"(), b2_query); println()

# === Reason 3: median densifies (no sparse path) — see profile-kernel-median-col.jl ===
# Already covered in detail there; just record the headline numbers here.

# === Reason 4: variance — does DAF.jl's path beat Statistics.var? Welford? ===
println("\n=== R4: Variance — Statistics.var vs Welford vs DAF.jl ===")
b4_stats = @benchmark var($A; dims = 1) samples = 20 evals = 1
println("(d) Statistics.var(A; dims=1):")
show(stdout, MIME"text/plain"(), b4_stats); println()

function welford_col_var(A::SparseMatrixCSC{Tv}) where Tv
    n_rows = size(A, 1)
    n_cols = size(A, 2)
    out = zeros(Float64, n_cols)
    @inbounds for j in 1:n_cols
        rng = nzrange(A, j)
        # Welford over the implicit (rows-of-zeros + nonzeros) sequence.
        # Trick: the implicit zeros all have the same value, so we can fold
        # them in O(1) instead of looping.
        n   = 0
        mu  = 0.0
        M2  = 0.0
        # Process the (n_rows - length(rng)) implicit zeros as one batch:
        n_zero = n_rows - length(rng)
        if n_zero > 0
            # batch add: mean stays 0, variance contribution is 0
            n  += n_zero
            # mu stays 0, M2 stays 0
        end
        # Now fold the nonzeros one by one
        for k in rng
            x = Float64(A.nzval[k])
            n     += 1
            d      = x - mu
            mu    += d / n
            d2     = x - mu
            M2    += d * d2
        end
        out[j] = M2 / (n - 1)
    end
    return out
end
welford_ref = welford_col_var(A)
stats_ref   = vec(var(A; dims = 1))
@assert isapprox(welford_ref, stats_ref; rtol = 1e-9) "welford disagrees with Statistics.var"

b4_welford = @benchmark welford_col_var($A) samples = 20 evals = 1
println("(e) welford_col_var(A)  [single-pass, sparse-aware]:")
show(stdout, MIME"text/plain"(), b4_welford); println()

b4_query = @benchmark begin
    empty_cache!($daf)
    get_query($daf, "@ row @ col :: value >- Var")
end samples = 20 evals = 1
println("(f) DAF.jl get_query(... >- Var):")
show(stdout, MIME"text/plain"(), b4_query); println()

println("\n========== SUMMARY ==========")
@printf "%-50s %12s %14s\n" "label" "time(ms)" "memory(MiB)"
println("-"^80)
for (label, b) in [
    ("(a) SparseArrays.sum(A; dims=1)",         b1_specialized),
    ("(b) generic [sum(view(A,:,j)) for j in 1:n]", b1_generic),
    ("(c) DAF.jl get_query(... Sum)",           b2_query),
    ("(d) Statistics.var(A; dims=1)",           b4_stats),
    ("(e) welford_col_var(A)",                  b4_welford),
    ("(f) DAF.jl get_query(... Var)",           b4_query),
]
    @printf "%-50s %12.2f %14.2f\n" label median(b).time/1e6 b.memory/1024^2
end
