#!/usr/bin/env julia
# Test the hypothesis: is the 650ms get_query cost actually JIT compilation
# masquerading as runtime?
#
# Three signals that distinguish JIT from real per-call cost:
#   1. First call is much slower than subsequent calls (compilation).
#   2. Without empty_cache!, the second call hits the cache and is ~0.
#   3. With empty_cache!, every call is the same (real per-call cost).

using DataAxesFormats, SparseArrays, Statistics, BenchmarkTools, LinearAlgebra, Printf
BLAS.set_num_threads(1)

const FIXTURE = "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/benchmarks/fixture/data/big_sparse"
const QUERY   = "@ row @ col :: value >- Sum"

daf = FilesDaf(FIXTURE, "r"; name = "big_sparse")

println("=" ^ 60)
println("Test 1: cold-vs-hot WITHOUT empty_cache! (cache hits allowed)")
println("If JIT/compilation is the cost, t1 >> t2 ≈ t3 ≈ ...")
println("If framework is the cost, t1 ≈ huge once, then t2..tN ≈ 0 (cache hit)")
println("=" ^ 60)
for i in 1:6
    t = @elapsed get_query(daf, QUERY)
    @printf "call %d: %8.2f ms\n" i t*1000
end

println()
println("=" ^ 60)
println("Test 2: with empty_cache! between every call (no cache hits)")
println("If JIT, first call >> rest. If framework, all calls ≈ same.")
println("=" ^ 60)
for i in 1:6
    empty_cache!(daf)
    t = @elapsed get_query(daf, QUERY)
    @printf "call %d: %8.2f ms\n" i t*1000
end

println()
println("=" ^ 60)
println("Test 3: cost of empty_cache! itself")
println("=" ^ 60)
for i in 1:5
    t = @elapsed empty_cache!(daf)
    @printf "empty_cache! %d: %8.3f ms\n" i t*1000
end

println()
println("=" ^ 60)
println("Test 4: BenchmarkTools view — does it agree?")
println("=" ^ 60)
b = @benchmark begin
    empty_cache!($daf)
    get_query($daf, $QUERY)
end samples = 30 evals = 1
show(stdout, MIME"text/plain"(), b); println()

# Also: minimum vs median is the most JIT-sensitive comparison. If JIT
# is significant, the *minimum* will be much smaller than the median
# (one outlier from first compile) — except BenchmarkTools warms up so
# this is usually not visible. Min ≈ median means the cost is steady.
@printf "\nmin/median ratio: %.3f  (1.00 = perfectly steady; <0.5 = JIT outlier)\n" minimum(b).time / median(b).time
