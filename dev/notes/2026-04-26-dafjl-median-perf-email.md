**Subject:** DataAxesFormats.jl: where the perf gap with dafr actually
comes from — two reasons, with reproducers

Hi,

I'm Aviezer Lifshitz from the Tanay lab. I've been writing a native R port
of `DataAxesFormats.jl` (`dafr` — pure R + C++, same on-disk format and DSL),
and while bake-off-ing it against the Julia reference I tracked down where
the wall-time gap on sparse reductions actually comes from. It boils down
to **two** distinct causes — both with clean fixes — and I wanted to share
the reproducers in case they're useful.

Setup for everything below: a 10 000 × 10 000 `SparseMatrixCSC{Float64}`,
5 % density (~5 M nonzeros), `OMP_NUM_THREADS=1`,
`BLAS.set_num_threads(1)`, Julia 1.12.5, `DataAxesFormats.jl` current `main`.

```julia
using DataAxesFormats, SparseArrays, Statistics, BenchmarkTools

daf   = FilesDaf("path/to/big_sparse", "r")
A     = parent(parent(get_matrix(daf, "row", "col", "value")))   # SparseMatrixCSC
@assert isa(A, SparseMatrixCSC) && size(A) == (10_000, 10_000)
```

---

## Reason 1: per-query framework overhead dominates wall time

For *cheap* reductions (Sum/Mean/Max/Var/Std on sparse), the underlying
kernel is sub-millisecond, but `get_query` takes hundreds of milliseconds
to a second-and-a-half. The cost is in the wrapper path, not the math.

```julia
# (a) bare SparseArrays — the kernel
@btime sum($A; dims = 1)
# 0.52 ms,  78 KiB,  3 allocs

# (b) generic per-column reduce — what a fully generic dispatch would cost
@btime [sum(view($A, :, j)) for j in 1:size($A, 2)]
# 0.55 ms,  78 KiB,  3 allocs

# (c) DAF.jl full DSL query — the same operation
@btime begin
    empty_cache!($daf)
    get_query($daf, "@ row @ col :: value >- Sum")
end
# 644 ms,  4.77 MiB,  108 499 allocs
```

Two things to notice:

1. The kernel itself (a) is ~0.5 ms. The framework around it adds ~643 ms.
   That's a **1240× wrapper-to-kernel ratio**, allocating ~108 k objects
   per call.
2. Generic per-column reduction (b) is *not* the slow part. A naive
   `[sum(view(A, :, j)) for j in 1:n]` is 0.55 ms — basically identical
   to the SparseArrays-specialized path. So the 643 ms in (c) is **not**
   explained by "DAF.jl forfeits the SparseArrays fast path." It's
   explained by everything *around* the reduction loop.

The same shape holds for variance:

```julia
@btime var($A; dims = 1)                                    # 1.06 ms
@btime begin empty_cache!($daf); get_query($daf, "@ row @ col :: value >- Var") end
# 1515 ms,  4.77 MiB,  108 499 allocs
```

A profile of (c) shows the time goes through:

```
get_query → get_query_result → get_query_final_state →
  do_query_phrase → reduce_matrix_to_row →
    compute_reduction(::Sum, ...) →
      parallel_loop_wo_rng → flame_timed →
        mapfoldl_impl → _foldl_impl    ← 99 % of samples
```

…with the hot frames inside that path including OrderedDict
`ht_keyindex2` / `setindex!`, NamedArray construction, `StringViews.hash`,
and `with_cache_write_lock`. Allocation-wise, the per-call 4.77 MiB / 108 k
allocations look like the dominant pressure source.

(Profile script + flat output: `dev/scripts/profile-kernel-sum-col.jl`,
 `dev/scripts/out/profile-flat.txt`.)

**Suggested fix:** find a way to amortize the per-call NamedArray /
OrderedDict / cache-locking work, or specialize the hot paths so a
`get_query("... >- Sum")` doesn't pay 100k allocations per invocation.

---

## Reason 2: no sparse-aware path for Median / Mode / Quantile / GeoMean

For these ops, the underlying Julia kernel is *also* slow (`Statistics`
has no specialized sparse `dims` path), so the per-query overhead from
Reason 1 is hidden — both DAF.jl and a naive bare-Julia call take
~1.4 s. But there is an algorithmic ceiling, and it's ~19× higher than
either. A 30-line sparse-aware median that sorts only the per-column
nonzero buffer and indexes through the implicit zeros:

```julia
# (d) bare Julia — what DAF.jl effectively reduces to
@btime vec(median.(eachcol($A)))
# 1400 ms,  1.49 GiB,  90 003 allocs    <- densifies every column

# (e) DAF.jl full DSL query
@btime begin
    empty_cache!($daf)
    get_query($daf, "@ row @ col :: value >- Median")
end
# 1370 ms,  1.49 GiB,  198 499 allocs   <- same ceiling, framework hidden

# (f) sparse-aware median — sort only the nonzeros, account for implicit zeros
function sparse_col_median(A::SparseMatrixCSC{Tv}, j::Integer) where Tv
    n   = size(A, 1)
    rng = nzrange(A, j)
    vals = sort(A.nzval[rng])
    nz_count = length(vals)
    z_count  = n - nz_count
    n_neg    = searchsortedfirst(vals, zero(Tv)) - 1

    # Full sorted column = vals[1:n_neg] ++ z_count zeros ++ vals[n_neg+1:end].
    # Index into it without materializing.
    @inline val_at(k) = k <= n_neg              ? vals[k]            :
                        k <= n_neg + z_count    ? zero(Tv)            :
                                                   vals[k - z_count]
    isodd(n) ? val_at((n + 1) >>> 1) :
               (val_at(n >>> 1) + val_at((n >>> 1) + 1)) / 2
end
sparse_col_medians(A) = [sparse_col_median(A, j) for j in 1:size(A, 2)]

@assert vec(median.(eachcol(A))) ≈ sparse_col_medians(A)

@btime sparse_col_medians($A)
# 73 ms,   128 MiB,    110 003 allocs   <- 19× faster, 12× less memory
```

The same trick generalizes to `Quantile` (same skeleton, parametrized
position), `Mode` (count-on-the-fly with implicit-zero bias), and
`GeoMean` (log-sum over nonzeros + skip implicit zeros via `eps`-floor).

**Suggested fix:** add `compute_reduction(::T, ::SparseMatrixCSC, axis)`
methods for `T ∈ {Median, Quantile, Mode, GeoMean}` that exploit implicit
zeros instead of densifying. Happy to share dafr's C++ kernels as a
translation reference if useful.

---

## Things I checked that turned out *not* to be the cause

I want to flag these briefly so they don't get folklore'd into the next
analysis:

```julia
# Welford for variance is NOT a speed win on sparse:
#   hand-written single-pass Welford on SparseMatrixCSC: 26.1 ms
#   Statistics.var(A; dims=1)                          :  1.06 ms
# Statistics' two-pass path is ~25× faster because its inner loops
# vectorize. Welford's value is numerical stability, not throughput.

# C++ kernels are NOT inherently faster than Julia here:
#   dafr's full `kernel_sum_col` query              : 162 ms (R + C++)
#   raw SparseArrays sum(A; dims=1) in Julia        : 0.52 ms
# Julia's specialized kernel beats dafr's C++ kernel by ~300×. The reason
# dafr "wins" the bake-off on column reductions is that dafr's R-side
# framework is leaner than DataAxesFormats.jl's, NOT that the C++ kernel
# is faster than Julia's BLAS-style loops.
```

So out of the eight reasons I initially hypothesized, only the two above
held up under measurement. The wrapper-overhead one is the dominant story
for the entire bake-off; the missing sparse-aware kernels for densifying
ops is a clean second.

---

Happy to open a GitHub issue or draft a PR for either or both. The
reproducer scripts are at `dev/scripts/profile-{kernel-sum-col,
kernel-median-col, reasons-evidence}.jl` in the dafr-native repo.

Best,
Aviezer
