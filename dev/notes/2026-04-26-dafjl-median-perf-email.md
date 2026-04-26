**Subject:** Two measured causes for the DataAxesFormats.jl sparse perf gap

Sparse-reduction wall-time gap: **two** measured causes, with reproducers and
fix directions.

Everything below is runnable on the lab cluster. The fixture is a real
`FilesDaf` directory at:

```
/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/benchmarks/fixture/data/big_sparse
```

It contains a 10 000 × 10 000 `SparseMatrixCSC{Float64}` at 5 % density
(~5 M nonzeros), stored on axes `(row, col)` under matrix name `value`.
A self-contained runnable script with all four measurements lives at:

```
/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/dev/scripts/profile-reasons-evidence.jl
```

Run with any Julia environment that has `DataAxesFormats`, `BenchmarkTools`,
`SparseArrays`, `Statistics` (e.g. the bake-off env at
`/net/mraid20/.../dafr-native/benchmarks/julia`):

```
$ julia --project=/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/benchmarks/julia \
        /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/dev/scripts/profile-reasons-evidence.jl
```

All numbers below were measured at `OMP_NUM_THREADS=1`,
`BLAS.set_num_threads(1)`, Julia 1.12.5, `DataAxesFormats.jl` current `main`.

Setup block (paste into the REPL):

```julia
using DataAxesFormats, SparseArrays, Statistics, BenchmarkTools, LinearAlgebra
BLAS.set_num_threads(1)

const FIXTURE = "/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/benchmarks/fixture/data/big_sparse"
daf = FilesDaf(FIXTURE, "r"; name = "big_sparse")

# Unwrap NamedArrays / ReadOnly down to the underlying SparseMatrixCSC.
function unwrap_sparse(x)
    cur = x
    while !isa(cur, SparseMatrixCSC)
        p = parent(cur); p === cur && break
        cur = p
    end
    return cur
end
A = unwrap_sparse(get_matrix(daf, "row", "col", "value"))
@assert isa(A, SparseMatrixCSC) && size(A) == (10_000, 10_000)
println("matrix: $(size(A)), nnz=$(nnz(A))")
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

Same shape on variance:

```julia
@btime var($A; dims = 1)                                    # 1.06 ms
@btime begin
    empty_cache!($daf)
    get_query($daf, "@ row @ col :: value >- Var")
end
# 1515 ms,  4.77 MiB,  108 499 allocs
```

A profile of (c) shows 99 % of samples in this stack:

```
get_query → get_query_result → get_query_final_state →
  do_query_phrase → reduce_matrix_to_row →
    compute_reduction(::Sum, ...) →
      parallel_loop_wo_rng → flame_timed →
        mapfoldl_impl → _foldl_impl
```

…with the hot frames inside that path including `OrderedCollections`
`ht_keyindex2` / `setindex!`, NamedArray construction,
`StringViews.hash`, and `with_cache_write_lock`. The per-call 4.77 MiB /
108 k allocations look like the dominant pressure source. Profile data:
`/net/mraid20/.../dafr-native/dev/scripts/out/profile-flat.txt`.

**Suggested fix:** find a way to amortize the per-call NamedArray /
OrderedDict / cache-locking work, or specialize the hot paths so a
`get_query("... >- Sum")` doesn't pay 100k allocations per invocation.

---

## Reason 2: no sparse-aware path for Median / Mode / Quantile / GeoMean

For these ops, the underlying Julia kernel is *also* slow — `Statistics`
has no specialized sparse `dims` path, so `median(::SparseColumnView)`
densifies via `sort!`. The Reason 1 framework cost gets hidden behind
this; both DAF.jl and a naive bare-Julia call take ~1.4 s. But there is
an algorithmic ceiling, and it's ~19× higher than either.

A 30-line sparse-aware median that sorts only the per-column nonzero
buffer and indexes through the implicit zeros:

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
translation reference if useful — they're at
`/net/mraid20/.../dafr-native/src/kernel_{quantile,mode,geomean}_csc.cpp`.

---

Reproducer scripts and profile data:

```
/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/dev/scripts/
    profile-reasons-evidence.jl    # full Reason-1 + Reason-2
    profile-kernel-sum-col.jl      # Reason 1 alone, with sampling profile
    profile-kernel-median-col.jl   # Reason 2 alone
    out/profile-flat.txt           # the 99% mapfoldl_impl profile output
```

