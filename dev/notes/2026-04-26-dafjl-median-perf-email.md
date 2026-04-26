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
The R-side numbers (dafr column) were measured under R 4.4.1 with the same
single-threaded constraint. To make the comparison concrete, every Julia
snippet is paired with the equivalent dafr (R) snippet immediately below.

Julia setup (paste into the REPL):

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

R / dafr setup (paste into an R session):

```r
library(dafr); library(bench); library(Matrix)
options(dafr.num_threads = 1L); Sys.setenv(OMP_NUM_THREADS = "1")

d <- files_daf("/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/benchmarks/fixture/data/big_sparse",
               mode = "r")
m <- get_matrix(d, "row", "col", "value")   # dgCMatrix, same data as A
stopifnot(dim(m) == c(10000L, 10000L))
```

A self-contained R reproducer with the same measurements lives at
`/net/mraid20/.../dafr-native/dev/scripts/profile-dafr-equivalents.R`.

---

## Reason 1: per-query framework overhead dominates wall time

For *cheap* reductions (Sum/Mean/Max/Var/Std on sparse), the underlying
kernel is sub-millisecond, but `get_query` takes hundreds of milliseconds
to a second-and-a-half. The cost is in the wrapper path, not the math.

```julia
# Julia
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

```r
# R / dafr — equivalents on the same fixture
# (a') Matrix package's bare colSums on the dgCMatrix
bench::mark(Matrix::colSums(m))
# 14.6 ms median   (R/Matrix bare kernel; ~28× slower than Julia's 0.52 ms)

# (c') dafr full DSL query — identical query string
bench::mark({ empty_cache(d); get_query(d, "@ row @ col :: value >- Sum") })
# 56 ms median     (~11× faster than DAF.jl's 644 ms)
```

Two things to notice:

1. The kernel itself (a) is ~0.5 ms. The wrapper around it adds ~643 ms —
   a **1240× ratio**. dafr, running the *same* DSL query through its own
   R + C++ wrapper, takes 56 ms (Sum) / 60 ms (Var) — also above its
   bare kernel, but the wrapper ratio is ~4× rather than 1240×. So the
   cost ceiling for a DSL wrapper is *much* lower than DAF.jl currently
   sits at.
2. Generic per-column reduction (b) is *not* the slow part. The
   `[sum(view(A, :, j)) for j in 1:n]` form is 0.55 ms because each
   `sum` call dispatches to the SparseArrays-specialized method — even
   the "generic" Julia path is fast, as long as the dispatch finds the
   specialization.

Same shape on variance:

```julia
# Julia
@btime var($A; dims = 1)                                    # 1.06 ms
@btime begin
    empty_cache!($daf)
    get_query($daf, "@ row @ col :: value >- Var")
end
# 1515 ms,  4.77 MiB,  108 499 allocs
```

```r
# R / dafr
bench::mark(sparseMatrixStats::colVars(m))
# 36.9 ms median   (R sparse-aware bare kernel)

bench::mark({ empty_cache(d); get_query(d, "@ row @ col :: value >- Var") })
# 59.6 ms median   (~25× faster than DAF.jl's 1515 ms)
```

### Where exactly the 643 ms goes

A 10-second sampling profile of `empty_cache!(daf); get_query(daf, "... >- Sum")`
(33 082 samples, 16 541 effective at 50 % util) — 99 % of samples in this
stack:

```
get_query (queries.jl:1330)
  → with_data_read_lock
    → write_slow_through_cache
      → get_query_result (queries.jl:1691)
        → get_query_final_state (queries.jl:1777)
          → do_query_phrase (queries.jl:5568)
            → reduce_matrix_to_row (queries.jl:4043)
              → compute_reduction(::Sum, …) (operations.jl:1147)         16 428 samp
                → parallel_loop_wo_rng → flame_timed
                  → mapfoldl_impl → foldl_impl → _foldl_impl              16 282
                    → iterate(::SubArray, …)                              15 936
                      → SubArray.getindex (subarray.jl:316)               15 936
                        → SparseArrays.ReadOnly.getindex (readonly.jl:20) 15 936
                          → SparseMatrixCSC.getindex (sparsematrix.jl:2748) 13 466  (81 %)
                            → searchsortedfirst (sort.jl:308–310)         13 466  (81 %)
                              → < (int.jl:519)                             9 003  (54 %)
```

**The single most expensive function call is `searchsortedfirst` on an
Int range** — 81 % of all samples — with the bare `<` integer compare at
54 %.

What's actually happening: `compute_reduction(::Sum, …)` reduces to a
generic `mapfoldl(identity, op, view(A, …))` over a `SubArray` of a
`SparseArrays.ReadOnly{SparseMatrixCSC}`. Iterating that view yields
elements one at a time via **scalar `A[i, j]` indexing**, and each scalar
index into a CSC matrix is a binary search over the column's `rowval`.
On this fixture: 100 M scalar reads × ~9 binary-search compares =
~900 M integer comparisons. At sub-ns/compare in L1, that's ~700 ms.

The "framework overhead" is literally this binary-search loop:

```julia
# Anti-pattern — what DAF.jl currently does, via mapfoldl over a view:
s = zero(eltype(A))
for i in 1:size(A, 1)
    s += A[i, j]    # ← binary search every iteration
end

# Correct pattern — what `sum(A; dims=1)` already does internally,
# 1300× faster on this fixture:
s = zero(eltype(A))
for k in nzrange(A, j)
    s += A.nzval[k] # ← direct array read, O(nnz_in_col)
end
```

Profile artifacts:
`/net/mraid20/.../dafr-native/dev/scripts/out/{deep-flat.txt, deep-tree.txt}`
(reproduce via `dev/scripts/probe-profile-deep.jl`).

**Suggested fix.** Two equivalent paths:

1. **Inside `compute_reduction(::Sum/Mean/Max/...)`**, replace the
   generic `mapfoldl(identity, op, view(A, …))` with one that walks
   `nzrange + nzval` directly — or, simpler, dispatch to
   `SparseArrays.sum(A; dims=…)` / `mean` / `maximum` for the ops where
   that already exists.
2. **More general:** any `mapfoldl` / `mapreducedim!` over a
   `SubArray{<:SparseMatrixCSC}` will hit this. A specialized
   `iterate(::SubArray{<:SparseMatrixCSC})` that walks nonzeros directly
   would fix it without touching `compute_reduction` at all.

This also explains the asymmetry in the dafr↔DAF.jl bake-off table:
`*_col` queries are "only" 4× slower in DAF.jl because column-direction
views keep the binary-search constant manageable, while `*_row` queries
hit 15–37× because every element costs a separate column-search — same
anti-pattern, much worse asymptotic constant.

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

```r
# R / dafr — same DSL query; dafr's `kernel_quantile_csc` is sparse-aware
bench::mark({ empty_cache(d); get_query(d, "@ row @ col :: value >- Median") })
# 56 ms median   (~24× faster than DAF.jl's 1370 ms; comparable to the
#                 30-line sparse_col_medians kernel above at 73 ms — both
#                 do the same trick: sort only nonzeros, account for
#                 implicit zeros)

# Reference: R's sparseMatrixStats package has shipped a sparse-aware
# colMedians for years
bench::mark(sparseMatrixStats::colMedians(m))
# 13.4 ms median
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
    profile-reasons-evidence.jl    # Julia: full Reason-1 + Reason-2
    profile-kernel-sum-col.jl      # Julia: Reason 1 alone
    profile-kernel-median-col.jl   # Julia: Reason 2 alone
    probe-profile-deep.jl          # Julia: the deep call-chain profile that
                                   # pinned the cost to scalar getindex /
                                   # searchsortedfirst
    probe-jit-vs-runtime.jl        # Julia: confirms the 715ms is steady-state,
                                   # not JIT (first call ~10s, all subsequent
                                   # calls 709-725ms; cache hits = 40us)
    profile-dafr-equivalents.R     # R/dafr: paired equivalents on the same fixture
    out/deep-flat.txt              # flat profile, sorted by self-time
    out/deep-tree.txt              # full call-tree profile
```

