**Subject:** DataAxesFormats.jl: per-column `Median` (and other quantile-like
reductions) densifies sparse columns — small reproducer + ~19× fix

Hi,

I'm Aviezer Lifshitz from the Tanay lab. I've been writing a native R port
of `DataAxesFormats.jl` (`dafr` — pure R + C++, same on-disk format and DSL),
and while bake-off-ing it against the Julia reference I noticed a workload
where DAF.jl is leaving a lot on the table on sparse data. I wanted to flag
it in case it's useful, with a small self-contained reproducer.

The case is per-column reductions on a `SparseMatrixCSC` whose op has no
specialized sparse fast path — `Median`, `Quantile`, `Mode`, `GeoMean`. On a
representative single-cell-scale fixture (10 000 × 10 000, 5% density, ~5M
nonzeros), `get_query(daf, "@ row @ col :: value >- Median")` runs in ~1.4 s
and allocates ~1.5 GiB. That matches the cost of `vec(median.(eachcol(A)))`
on the underlying `SparseMatrixCSC` almost exactly, so the framework
overhead is negligible here — the time is in `median(::SparseColumnView)`,
which has no specialized path and densifies each column to call `sort!`.

A ~30-line sparse-aware median that sorts only the per-column nonzero
buffer and indexes through the implicit zeros is ~19× faster on the same
data and uses ~12× less memory. Reproducer:

```julia
using DataAxesFormats, SparseArrays, Statistics, BenchmarkTools

# 10 000 × 10 000 CSC, 5% density (~5M nnz). Any FilesDaf with a sparse
# matrix `value` on axes (row, col) of similar shape will reproduce.
daf   = FilesDaf("path/to/big_sparse", "r")
named = get_matrix(daf, "row", "col", "value")
A     = parent(parent(named))            # underlying SparseMatrixCSC
@assert isa(A, SparseMatrixCSC)

# (1) DAF.jl full DSL query
@btime begin
    empty_cache!($daf)
    get_query($daf, "@ row @ col :: value >- Median")
end
# 1.37 s, 1.49 GiB, 198 499 allocs

# (2) Bare Julia equivalent — same cost, so the time is not in DAF.jl
@btime vec(median.(eachcol($A)))
# 1.40 s, 1.49 GiB, 90 003 allocs

# (3) Sparse-aware median: avoid densifying. Sort only the nonzeros for
# each column, then index through the implicit zeros without materializing
# them.
function sparse_col_median(A::SparseMatrixCSC{Tv}, j::Integer) where Tv
    n   = size(A, 1)
    rng = nzrange(A, j)
    vals = sort(A.nzval[rng])
    nz_count = length(vals)
    z_count  = n - nz_count
    n_neg    = searchsortedfirst(vals, zero(Tv)) - 1

    # The full sorted column = vals[1:n_neg] ++ z_count zeros ++ vals[n_neg+1:end].
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
# 73 ms, 128 MiB, 110 003 allocs
```

Headline:

| approach                              |   time | memory  | vs. naive |
|---------------------------------------|-------:|--------:|----------:|
| `get_query(daf, "... >- Median")`     | 1370 ms | 1.5 GiB | 0.98×     |
| `vec(median.(eachcol(A)))` (naive)    | 1400 ms | 1.5 GiB | 1.00×     |
| `sparse_col_medians(A)`               |   73 ms | 128 MiB | **19.2×** |

(`OMP_NUM_THREADS=1`, `BLAS.set_num_threads(1)`, Julia 1.12.5,
`DataAxesFormats.jl` current `main`.)

The same densification trap applies to the other quantile-like reductions
(`Quantile`, `Mode`, `GeoMean`) — they all currently route through the
generic `compute_reduction` → `parallel_loop_wo_rng` → `mapfoldl` path, which
on sparse columns ends up calling the densifying generic implementation.
Adding a `compute_reduction(::T, ::SparseMatrixCSC, axis)` method that
dispatches to a sparse-aware kernel for each of these ops would close most
of the gap. (I have C++ kernels for all four in `dafr` if a translation
reference is useful.)

Happy to open a GitHub issue / draft a PR if that's preferable. The
reproducer above is also checked in at `dev/scripts/profile-kernel-median-col.jl`
in the dafr-native repo.

Best,
Aviezer

---

*Internal notes:*

- Profile data: `dev/scripts/out/profile-flat.txt`, `dev/scripts/out/summary.txt`.
- Same densification pattern was confirmed for the row direction in
  `dev/scripts/profile-kernel-row.jl` (`median_row` raw vs DAF.jl ratio:
  1.0×, both ~3.5 s; `var_row` raw vs DAF.jl ratio: 379× — that one is
  the *opposite* problem, where the kernel is fast but DAF.jl's wrapper
  is slow).
- For column `Sum`/`Mean`/`Max`, the picture is very different: DAF.jl
  does ~99.9% of its time in framework overhead, and raw
  `sum(A; dims=1)` runs in ~0.5 ms. That's a separate fix (dispatch
  `compute_reduction(::Sum, ...)` to `SparseArrays`' specialized `dims=1`
  path) and probably worth a separate email if we want to push it.
