# Slice 4 — Perf wedge + Chains + Contracts + Slice 3 follow-ups

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port Julia `DataAxesFormats.Chains` + `DataAxesFormats.Contracts` to the native-R `dafr` package (read-only and write chain federation; typed pre/post-condition contracts with access tracking), close the Slice 3 follow-up backlog (ViewDaf axis-rename / axis-filter propagation, real `IfNot` / `AsAxis` semantics, NA-in-mask alignment with Julia, view cache decision), AND wedge a four-part perf pass on the eltwise+reduce hot path BEFORE the chain/contract scaffolding lands — so contract verification doesn't compound the `log(x + 1)`-densifies-your-UMIs footgun on real pipelines.

**Architecture:**
- **Chains federate a sequence of `DafReader`s.** Two concrete S7 classes — `ReadOnlyChainDaf` (under `DafReadOnly`) and `WriteChainDaf` (under `DafWriter`) — each carry `dafs` (ordered vector of underlying `DafReader`s) plus, for the writer, a final `DafWriter`. All 22 `format_*` generics dispatch against these classes: reads walk `dafs` in reverse order (last wins), writes go to the top writer, axis consistency across overlapping axis names is validated at construction. Each chain has its own `cache` / version-counter environments (isolated from the underlying dafs); invalidation on write propagates to the top writer only.
- **Contracts enforce typed pre/post-conditions.** A `Contract` S7 class carries `name`, `is_relaxed`, `axes` (named list: axis → `list(expectation, description)`), and `data` (list of tuples: `list(kind = "scalar"|"vector"|"matrix", key, expectation, type, description)`). `contractor(computation, contract, daf)` wraps `daf` in a `ContractDaf` (itself a `DafWriter` subclass, for API uniformity — mirrors Julia's choice). `ContractDaf` dispatches every `format_*` generic against the inner `daf` while tracking access via mutable `Tracker` environments. `verify_input(d)` / `verify_output(d)` check existence/type/access; on a plain `DafReader` they are no-ops. A global `DAF_ENFORCE_CONTRACTS` flag (env var + R option) gates contract wrapping; when off, `contractor()` returns `daf` unchanged.
- **Slice 3 follow-ups land first.** Before chains/contracts build on the query surface, we close the four deferred items. View axis-rename and axis-filter propagate into `format_get_vector` / `format_get_matrix` so `get_vector(viewer_daf, renamed_axis, name)` works. `IfNot` / `AsAxis` gain real evaluator semantics (chained lookup: a vector whose values are entries of a target axis can be used to look up another property of that axis, with `??` giving a fallback for empty values). NA in mask comparators now drops silently, matching Julia. The dead per-view cache bucket is removed (chains will establish their own cache namespace).
- **Perf wedge — sparsity-preserving eltwise + bare-reduction fast paths + one fused log-reduce kernel.** The dominant single-cell motif `:: UMIs % Log eps: 1 >| Sum` today densifies a ~200 MB sparse `dgCMatrix` into a ~7 GB dense intermediate before reducing. We hook in at the `.eval_eltwise` / `.eval_reduction` boundaries only — the evaluator state machine (especially `.apply_axis`, load-bearing for `>|`/`>-`) is untouched. Four pieces, in cost order: (P1) wire the orphaned `dafr.omp_threshold` option into existing kernels; (P2) sparsity-preserving `Log eps: 1` on `dgCMatrix` via in-place rewrite of the `@x` slot (covers `log1p` semantics, the only zero-preserving Log call); (P3) bare-reduction routing — built-in `Sum`/`Mean`/`Max`/`Min`/`Count` with no params dispatches directly to `rowSums` / `Matrix::rowSums` / `matrixStats::rowMaxs` etc.; (P4) a single fused C++ kernel `kernel_log_reduce.cpp` (dense + CSC variants) for the `% Log → >| Sum|Mean / >- Sum|Mean` motif, OpenMP on the outer loop, single pass over `nnz` for sparse with no dense intermediate.
- **Julia compatibility tested via fixtures.** Reuse `tests/testthat/helper-julia.R::.have_julia_env()`. Extend fixtures with a chain + contract round-trip.

**Tech Stack:** R 4.4+, S7 0.2.1, `jsonlite`, `Matrix` (dgCMatrix / lgCMatrix), `bit64` (Int64 vectors), `stringi` for regex + escape handling, `matrixStats` (added in Phase P for fast `rowMaxs` / `rowMins` / `colMaxs` / `colMins`). One new C++ kernel — `src/kernel_log_reduce.cpp` — landing in Phase P (P4). Otherwise no new native code.

**Repo layout:**
- Package repo: `/home/aviezerl/src/dafr-native/` (`main`, tracks `origin/main` at `git@github.com:tanaylab/dafr.git`, tag `slice-3` at `f3bcc24`, post-tag styler commit `af842d7`).
- Dev repo (nested, gitignored): `/home/aviezerl/src/dafr-native/dev/`. Plans + notes + specs live here.
- Source + tests commits → package repo. Plan + notes + spec commits → dev repo. Infer from file path; use `cd ~/src/dafr-native` or `cd ~/src/dafr-native/dev` explicitly.

**Dev loop per task:**
1. From the package root:
   ```
   Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "<tag>")'
   ```
2. Inspect output; iterate until green.
3. Stage + commit with the provided message.

---

## Pre-planning decisions (settled before tasks)

### 1. Scope — perf wedge + primary + follow-ups + Julia compat

- Phase P (perf wedge, lands ahead of chains/contracts): wire `dafr.omp_threshold` into existing kernels; sparsity-preserving Log on `dgCMatrix`; bare-reduction routing to `rowSums` / `colSums` / `matrixStats`; one fused `kernel_log_reduce.cpp`.
- Primary: `ChainDaf` (read + write) and `Contract` + `ContractDaf` + `verify_input` / `verify_output` + `contractor`.
- Slice 3 follow-ups (all in scope): view rename propagation, view filter propagation, `IfNot` / `AsAxis` real semantics, NA-in-mask alignment, view cache bucket decision (remove).
- Julia compat: chain + contract fixtures extending the existing `tests/testthat/fixtures/julia-queries/` + `tests/testthat/fixtures/julia-views/` patterns.
- Out of scope (Phase P): full kernel buildout per design spec §6 (`kernels_eltwise.cpp`, `kernels_reduce.cpp`, `kernels_matvec.cpp` etc.) — defer to a dedicated perf slice; general fusion planner — hand-code the one motif that matters; touching `.apply_axis` or the evaluator state machine — load-bearing for `>|` / `>-` per Slice 3 mines; user-registered custom ops on the fast path — they continue through `apply()` (document the fast-path names in `?register_reduction` so users can opt in by matching the default-op signature).
- Out of scope (chains/contracts): `complete_chain!` (Julia's disk-chain helper), `@computation` macro equivalent, `function_contract` introspection, tensor keys in contracts (rare, UNTESTED in Julia), `@examples` roxygen blocks (Z-polish, deferred again per user).

### 2. Two chain classes, not one

Julia has `ReadOnlyChain <: DafReadOnly` and `WriteChain <: DafWriter` as separate concrete structs. We mirror this in R. A single class parameterised by "has writer" would need runtime dispatch for every mutating `format_*` method to gate on a flag — the two-class approach gets free dispatch via S7.

### 3. Contract = S7 class with list slots; `ContractDaf = DafWriter subclass`

Contract itself is a passive description. `ContractDaf` is the enforcing wrapper and is modelled as `DafWriter` (not `DafReader`) even when the underlying contract specifies no outputs, mirroring Julia. Reason: a single class per wrapper simplifies method dispatch. Write attempts on a no-output contract are rejected via tracker expectation checks, not via class.

### 4. Contract data = list of tagged records, not pair-lists

Julia uses `Vector{Pair{DataKey, DataSpecification}}` where `DataKey` is a union of `AbstractString`, `Tuple{...,...}`, `Tuple{...,...,...}`. R has no nice pair-list type and no tuples. We use a list of records:

```r
list(
    list(kind = "scalar", name = "version",
         expectation = "RequiredInput", type = "integer",
         description = "dataset version"),
    list(kind = "vector", axis = "cell", name = "age",
         expectation = "RequiredInput", type = "integer",
         description = "cell age"),
    list(kind = "matrix", rows_axis = "cell", columns_axis = "gene",
         name = "UMIs", expectation = "RequiredInput", type = "integer",
         description = "UMI counts")
)
```

Construction helpers `contract_scalar(...)`, `contract_vector(...)`, `contract_matrix(...)` keep call-sites tidy. Equality is by structural content.

### 5. `ContractExpectation` = character enum

Julia's `@enum` becomes a character vector of allowed literals: `c("RequiredInput", "OptionalInput", "CreatedOutput", "GuaranteedOutput", "OptionalOutput")`. All public constructors validate the string; the string value itself is stored (no numeric coding). Comparators are `identical()`.

### 6. Tracker = environment, not S7 / R6

Each axis / scalar / vector / matrix tracker is a single-entry environment holding `expectation`, `type`, and `accessed`. Environments are mutable in place; S7/R6 would force `setproperty!`-style rewrites. Environment-based trackers are also how we already implement version counters (`R/cache.R:96-113`).

### 7. `DAF_ENFORCE_CONTRACTS` gate — env var OR R option, default FALSE

Matches Julia's `DAF_ENFORCE_CONTRACTS`: read from `Sys.getenv("DAF_ENFORCE_CONTRACTS")` as a truthy-ish string on first call; also settable via `options(dafr.enforce_contracts = TRUE)`. When off, `contractor()` returns the `daf` argument unchanged (not a wrapper). Verification APIs (`verify_input` / `verify_output`) are plain functions that no-op on non-`ContractDaf` arguments.

### 8. Chain cache isolation

Each chain gets a fresh `cache`, `axis_version_counter`, `vector_version_counter`, `matrix_version_counter` via the same `new_*_env()` helpers used by the existing daf constructors. The chain's own cache is the "chain memory" layer; read federation does NOT pull through each underlying daf's cache — we call `format_*` on each daf in reverse order and let that daf's cache do its thing. The chain's cache sits on top of that.

The chain's `format_get_version_counter` aggregation across underlying dafs (Julia's `sum` of per-daf counters) is emulated: whenever the chain writer mutates, we bump the chain's own counter; when an underlying daf is mutated behind the chain's back (anti-pattern, but Julia warns against it), the chain's cache is stale. That's the Julia contract too — "Don't do that".

### 9. No `complete_chain!` / `complete_path` plumbing

Julia's `complete_chain!` is UNTESTED and writes a `base_daf_repository` scalar to a new disk-backed daf to allow reconstructing the chain later via `complete_daf`. Neither `complete_daf` nor `base_daf_repository` support exists in our R package. Defer until a consumer needs it.

### 10. Python `.j` → R `$` escape for the enum values

Julia's `RequiredInput` etc. are bare identifiers exported from the `Contracts` module. In R these become string literals. To keep the bare-identifier feel we export five string constants:

```r
#' @export
RequiredInput <- "RequiredInput"
#' @export
OptionalInput <- "OptionalInput"
#' @export
CreatedOutput <- "CreatedOutput"
#' @export
GuaranteedOutput <- "GuaranteedOutput"
#' @export
OptionalOutput <- "OptionalOutput"
```

So users write `contract_scalar("version", RequiredInput, "integer", "dataset version")` and it reads like the Julia API.

### 11. Phase ordering — P → F → C → T → Z

P (perf wedge) runs first so it lands as an isolated set of commits and benchmarks cleanly without F's evaluator changes mixed into the delta. The four perf commits + the benchmark CSV form one self-contained perf landing that Slice 5 can compare against.

F (Slice 3 follow-ups) runs after P. F2 (NA-in-mask), F3 (IfNot), F4 (AsAxis) all touch `R/query_eval.R`, but they edit different code paths from P2/P3/P4 (eltwise + reduction fast paths), so reordering doesn't conflict. F1 / F5 / F6 are in `R/view_daf.R` and don't touch the evaluator at all.

C (chains) and T (contracts) come last because they wrap the evaluator surface and benefit from both the perf wedge (so contract fixtures don't measure densification overhead) and the F follow-ups (so contracts can rely on correct view rename/filter propagation).

### 12. NA-in-mask alignment — drop NA silently, match Julia

Julia's boolean indexing drops NA-masked entries silently. R's `x[mask]` propagates NA to the result (both the mask element and the corresponding data element become NA in output). We replace `entries[mask]` with `entries[mask & !is.na(mask)]` at the two evaluator sites that currently use it (`R/query_eval.R:254` and the logical-combinator / comparator dispatch). Documented in NEWS.

### 13. View cache bucket — remove (don't populate)

The `cache$query` bucket on a `ViewDaf` is allocated by `new_cache_env()` but never written. Options: populate it with per-view query results (faster repeat reads on the view, but requires view-scoped cache keys and version-tracking against the base daf); or remove the allocation.

We remove it. Rationale: `format_get_*` on `ViewDaf` dispatches to `get_query` on the base daf, which already caches under the canonical query string in the base daf's `query` tier. Populating a per-view cache would add an extra layer without a measured benefit. If a future profile shows view-level caching pays off, we re-add it with a namespaced key (`"view:<viewname>:<canon>"`).

Implementation: give `ViewDaf` the base daf's cache env (not a fresh one). Per-view independent cache invalidation is no longer possible — but per-view invalidation was never actually wired anyway.

### 14. Julia `|>` for Contract merge — R operator or function?

Julia's `left |> right` (Base.:(|>)) merges two contracts. R's native `|>` is a reserved operator for pipe since R 4.1 and can't be overloaded. We expose a function `merge_contracts(left, right)` and optionally an infix operator `%|>%` that calls it. The Julia test suite exercises merging heavily; we mirror the behaviour under a function API.

### 15. Perf wedge — hand-code the one fused motif, don't build a planner

The dominant single-cell motif `:: UMIs % Log eps: 1 >| Sum` accounts for the bulk of evaluator wall-clock on real workloads. We add ONE fused C++ kernel for `Log → Sum|Mean` (dense + CSC) plus the sparsity-preserving Log path on dgCMatrix. We do NOT build a fusion planner, do not extend to `Log → Max|Min` (low value, would multiply variant count), and do not touch the evaluator state machine.

**Detection rule (peephole, applied in `.eval_query`):**
```
when current node is Eltwise(name = "Log", params)
  AND next node is ReduceToColumn|ReduceToRow(reduction in c("Sum", "Mean"), no params)
  AND state$kind == "matrix"
  AND state$value is dense matrix or dgCMatrix:
    consume both nodes, dispatch to kernel_log_reduce_*
    advance i by 2
otherwise: existing dispatch
```

**Sparsity preservation rule (in `.apply_eltwise`, P2):**
```
when node$name == "Log" AND state$value is dgCMatrix
  AND eps == 1 (the only zero-preserving Log call)
  AND base is unspecified or equals exp(1):
    apply log1p in place to @x slot; reuse @i / @p; return new dgCMatrix
otherwise: existing dispatch (densify + apply)
```

**Bare-reduction rule (in `.apply_reduction`, P3):**
```
when node$reduction in c("Sum", "Mean", "Max", "Min", "Count")
  AND no params
  AND state$value is matrix (dense or sparse):
    dispatch to rowSums / Matrix::rowSums / matrixStats::rowMaxs / etc.
otherwise: existing apply()-based dispatch
```

User-registered custom ops continue through `apply()`. Document the fast-path names so users can opt in by matching the default-op identity.

### 16. Live Julia env reuse

The chain + contract fixture scripts (`dev/scripts/regen-julia-chains-fixture.jl`, `dev/scripts/regen-julia-contracts-fixture.jl`) run under the same `dafr-mcview` conda env used by Slice 2 + Slice 3 (`conda run -n dafr-mcview julia ...`). Gate: `tests/testthat/helper-julia.R::.have_julia_env()`. Static fixture runs always; live regeneration skipped on CI without conda.

---

## File structure

### New R source files (package repo)

| File | Responsibility |
|------|---------------|
| `R/chain_daf.R` | `ReadOnlyChainDaf` + `WriteChainDaf` S7 classes + `chain_reader()` + `chain_writer()` + all 22 `format_*` method registrations. ~700 lines. |
| `R/contracts.R` | `ContractExpectation` constants + `Contract` S7 class + `contract_scalar/vector/matrix` constructors + `ContractDaf` S7 class + `contractor()` + `verify_input()` + `verify_output()` + `merge_contracts()` + access-tracking machinery + every `format_*` method registration. ~900 lines. |

### Modified R source files (package repo)

| File | Scope of change |
|------|-----------------|
| `R/query_eval.R` | Phase F2: NA-drop in mask indexing. Phase F3: `IfNot` real semantics (chain-lookup final value). Phase F4: `AsAxis` real semantics (chained lookup). Phase P2: sparsity-preserving Log on dgCMatrix in `.apply_eltwise`. Phase P3: bare-reduction routing in `.apply_reduction`. Phase P4: peephole in `.eval_query` to fuse `Log → Sum|Mean` into the kernel. |
| `R/view_daf.R` | Phase F1: remove dead `cache$query` allocation (reuse base daf's cache env). Phase F5: axis rename propagation to vector/matrix reads. Phase F6: axis filter propagation to vector/matrix reads. |
| `R/queries.R` | If Phase F3/F4 requires extra surface on public `get_query`, update here. |
| `R/operations.R` | Phase P3: tag default ops with a `.builtin` attribute so the bare-reduction fast path can detect identity by reference rather than name. Phase P4: same tag for `Log` so the fused-kernel peephole can dispatch only when the registered eltwise is the default. |
| `R/cpp11.R` | Phase P1: regenerated to add `int threshold` arg to existing kernels. Phase P4: regenerated to add `kernel_log_reduce_dense_cpp` + `kernel_log_reduce_csc_cpp` bindings. |
| `R/options.R` | Phase P1: confirm `dafr.omp_threshold` default is sane (10000) and exported via `dafr_opt()`. |
| `DESCRIPTION` | Phase P3: add `matrixStats` to Imports. |
| `src/kernel_eltwise_log_add.cpp` | Phase P1: take threshold as int arg instead of hardcoded 10000. |
| `src/kernel_csc_colsums.cpp` | Phase P1: take threshold as int arg instead of hardcoded 1000. |
| `NAMESPACE` | Regenerated (Z1). |

### New C++ source files (package repo)

| File | Responsibility |
|------|---------------|
| `src/kernel_log_reduce.cpp` | Phase P4: fused log-then-reduce kernel. Two cpp11-registered entry points: `kernel_log_reduce_dense_cpp(matrix, eps, base, axis, reducer, threshold)` and `kernel_log_reduce_csc_cpp(x, i, p, nrow, ncol, eps, base, axis, reducer, threshold)`. `axis` ∈ {0L, 1L} (row vs column reduction). `reducer` ∈ {"Sum", "Mean"}. OpenMP parallelizes over the outer loop. CSC variant single-passes `@x` slot for Sum (and additionally tracks per-row counts for Mean), no dense intermediate. ~250 lines incl. both variants. |

### New test files (package repo)

| File | Responsibility |
|------|---------------|
| `tests/testthat/test-perf-log-sparsity.R` | P2: Log on dgCMatrix preserves sparsity (when eps == 1, base default). |
| `tests/testthat/test-perf-bare-reductions.R` | P3: bare-reduction routing matches `apply()`-based output bit-for-bit. |
| `tests/testthat/test-perf-log-reduce-kernel.R` | P4: fused `kernel_log_reduce_*` matches the unfused path numerically (dense + sparse). |
| `tests/testthat/test-chain-readers.R` | Chain reader federation + axis consistency validation. |
| `tests/testthat/test-chain-writers.R` | Chain writer: writes to top, delete-earlier-error, auto-add-axis. |
| `tests/testthat/test-chain-julia-compat.R` | Julia fixture round-trip for chains. |
| `tests/testthat/test-contracts-class.R` | Contract class, constructors, merge. |
| `tests/testthat/test-contracts-verify.R` | `verify_input` / `verify_output` matrix: expectation × direction × access. |
| `tests/testthat/test-contracts-access.R` | Access-tracking + relaxed-mode behaviour. |
| `tests/testthat/test-view-rename-propagation.R` | F5: renamed axis reads through `get_vector` / `get_matrix`. |
| `tests/testthat/test-view-filter-propagation.R` | F6: filtered axis reads through `get_vector` / `get_matrix`. |

### Modified test files (package repo)

| File | Scope of change |
|------|-----------------|
| `tests/testthat/test-query-eval-masks.R` | F2: add a test covering NA-in-vector drop semantics. |
| `tests/testthat/test-query-parse.R` or new `test-query-eval-chains.R` | F3 + F4: `IfNot` / `AsAxis` chained-lookup evaluator tests. |
| `tests/testthat/test-view-daf.R` | F1: assert the view cache bucket is the base cache. |

### New fixtures

- `tests/testthat/fixtures/julia-chains/fixture.json` — chain scenarios: overlapping scalars, axes, vectors, matrices; mismatched-axis error cases.
- `tests/testthat/fixtures/julia-contracts/fixture.json` — contract verification scenarios: required / optional / created / guaranteed / optional-output × input / output direction.

### New dev-repo artefacts

- `dev/scripts/regen-julia-chains-fixture.jl` — regenerates chain fixture.
- `dev/scripts/regen-julia-contracts-fixture.jl` — regenerates contract fixture.
- `dev/notes/slice-4-exit.md` — exit gate (written in Phase Z).

---

## Phase P — Perf hot-path fix (wedge before chains/contracts)

### Task P1: Wire `dafr.omp_threshold` into existing C++ kernels

**Files:**
- Modify: `src/kernel_eltwise_log_add.cpp` (replace hardcoded `n >= 10000`)
- Modify: `src/kernel_csc_colsums.cpp` (replace hardcoded `ncol >= 1000`)
- Modify: `R/cpp11.R` (regenerated)
- Modify: R-side wrappers that call these (audit via grep — kernels are only invoked from internal helpers)
- Create: `tests/testthat/test-kernel-omp-threshold.R`

Context: `dafr.omp_threshold` was declared in `R/options.R:6` (default `10000L`) but never read. The two existing kernels use hardcoded thresholds inside `DAFR_PARALLEL_FOR(...)`. We thread the option as a positional `int threshold` argument to each kernel; the R wrapper reads `dafr_opt("dafr.omp_threshold")`.

- [ ] **Step 1: Write failing test in `tests/testthat/test-kernel-omp-threshold.R`**

```r
test_that("kernel_log_add_cpp accepts a threshold argument", {
    args <- formals(dafr:::kernel_log_add_cpp)
    expect_true("threshold" %in% names(args))
})

test_that("kernel_csc_colsums_cpp accepts a threshold argument", {
    args <- formals(dafr:::kernel_csc_colsums_cpp)
    expect_true("threshold" %in% names(args))
})

test_that("dafr.omp_threshold default propagates through dafr_opt()", {
    expect_identical(dafr:::dafr_opt("dafr.omp_threshold"), 10000L)
    withr::local_options(dafr.omp_threshold = 1L)
    expect_identical(dafr:::dafr_opt("dafr.omp_threshold"), 1L)
})
```

- [ ] **Step 2: Run — expect failure**

```
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_file("tests/testthat/test-kernel-omp-threshold.R")'
```

Expected: FAIL — kernels have no `threshold` arg.

- [ ] **Step 3: Modify `src/kernel_eltwise_log_add.cpp`**

```cpp
// Eltwise kernel: out[k] = log(x[k]) + y[k] for k in [0, n).
// Pure cpp11 + BLAS (no BLAS used here because log isn't a BLAS primitive;
// this is the "hand-rolled C++" arm of the bake-off).

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>

[[cpp11::register]]
cpp11::writable::doubles kernel_log_add_cpp(
    cpp11::doubles x,
    cpp11::doubles y,
    int threshold
) {
    const R_xlen_t n = x.size();
    if (y.size() != n) cpp11::stop("x and y must have the same length");
    cpp11::writable::doubles out(n);
    const double *px = REAL(x.data());
    const double *py = REAL(y.data());
    double *pout = REAL(out.data());
    DAFR_PARALLEL_FOR(n >= threshold)
    for (R_xlen_t k = 0; k < n; ++k) {
        pout[k] = std::log(px[k]) + py[k];
    }
    return out;
}
```

And `src/kernel_csc_colsums.cpp`:

```cpp
#include <cpp11.hpp>
#include "openmp_shim.h"

[[cpp11::register]]
cpp11::writable::doubles kernel_csc_colsums_cpp(
    cpp11::doubles x,
    cpp11::integers p,
    int ncol,
    int threshold
) {
    cpp11::writable::doubles out(ncol);
    const double *px = REAL(x.data());
    const int *pp = INTEGER(p.data());
    double *pout = REAL(out.data());
    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int j = 0; j < ncol; ++j) {
        double s = 0.0;
        for (int k = pp[j]; k < pp[j + 1]; ++k) {
            s += px[k];
        }
        pout[j] = s;
    }
    return out;
}
```

- [ ] **Step 4: Regenerate `R/cpp11.R` and update R callers**

```
Rscript -e 'cpp11::cpp_register()'
```

Then audit R callers:

```
Rscript -e 'cat(grep(c("kernel_log_add_cpp|kernel_csc_colsums_cpp"), readLines, perl = TRUE))'
```

Find each call site and pass `threshold = dafr_opt("dafr.omp_threshold")`. For example, an existing call like:

```r
kernel_csc_colsums_cpp(x, p, ncol)
```

becomes:

```r
kernel_csc_colsums_cpp(x, p, ncol, threshold = dafr_opt("dafr.omp_threshold"))
```

- [ ] **Step 5: Build, run the new test + the existing kernel tests — expect pass**

```
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_file("tests/testthat/test-kernel-omp-threshold.R"); testthat::test_file("tests/testthat/test-kernel-openmp.R")'
```

Expected: PASS on the new test + zero regression on existing kernel tests.

- [ ] **Step 6: Commit**

```bash
git add src/kernel_eltwise_log_add.cpp src/kernel_csc_colsums.cpp R/cpp11.R tests/testthat/test-kernel-omp-threshold.R
git commit -m "fix(kernels): wire dafr.omp_threshold through (was orphaned)"
```

If R-source callers were modified, include them in the commit too.

### Task P2: Sparsity-preserving Log on `dgCMatrix` (covers `log1p` motif)

**Files:**
- Modify: `R/operations.R` (tag `.op_log` as built-in, refactor body)
- Modify: `R/query_eval.R` (`.apply_eltwise` shortcut for sparse Log)
- Create: `tests/testthat/test-perf-log-sparsity.R`

Context: `:: UMIs % Log eps: 1` densifies a sparse `dgCMatrix` because the user-facing `.op_log` does `log(x + eps, base)`. For `eps == 1` and default `base == exp(1)`, the operation IS `log1p`, which preserves sparsity (`log1p(0) == 0`). We add a fast path that detects this case and operates on the `@x` slot in place, returning a new `dgCMatrix` with the same `@i / @p`.

- [ ] **Step 1: Write failing test in `tests/testthat/test-perf-log-sparsity.R`**

```r
test_that("Log eps: 1 on dgCMatrix preserves sparse class", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- methods::as(matrix(c(0, 1, 0, 2, 0, 0, 0, 3, 4), nrow = 3), "dgCMatrix")
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 1")
    expect_s4_class(result, "dgCMatrix")
    expect_equal(result@x, log1p(m@x))
    expect_identical(result@i, m@i)
    expect_identical(result@p, m@p)
})

test_that("Log eps: 0 on dgCMatrix densifies (no fast path)", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", "A"); add_axis(d, "gene", "X")
    m <- methods::as(matrix(2.0, 1, 1), "dgCMatrix")
    set_matrix(d, "cell", "gene", "UMIs", m)
    # eps == 0 (default of the parser if user writes no eps): log(0) is -Inf,
    # which densifies. We do NOT short-circuit here.
    result <- get_query(d, "@ cell @ gene :: UMIs % Log")
    expect_true(is.matrix(result))   # densified to dense
    expect_equal(result[1, 1], log(2))
})
```

- [ ] **Step 2: Run — expect failure**

Expected: FAIL — current `.apply_eltwise` calls `do.call(.op_log, ...)` which returns a dense matrix.

- [ ] **Step 3: Tag `.op_log` as built-in + add fast path in `.apply_eltwise`**

In `R/operations.R`, mark `.op_log` so the evaluator can detect it by reference:

```r
.op_log <- function(x, ..., eps = 0, base = exp(1)) log(x + eps, base = base)
attr(.op_log, ".dafr_builtin") <- "Log"
```

Apply the same `.dafr_builtin` attribute to `.op_sum` (`"Sum"`), `.op_mean` (`"Mean"`), `.op_max` (`"Max"`), `.op_min` (`"Min"`), `.op_count` (`"Count"`), `.op_abs` (`"Abs"`), `.op_exp` (`"Exp"`), `.op_sqrt` (`"Sqrt"`), `.op_round` (`"Round"`). This single hook is reused by P3 and P4.

```r
attr(.op_sum, ".dafr_builtin") <- "Sum"
attr(.op_mean, ".dafr_builtin") <- "Mean"
attr(.op_max, ".dafr_builtin") <- "Max"
attr(.op_min, ".dafr_builtin") <- "Min"
attr(.op_count, ".dafr_builtin") <- "Count"
attr(.op_abs, ".dafr_builtin") <- "Abs"
attr(.op_exp, ".dafr_builtin") <- "Exp"
attr(.op_sqrt, ".dafr_builtin") <- "Sqrt"
attr(.op_round, ".dafr_builtin") <- "Round"
```

In `R/query_eval.R`, modify `.apply_eltwise` to check for the sparse-Log fast path:

```r
.apply_eltwise <- function(node, state, daf) {
    if (!state$kind %in% c("vector", "matrix")) {
        stop("'%' eltwise requires vector or matrix in scope", call. = FALSE)
    }
    fn <- get_eltwise(node$name)
    params <- .coerce_params(node$params)

    # Fast path: sparsity-preserving Log on dgCMatrix.
    # log1p(0) == 0, so eps == 1 with default base (e) is the only Log
    # parameterisation that preserves sparsity. Use all.equal() (not
    # identical) because .coerce_params yields numeric(1) which does not
    # compare identical to integer(1) 1L.
    builtin <- attr(fn, ".dafr_builtin")
    if (identical(builtin, "Log") &&
        methods::is(state$value, "dgCMatrix") &&
        isTRUE(all.equal(params$eps %||% 0, 1)) &&
        (is.null(params$base) ||
         isTRUE(all.equal(params$base, exp(1))))) {
        out <- state$value
        out@x <- log1p(out@x)
        state$value <- out
        return(state)
    }

    state$value <- do.call(fn, c(list(state$value), params))
    state
}
```

Add a one-line comment block above the fast path explaining it (per the no-comments default, this one earns a comment because the WHY — sparsity preservation — is non-obvious from the code).

- [ ] **Step 4: Run — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-perf-log-sparsity.R"); testthat::test_dir("tests/testthat", filter = "query")'
```

Expected: PASS on new test + zero regression across all query tests.

- [ ] **Step 5: Commit**

```bash
git add R/operations.R R/query_eval.R tests/testthat/test-perf-log-sparsity.R
git commit -m "perf(query-eval): sparsity-preserving Log eps: 1 on dgCMatrix (log1p in place)"
```

### Task P3: Bare-reduction routing to `rowSums` / `Matrix::rowSums` / `matrixStats`

**Files:**
- Modify: `DESCRIPTION` (add `matrixStats` to Imports)
- Modify: `R/query_eval.R` (`.apply_reduction` fast path)
- Create: `tests/testthat/test-perf-bare-reductions.R`

Context: bare reductions over the matrix (no params) are the hottest tail after the Log densification. We detect default ops by the `.dafr_builtin` attribute (added in P2) and dispatch to vectorised primitives. ReduceToColumn (collapse across columns within each row) → `rowSums` / `rowMeans`. ReduceToRow → `colSums` / `colMeans`. Sparse uses `Matrix::rowSums` etc. Max / Min use `matrixStats` (if available) or fall back.

- [ ] **Step 1: Write failing tests in `tests/testthat/test-perf-bare-reductions.R`**

```r
test_that("ReduceToColumn Sum on dense matrix matches rowSums", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >| Sum")
    expect_identical(unname(result), as.numeric(rowSums(m)))
})

test_that("ReduceToColumn Sum on dgCMatrix uses Matrix::rowSums", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- methods::as(matrix(c(1, 0, 3, 0, 5, 0), nrow = 2), "dgCMatrix")
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >| Sum")
    expect_equal(unname(result), as.numeric(Matrix::rowSums(m)))
})

test_that("ReduceToColumn Mean matches rowMeans", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(2, 4, 6, 8), nrow = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >| Mean")
    expect_identical(unname(result), as.numeric(rowMeans(m)))
})

test_that("ReduceToRow Sum matches colSums", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4), nrow = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >- Sum")
    expect_identical(unname(result), as.numeric(colSums(m)))
})

test_that("Max via matrixStats::rowMaxs matches", {
    skip_if_not_installed("matrixStats")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 5, 9, 3), nrow = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs >| Max")
    expect_identical(unname(result), as.numeric(matrixStats::rowMaxs(m)))
})

# Count is intentionally NOT in the fast path: the slow apply()-based path
# already evaluates to length(row), which is constant ncol(m); replicating
# that constant in a fast path would not save anything. Falls through to
# the slow path with the existing Slice-3 semantics.
```

- [ ] **Step 2: Run — expect failure**

Expected: FAIL — current `.apply_reduction` uses `apply()`.

- [ ] **Step 3: Add `matrixStats` to DESCRIPTION**

Open `DESCRIPTION` and add `matrixStats` to the `Imports:` field (alphabetised). Then:

```
Rscript -e 'devtools::document()'
```

- [ ] **Step 4: Add the fast path to `.apply_reduction` in `R/query_eval.R`**

```r
.apply_reduction <- function(node, state, daf) {
    if (identical(state$kind, "grouped_vector")) {
        return(.apply_reduction_grouped_vector(node, state, daf))
    }
    if (identical(state$kind, "grouped_matrix_rows")) {
        return(.apply_reduction_grouped_matrix(node, state, daf, by = "rows"))
    }
    if (identical(state$kind, "grouped_matrix_cols")) {
        return(.apply_reduction_grouped_matrix(node, state, daf, by = "cols"))
    }
    if (!identical(state$kind, "matrix")) {
        stop(sprintf("%s requires a matrix or grouped scope", node$op),
            call. = FALSE
        )
    }
    fn <- get_reduction(node$reduction)
    params <- .coerce_params(node$params)

    # Fast path: bare default reduction → vectorised primitive.
    if (length(params) == 0L) {
        fast <- .apply_reduction_fast(node, state, fn, daf)
        if (!is.null(fast)) return(fast)
    }

    .apply_reduction_slow(node, state, fn, params, daf)
}

.apply_reduction_fast <- function(node, state, fn, daf) {
    builtin <- attr(fn, ".dafr_builtin")
    if (is.null(builtin)) return(NULL)
    m <- state$value
    is_sparse <- methods::is(m, "dgCMatrix") || methods::is(m, "lgCMatrix")
    is_dense <- is.matrix(m)
    if (!is_sparse && !is_dense) return(NULL)

    if (identical(node$op, "ReduceToColumn")) {
        # row-wise reduction
        row_names <- if (is_dense) rownames(m) else m@Dimnames[[1L]]
        if (is.null(row_names)) row_names <- format_axis_array(daf, state$rows_axis)
        vals <- switch(builtin,
            Sum   = if (is_sparse) Matrix::rowSums(m) else rowSums(m),
            Mean  = if (is_sparse) Matrix::rowMeans(m) else rowMeans(m),
            Max   = if (requireNamespace("matrixStats", quietly = TRUE)) {
                if (is_sparse) matrixStats::rowMaxs(as.matrix(m)) else matrixStats::rowMaxs(m)
            } else return(NULL),
            Min   = if (requireNamespace("matrixStats", quietly = TRUE)) {
                if (is_sparse) matrixStats::rowMins(as.matrix(m)) else matrixStats::rowMins(m)
            } else return(NULL),
            return(NULL)  # unhandled built-in (e.g. Count) falls through to slow path
        )
        return(list(
            kind = "vector", axis = state$rows_axis,
            value = setNames(as.numeric(vals), row_names)
        ))
    }
    # ReduceToRow: column-wise reduction
    col_names <- if (is_dense) colnames(m) else m@Dimnames[[2L]]
    if (is.null(col_names)) col_names <- format_axis_array(daf, state$cols_axis)
    vals <- switch(builtin,
        Sum   = if (is_sparse) Matrix::colSums(m) else colSums(m),
        Mean  = if (is_sparse) Matrix::colMeans(m) else colMeans(m),
        Max   = if (requireNamespace("matrixStats", quietly = TRUE)) {
            if (is_sparse) matrixStats::colMaxs(as.matrix(m)) else matrixStats::colMaxs(m)
        } else return(NULL),
        Min   = if (requireNamespace("matrixStats", quietly = TRUE)) {
            if (is_sparse) matrixStats::colMins(as.matrix(m)) else matrixStats::colMins(m)
        } else return(NULL),
        return(NULL)  # unhandled built-in (e.g. Count) falls through to slow path
    )
    list(
        kind = "vector", axis = state$cols_axis,
        value = setNames(as.numeric(vals), col_names)
    )
}

.apply_reduction_slow <- function(node, state, fn, params, daf) {
    m <- state$value
    if (identical(node$op, "ReduceToColumn")) {
        row_names <- rownames(m)
        if (is.null(row_names)) row_names <- format_axis_array(daf, state$rows_axis)
        vals <- apply(m, 1L, function(row) do.call(fn, c(list(row), params)))
        return(list(
            kind = "vector", axis = state$rows_axis,
            value = setNames(vals, row_names)
        ))
    }
    col_names <- colnames(m)
    if (is.null(col_names)) col_names <- format_axis_array(daf, state$cols_axis)
    vals <- apply(m, 2L, function(col) do.call(fn, c(list(col), params)))
    list(
        kind = "vector", axis = state$cols_axis,
        value = setNames(vals, col_names)
    )
}
```

The original `.apply_reduction` body that did the `apply()` call is now `.apply_reduction_slow`. Move the existing body verbatim, only renaming the function. Confirm by re-reading the existing dense code block in `R/query_eval.R:359-410` and copying matching lines into `.apply_reduction_slow`. The fast path and slow path produce identical numeric output for built-in ops; only the route differs.

- [ ] **Step 5: Run the new test + the full reduction tests + Julia compat — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-perf-bare-reductions.R"); testthat::test_dir("tests/testthat", filter = "query")'
```

Expected: PASS across all.

- [ ] **Step 6: Commit**

```bash
git add DESCRIPTION NAMESPACE R/query_eval.R tests/testthat/test-perf-bare-reductions.R
git commit -m "perf(query-eval): bare default reductions route to rowSums/colSums/matrixStats"
```

### Task P4: Fused `kernel_log_reduce.cpp` (dense + CSC) — the one motif that matters

**Files:**
- Create: `src/kernel_log_reduce.cpp`
- Modify: `R/cpp11.R` (regenerated)
- Modify: `R/query_eval.R` (peephole in `.eval_query` for the fused dispatch)
- Create: `tests/testthat/test-perf-log-reduce-kernel.R`

Context: `:: UMIs % Log eps: 1 >| Sum` (and the `Mean` variant) is the dominant motif. P2 + P3 already give us a sparse-preserving Log followed by `Matrix::rowSums` — but that's still two passes (one to allocate the log result, one to reduce). The fused kernel does it in one pass over `nnz` for sparse, with no intermediate. For dense it's a single pass that's O(n) instead of two passes.

- [ ] **Step 1: Write failing tests in `tests/testthat/test-perf-log-reduce-kernel.R`**

```r
test_that("kernel_log_reduce_dense matches Log + Sum unfused", {
    skip_if_not(exists("kernel_log_reduce_dense_cpp",
        envir = asNamespace("dafr")
    ))
    m <- matrix(c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0), nrow = 2)
    eps <- 1.0; base <- exp(1)
    expected <- rowSums(log(m + eps))
    actual <- dafr:::kernel_log_reduce_dense_cpp(
        m, eps, base, axis = 0L, reducer = "Sum",
        threshold = 1L
    )
    expect_equal(actual, expected)
})

test_that("kernel_log_reduce_csc matches Log + Sum unfused", {
    skip_if_not(exists("kernel_log_reduce_csc_cpp",
        envir = asNamespace("dafr")
    ))
    skip_if_not_installed("Matrix")
    m <- methods::as(matrix(c(1, 0, 3, 0, 5, 0), nrow = 2), "dgCMatrix")
    eps <- 1.0; base <- exp(1)
    # log(0 + 1) = 0, so zero entries contribute 0 to the sum.
    expected <- as.numeric(Matrix::rowSums(log(m + eps)))
    actual <- dafr:::kernel_log_reduce_csc_cpp(
        m@x, m@i, m@p,
        nrow(m), ncol(m),
        eps, base, axis = 0L, reducer = "Sum",
        threshold = 1L
    )
    expect_equal(actual, expected)
})

test_that("get_query routes UMIs % Log eps:1 >| Sum through fused kernel", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- methods::as(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE), "dgCMatrix")
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 1 >| Sum")
    expected <- as.numeric(Matrix::rowSums(log1p(m)))
    expect_equal(unname(result), expected)
})

test_that("Mean variant matches", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y", "Z"))
    m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
    set_matrix(d, "cell", "gene", "UMIs", m)
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 1 >| Mean")
    expected <- rowMeans(log(m + 1))
    expect_equal(unname(result), expected)
})
```

- [ ] **Step 2: Run — expect failure**

Expected: FAIL — kernels don't exist; the fused R-side peephole isn't wired.

- [ ] **Step 3: Implement `src/kernel_log_reduce.cpp`**

```cpp
// Fused log+reduce kernel.
//
// Single-pass log(x + eps)/log(base) accumulated into a row or column
// sum/mean. Two variants: dense matrix (column-major like R's storage),
// CSC (Matrix::dgCMatrix layout: x = nnz values, i = row indices, p =
// column-pointer of length ncol+1). For CSC + Sum/Mean this avoids the
// dense intermediate the unfused path allocates.

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <cmath>
#include <string>

[[cpp11::register]]
cpp11::writable::doubles kernel_log_reduce_dense_cpp(
    cpp11::doubles_matrix<> m,
    double eps,
    double base,
    int axis,           // 0 = row reduction, 1 = column reduction
    std::string reducer,
    int threshold
) {
    const int nrow = m.nrow();
    const int ncol = m.ncol();
    const double *pm = REAL(m.data());
    const double inv_log_base = 1.0 / std::log(base);
    const bool is_mean = (reducer == "Mean");

    if (axis == 0) {
        cpp11::writable::doubles out(nrow);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(nrow >= threshold)
        for (int r = 0; r < nrow; ++r) {
            double s = 0.0;
            for (int c = 0; c < ncol; ++c) {
                s += std::log(pm[r + c * nrow] + eps) * inv_log_base;
            }
            pout[r] = is_mean ? s / ncol : s;
        }
        return out;
    } else {
        cpp11::writable::doubles out(ncol);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int c = 0; c < ncol; ++c) {
            double s = 0.0;
            for (int r = 0; r < nrow; ++r) {
                s += std::log(pm[r + c * nrow] + eps) * inv_log_base;
            }
            pout[c] = is_mean ? s / nrow : s;
        }
        return out;
    }
}

[[cpp11::register]]
cpp11::writable::doubles kernel_log_reduce_csc_cpp(
    cpp11::doubles x,         // nnz values
    cpp11::integers i,        // row indices, 0-based
    cpp11::integers p,        // ncol+1 column-pointers, 0-based
    int nrow,
    int ncol,
    double eps,
    double base,
    int axis,
    std::string reducer,
    int threshold
) {
    const double *px = REAL(x.data());
    const int *pi = INTEGER(i.data());
    const int *pp = INTEGER(p.data());
    const double inv_log_base = 1.0 / std::log(base);
    const double zero_log = std::log(eps) * inv_log_base;
    const bool is_mean = (reducer == "Mean");

    if (axis == 0) {
        cpp11::writable::doubles out(nrow);
        double *pout = REAL(out.data());
        // Each row sees:
        //   sum_explicit = sum over nonzero entries in this row of log(x + eps)/log(base)
        //   sum_zeros = (ncol - nnz_in_this_row) * log(eps)/log(base)
        // Total for Sum = sum_explicit + sum_zeros.
        // For Mean divide by ncol.
        //
        // Per-thread accumulator buffers: each thread gets its own (nrow)
        // arrays of partial sums + nnz counts so the inner nnz scan
        // parallelises across columns without atomic contention. After the
        // parallel region we reduce thread buffers into the final output in
        // an O(nthreads * nrow) pass.
        for (int r = 0; r < nrow; ++r) { pout[r] = 0.0; }
        cpp11::writable::integers nnz_per_row(nrow);
        int *pnnz = INTEGER(nnz_per_row.data());
        for (int r = 0; r < nrow; ++r) { pnnz[r] = 0; }

        const int nthreads = dafr_omp_get_max_threads_capped(ncol, threshold);
        std::vector<std::vector<double>> tsum(nthreads,
            std::vector<double>(nrow, 0.0));
        std::vector<std::vector<int>>    tnnz(nthreads,
            std::vector<int>(nrow, 0));

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            const int tid = dafr_omp_get_thread_num();
            std::vector<double> &ts = tsum[tid];
            std::vector<int>    &tn = tnnz[tid];
            for (int k = pp[j]; k < pp[j + 1]; ++k) {
                const int r = pi[k];
                ts[r] += std::log(px[k] + eps) * inv_log_base;
                tn[r] += 1;
            }
        }

        // Reduce thread buffers serially into the row outputs.
        for (int t = 0; t < nthreads; ++t) {
            const std::vector<double> &ts = tsum[t];
            const std::vector<int>    &tn = tnnz[t];
            for (int r = 0; r < nrow; ++r) {
                pout[r] += ts[r];
                pnnz[r] += tn[r];
            }
        }

        DAFR_PARALLEL_FOR(nrow >= threshold)
        for (int r = 0; r < nrow; ++r) {
            const int zeros = ncol - pnnz[r];
            pout[r] += zeros * zero_log;
            if (is_mean) pout[r] /= ncol;
        }
        return out;
    } else {
        cpp11::writable::doubles out(ncol);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            const int nnz_j = pp[j + 1] - pp[j];
            double s = 0.0;
            for (int k = pp[j]; k < pp[j + 1]; ++k) {
                s += std::log(px[k] + eps) * inv_log_base;
            }
            const int zeros = nrow - nnz_j;
            s += zeros * zero_log;
            pout[j] = is_mean ? s / nrow : s;
        }
        return out;
    }
}
```

Note on threading: the row-axis CSC variant uses per-thread accumulator buffers (one `nrow`-sized partial-sum + nnz-count array per OpenMP thread) so the inner nnz scan parallelises over columns without write contention on per-row counters. The post-parallel reduction over thread buffers is `O(nthreads * nrow)` — negligible relative to `O(nnz)` for typical UMI matrices. Memory cost: `nthreads * nrow * (8 + 4)` bytes — for 8 threads on a 100K-row matrix that's ~10 MB, acceptable. The two helpers `dafr_omp_get_max_threads_capped(work, threshold)` and `dafr_omp_get_thread_num()` need to be added to `src/openmp_shim.h` if not already present (the latter must return `0` when `_OPENMP` is not defined; the former returns `1` when work falls below threshold).

Edit `src/openmp_shim.h` to add (if missing):

```cpp
#ifdef _OPENMP
#include <omp.h>
inline int dafr_omp_get_thread_num() { return omp_get_thread_num(); }
inline int dafr_omp_get_max_threads_capped(int work, int threshold) {
    if (work < threshold) return 1;
    return omp_get_max_threads();
}
#else
inline int dafr_omp_get_thread_num() { return 0; }
inline int dafr_omp_get_max_threads_capped(int /*work*/, int /*threshold*/) { return 1; }
#endif
```

- [ ] **Step 4: Regenerate `R/cpp11.R`**

```
Rscript -e 'cpp11::cpp_register()'
```

- [ ] **Step 5: Add the peephole in `R/query_eval.R`**

Modify `.eval_query` to detect the fused motif:

```r
.eval_query <- function(daf, ast) {
    state <- list(kind = "init", value = NULL, if_missing = NULL)
    i <- 1L
    n <- length(ast)
    while (i <= n) {
        node <- ast[[i]]
        # Lookahead 1: IfMissing
        if (i < n && identical(ast[[i + 1L]]$op, "IfMissing")) {
            state$if_missing <- ast[[i + 1L]]$default
            state <- .apply_node(node, state, daf)
            state$if_missing <- NULL
            i <- i + 2L
            next
        }
        # Lookahead 1: fused Log + Sum/Mean reduction.
        if (i < n &&
            identical(node$op, "Eltwise") &&
            identical(node$name, "Log") &&
            ast[[i + 1L]]$op %in% c("ReduceToColumn", "ReduceToRow") &&
            ast[[i + 1L]]$reduction %in% c("Sum", "Mean") &&
            length(ast[[i + 1L]]$params) == 0L &&
            identical(state$kind, "matrix")) {
            fused <- .try_fused_log_reduce(node, ast[[i + 1L]], state, daf)
            if (!is.null(fused)) {
                state <- fused
                i <- i + 2L
                next
            }
        }
        state <- .apply_node(node, state, daf)
        i <- i + 1L
    }
    state$value
}

.try_fused_log_reduce <- function(log_node, red_node, state, daf) {
    fn <- get_eltwise(log_node$name)
    if (!identical(attr(fn, ".dafr_builtin"), "Log")) return(NULL)
    params <- .coerce_params(log_node$params)
    eps <- params$eps %||% 0
    base <- params$base %||% exp(1)
    threshold <- as.integer(dafr_opt("dafr.omp_threshold"))
    axis <- if (identical(red_node$op, "ReduceToColumn")) 0L else 1L
    reducer <- red_node$reduction
    m <- state$value

    if (methods::is(m, "dgCMatrix")) {
        # CSC: implicit zeros are NOT in @x. The kernel accounts for them by
        # adding (n_zeros * log(eps)/log(base)) per row/column. With eps <= 0
        # that contribution is -Inf or NaN and silently poisons every row/col
        # that has any implicit zero (which for typical UMI matrices is
        # essentially all of them). Bail unconditionally in that case so the
        # unfused path runs and the user sees the -Inf / NaN they asked for.
        if (eps <= 0) return(NULL)
        out <- kernel_log_reduce_csc_cpp(
            m@x, m@i, m@p,
            nrow(m), ncol(m),
            eps, base, axis, reducer, threshold
        )
        target_axis <- if (axis == 0L) state$rows_axis else state$cols_axis
        names(out) <- format_axis_array(daf, target_axis)
        return(list(kind = "vector", axis = target_axis, value = out))
    }
    if (is.matrix(m)) {
        # Dense: the value at every cell is materialised, so eps == 0 with
        # actual zero entries produces -Inf in those cells and the per-row
        # sum becomes -Inf. The user can see this. Only bail if eps < 0
        # which is always nonsense.
        if (eps < 0) return(NULL)
        if (eps == 0 && any(m == 0, na.rm = TRUE)) return(NULL)
        out <- kernel_log_reduce_dense_cpp(
            m, eps, base, axis, reducer, threshold
        )
        target_axis <- if (axis == 0L) state$rows_axis else state$cols_axis
        names(out) <- format_axis_array(daf, target_axis)
        return(list(kind = "vector", axis = target_axis, value = out))
    }
    NULL
}
```

Note: the lookahead chain inside `.eval_query` is now ordered IfMissing → fused-Log-reduce → default `.apply_node`. Both lookaheads are guarded by `i < n` and check the *next* node by op tag; they consume two AST nodes via `i + 2L` when they fire.

- [ ] **Step 6: Build, run new tests + full query suite — expect pass**

```
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_file("tests/testthat/test-perf-log-reduce-kernel.R"); testthat::test_dir("tests/testthat", filter = "query")'
```

Expected: PASS on all (including no regression on Julia query compat).

- [ ] **Step 7: Commit**

```bash
git add src/kernel_log_reduce.cpp R/cpp11.R R/query_eval.R tests/testthat/test-perf-log-reduce-kernel.R
git commit -m "perf(query-eval): fused kernel_log_reduce_{dense,csc} for Log->Sum|Mean motif"
```

### Task P5: Benchmark wall-clock + peak RSS, commit results

**Files:**
- Create: `dev/benchmarks/run-slice-4-perf-wedge.R`
- Create: `dev/benchmarks/slice-4-perf-wedge-<YYYY-MM-DD>.csv` (committed result)

Context: P1-P4 are four perf commits landed on faith without measurement. The justification is the densification footgun, but we have no numbers proving the wedge actually closes it. Slice 0 committed bake-off CSVs under `dev/benchmarks/`; we follow the same pattern. The benchmark must measure both the unfused-baseline (pre-wedge behaviour, simulated by routing around the fast paths) and the post-wedge path on representative dgCMatrix sizes — reads the same dafr code, just toggles the fast paths off via a hidden option for the baseline measurement.

- [ ] **Step 1: Add a runtime gate to skip the fast paths for baseline measurement**

In `R/options.R`, add a new internal-only option (default TRUE; flipping to FALSE forces the slow paths):

```r
.dafr_default_options <- list(
    dafr.cache.memory_mb     = 1024L,
    dafr.cache.disable       = FALSE,
    dafr.cache.stats         = FALSE,
    dafr.mmap                = TRUE,
    dafr.omp_threshold       = 10000L,
    dafr.inefficient         = "warn",
    dafr.verbose             = FALSE,
    dafr.perf.fast_paths     = TRUE   # off => bench-only baseline route
)
```

In `R/query_eval.R`, gate the three fast paths on `isTRUE(dafr_opt("dafr.perf.fast_paths"))`:

```r
# In .apply_eltwise (P2 sparse-Log fast path):
if (isTRUE(dafr_opt("dafr.perf.fast_paths")) &&
    identical(builtin, "Log") && ...) { ... }

# In .apply_reduction (P3 bare-reduction fast path):
if (isTRUE(dafr_opt("dafr.perf.fast_paths")) && length(params) == 0L) { ... }

# In .eval_query (P4 fused-kernel peephole):
if (isTRUE(dafr_opt("dafr.perf.fast_paths")) && i < n &&
    identical(node$op, "Eltwise") && ...) { ... }
```

Document the gate as bench-only in a one-line comment at the option declaration (the WHY is non-obvious; this comment earns its place per the in-code comments rule).

- [ ] **Step 2: Write the benchmark script `dev/benchmarks/run-slice-4-perf-wedge.R`**

```r
#!/usr/bin/env Rscript
# Slice 4 perf-wedge benchmark.
#
# Measures wall-clock + peak RSS for the dominant motif
#   @ cell @ gene :: UMIs % Log eps: 1 >| Sum
# at two representative sizes, with fast paths ON vs OFF.
# Writes results to dev/benchmarks/slice-4-perf-wedge-<DATE>.csv.

suppressPackageStartupMessages({
    library(dafr)
    library(Matrix)
})

set.seed(42)

make_umis <- function(nrow, ncol, density = 0.05, max_count = 50L) {
    nnz <- ceiling(nrow * ncol * density)
    i <- sample.int(nrow, nnz, replace = TRUE) - 1L
    j <- sample.int(ncol, nnz, replace = TRUE) - 1L
    x <- as.double(sample.int(max_count, nnz, replace = TRUE))
    sparseMatrix(
        i = i + 1L, j = j + 1L, x = x,
        dims = c(nrow, ncol)
    ) |> as("dgCMatrix")
}

bench_one <- function(label, nrow, ncol, fast_paths) {
    options(dafr.perf.fast_paths = fast_paths)
    d <- memory_daf(name = sprintf("bench-%s", label))
    add_axis(d, "cell", sprintf("c%d", seq_len(nrow)))
    add_axis(d, "gene", sprintf("g%d", seq_len(ncol)))
    m <- make_umis(nrow, ncol)
    set_matrix(d, "cell", "gene", "UMIs", m)

    gc(verbose = FALSE)
    rss_before <- as.numeric(sum(gc()[, 2]))
    t0 <- Sys.time()
    result <- get_query(d, "@ cell @ gene :: UMIs % Log eps: 1 >| Sum")
    elapsed <- as.numeric(Sys.time() - t0, units = "secs")
    rss_after <- as.numeric(sum(gc()[, 2]))
    list(
        label = label, nrow = nrow, ncol = ncol,
        fast_paths = fast_paths,
        elapsed_s = elapsed,
        peak_mb = rss_after - rss_before,
        result_len = length(result),
        result_sum = sum(result)
    )
}

shapes <- list(
    list(label = "10k_x_10k", nrow = 10000L, ncol = 10000L),
    list(label = "30k_x_30k", nrow = 30000L, ncol = 30000L)
)

rows <- list()
for (sh in shapes) {
    for (fp in c(FALSE, TRUE)) {
        cat(sprintf("> %s fast_paths=%s\n", sh$label, fp))
        rows[[length(rows) + 1L]] <- do.call(bench_one,
            c(list(label = sh$label), sh[c("nrow", "ncol")],
              list(fast_paths = fp))
        )
    }
}

df <- do.call(rbind, lapply(rows, as.data.frame))
date_str <- format(Sys.Date(), "%Y-%m-%d")
out_path <- file.path("dev", "benchmarks",
    sprintf("slice-4-perf-wedge-%s.csv", date_str))
write.csv(df, out_path, row.names = FALSE)
cat("wrote", out_path, "\n")
print(df)
```

- [ ] **Step 3: Run the benchmark**

```
cd ~/src/dafr-native
Rscript dev/benchmarks/run-slice-4-perf-wedge.R
```

Expected: produces `dev/benchmarks/slice-4-perf-wedge-<YYYY-MM-DD>.csv`. Visually inspect: at 30K×30K the `fast_paths = FALSE` row should show multi-GB peak_mb and elapsed in the tens of seconds; the `fast_paths = TRUE` row should show sub-GB peak and seconds-class elapsed. If the deltas are not at least 5× wall-clock + 10× peak_mb on the 30K×30K shape, P2/P3/P4 didn't actually fire — STOP, reread the dispatch logic, and confirm the peephole is matching.

- [ ] **Step 4: Commit results to dev repo**

```bash
cd ~/src/dafr-native/dev
git add benchmarks/run-slice-4-perf-wedge.R \
        benchmarks/slice-4-perf-wedge-<YYYY-MM-DD>.csv
git commit -m "bench(slice-4-perf-wedge): wall-clock + peak RSS, fast on/off, 10K^2 + 30K^2"
```

(Substitute the actual date in both the filename and commit message.)

- [ ] **Step 5: Commit the runtime gate to package repo**

```bash
cd ~/src/dafr-native
git add R/options.R R/query_eval.R
git commit -m "chore(perf): add dafr.perf.fast_paths bench gate (default TRUE)"
```

---

## Phase F — Slice 3 follow-ups

### Task F1: Remove dead view cache bucket

**Files:**
- Modify: `R/view_daf.R:115-118` (`viewer()` constructor)
- Modify: `tests/testthat/test-view-daf.R` (add assertion)

- [ ] **Step 1: Write failing test at the tail of `tests/testthat/test-view-daf.R`**

```r
test_that("viewer reuses base daf cache env (no per-view query bucket)", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B"))
    v <- viewer(d)
    expect_identical(S7::prop(v, "cache"), S7::prop(d, "cache"))
})
```

- [ ] **Step 2: Run test — expect failure**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-view-daf.R")'
```

Expected: FAIL — the current `viewer()` body calls `new_cache_env()` so the cache env is fresh.

- [ ] **Step 3: Change `viewer()` in `R/view_daf.R` to reuse the base cache + counters**

Replace the body block that creates a fresh cache env:

```r
viewer <- function(daf, name = NULL, axes = NULL, data = NULL) {
    if (is.null(name)) name <- paste0(S7::prop(daf, "name"), ".view")
    ViewDaf(
        name                    = name,
        internal                = new_internal_env(),
        cache                   = S7::prop(daf, "cache"),
        axis_version_counter    = S7::prop(daf, "axis_version_counter"),
        vector_version_counter  = S7::prop(daf, "vector_version_counter"),
        matrix_version_counter  = S7::prop(daf, "matrix_version_counter"),
        base                    = daf,
        view_axes               = .resolve_view_axes(daf, axes),
        view_scalars            = .resolve_view_scalars(daf, data),
        view_vectors            = .resolve_view_vectors(daf, data),
        view_matrices           = .resolve_view_matrices(daf, data)
    )
}
```

- [ ] **Step 4: Run the full view test file — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-view-daf.R")'
```

Expected: PASS on the new assertion + all existing view tests.

- [ ] **Step 5: Commit**

```bash
git add R/view_daf.R tests/testthat/test-view-daf.R
git commit -m "fix(view_daf): reuse base daf cache env (remove dead per-view bucket)"
```

### Task F2: NA-in-mask drops silently (Julia parity)

**Files:**
- Modify: `R/query_eval.R:251-255` (`.apply_end_mask`)
- Modify: `tests/testthat/test-query-eval-masks.R`

- [ ] **Step 1: Add failing test to `tests/testthat/test-query-eval-masks.R`**

```r
test_that("NA in masked property drops entries (Julia parity)", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    set_vector(d, "cell", "score", c(1.0, NA_real_, 3.0, NA_real_))
    # '> 0' on NA returns NA; Julia drops NA mask entries silently.
    # Expected kept entries: A, C.
    result <- get_query(d, "@ cell [ score > 0 ]")
    expect_identical(result, c("A", "C"))
})
```

- [ ] **Step 2: Run test — expect failure**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-masks.R")'
```

Expected: FAIL — current behaviour returns `c("A", NA_character_, "C", NA_character_)`.

- [ ] **Step 3: Fix `.apply_end_mask` in `R/query_eval.R`**

Replace the body:

```r
.apply_end_mask <- function(node, state, daf) {
    axis <- state$axis
    entries <- format_axis_array(daf, axis)
    mask <- state$pending_mask
    keep <- !is.na(mask) & mask
    list(kind = "axis", axis = axis, value = entries[keep])
}
```

- [ ] **Step 4: Run the full mask test file + the Julia query compat — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-masks.R"); testthat::test_file("tests/testthat/test-query-julia-compat.R")'
```

Expected: PASS on all.

- [ ] **Step 5: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-masks.R
git commit -m "fix(query-eval): drop NA in mask output (Julia parity)"
```

### Task F3: Real `IfNot` evaluator semantics — chain-lookup fallback

**Files:**
- Modify: `R/query_eval.R:228-231` (`.apply_if_not`)
- Modify: `R/query_eval.R:11-29` (`.eval_query`) for `IfNot` lookahead
- Create: `tests/testthat/test-query-eval-chains.R`

Context: in Julia, `IfNot` (`??`) is a chain-lookup annotation. `@ cell : metacell ?? : type` means: for every cell, look up `metacell`; treat empty string `""` as "no metacell"; for those cells, the chain-final result after the next `:type` lookup is `""` (drop these cells); `?? "X"` gives them `"X"` instead. In R we follow the Julia reading: `IfNot` captures a final-value sentinel, applied after the chained lookup completes; when the sentinel is absent (`??` alone), empty values propagate and the cells are dropped from the final result axis.

Slice 4 scope: implement `IfNot` for the linear one-hop chain `@ A : v ?? X : w` (vector `v` on axis A holds entries of axis B; look up `w` on axis B; return per-A vector). Multi-hop chains (`@ A : v ?? X : w : u`) are deferred unless trivial. AsAxis (Task F4) is the other half — they compose.

- [ ] **Step 1: Create `tests/testthat/test-query-eval-chains.R` with a failing test**

```r
test_that("IfNot drop: @ A : v ?? : w drops empty-value rows", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("C1", "C2", "C3"))
    add_axis(d, "metacell", c("M1", "M2"))
    set_vector(d, "cell", "metacell", c("M1", "", "M2"))
    set_vector(d, "metacell", "type", c("T1", "T2"))
    # "?? :" means drop cells whose metacell is "" (no default supplied).
    result <- get_query(d, "@ cell : metacell ?? =@ : type")
    expect_identical(as.character(result), c("T1", "T2"))
    expect_identical(names(result), c("C1", "C3"))
})

test_that("IfNot default: @ A : v ?? X : w uses X for empty-value rows", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("C1", "C2", "C3"))
    add_axis(d, "metacell", c("M1", "M2"))
    set_vector(d, "cell", "metacell", c("M1", "", "M2"))
    set_vector(d, "metacell", "type", c("T1", "T2"))
    result <- get_query(d, '@ cell : metacell ?? "UNK" =@ : type')
    expect_identical(as.character(result), c("T1", "UNK", "T2"))
    expect_identical(names(result), c("C1", "C2", "C3"))
})
```

Note: these tests depend on Task F4 (`AsAxis` chained resolution). In isolation they will error with "AsAxis stub"; we will build F3 + F4 together and commit F4 before these tests turn green. For Task F3 the scope is narrower: store the IfNot value on state; Task F4 consumes it.

Revise Step 1 to a narrower F3-only failing test:

```r
test_that("IfNot stores sentinel on evaluator state for downstream chain lookup", {
    ast <- parse_query('@ cell : metacell ?? "UNK" =@ : type')
    # The evaluator should leave state$if_not populated after the IfNot node
    # without yet applying it (AsAxis consumes it in F4).
    # Indirect test: confirm the query parses and its canonical string
    # round-trips. Real semantics land in F4.
    canon <- canonical_query('@ cell : metacell ?? "UNK" =@ : type')
    expect_match(canon, 'metacell \\?\\? "UNK" =@')
})
```

- [ ] **Step 2: Run test — expect pass (canonicalisation was already in Slice 3)**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R")'
```

Expected: PASS — the parser / canoniser path is unchanged from Slice 3.

- [ ] **Step 3: Replace `.apply_if_not` in `R/query_eval.R` to populate state**

```r
.apply_if_not <- function(node, state, daf) {
    # Record the chain-final sentinel for consumption by AsAxis-driven
    # chained lookup. An absent value (node$value == NULL) means drop
    # empty entries; a present value is substituted for empty entries.
    state$if_not_present <- TRUE
    state$if_not_value <- node$value
    state
}
```

- [ ] **Step 4: Rerun tests — expect pass**

Expected: PASS. F4 will consume `state$if_not_*`.

- [ ] **Step 5: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-chains.R
git commit -m "feat(query-eval): IfNot records chain-lookup sentinel on state"
```

- [ ] **Step 6: Audit the existing 17-query Julia fixture for multi-hop IfNot**

Slice 3's kickoff explicitly warned that `IfNot` may need wider lookahead for chained lookups. Slice 4 only implements single-hop. Before declaring F3 done, confirm none of the existing fixture queries silently depend on multi-hop IfNot behaviour:

```
Rscript -e 'library(jsonlite); fx <- fromJSON("tests/testthat/fixtures/julia-queries/fixture.json", simplifyVector = FALSE); for (q in fx) { qs <- q$query; hops <- length(gregexpr("\\?\\?", qs)[[1L]]); after_first_q <- sub(".*\\?\\?[^:]*", "", qs); chain_after_q <- length(gregexpr(":[^:]", after_first_q)[[1L]]); if (hops >= 1 && chain_after_q >= 2) cat("multi-hop?:", qs, "\n") }'
```

If the script prints any queries, those are candidate multi-hop IfNot chains. Run `test-query-julia-compat.R` after F4 lands; any fixture queries that fail will identify which (if any) need multi-hop support. If failures appear, document them in the NEWS "Known limitations" subsection and add a Slice 5 deferral note in the exit gate. If no failures appear, no action — single-hop is sufficient for the current fixture.

### Task F4: Real `AsAxis` evaluator semantics — single-hop chained lookup

**Files:**
- Modify: `R/query_eval.R:232-236` (`.apply_as_axis`)
- Modify: `R/query_eval.R:154-187` (`.apply_lookup_vector`) for chained consumption
- Modify: `tests/testthat/test-query-eval-chains.R` (un-narrow the F3 tests)

Context: `AsAxis` (`=@`) marks a vector's values as entries of another axis. The next `:` lookup then resolves against that axis. Combined with `IfNot` for empty-value handling, this gives the chained-lookup workflow.

- [ ] **Step 1: Replace the narrowed F3 test with the full F3/F4 integration tests**

Overwrite `tests/testthat/test-query-eval-chains.R`:

```r
test_that("AsAxis drop: @ A : v =@ : w drops empty-value rows when IfNot bare", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("C1", "C2", "C3"))
    add_axis(d, "metacell", c("M1", "M2"))
    set_vector(d, "cell", "metacell", c("M1", "", "M2"))
    set_vector(d, "metacell", "type", c("T1", "T2"))
    result <- get_query(d, "@ cell : metacell ?? =@ : type")
    expect_identical(as.character(result), c("T1", "T2"))
    expect_identical(names(result), c("C1", "C3"))
})

test_that("AsAxis default: @ A : v ?? X =@ : w substitutes X for empty rows", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("C1", "C2", "C3"))
    add_axis(d, "metacell", c("M1", "M2"))
    set_vector(d, "cell", "metacell", c("M1", "", "M2"))
    set_vector(d, "metacell", "type", c("T1", "T2"))
    result <- get_query(d, '@ cell : metacell ?? "UNK" =@ : type')
    expect_identical(as.character(result), c("T1", "UNK", "T2"))
    expect_identical(names(result), c("C1", "C2", "C3"))
})

test_that("AsAxis with explicit target: @ A : v =@ B : w", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("C1", "C2"))
    add_axis(d, "batch", c("B1", "B2"))
    set_vector(d, "cell", "origin", c("B1", "B2"))
    set_vector(d, "batch", "year", c(2023L, 2024L))
    result <- get_query(d, "@ cell : origin =@ batch : year")
    expect_identical(as.integer(result), c(2023L, 2024L))
    expect_identical(names(result), c("C1", "C2"))
})
```

- [ ] **Step 2: Run tests — expect failure**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R")'
```

Expected: FAIL — current `.apply_as_axis` is identity; chained `:` dispatches against the wrong axis.

- [ ] **Step 3: Replace `.apply_as_axis` + chain handling in `R/query_eval.R`**

```r
.apply_as_axis <- function(node, state, daf) {
    if (!identical(state$kind, "vector")) {
        stop("'=@' requires a vector in scope", call. = FALSE)
    }
    target_axis <- node$axis_name
    if (is.null(target_axis)) {
        # Empty AsAxis: use the vector's base name as axis name (Julia convention).
        # We stash this as an inference in a downstream step.
        target_axis <- NA_character_  # resolved lazily in .apply_lookup_vector chain
    }
    state$chain_target_axis <- target_axis
    state$kind <- "vector_axis"
    state
}
```

Then extend `.apply_lookup_vector` to handle the chain-consumption case:

```r
.apply_lookup_vector <- function(node, state, daf) {
    if (identical(state$kind, "vector_axis")) {
        return(.apply_chained_lookup_vector(node, state, daf))
    }
    if (!identical(state$kind, "axis")) {
        stop(sprintf("':' requires an axis in scope (got %s)", state$kind),
            call. = FALSE
        )
    }
    axis <- state$axis
    if (is.null(node$name)) {
        state$kind <- "vector_names_ready"
        return(state)
    }
    if (!format_has_vector(daf, axis, node$name)) {
        if (!is.null(state$if_missing)) {
            return(list(
                kind = "vector",
                value = rep(
                    state$if_missing,
                    format_axis_length(daf, axis)
                ),
                axis = axis
            ))
        }
        stop(sprintf(
            "no vector %s on axis %s",
            sQuote(node$name), sQuote(axis)
        ), call. = FALSE)
    }
    list(
        kind = "vector",
        value = format_get_vector(daf, axis, node$name),
        axis = axis
    )
}

.apply_chained_lookup_vector <- function(node, state, daf) {
    # state$value: vector of length len(state$axis), entries are names of
    # state$chain_target_axis. Look up node$name on chain_target_axis,
    # index by the current vector's values.
    base_axis <- state$axis
    pivot_values <- state$value
    target_axis <- state$chain_target_axis
    if (is.na(target_axis)) {
        # Empty AsAxis: target axis = pivot vector property name (needs parser
        # context). For Slice 4 we require explicit axis; bare '=@' is not yet
        # supported unless there is an obvious singular candidate.
        stop("bare '=@' without an explicit target axis is not yet supported",
            call. = FALSE
        )
    }
    if (!format_has_axis(daf, target_axis)) {
        stop(sprintf("AsAxis target axis %s does not exist",
            sQuote(target_axis)
        ), call. = FALSE)
    }
    lookup_vec <- format_get_vector(daf, target_axis, node$name)
    target_entries <- format_axis_array(daf, target_axis)
    indices <- match(pivot_values, target_entries)

    empty_mask <- is.na(indices) | (!nzchar(pivot_values) & is.character(pivot_values))
    out <- rep(NA, length(pivot_values))
    mode(out) <- mode(lookup_vec)
    out[!empty_mask] <- lookup_vec[indices[!empty_mask]]

    base_entries <- format_axis_array(daf, base_axis)
    if (isTRUE(state$if_not_present)) {
        sentinel <- state$if_not_value
        if (is.null(sentinel)) {
            # Drop empty
            keep <- !empty_mask
            out <- out[keep]
            base_entries <- base_entries[keep]
        } else {
            # Coerce sentinel to the lookup vector's type
            sentinel_typed <- methods::as(sentinel, class(lookup_vec)[[1L]])
            out[empty_mask] <- sentinel_typed
        }
    } else {
        # No IfNot: empty values that match no target become NA in output.
        # Julia errors in this case; we error to match.
        if (any(empty_mask)) {
            stop(
                sprintf(
                    "chain lookup on axis %s has %d empty pivot values and no '??' sentinel",
                    sQuote(base_axis), sum(empty_mask)
                ),
                call. = FALSE
            )
        }
    }
    names(out) <- base_entries
    list(kind = "vector", value = out, axis = base_axis)
}
```

Add `"vector_axis"` to the list of known `state$kind` values in the comment block at the top of `R/query_eval.R`. Also add an entry to the closed enum documentation.

- [ ] **Step 4: Run the chain tests + the full query suite — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R"); testthat::test_dir("tests/testthat", filter = "query")'
```

Expected: PASS on the chain tests + no regression across all query tests.

- [ ] **Step 5: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-chains.R
git commit -m "feat(query-eval): AsAxis single-hop chained lookup + IfNot sentinel"
```

### Task F5: ViewDaf axis rename propagation to vector/matrix reads

**Files:**
- Modify: `R/view_daf.R:308-324` (`.view_query_for_vector`, `.view_query_for_matrix`)
- Modify: `R/view_daf.R:400-430` (`format_get_vector` / `format_get_matrix` / `format_has_vector` / `format_has_matrix` methods)
- Create: `tests/testthat/test-view-rename-propagation.R`

Context: `viewer(d, axes = list(list("obs", "@ cell")))` renames axis `cell` → `obs`. In Slice 3, `axis_vector(v, "obs")` works but `get_vector(v, "obs", "donor")` fails (the view's `view_vectors` keys are still of the form `"cell|donor"`). Fix: the view axis-resolver needs to track rename mapping from view-name → base-name; the vector/matrix dispatch rewrites the axis name at lookup.

- [ ] **Step 1: Create `tests/testthat/test-view-rename-propagation.R`**

```r
test_that("viewer rename: get_vector(view, renamed_axis, name) resolves", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B", "C"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
    v <- viewer(d, name = "view",
        axes = list(list("obs", "@ cell")),
        data = list(list(c("obs", "donor"), "="))
    )
    out <- get_vector(v, "obs", "donor")
    expect_identical(as.character(out), c("d1", "d2", "d1"))
    expect_identical(names(out), c("A", "B", "C"))
})

test_that("viewer rename: get_matrix(view, renamed_rows, cols, name) resolves", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B"))
    add_axis(d, "gene", c("X", "Y"))
    m <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    v <- viewer(d, name = "view",
        axes = list(
            list("obs",  "@ cell"),
            list("feat", "@ gene")
        ),
        data = list(list(c("obs", "feat", "UMIs"), "="))
    )
    out <- get_matrix(v, "obs", "feat", "UMIs")
    expect_equal(unname(as.matrix(out)), m)
    expect_identical(rownames(out), c("A", "B"))
    expect_identical(colnames(out), c("X", "Y"))
})
```

- [ ] **Step 2: Run test — expect failure**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-view-rename-propagation.R")'
```

Expected: FAIL — `view_vectors` / `view_matrices` lookup keyed by renamed axis name misses the base entry.

- [ ] **Step 3: Store rename mapping + resolver in `R/view_daf.R`**

In `viewer()`, after `view_axes` is built, record the renamed-axis-to-base-axis mapping as an extra slot on the ViewDaf class. Extend the class definition:

```r
ViewDaf <- S7::new_class(
    name = "ViewDaf",
    package = "dafr",
    parent = DafReadOnly,
    properties = list(
        base              = DafReader,
        view_axes         = S7::class_list,
        view_axis_renames = S7::class_list,   # view_name -> base_name
        view_scalars      = S7::class_list,
        view_vectors      = S7::class_list,
        view_matrices     = S7::class_list
    )
)
```

Compute the rename map from parsed axis queries: an axis override query like `"@ cell"` (or `. cell` after canonicalisation) means rename `view_name = obs` -> `base_axis = cell`. Extract via `query_axis_name()`:

```r
.resolve_view_axis_renames <- function(daf, view_axes) {
    out <- list()
    for (view_name in names(view_axes)) {
        q <- view_axes[[view_name]]
        if (identical(q, "=") || identical(q, view_name)) {
            out[[view_name]] <- view_name
        } else {
            base_axis <- query_axis_name(q)
            if (is.na(base_axis)) {
                stop(sprintf(
                    "view axis %s: cannot infer base axis from query %s",
                    sQuote(view_name), sQuote(q)
                ), call. = FALSE)
            }
            out[[view_name]] <- base_axis
        }
    }
    out
}
```

Update `viewer()` to populate the new slot:

```r
viewer <- function(daf, name = NULL, axes = NULL, data = NULL) {
    if (is.null(name)) name <- paste0(S7::prop(daf, "name"), ".view")
    view_axes <- .resolve_view_axes(daf, axes)
    view_renames <- .resolve_view_axis_renames(daf, view_axes)
    ViewDaf(
        name                    = name,
        internal                = new_internal_env(),
        cache                   = S7::prop(daf, "cache"),
        axis_version_counter    = S7::prop(daf, "axis_version_counter"),
        vector_version_counter  = S7::prop(daf, "vector_version_counter"),
        matrix_version_counter  = S7::prop(daf, "matrix_version_counter"),
        base                    = daf,
        view_axes               = view_axes,
        view_axis_renames       = view_renames,
        view_scalars            = .resolve_view_scalars(daf, data),
        view_vectors            = .resolve_view_vectors(daf, data, view_renames),
        view_matrices           = .resolve_view_matrices(daf, data, view_renames)
    )
}
```

Update `.resolve_view_vectors` and `.resolve_view_matrices` to key by view-axis-name but resolve to base-axis-name for the query generation:

```r
.resolve_view_vectors <- function(daf, data, renames) {
    out <- list()
    # Seed every renamed axis with every base vector visible.
    for (view_axis in names(renames)) {
        base_axis <- renames[[view_axis]]
        for (v in format_vectors_set(daf, base_axis)) {
            out[[paste(view_axis, v, sep = "|")]] <- list(
                view_axis = view_axis,
                base_axis = base_axis,
                name = v,
                query = "="
            )
        }
    }
    if (is.null(data)) return(out)
    for (item in .flatten_view_data(data)) {
        parsed <- .parse_view_item(item)
        if (is.character(parsed$key) && length(parsed$key) == 2L) {
            a <- parsed$key[[1L]]; v <- parsed$key[[2L]]; q <- parsed$value
            base_axis <- renames[[a]] %||% a
            if (identical(a, "*") && identical(v, "*")) {
                if (is.null(q)) {
                    out <- list()
                } else if (!identical(q, "=")) {
                    for (k in names(out)) out[[k]]$query <- q
                }
            } else {
                key <- paste(a, v, sep = "|")
                if (is.null(q)) {
                    out[[key]] <- NULL
                } else {
                    out[[key]] <- list(
                        view_axis = a, base_axis = base_axis,
                        name = v, query = q
                    )
                }
            }
        }
    }
    out
}

.resolve_view_matrices <- function(daf, data, renames) {
    out <- list()
    for (rv in names(renames)) {
        rb <- renames[[rv]]
        for (cv in names(renames)) {
            cb <- renames[[cv]]
            for (m in format_matrices_set(daf, rb, cb)) {
                out[[paste(rv, cv, m, sep = "|")]] <- list(
                    view_rows = rv, view_cols = cv,
                    base_rows = rb, base_cols = cb,
                    name = m, query = "="
                )
            }
        }
    }
    if (is.null(data)) return(out)
    for (item in .flatten_view_data(data)) {
        parsed <- .parse_view_item(item)
        if (is.character(parsed$key) && length(parsed$key) == 3L) {
            rr <- parsed$key[[1L]]; cc <- parsed$key[[2L]]; nn <- parsed$key[[3L]]
            q <- parsed$value
            rb <- renames[[rr]] %||% rr
            cb <- renames[[cc]] %||% cc
            if (rr == "*" && cc == "*" && nn == "*") {
                if (is.null(q)) out <- list()
            } else {
                key <- paste(rr, cc, nn, sep = "|")
                if (is.null(q)) {
                    out[[key]] <- NULL
                } else {
                    out[[key]] <- list(
                        view_rows = rr, view_cols = cc,
                        base_rows = rb, base_cols = cb,
                        name = nn, query = q
                    )
                }
            }
        }
    }
    out
}
```

Update the query rewriters to use the base axis name:

```r
.view_query_for_vector <- function(view, axis, name) {
    key <- paste(axis, name, sep = "|")
    override <- view@view_vectors[[key]]
    if (is.null(override)) return(NULL)
    if (identical(override$query, "=")) {
        return(sprintf("@ %s : %s", override$base_axis, override$name))
    }
    override$query
}

.view_query_for_matrix <- function(view, rows_axis, columns_axis, name) {
    key <- paste(rows_axis, columns_axis, name, sep = "|")
    override <- view@view_matrices[[key]]
    if (is.null(override)) return(NULL)
    if (identical(override$query, "=")) {
        return(sprintf(
            "@ %s @ %s :: %s",
            override$base_rows, override$base_cols, override$name
        ))
    }
    override$query
}
```

- [ ] **Step 4: Run the new test + existing view tests — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-view-rename-propagation.R"); testthat::test_dir("tests/testthat", filter = "view")'
```

Expected: PASS across all view tests (rename propagation fix plus zero regression).

- [ ] **Step 5: Commit**

```bash
git add R/view_daf.R tests/testthat/test-view-rename-propagation.R
git commit -m "feat(view_daf): axis rename propagates to vector/matrix reads"
```

### Task F6: ViewDaf axis filter propagation to vector/matrix reads

**Files:**
- Modify: `R/view_daf.R` (vector / matrix dispatch)
- Create: `tests/testthat/test-view-filter-propagation.R`

Context: `viewer(d, axes = list(list("cell", "@ cell [ keep ]")))` filters axis `cell` to entries where `keep` is truthy. In Slice 3 `axis_vector(v, "cell")` returns the filtered entries, but `get_vector(v, "cell", "donor")` returns the full base vector. Fix: if the axis override query carries a mask, evaluate the mask to a logical index and apply it to the vector / matrix read before returning.

- [ ] **Step 1: Create `tests/testthat/test-view-filter-propagation.R`**

```r
test_that("viewer filter: get_vector(view, axis, name) returns filtered entries", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B", "C", "D"))
    set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE, FALSE))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1", "d3"))
    v <- viewer(d, name = "view",
        axes = list(list("cell", "@ cell [ keep ]")),
        data = list(list(c("cell", "donor"), "="))
    )
    out <- get_vector(v, "cell", "donor")
    expect_identical(as.character(out), c("d1", "d1"))
    expect_identical(names(out), c("A", "C"))
})

test_that("viewer filter: get_matrix rows/cols filtered", {
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("A", "B", "C"))
    add_axis(d, "gene", c("X", "Y"))
    set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE))
    m <- matrix(1:6, nrow = 3, ncol = 2)
    set_matrix(d, "cell", "gene", "UMIs", m)
    v <- viewer(d, name = "view",
        axes = list(
            list("cell", "@ cell [ keep ]"),
            list("gene", "@ gene")
        ),
        data = list(list(c("cell", "gene", "UMIs"), "="))
    )
    out <- get_matrix(v, "cell", "gene", "UMIs")
    expect_equal(unname(as.matrix(out)), m[c(1, 3), , drop = FALSE])
    expect_identical(rownames(out), c("A", "C"))
    expect_identical(colnames(out), c("X", "Y"))
})
```

- [ ] **Step 2: Run — expect failure**

Expected: FAIL — filter is currently not applied to vector/matrix reads.

- [ ] **Step 3: Extend viewer to precompute per-axis filter indices**

Add another slot to `ViewDaf` for filter indices — or evaluate on demand. For predictable performance we precompute at `viewer()` construction and store a 1-based integer index vector per view axis:

```r
ViewDaf <- S7::new_class(
    name = "ViewDaf",
    package = "dafr",
    parent = DafReadOnly,
    properties = list(
        base               = DafReader,
        view_axes          = S7::class_list,
        view_axis_renames  = S7::class_list,
        view_axis_indices  = S7::class_list,   # view_name -> integer indices into base axis
        view_scalars       = S7::class_list,
        view_vectors       = S7::class_list,
        view_matrices      = S7::class_list
    )
)
```

Compute indices by evaluating each axis override query against the base and matching against the base entry vector:

```r
.resolve_view_axis_indices <- function(daf, view_axes, renames) {
    out <- list()
    for (view_name in names(view_axes)) {
        q <- view_axes[[view_name]]
        base_axis <- renames[[view_name]]
        base_entries <- format_axis_array(daf, base_axis)
        if (identical(q, "=") || identical(q, view_name)) {
            out[[view_name]] <- seq_along(base_entries)
        } else {
            view_entries <- get_query(daf, q)
            idx <- match(view_entries, base_entries)
            if (anyNA(idx)) {
                stop(sprintf(
                    "view axis %s: entry %s not in base axis %s",
                    sQuote(view_name),
                    sQuote(view_entries[is.na(idx)][[1L]]),
                    sQuote(base_axis)
                ), call. = FALSE)
            }
            out[[view_name]] <- idx
        }
    }
    out
}
```

Update `viewer()`:

```r
viewer <- function(daf, name = NULL, axes = NULL, data = NULL) {
    if (is.null(name)) name <- paste0(S7::prop(daf, "name"), ".view")
    view_axes    <- .resolve_view_axes(daf, axes)
    view_renames <- .resolve_view_axis_renames(daf, view_axes)
    view_indices <- .resolve_view_axis_indices(daf, view_axes, view_renames)
    ViewDaf(
        name = name,
        internal = new_internal_env(),
        cache = S7::prop(daf, "cache"),
        axis_version_counter = S7::prop(daf, "axis_version_counter"),
        vector_version_counter = S7::prop(daf, "vector_version_counter"),
        matrix_version_counter = S7::prop(daf, "matrix_version_counter"),
        base = daf,
        view_axes = view_axes,
        view_axis_renames = view_renames,
        view_axis_indices = view_indices,
        view_scalars = .resolve_view_scalars(daf, data),
        view_vectors = .resolve_view_vectors(daf, data, view_renames),
        view_matrices = .resolve_view_matrices(daf, data, view_renames)
    )
}
```

Update `format_get_vector` / `format_get_matrix` / `format_axis_length` / `format_axis_array` methods to apply filter indices:

```r
S7::method(
    format_axis_length,
    list(ViewDaf, S7::class_character)
) <- function(daf, axis) {
    length(daf@view_axis_indices[[axis]])
}

S7::method(
    format_axis_array,
    list(ViewDaf, S7::class_character)
) <- function(daf, axis) {
    idx <- daf@view_axis_indices[[axis]]
    base_axis <- daf@view_axis_renames[[axis]]
    format_axis_array(daf@base, base_axis)[idx]
}

S7::method(
    format_get_vector,
    list(ViewDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    q_str <- .view_query_for_vector(daf, axis, name)
    if (is.null(q_str)) {
        stop(sprintf(
            "no vector %s on view axis %s",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
    raw <- get_query(daf@base, q_str)
    idx <- daf@view_axis_indices[[axis]]
    raw[idx]
}

S7::method(
    format_get_matrix,
    list(ViewDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    q_str <- .view_query_for_matrix(daf, rows_axis, columns_axis, name)
    if (is.null(q_str)) {
        stop(sprintf(
            "no matrix %s on view axes (%s, %s)",
            sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
        ), call. = FALSE)
    }
    raw <- get_query(daf@base, q_str)
    r_idx <- daf@view_axis_indices[[rows_axis]]
    c_idx <- daf@view_axis_indices[[columns_axis]]
    if (methods::is(raw, "dgCMatrix") || methods::is(raw, "lgCMatrix")) {
        raw[r_idx, c_idx, drop = FALSE]
    } else {
        raw[r_idx, c_idx, drop = FALSE]
    }
}
```

- [ ] **Step 4: Run tests — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-view-filter-propagation.R"); testthat::test_dir("tests/testthat", filter = "view")'
```

Expected: PASS across all view tests.

- [ ] **Step 5: Commit**

```bash
git add R/view_daf.R tests/testthat/test-view-filter-propagation.R
git commit -m "feat(view_daf): axis filter propagates to vector/matrix reads"
```

---

## Phase C — Chains

### Task C1: Scaffold `R/chain_daf.R` + classes + test file

**Files:**
- Create: `R/chain_daf.R`
- Create: `tests/testthat/test-chain-readers.R`
- Create: `tests/testthat/test-chain-writers.R`

- [ ] **Step 1: Write a smoke test in `tests/testthat/test-chain-readers.R`**

```r
test_that("chain_reader exists and produces a DafReadOnly", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_s3_class(ch, "dafr::DafReadOnly")
    expect_identical(S7::prop(ch, "name"), "chain")
})

test_that("chain_reader with empty list raises", {
    expect_error(chain_reader(list(), name = "empty"), "empty chain")
})

test_that("chain_reader with single daf returns a read-only view of it", {
    d <- memory_daf(name = "only")
    ch <- chain_reader(list(d), name = "chain")
    expect_s3_class(ch, "dafr::DafReadOnly")
})
```

And a smoke test in `tests/testthat/test-chain-writers.R`:

```r
test_that("chain_writer exists and produces a DafWriter", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_s3_class(ch, "dafr::DafWriter")
    expect_identical(S7::prop(ch, "name"), "chain")
})

test_that("chain_writer with empty list raises", {
    expect_error(chain_writer(list(), name = "empty"), "empty chain")
})
```

- [ ] **Step 2: Run both test files — expect failure**

Expected: FAIL — `chain_reader` / `chain_writer` undefined.

- [ ] **Step 3: Implement `R/chain_daf.R`**

```r
#' @include classes.R format_api.R cache.R
NULL

#' Read-only chain of DafReaders.
#'
#' Produced by [chain_reader()]. Every `format_*` read falls through the
#' chain in reverse order (last wins); writes raise.
#' @inheritParams DafReader
#' @param dafs Ordered list of base `DafReader`s.
#' @export
ReadOnlyChainDaf <- S7::new_class(
    name = "ReadOnlyChainDaf",
    package = "dafr",
    parent = DafReadOnly,
    properties = list(dafs = S7::class_list)
)

#' Write chain of DafReaders with a final DafWriter.
#'
#' Produced by [chain_writer()]. Reads fall through in reverse order
#' (writer last-wins); writes go to the final writer; deletes succeed
#' only if the entry does not exist in any earlier daf.
#' @inheritParams DafReader
#' @param dafs Ordered list of base `DafReader`s.
#' @param writer The final `DafWriter` (== `dafs[[length(dafs)]]`).
#' @export
WriteChainDaf <- S7::new_class(
    name = "WriteChainDaf",
    package = "dafr",
    parent = DafWriter,
    properties = list(
        dafs   = S7::class_list,
        writer = DafWriter
    )
)

#' Create a read-only chain of DafReaders.
#'
#' @param dafs Ordered list of `DafReader`s. Later entries override earlier
#'   entries on read.
#' @param name Optional chain name; defaults to `paste(names, collapse = ";")`.
#' @return A `ReadOnlyChainDaf`.
#' @export
chain_reader <- function(dafs, name = NULL) {
    if (!is.list(dafs) || length(dafs) == 0L) {
        stop(sprintf("empty chain%s",
            if (is.null(name)) "" else paste0(": ", name)
        ), call. = FALSE)
    }
    for (d in dafs) {
        if (!S7::S7_inherits(d, DafReader)) {
            stop("chain entries must all be DafReaders", call. = FALSE)
        }
    }
    if (is.null(name)) {
        name <- paste(vapply(dafs, function(d) S7::prop(d, "name"), character(1)),
            collapse = ";"
        )
    }
    .validate_chain_axes(dafs, name)
    ReadOnlyChainDaf(
        name                   = name,
        internal               = new_internal_env(),
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env(),
        dafs                   = dafs
    )
}

#' Create a chain of DafReaders with a final DafWriter.
#' @inheritParams chain_reader
#' @return A `WriteChainDaf`.
#' @export
chain_writer <- function(dafs, name = NULL) {
    if (!is.list(dafs) || length(dafs) == 0L) {
        stop(sprintf("empty chain%s",
            if (is.null(name)) "" else paste0(": ", name)
        ), call. = FALSE)
    }
    for (d in dafs) {
        if (!S7::S7_inherits(d, DafReader)) {
            stop("chain entries must all be DafReaders", call. = FALSE)
        }
    }
    writer <- dafs[[length(dafs)]]
    if (!S7::S7_inherits(writer, DafWriter)) {
        stop(sprintf(
            "read-only final data: %s in write chain%s",
            S7::prop(writer, "name"),
            if (is.null(name)) "" else paste0(": ", name)
        ), call. = FALSE)
    }
    if (is.null(name)) {
        name <- paste(vapply(dafs, function(d) S7::prop(d, "name"), character(1)),
            collapse = ";"
        )
    }
    .validate_chain_axes(dafs, name)
    WriteChainDaf(
        name                   = name,
        internal               = new_internal_env(),
        cache                  = new_cache_env(),
        axis_version_counter   = new_counter_env(),
        vector_version_counter = new_counter_env(),
        matrix_version_counter = new_counter_env(),
        dafs                   = dafs,
        writer                 = writer
    )
}

.validate_chain_axes <- function(dafs, chain_name) {
    seen <- list()   # axis -> list(daf_name, entries)
    for (d in dafs) {
        dname <- S7::prop(d, "name")
        for (axis in format_axes_set(d)) {
            entries <- format_axis_array(d, axis)
            prior <- seen[[axis]]
            if (is.null(prior)) {
                seen[[axis]] <- list(name = dname, entries = entries)
                next
            }
            if (length(entries) != length(prior$entries)) {
                stop(sprintf(
                    "different number of entries: %d for the axis: %s in the daf data: %s from the number of entries: %d for the axis: %s in the daf data: %s in the chain: %s",
                    length(entries), axis, dname,
                    length(prior$entries), axis, prior$name, chain_name
                ), call. = FALSE)
            }
            mismatch <- which(entries != prior$entries)
            if (length(mismatch)) {
                i <- mismatch[[1L]]
                stop(sprintf(
                    "different entry#%d: %s for the axis: %s in the daf data: %s from the entry#%d: %s for the axis: %s in the daf data: %s in the chain: %s",
                    i, entries[[i]], axis, dname,
                    i, prior$entries[[i]], axis, prior$name, chain_name
                ), call. = FALSE)
            }
        }
    }
    invisible()
}
```

Add `R/chain_daf.R` to DESCRIPTION `Collate:` via `devtools::document()` (Z-phase will regenerate).

- [ ] **Step 4: Rerun the smoke tests — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-chain-readers.R"); testthat::test_file("tests/testthat/test-chain-writers.R")'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add R/chain_daf.R tests/testthat/test-chain-readers.R tests/testthat/test-chain-writers.R
git commit -m "scaffold(chains): S7 classes + chain_reader/chain_writer constructors"
```

### Task C2: Chain reader — scalar dispatch (fall-through)

**Files:**
- Modify: `R/chain_daf.R` (add scalar methods)
- Modify: `tests/testthat/test-chain-readers.R`

- [ ] **Step 1: Extend `tests/testthat/test-chain-readers.R`**

```r
test_that("chain_reader: scalar falls through in reverse order (last wins)", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    set_scalar(d1, "version", 1L)
    set_scalar(d2, "version", 2L)
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_identical(get_scalar(ch, "version"), 2L)
})

test_that("chain_reader: scalar from only-first daf is visible", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    set_scalar(d1, "version", 1L)
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_identical(get_scalar(ch, "version"), 1L)
})

test_that("chain_reader: scalars_set is union of all dafs", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    set_scalar(d1, "a", 1L)
    set_scalar(d2, "b", 2L)
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_setequal(scalars_set(ch), c("a", "b"))
})
```

- [ ] **Step 2: Run — expect failure**

Expected: FAIL — `format_has_scalar` etc. not yet registered on `ReadOnlyChainDaf`.

- [ ] **Step 3: Add scalar methods to `R/chain_daf.R`**

```r
.chain_dafs <- function(daf) S7::prop(daf, "dafs")

S7::method(
    format_has_scalar,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_scalar(d, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_scalar,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_scalar(d, name)) return(format_get_scalar(d, name))
    }
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
}

S7::method(format_scalars_set, ReadOnlyChainDaf) <- function(daf) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        out <- c(out, format_scalars_set(d))
    }
    sort(unique(out), method = "radix")
}
```

And the mirror methods on `WriteChainDaf`:

```r
S7::method(
    format_has_scalar,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_scalar(d, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_scalar,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_scalar(d, name)) return(format_get_scalar(d, name))
    }
    stop(sprintf("scalar %s does not exist", sQuote(name)), call. = FALSE)
}

S7::method(format_scalars_set, WriteChainDaf) <- function(daf) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        out <- c(out, format_scalars_set(d))
    }
    sort(unique(out), method = "radix")
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/chain_daf.R tests/testthat/test-chain-readers.R
git commit -m "feat(chains): scalar read dispatch (reverse-order fall-through)"
```

### Task C3: Chain reader — axis dispatch

**Files:**
- Modify: `R/chain_daf.R`
- Modify: `tests/testthat/test-chain-readers.R`

- [ ] **Step 1: Append tests**

```r
test_that("chain_reader: axis union + entry agreement", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "B"))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_true(has_axis(ch, "cell"))
    expect_identical(axis_vector(ch, "cell"), c("A", "B"))
    expect_identical(axis_length(ch, "cell"), 2L)
})

test_that("chain_reader: axis mismatch raises at construction", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "C"))
    expect_error(chain_reader(list(d1, d2), name = "chain"),
        "different entry#2: C"
    )
})

test_that("chain_reader: axis length mismatch raises at construction", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A"))
    expect_error(chain_reader(list(d1, d2), name = "chain"),
        "different number of entries"
    )
})
```

- [ ] **Step 2: Run — expect failure** (axis methods not registered)

- [ ] **Step 3: Add axis methods to `R/chain_daf.R`**

```r
S7::method(
    format_has_axis,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(TRUE)
    }
    FALSE
}

S7::method(format_axes_set, ReadOnlyChainDaf) <- function(daf) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        out <- c(out, format_axes_set(d))
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_axis_array,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_array(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_axis_length,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_length(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_axis_dict,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_dict(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}
```

Mirror for `WriteChainDaf`: the same five generics, same bodies. Copy them verbatim underneath.

```r
S7::method(
    format_has_axis,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(TRUE)
    }
    FALSE
}

S7::method(format_axes_set, WriteChainDaf) <- function(daf) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        out <- c(out, format_axes_set(d))
    }
    sort(unique(out), method = "radix")
}

S7::method(
    format_axis_array,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_array(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_axis_length,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_length(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_axis_dict,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis)) return(format_axis_dict(d, axis))
    }
    stop(sprintf("axis %s does not exist", sQuote(axis)), call. = FALSE)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/chain_daf.R tests/testthat/test-chain-readers.R
git commit -m "feat(chains): axis read dispatch + construction-time consistency check"
```

### Task C4: Chain reader — vector dispatch

**Files:**
- Modify: `R/chain_daf.R`
- Modify: `tests/testthat/test-chain-readers.R`

- [ ] **Step 1: Append tests**

```r
test_that("chain_reader: vector falls through in reverse order (last wins)", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "B"))
    set_vector(d1, "cell", "age", c(1L, 2L))
    set_vector(d2, "cell", "age", c(3L, 4L))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_identical(unname(get_vector(ch, "cell", "age")), c(3L, 4L))
})

test_that("chain_reader: vector from only-first daf is visible", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "B"))
    set_vector(d1, "cell", "age", c(1L, 2L))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_true(has_vector(ch, "cell", "age"))
    expect_identical(unname(get_vector(ch, "cell", "age")), c(1L, 2L))
})

test_that("chain_reader: vectors_set is union across all dafs that have the axis", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d2, "cell", c("A", "B"))
    set_vector(d1, "cell", "age", c(1L, 2L))
    set_vector(d2, "cell", "donor", c("x", "y"))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_setequal(vectors_set(ch, "cell"), c("age", "donor"))
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Add vector read methods for both chain classes**

```r
S7::method(
    format_has_vector,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_vector,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) {
            return(format_get_vector(d, axis, name))
        }
    }
    stop(sprintf(
        "vector %s does not exist on axis %s",
        sQuote(name), sQuote(axis)
    ), call. = FALSE)
}

S7::method(
    format_vectors_set,
    list(ReadOnlyChainDaf, S7::class_character)
) <- function(daf, axis) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        if (format_has_axis(d, axis)) {
            out <- c(out, format_vectors_set(d, axis))
        }
    }
    sort(unique(out), method = "radix")
}
```

Mirror for `WriteChainDaf`:

```r
S7::method(
    format_has_vector,
    list(WriteChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_vector,
    list(WriteChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) {
            return(format_get_vector(d, axis, name))
        }
    }
    stop(sprintf(
        "vector %s does not exist on axis %s",
        sQuote(name), sQuote(axis)
    ), call. = FALSE)
}

S7::method(
    format_vectors_set,
    list(WriteChainDaf, S7::class_character)
) <- function(daf, axis) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        if (format_has_axis(d, axis)) {
            out <- c(out, format_vectors_set(d, axis))
        }
    }
    sort(unique(out), method = "radix")
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/chain_daf.R tests/testthat/test-chain-readers.R
git commit -m "feat(chains): vector read dispatch"
```

### Task C5: Chain reader — matrix dispatch

**Files:**
- Modify: `R/chain_daf.R`
- Modify: `tests/testthat/test-chain-readers.R`

- [ ] **Step 1: Append tests**

```r
test_that("chain_reader: matrix falls through last wins", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d1, "gene", c("X", "Y"))
    add_axis(d2, "cell", c("A", "B"))
    add_axis(d2, "gene", c("X", "Y"))
    m1 <- matrix(1:4, nrow = 2)
    m2 <- matrix(5:8, nrow = 2)
    set_matrix(d1, "cell", "gene", "UMIs", m1)
    set_matrix(d2, "cell", "gene", "UMIs", m2)
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_identical(unname(as.matrix(get_matrix(ch, "cell", "gene", "UMIs"))), m2)
})

test_that("chain_reader: matrices_set is union", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", "A")
    add_axis(d1, "gene", "X")
    add_axis(d2, "cell", "A")
    add_axis(d2, "gene", "X")
    set_matrix(d1, "cell", "gene", "A", matrix(1, 1, 1))
    set_matrix(d2, "cell", "gene", "B", matrix(2, 1, 1))
    ch <- chain_reader(list(d1, d2), name = "chain")
    expect_setequal(matrices_set(ch, "cell", "gene"), c("A", "B"))
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Add matrix read methods for both chain classes**

```r
S7::method(
    format_has_matrix,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_matrix,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) {
            return(format_get_matrix(d, rows_axis, columns_axis, name))
        }
    }
    stop(sprintf(
        "matrix %s does not exist on axes (%s, %s)",
        sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
    ), call. = FALSE)
}

S7::method(
    format_matrices_set,
    list(ReadOnlyChainDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        if (format_has_axis(d, rows_axis) && format_has_axis(d, columns_axis)) {
            out <- c(out, format_matrices_set(d, rows_axis, columns_axis))
        }
    }
    sort(unique(out), method = "radix")
}
```

Mirror for `WriteChainDaf`:

```r
S7::method(
    format_has_matrix,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) return(TRUE)
    }
    FALSE
}

S7::method(
    format_get_matrix,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    for (d in rev(.chain_dafs(daf))) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) {
            return(format_get_matrix(d, rows_axis, columns_axis, name))
        }
    }
    stop(sprintf(
        "matrix %s does not exist on axes (%s, %s)",
        sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
    ), call. = FALSE)
}

S7::method(
    format_matrices_set,
    list(WriteChainDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    out <- character(0)
    for (d in .chain_dafs(daf)) {
        if (format_has_axis(d, rows_axis) && format_has_axis(d, columns_axis)) {
            out <- c(out, format_matrices_set(d, rows_axis, columns_axis))
        }
    }
    sort(unique(out), method = "radix")
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/chain_daf.R tests/testthat/test-chain-readers.R
git commit -m "feat(chains): matrix read dispatch"
```

### Task C6: Write chain — scalar set / delete

**Files:**
- Modify: `R/chain_daf.R`
- Modify: `tests/testthat/test-chain-writers.R`

- [ ] **Step 1: Append tests**

```r
test_that("chain_writer: set_scalar writes to top writer", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_writer(list(d1, d2), name = "chain")
    set_scalar(ch, "version", 7L)
    expect_false(has_scalar(d1, "version"))
    expect_true(has_scalar(d2, "version"))
    expect_identical(get_scalar(ch, "version"), 7L)
})

test_that("chain_writer: delete_scalar errors when scalar exists in earlier daf", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    set_scalar(d1, "version", 1L)
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_error(delete_scalar(ch, "version"),
        "because it exists in the earlier: one"
    )
})

test_that("chain_writer: delete_scalar removes from top writer only", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_writer(list(d1, d2), name = "chain")
    set_scalar(ch, "version", 1L)
    expect_true(has_scalar(d2, "version"))
    delete_scalar(ch, "version")
    expect_false(has_scalar(d2, "version"))
    expect_false(has_scalar(ch, "version"))
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Add `format_set_scalar` / `format_delete_scalar` for `WriteChainDaf`**

```r
.chain_writer <- function(daf) S7::prop(daf, "writer")

S7::method(
    format_set_scalar,
    list(WriteChainDaf, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    format_set_scalar(.chain_writer(daf), name, value, overwrite)
}

S7::method(
    format_delete_scalar,
    list(WriteChainDaf, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    earlier <- .chain_dafs(daf)
    earlier <- earlier[-length(earlier)]  # all except writer
    for (d in rev(earlier)) {
        if (format_has_scalar(d, name)) {
            stop(sprintf(
                "failed to delete the scalar: %s from the daf data: %s of the chain: %s because it exists in the earlier: %s",
                name, S7::prop(.chain_writer(daf), "name"),
                S7::prop(daf, "name"), S7::prop(d, "name")
            ), call. = FALSE)
        }
    }
    format_delete_scalar(.chain_writer(daf), name, must_exist)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/chain_daf.R tests/testthat/test-chain-writers.R
git commit -m "feat(chains): write-chain scalar set/delete (delete-earlier-error)"
```

### Task C7: Write chain — axis add / delete + vector set / delete

**Files:**
- Modify: `R/chain_daf.R`
- Modify: `tests/testthat/test-chain-writers.R`

Context: writing a vector to an axis that exists *only in an earlier daf* requires the writer to auto-add the axis. Mirrors Julia's `format_set_vector!` logic.

- [ ] **Step 1: Append tests**

```r
test_that("chain_writer: add_axis adds on top writer only", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    ch <- chain_writer(list(d1, d2), name = "chain")
    add_axis(ch, "cell", c("A", "B"))
    expect_false(has_axis(d1, "cell"))
    expect_true(has_axis(d2, "cell"))
    expect_identical(axis_vector(ch, "cell"), c("A", "B"))
})

test_that("chain_writer: delete_axis rejects deletion of earlier-only axis", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_error(delete_axis(ch, "cell"),
        "because it exists in the earlier: one"
    )
})

test_that("chain_writer: set_vector auto-adds missing axis on writer", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    ch <- chain_writer(list(d1, d2), name = "chain")
    set_vector(ch, "cell", "age", c(1L, 2L))
    expect_true(has_axis(d2, "cell"))
    expect_identical(axis_vector(d2, "cell"), c("A", "B"))
    expect_identical(unname(get_vector(ch, "cell", "age")), c(1L, 2L))
})

test_that("chain_writer: delete_vector error when vector exists in earlier", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", "A")
    set_vector(d1, "cell", "age", 1L)
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_error(delete_vector(ch, "cell", "age"),
        "because it exists in the earlier: one"
    )
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Add axis + vector mutation methods to `WriteChainDaf`**

```r
S7::method(
    format_add_axis,
    list(WriteChainDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, entries) {
    format_add_axis(.chain_writer(daf), axis, entries)
}

S7::method(
    format_delete_axis,
    list(WriteChainDaf, S7::class_character, S7::class_logical)
) <- function(daf, axis, must_exist) {
    earlier <- .chain_dafs(daf)
    earlier <- earlier[-length(earlier)]
    for (d in rev(earlier)) {
        if (format_has_axis(d, axis)) {
            stop(sprintf(
                "failed to delete the axis: %s from the daf data: %s of the chain: %s because it exists in the earlier: %s",
                axis, S7::prop(.chain_writer(daf), "name"),
                S7::prop(daf, "name"), S7::prop(d, "name")
            ), call. = FALSE)
        }
    }
    format_delete_axis(.chain_writer(daf), axis, must_exist)
}

.chain_ensure_axis_on_writer <- function(daf, axis) {
    writer <- .chain_writer(daf)
    if (format_has_axis(writer, axis)) return(invisible())
    # Pull entries from the first earlier daf that has this axis.
    earlier <- .chain_dafs(daf)
    earlier <- earlier[-length(earlier)]
    for (d in rev(earlier)) {
        if (format_has_axis(d, axis)) {
            entries <- format_axis_array(d, axis)
            format_add_axis(writer, axis, entries)
            return(invisible())
        }
    }
    stop(sprintf("axis %s does not exist in chain", sQuote(axis)), call. = FALSE)
}

S7::method(
    format_set_vector,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, axis, name, vec, overwrite) {
    .chain_ensure_axis_on_writer(daf, axis)
    format_set_vector(.chain_writer(daf), axis, name, vec, overwrite)
}

S7::method(
    format_delete_vector,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, axis, name, must_exist) {
    earlier <- .chain_dafs(daf)
    earlier <- earlier[-length(earlier)]
    for (d in rev(earlier)) {
        if (format_has_axis(d, axis) && format_has_vector(d, axis, name)) {
            stop(sprintf(
                "failed to delete the vector: %s of the axis: %s from the daf data: %s of the chain: %s because it exists in the earlier: %s",
                name, axis,
                S7::prop(.chain_writer(daf), "name"),
                S7::prop(daf, "name"), S7::prop(d, "name")
            ), call. = FALSE)
        }
    }
    writer <- .chain_writer(daf)
    if (format_has_axis(writer, axis) && format_has_vector(writer, axis, name)) {
        format_delete_vector(writer, axis, name, must_exist)
    } else if (must_exist) {
        stop(sprintf(
            "vector %s does not exist on axis %s",
            sQuote(name), sQuote(axis)
        ), call. = FALSE)
    }
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/chain_daf.R tests/testthat/test-chain-writers.R
git commit -m "feat(chains): write-chain axis/vector set/delete (auto-add-axis + delete-earlier-error)"
```

### Task C8: Write chain — matrix set / delete / relayout

**Files:**
- Modify: `R/chain_daf.R`
- Modify: `tests/testthat/test-chain-writers.R`

- [ ] **Step 1: Append tests**

```r
test_that("chain_writer: set_matrix auto-adds axes on writer", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", c("A", "B"))
    add_axis(d1, "gene", c("X", "Y"))
    ch <- chain_writer(list(d1, d2), name = "chain")
    m <- matrix(1:4, nrow = 2)
    set_matrix(ch, "cell", "gene", "UMIs", m)
    expect_true(has_axis(d2, "cell"))
    expect_true(has_axis(d2, "gene"))
    expect_identical(unname(as.matrix(get_matrix(ch, "cell", "gene", "UMIs"))), m)
})

test_that("chain_writer: delete_matrix errors when it exists in earlier", {
    d1 <- memory_daf(name = "one")
    d2 <- memory_daf(name = "two")
    add_axis(d1, "cell", "A"); add_axis(d1, "gene", "X")
    set_matrix(d1, "cell", "gene", "M", matrix(1, 1, 1))
    ch <- chain_writer(list(d1, d2), name = "chain")
    expect_error(delete_matrix(ch, "cell", "gene", "M"),
        "because it exists in the earlier: one"
    )
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Add matrix mutation methods**

```r
S7::method(
    format_set_matrix,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
    .chain_ensure_axis_on_writer(daf, rows_axis)
    .chain_ensure_axis_on_writer(daf, columns_axis)
    format_set_matrix(.chain_writer(daf), rows_axis, columns_axis, name, mat, overwrite)
}

S7::method(
    format_delete_matrix,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, must_exist) {
    earlier <- .chain_dafs(daf)
    earlier <- earlier[-length(earlier)]
    for (d in rev(earlier)) {
        if (format_has_axis(d, rows_axis) &&
            format_has_axis(d, columns_axis) &&
            format_has_matrix(d, rows_axis, columns_axis, name)) {
            stop(sprintf(
                "failed to delete the matrix: %s for the rows axis: %s and the columns axis: %s from the daf data: %s of the chain: %s because it exists in the earlier: %s",
                name, rows_axis, columns_axis,
                S7::prop(.chain_writer(daf), "name"),
                S7::prop(daf, "name"), S7::prop(d, "name")
            ), call. = FALSE)
        }
    }
    writer <- .chain_writer(daf)
    if (format_has_axis(writer, rows_axis) &&
        format_has_axis(writer, columns_axis) &&
        format_has_matrix(writer, rows_axis, columns_axis, name)) {
        format_delete_matrix(writer, rows_axis, columns_axis, name, must_exist)
    } else if (must_exist) {
        stop(sprintf(
            "matrix %s does not exist on axes (%s, %s)",
            sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
        ), call. = FALSE)
    }
}

S7::method(
    format_relayout_matrix,
    list(WriteChainDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    # Relayout must write back to the top writer. If the matrix lives in an
    # earlier daf, pull it into the writer first.
    writer <- .chain_writer(daf)
    if (!(format_has_axis(writer, rows_axis) &&
          format_has_axis(writer, columns_axis) &&
          format_has_matrix(writer, rows_axis, columns_axis, name))) {
        earlier <- .chain_dafs(daf)
        earlier <- earlier[-length(earlier)]
        found <- NULL
        for (d in rev(earlier)) {
            if (format_has_axis(d, rows_axis) &&
                format_has_axis(d, columns_axis) &&
                format_has_matrix(d, rows_axis, columns_axis, name)) {
                found <- d; break
            }
        }
        if (is.null(found)) {
            stop(sprintf(
                "matrix %s does not exist on axes (%s, %s)",
                sQuote(name), sQuote(rows_axis), sQuote(columns_axis)
            ), call. = FALSE)
        }
        .chain_ensure_axis_on_writer(daf, rows_axis)
        .chain_ensure_axis_on_writer(daf, columns_axis)
        m <- format_get_matrix(found, rows_axis, columns_axis, name)
        format_set_matrix(writer, rows_axis, columns_axis, name, m, TRUE)
    }
    format_relayout_matrix(writer, rows_axis, columns_axis, name)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/chain_daf.R tests/testthat/test-chain-writers.R
git commit -m "feat(chains): write-chain matrix set/delete/relayout"
```

### Task C9: Chain Julia compat fixture

**Files:**
- Create: `dev/scripts/regen-julia-chains-fixture.jl`
- Create: `tests/testthat/fixtures/julia-chains/fixture.json` (bytes committed)
- Create: `tests/testthat/test-chain-julia-compat.R`

- [ ] **Step 1: Write the Julia script**

```julia
#!/usr/bin/env julia
# dev/scripts/regen-julia-chains-fixture.jl
#
# Generate chain fixture: two MemoryDaf instances with overlapping data
# + chain_reader + chain_writer expectations.
#
# Usage: conda run -n dafr-mcview julia --project=$HOME/src/DataAxesFormats.jl \
#   dev/scripts/regen-julia-chains-fixture.jl

using DataAxesFormats
using DataAxesFormats.Formats
using JSON

function make_daf(name, scalars, axes, vectors, matrices)
    d = MemoryDaf(; name = name)
    for (n, v) in scalars
        set_scalar!(d, n, v)
    end
    for (a, e) in axes
        add_axis!(d, a, e)
    end
    for (axis, name, value) in vectors
        set_vector!(d, axis, name, value)
    end
    for (rows, cols, name, value) in matrices
        set_matrix!(d, rows, cols, name, value)
    end
    return d
end

function emit_scalar(v)
    v isa Int   ? "integer" :
    v isa Float64 ? "double" :
    v isa String  ? "character" :
    v isa Bool    ? "logical" : error("unknown scalar type")
end

function main()
    d1 = make_daf("first",
        Dict("version" => 1),
        Dict("cell" => ["A", "B", "C"]),
        [("cell", "age", [10, 20, 30])],
        []
    )
    d2 = make_daf("second",
        Dict("version" => 2, "owner" => "me"),
        Dict("cell" => ["A", "B", "C"]),
        [("cell", "age", [100, 200, 300]),
         ("cell", "donor", ["d1", "d2", "d1"])],
        []
    )
    read_chain = chain_reader([d1, d2]; name = "chain")

    fixture = Dict(
        "chain_name"  => "chain",
        "daf_names"   => ["first", "second"],
        "scalars"     => Dict(
            "version" => Dict("value" => get_scalar(read_chain, "version"),
                              "type"  => emit_scalar(get_scalar(read_chain, "version"))),
            "owner"   => Dict("value" => get_scalar(read_chain, "owner"),
                              "type"  => "character")
        ),
        "axes"        => Dict("cell" => axis_vector(read_chain, "cell")),
        "vectors"     => Dict(
            "cell" => Dict(
                "age"   => get_vector(read_chain, "cell", "age").array,
                "donor" => get_vector(read_chain, "cell", "donor").array
            )
        )
    )

    out_path = joinpath(@__DIR__, "..", "..",
        "tests", "testthat", "fixtures", "julia-chains", "fixture.json")
    mkpath(dirname(out_path))
    open(out_path, "w") do io
        JSON.print(io, fixture, 2)
    end
    println("Wrote $(out_path)")
end

main()
```

- [ ] **Step 2: Run the script under the conda env**

```
conda run -n dafr-mcview julia --project=$HOME/src/DataAxesFormats.jl dev/scripts/regen-julia-chains-fixture.jl
```

Expected: writes `tests/testthat/fixtures/julia-chains/fixture.json`.

- [ ] **Step 3: Write the R compat test `tests/testthat/test-chain-julia-compat.R`**

```r
test_that("R chain_reader matches Julia fixture", {
    skip_if_not(file.exists("fixtures/julia-chains/fixture.json"),
        "Julia chain fixture absent")
    fx <- jsonlite::fromJSON("fixtures/julia-chains/fixture.json",
        simplifyVector = TRUE
    )
    d1 <- memory_daf(name = "first")
    set_scalar(d1, "version", 1L)
    add_axis(d1, "cell", c("A", "B", "C"))
    set_vector(d1, "cell", "age", c(10L, 20L, 30L))

    d2 <- memory_daf(name = "second")
    set_scalar(d2, "version", 2L)
    set_scalar(d2, "owner", "me")
    add_axis(d2, "cell", c("A", "B", "C"))
    set_vector(d2, "cell", "age", c(100L, 200L, 300L))
    set_vector(d2, "cell", "donor", c("d1", "d2", "d1"))

    ch <- chain_reader(list(d1, d2), name = fx$chain_name)
    expect_identical(get_scalar(ch, "version"), as.integer(fx$scalars$version$value))
    expect_identical(get_scalar(ch, "owner"), as.character(fx$scalars$owner$value))
    expect_identical(axis_vector(ch, "cell"), fx$axes$cell)
    expect_identical(unname(get_vector(ch, "cell", "age")),
        as.integer(fx$vectors$cell$age)
    )
    expect_identical(unname(get_vector(ch, "cell", "donor")),
        as.character(fx$vectors$cell$donor)
    )
})
```

- [ ] **Step 4: Run the test — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-chain-julia-compat.R")'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add dev/scripts/regen-julia-chains-fixture.jl tests/testthat/fixtures/julia-chains/fixture.json tests/testthat/test-chain-julia-compat.R
git commit -m "test(chains): Julia fixture round-trip"
```

Note: `dev/scripts/*.jl` commits to the dev repo; the fixture JSON + test file commit to the package repo. Stage them separately if conventions require it (check git root with `git rev-parse --show-toplevel` before staging).

---

## Phase T — Contracts

### Task T1: Scaffold `R/contracts.R` + expectation constants + Contract class

**Files:**
- Create: `R/contracts.R`
- Create: `tests/testthat/test-contracts-class.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("expectation constants have string values", {
    expect_identical(RequiredInput, "RequiredInput")
    expect_identical(OptionalInput, "OptionalInput")
    expect_identical(CreatedOutput, "CreatedOutput")
    expect_identical(GuaranteedOutput, "GuaranteedOutput")
    expect_identical(OptionalOutput, "OptionalOutput")
})

test_that("Contract() builds an object with axes + data slots", {
    c1 <- Contract(
        axes = list(cell = list(RequiredInput, "cell axis")),
        data = list(
            contract_scalar("version", RequiredInput, "integer", "dataset version"),
            contract_vector("cell", "age", RequiredInput, "integer", "cell age")
        )
    )
    expect_s3_class(c1, "dafr::Contract")
    expect_named(S7::prop(c1, "axes"), "cell")
    expect_length(S7::prop(c1, "data"), 2L)
})

test_that("Contract() rejects unknown expectation", {
    expect_error(
        contract_scalar("v", "NotAnExpectation", "integer", "d"),
        "unknown expectation"
    )
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Implement `R/contracts.R` scaffold + constants + Contract class**

```r
#' @include classes.R format_api.R
NULL

#' @name expectation-constants
#' @title Contract expectation constants
#' @description String literals used in `contract_scalar()` /
#'   `contract_vector()` / `contract_matrix()` + axis specs.
#' @return Character scalar.
NULL

#' @rdname expectation-constants
#' @export
RequiredInput <- "RequiredInput"
#' @rdname expectation-constants
#' @export
OptionalInput <- "OptionalInput"
#' @rdname expectation-constants
#' @export
CreatedOutput <- "CreatedOutput"
#' @rdname expectation-constants
#' @export
GuaranteedOutput <- "GuaranteedOutput"
#' @rdname expectation-constants
#' @export
OptionalOutput <- "OptionalOutput"

.VALID_EXPECTATIONS <- c(
    RequiredInput, OptionalInput, CreatedOutput, GuaranteedOutput, OptionalOutput
)

.assert_expectation <- function(x, arg) {
    if (!is.character(x) || length(x) != 1L || is.na(x) ||
        !(x %in% .VALID_EXPECTATIONS)) {
        stop(sprintf(
            "unknown expectation for `%s`: %s",
            arg, if (is.character(x)) sQuote(x) else sQuote(toString(x))
        ), call. = FALSE)
    }
    invisible()
}

.assert_type <- function(type, arg) {
    if (!is.character(type) || length(type) != 1L || is.na(type) || !nzchar(type)) {
        stop(sprintf("`%s` must be a non-empty character scalar (R class name)", arg),
            call. = FALSE
        )
    }
    invisible()
}

#' A contract describing a computation's inputs and outputs.
#' @param name Optional name.
#' @param is_relaxed If TRUE, unknown properties don't error.
#' @param axes Named list: axis -> list(expectation, description).
#' @param data List of contract_scalar()/contract_vector()/contract_matrix() records.
#' @export
Contract <- S7::new_class(
    name = "Contract",
    package = "dafr",
    properties = list(
        name       = S7::new_property(S7::class_character, default = ""),
        is_relaxed = S7::new_property(S7::class_logical, default = FALSE),
        axes       = S7::new_property(S7::class_list, default = list()),
        data       = S7::new_property(S7::class_list, default = list())
    ),
    validator = function(self) {
        for (a in names(self@axes)) {
            spec <- self@axes[[a]]
            if (!is.list(spec) || length(spec) != 2L) {
                return(sprintf("axis %s spec must be list(expectation, description)", a))
            }
            .assert_expectation(spec[[1L]], sprintf("axis %s", a))
            if (!is.character(spec[[2L]]) || length(spec[[2L]]) != 1L) {
                return(sprintf("axis %s description must be character scalar", a))
            }
        }
        for (i in seq_along(self@data)) {
            rec <- self@data[[i]]
            if (!is.list(rec) || !("kind" %in% names(rec))) {
                return(sprintf("data[[%d]] must be a record with $kind", i))
            }
            .assert_expectation(rec$expectation, sprintf("data[[%d]] expectation", i))
            .assert_type(rec$type, sprintf("data[[%d]] type", i))
        }
        NULL
    }
)

#' @export
contract_scalar <- function(name, expectation, type, description) {
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    .assert_type(type, "type")
    list(
        kind        = "scalar", name = name, expectation = expectation,
        type        = type, description = description
    )
}

#' @export
contract_vector <- function(axis, name, expectation, type, description) {
    .assert_name(axis, "axis")
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    .assert_type(type, "type")
    list(
        kind = "vector", axis = axis, name = name,
        expectation = expectation, type = type, description = description
    )
}

#' @export
contract_matrix <- function(rows_axis, columns_axis, name, expectation, type, description) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    .assert_type(type, "type")
    list(
        kind = "matrix", rows_axis = rows_axis, columns_axis = columns_axis,
        name = name, expectation = expectation, type = type,
        description = description
    )
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/contracts.R tests/testthat/test-contracts-class.R
git commit -m "scaffold(contracts): expectation constants + Contract class + spec helpers"
```

### Task T2: `ContractDaf` class + `contractor()` + enforcement gate

**Files:**
- Modify: `R/contracts.R`
- Modify: `tests/testthat/test-contracts-class.R`

- [ ] **Step 1: Append tests**

```r
test_that("contractor() returns daf unchanged when enforcement is off", {
    withr::local_envvar(DAF_ENFORCE_CONTRACTS = "0")
    withr::local_options(dafr.enforce_contracts = FALSE)
    d <- memory_daf(name = "plain")
    c1 <- Contract()
    result <- contractor("comp", c1, d)
    expect_identical(result, d)
})

test_that("contractor() wraps daf when enforcement is on", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "plain")
    c1 <- Contract()
    result <- contractor("comp", c1, d)
    expect_s3_class(result, "dafr::ContractDaf")
    expect_s3_class(result, "dafr::DafWriter")
    expect_identical(S7::prop(result, "computation"), "comp")
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Extend `R/contracts.R`**

```r
#' @export
ContractDaf <- S7::new_class(
    name = "ContractDaf",
    package = "dafr",
    parent = DafWriter,
    properties = list(
        computation = S7::class_character,
        is_relaxed  = S7::class_logical,
        overwrite   = S7::class_logical,
        base        = DafReader,
        axes        = S7::class_environment,   # env(axis -> tracker env)
        data        = S7::class_environment    # env(key -> tracker env)
    )
)

.enforcement_enabled <- function() {
    opt <- getOption("dafr.enforce_contracts", NULL)
    if (!is.null(opt)) return(isTRUE(opt))
    env <- Sys.getenv("DAF_ENFORCE_CONTRACTS", unset = NA_character_)
    if (is.na(env)) return(FALSE)
    tolower(env) %in% c("1", "true", "t", "yes", "y")
}

.new_tracker <- function(expectation, type = NA_character_) {
    t <- new.env(parent = emptyenv())
    t$expectation <- expectation
    t$type <- type
    t$accessed <- FALSE
    t
}

.data_key <- function(rec) {
    switch(rec$kind,
        scalar = sprintf("scalar:%s", rec$name),
        vector = sprintf("vector:%s:%s", rec$axis, rec$name),
        matrix = sprintf("matrix:%s:%s:%s", rec$rows_axis, rec$columns_axis, rec$name),
        stop("unknown data kind")
    )
}

#' @export
contractor <- function(computation, contract, daf,
                       name = NULL, overwrite = FALSE) {
    if (!.enforcement_enabled()) {
        return(daf)
    }
    if (!S7::S7_inherits(daf, DafReader)) {
        stop("`daf` must be a DafReader", call. = FALSE)
    }
    if (is.null(name)) {
        name <- paste0(S7::prop(daf, "name"), ".", computation)
    }
    axes_env <- new.env(parent = emptyenv())
    for (ax in names(S7::prop(contract, "axes"))) {
        spec <- S7::prop(contract, "axes")[[ax]]
        axes_env[[ax]] <- .new_tracker(spec[[1L]])
    }
    data_env <- new.env(parent = emptyenv())
    for (rec in S7::prop(contract, "data")) {
        data_env[[.data_key(rec)]] <- .new_tracker(rec$expectation, rec$type)
    }
    ContractDaf(
        name = name,
        internal = new_internal_env(),
        cache = S7::prop(daf, "cache"),
        axis_version_counter = S7::prop(daf, "axis_version_counter"),
        vector_version_counter = S7::prop(daf, "vector_version_counter"),
        matrix_version_counter = S7::prop(daf, "matrix_version_counter"),
        computation = computation,
        is_relaxed = isTRUE(S7::prop(contract, "is_relaxed")),
        overwrite = overwrite,
        base = daf,
        axes = axes_env,
        data = data_env
    )
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/contracts.R tests/testthat/test-contracts-class.R
git commit -m "feat(contracts): ContractDaf class + contractor() + enforcement gate"
```

### Task T3: `ContractDaf` format_* dispatch + access tracking

**Files:**
- Modify: `R/contracts.R`
- Create: `tests/testthat/test-contracts-access.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("get_scalar on ContractDaf marks scalar as accessed", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    expect_identical(get_scalar(cd, "version"), 1L)
    expect_true(S7::prop(cd, "data")[["scalar:version"]]$accessed)
})

test_that("get_scalar on out-of-contract scalar errors (non-relaxed)", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract()
    cd <- contractor("comp", cn, d)
    expect_error(get_scalar(cd, "version"), "non-contract scalar")
})

test_that("relaxed contract allows out-of-contract access", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract(is_relaxed = TRUE)
    cd <- contractor("comp", cn, d)
    expect_identical(get_scalar(cd, "version"), 1L)
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Add format_* dispatch for ContractDaf + access_* helpers**

```r
.access_key_scalar <- function(name) sprintf("scalar:%s", name)
.access_key_vector <- function(axis, name) sprintf("vector:%s:%s", axis, name)
.access_key_matrix <- function(ra, ca, name) sprintf("matrix:%s:%s:%s", ra, ca, name)

.IMMUTABLE_EXPECTATIONS <- c(RequiredInput, OptionalInput)

.is_immutable <- function(expectation, is_for_modify) {
    is_for_modify && expectation %in% .IMMUTABLE_EXPECTATIONS
}

.access_scalar <- function(cd, name, is_for_modify) {
    key <- .access_key_scalar(name)
    tracker <- S7::prop(cd, "data")[[key]]
    if (is.null(tracker)) {
        if (isTRUE(S7::prop(cd, "is_relaxed"))) return(invisible())
        stop(sprintf(
            "accessing non-contract scalar: %s for the computation: %s on the daf data: %s",
            name, S7::prop(cd, "computation"), S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    if (.is_immutable(tracker$expectation, is_for_modify)) {
        stop(sprintf(
            "modifying %s scalar: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    tracker$accessed <- TRUE
    invisible()
}

.access_axis <- function(cd, axis, is_for_modify) {
    tracker <- S7::prop(cd, "axes")[[axis]]
    if (is.null(tracker)) {
        if (isTRUE(S7::prop(cd, "is_relaxed"))) return(invisible())
        stop(sprintf(
            "accessing non-contract axis: %s for the computation: %s on the daf data: %s",
            axis, S7::prop(cd, "computation"), S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    if (.is_immutable(tracker$expectation, is_for_modify)) {
        stop(sprintf(
            "modifying %s axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, axis, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    tracker$accessed <- TRUE
    invisible()
}

.access_vector <- function(cd, axis, name, is_for_modify) {
    # Axis access is non-modifying even on a vector write (Julia semantics).
    .access_axis(cd, axis, FALSE)
    key <- .access_key_vector(axis, name)
    tracker <- S7::prop(cd, "data")[[key]]
    if (is.null(tracker)) {
        if (isTRUE(S7::prop(cd, "is_relaxed")) || name %in% c("name", "index")) {
            return(invisible())
        }
        stop(sprintf(
            "accessing non-contract vector: %s of the axis: %s for the computation: %s on the daf data: %s",
            name, axis, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    if (.is_immutable(tracker$expectation, is_for_modify)) {
        stop(sprintf(
            "modifying %s vector: %s of the axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, axis, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    tracker$accessed <- TRUE
    invisible()
}

.access_matrix <- function(cd, ra, ca, name, is_for_modify) {
    .access_axis(cd, ra, FALSE)
    .access_axis(cd, ca, FALSE)
    key <- .access_key_matrix(ra, ca, name)
    tracker <- S7::prop(cd, "data")[[key]]
    if (is.null(tracker)) {
        # Try flipped
        tracker <- S7::prop(cd, "data")[[.access_key_matrix(ca, ra, name)]]
    }
    if (is.null(tracker)) {
        if (isTRUE(S7::prop(cd, "is_relaxed"))) return(invisible())
        stop(sprintf(
            "accessing non-contract matrix: %s of the rows axis: %s and the columns axis: %s for the computation: %s on the daf data: %s",
            name, ra, ca, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    if (.is_immutable(tracker$expectation, is_for_modify)) {
        stop(sprintf(
            "modifying %s matrix: %s of the rows_axis: %s and the columns_axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, ra, ca, S7::prop(cd, "computation"),
            S7::prop(S7::prop(cd, "base"), "name")
        ), call. = FALSE)
    }
    tracker$accessed <- TRUE
    invisible()
}

# -- format_* dispatch --------------------------------------------------

S7::method(
    format_has_scalar,
    list(ContractDaf, S7::class_character)
) <- function(daf, name) format_has_scalar(S7::prop(daf, "base"), name)

S7::method(
    format_get_scalar,
    list(ContractDaf, S7::class_character)
) <- function(daf, name) {
    .access_scalar(daf, name, is_for_modify = FALSE)
    format_get_scalar(S7::prop(daf, "base"), name)
}

S7::method(format_scalars_set, ContractDaf) <- function(daf) {
    format_scalars_set(S7::prop(daf, "base"))
}

S7::method(
    format_set_scalar,
    list(ContractDaf, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, name, value, overwrite) {
    .access_scalar(daf, name, is_for_modify = TRUE)
    format_set_scalar(S7::prop(daf, "base"), name, value, overwrite)
}

S7::method(
    format_delete_scalar,
    list(ContractDaf, S7::class_character, S7::class_logical)
) <- function(daf, name, must_exist) {
    .access_scalar(daf, name, is_for_modify = TRUE)
    format_delete_scalar(S7::prop(daf, "base"), name, must_exist)
}

S7::method(
    format_has_axis,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) format_has_axis(S7::prop(daf, "base"), axis)

S7::method(format_axes_set, ContractDaf) <- function(daf) {
    format_axes_set(S7::prop(daf, "base"))
}

S7::method(
    format_axis_array,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) {
    .access_axis(daf, axis, is_for_modify = FALSE)
    format_axis_array(S7::prop(daf, "base"), axis)
}

S7::method(
    format_axis_length,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) format_axis_length(S7::prop(daf, "base"), axis)

S7::method(
    format_axis_dict,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) format_axis_dict(S7::prop(daf, "base"), axis)

S7::method(
    format_add_axis,
    list(ContractDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, entries) {
    .access_axis(daf, axis, is_for_modify = TRUE)
    format_add_axis(S7::prop(daf, "base"), axis, entries)
}

S7::method(
    format_delete_axis,
    list(ContractDaf, S7::class_character, S7::class_logical)
) <- function(daf, axis, must_exist) {
    .access_axis(daf, axis, is_for_modify = TRUE)
    format_delete_axis(S7::prop(daf, "base"), axis, must_exist)
}

S7::method(
    format_has_vector,
    list(ContractDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) format_has_vector(S7::prop(daf, "base"), axis, name)

S7::method(
    format_get_vector,
    list(ContractDaf, S7::class_character, S7::class_character)
) <- function(daf, axis, name) {
    .access_vector(daf, axis, name, is_for_modify = FALSE)
    format_get_vector(S7::prop(daf, "base"), axis, name)
}

S7::method(
    format_vectors_set,
    list(ContractDaf, S7::class_character)
) <- function(daf, axis) format_vectors_set(S7::prop(daf, "base"), axis)

S7::method(
    format_set_vector,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, axis, name, vec, overwrite) {
    .access_vector(daf, axis, name, is_for_modify = TRUE)
    format_set_vector(S7::prop(daf, "base"), axis, name, vec, overwrite)
}

S7::method(
    format_delete_vector,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, axis, name, must_exist) {
    .access_vector(daf, axis, name, is_for_modify = TRUE)
    format_delete_vector(S7::prop(daf, "base"), axis, name, must_exist)
}

S7::method(
    format_has_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    format_has_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name)
}

S7::method(
    format_get_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    .access_matrix(daf, rows_axis, columns_axis, name, is_for_modify = FALSE)
    format_get_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name)
}

S7::method(
    format_matrices_set,
    list(ContractDaf, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis) {
    format_matrices_set(S7::prop(daf, "base"), rows_axis, columns_axis)
}

S7::method(
    format_set_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_any, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, mat, overwrite) {
    .access_matrix(daf, rows_axis, columns_axis, name, is_for_modify = TRUE)
    format_set_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name, mat, overwrite)
}

S7::method(
    format_delete_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character, S7::class_logical)
) <- function(daf, rows_axis, columns_axis, name, must_exist) {
    .access_matrix(daf, rows_axis, columns_axis, name, is_for_modify = TRUE)
    format_delete_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name, must_exist)
}

S7::method(
    format_relayout_matrix,
    list(ContractDaf, S7::class_character, S7::class_character, S7::class_character)
) <- function(daf, rows_axis, columns_axis, name) {
    .access_matrix(daf, rows_axis, columns_axis, name, is_for_modify = FALSE)
    format_relayout_matrix(S7::prop(daf, "base"), rows_axis, columns_axis, name)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/contracts.R tests/testthat/test-contracts-access.R
git commit -m "feat(contracts): format_* dispatch + access tracking + relaxed mode"
```

### Task T4: `verify_input` + `verify_output`

**Files:**
- Modify: `R/contracts.R`
- Create: `tests/testthat/test-contracts-verify.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("verify_input on plain DafReader is a no-op", {
    d <- memory_daf(name = "d")
    expect_null(verify_input(d))
    expect_null(verify_output(d))
})

test_that("verify_input on RequiredInput scalar fails when missing", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    expect_error(verify_input(cd), "missing input scalar: version")
})

test_that("verify_input on RequiredInput scalar of wrong type fails", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", "oops")
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    expect_error(verify_input(cd), "unexpected type: character")
})

test_that("verify_output on CreatedOutput fails when pre-existing + !overwrite", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "result", 1L)
    cn <- Contract(
        data = list(contract_scalar("result", CreatedOutput, "integer", "r"))
    )
    cd <- contractor("comp", cn, d)
    expect_error(verify_input(cd), "pre-existing CreatedOutput scalar: result")
})

test_that("verify_output on unused RequiredInput fails after computation", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    # Never access version.
    expect_error(verify_output(cd), "unused RequiredInput scalar: version")
})

test_that("verify_output pass after accessing RequiredInput", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    set_scalar(d, "version", 1L)
    cn <- Contract(
        data = list(contract_scalar("version", RequiredInput, "integer", "v"))
    )
    cd <- contractor("comp", cn, d)
    invisible(get_scalar(cd, "version"))
    expect_null(verify_output(cd))
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Implement verify_input / verify_output in `R/contracts.R`**

```r
.is_mandatory <- function(expectation, is_for_output) {
    (is_for_output && expectation == CreatedOutput) ||
        (!is_for_output && expectation == RequiredInput)
}

.is_forbidden <- function(expectation, is_for_output, overwrite) {
    !is_for_output && expectation == CreatedOutput && !overwrite
}

.direction_name <- function(is_for_output) if (is_for_output) "output" else "input"

.type_ok <- function(value, type_name) {
    switch(type_name,
        integer   = is.integer(value),
        numeric   = is.numeric(value),
        double    = is.double(value),
        character = is.character(value),
        logical   = is.logical(value),
        # fall back to class check for user-defined types
        inherits(value, type_name)
    )
}

.vector_type_ok <- function(v, type_name) {
    switch(type_name,
        integer   = is.integer(v),
        numeric   = is.numeric(v),
        double    = is.double(v),
        character = is.character(v),
        logical   = is.logical(v),
        inherits(v, type_name)
    )
}

.matrix_type_ok <- function(m, type_name) {
    eltype_ok <- switch(type_name,
        integer   = is.integer(m[1L]),
        numeric   = is.numeric(m[1L]),
        double    = is.double(m[1L]),
        logical   = is.logical(m[1L]),
        inherits(m, type_name)
    )
    eltype_ok
}

.verify_scalar_data <- function(cd, rec, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    name <- rec$name
    exists_ <- format_has_scalar(base, name)
    tracker <- S7::prop(cd, "data")[[.data_key(rec)]]
    if (!exists_) {
        if (.is_mandatory(tracker$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s scalar: %s with type: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), name, tracker$type, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    if (.is_forbidden(tracker$expectation, is_for_output, S7::prop(cd, "overwrite"))) {
        stop(sprintf(
            "pre-existing %s scalar: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, comp, dname
        ), call. = FALSE)
    }
    value <- format_get_scalar(base, name)
    if (!.type_ok(value, tracker$type)) {
        stop(sprintf(
            "unexpected type: %s instead of type: %s for the %s scalar: %s for the computation: %s on the daf data: %s",
            class(value)[[1L]], tracker$type,
            .direction_name(is_for_output), name, comp, dname
        ), call. = FALSE)
    }
    invisible()
}

.verify_vector_data <- function(cd, rec, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    axis <- rec$axis; name <- rec$name
    tracker <- S7::prop(cd, "data")[[.data_key(rec)]]
    exists_ <- format_has_axis(base, axis) && format_has_vector(base, axis, name)
    if (!exists_) {
        if (.is_mandatory(tracker$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s vector: %s of the axis: %s with element type: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), name, axis, tracker$type, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    if (.is_forbidden(tracker$expectation, is_for_output, S7::prop(cd, "overwrite"))) {
        stop(sprintf(
            "pre-existing %s vector: %s of the axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, axis, comp, dname
        ), call. = FALSE)
    }
    v <- format_get_vector(base, axis, name)
    if (!.vector_type_ok(v, tracker$type)) {
        stop(sprintf(
            "unexpected type: %s instead of type: %s for the %s vector: %s of the axis: %s for the computation: %s on the daf data: %s",
            class(v)[[1L]], tracker$type,
            .direction_name(is_for_output), name, axis, comp, dname
        ), call. = FALSE)
    }
    invisible()
}

.verify_matrix_data <- function(cd, rec, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    ra <- rec$rows_axis; ca <- rec$columns_axis; name <- rec$name
    tracker <- S7::prop(cd, "data")[[.data_key(rec)]]
    exists_ <- format_has_axis(base, ra) && format_has_axis(base, ca) &&
        format_has_matrix(base, ra, ca, name)
    if (!exists_) {
        if (.is_mandatory(tracker$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s matrix: %s of the rows axis: %s and the columns axis: %s with element type: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), name, ra, ca, tracker$type, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    if (.is_forbidden(tracker$expectation, is_for_output, S7::prop(cd, "overwrite"))) {
        stop(sprintf(
            "pre-existing %s matrix: %s of the rows axis: %s and the columns axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, name, ra, ca, comp, dname
        ), call. = FALSE)
    }
    m <- format_get_matrix(base, ra, ca, name)
    if (!.matrix_type_ok(m, tracker$type)) {
        stop(sprintf(
            "unexpected type: %s instead of type: %s for the %s matrix: %s of the rows axis: %s and the columns axis: %s for the computation: %s on the daf data: %s",
            class(m)[[1L]], tracker$type,
            .direction_name(is_for_output), name, ra, ca, comp, dname
        ), call. = FALSE)
    }
    invisible()
}

.verify_axis_data <- function(cd, axis, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    tracker <- S7::prop(cd, "axes")[[axis]]
    exists_ <- format_has_axis(base, axis)
    if (!exists_) {
        if (.is_mandatory(tracker$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s axis: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), axis, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    if (.is_forbidden(tracker$expectation, is_for_output, S7::prop(cd, "overwrite"))) {
        stop(sprintf(
            "pre-existing %s axis: %s for the computation: %s on the daf data: %s",
            tracker$expectation, axis, comp, dname
        ), call. = FALSE)
    }
    invisible()
}

.verify_access <- function(cd) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    for (ax in ls(S7::prop(cd, "axes"), all.names = TRUE)) {
        tracker <- S7::prop(cd, "axes")[[ax]]
        if (format_has_axis(base, ax) && !tracker$accessed &&
            identical(tracker$expectation, RequiredInput)) {
            stop(sprintf(
                "unused RequiredInput axis: %s of the computation: %s on the daf data: %s",
                ax, comp, dname
            ), call. = FALSE)
        }
    }
    for (key in ls(S7::prop(cd, "data"), all.names = TRUE)) {
        tracker <- S7::prop(cd, "data")[[key]]
        if (!identical(tracker$expectation, RequiredInput) || tracker$accessed) next
        parts <- strsplit(key, ":", fixed = TRUE)[[1L]]
        kind <- parts[[1L]]
        if (kind == "scalar") {
            if (format_has_scalar(base, parts[[2L]])) {
                stop(sprintf(
                    "unused RequiredInput scalar: %s of the computation: %s on the daf data: %s",
                    parts[[2L]], comp, dname
                ), call. = FALSE)
            }
        } else if (kind == "vector") {
            if (format_has_axis(base, parts[[2L]]) &&
                format_has_vector(base, parts[[2L]], parts[[3L]])) {
                stop(sprintf(
                    "unused RequiredInput vector: %s of the axis: %s of the computation: %s on the daf data: %s",
                    parts[[3L]], parts[[2L]], comp, dname
                ), call. = FALSE)
            }
        } else if (kind == "matrix") {
            if (format_has_axis(base, parts[[2L]]) &&
                format_has_axis(base, parts[[3L]]) &&
                format_has_matrix(base, parts[[2L]], parts[[3L]], parts[[4L]])) {
                stop(sprintf(
                    "unused RequiredInput matrix: %s of the rows axis: %s and the columns axis: %s of the computation: %s on the daf data: %s",
                    parts[[4L]], parts[[2L]], parts[[3L]], comp, dname
                ), call. = FALSE)
            }
        }
    }
    invisible()
}

.verify_contract <- function(cd, is_for_output) {
    for (ax in ls(S7::prop(cd, "axes"), all.names = TRUE)) {
        .verify_axis_data(cd, ax, is_for_output)
    }
    for (key in ls(S7::prop(cd, "data"), all.names = TRUE)) {
        # Reconstruct the original record shape for verify_* helpers.
        parts <- strsplit(key, ":", fixed = TRUE)[[1L]]
        tracker <- S7::prop(cd, "data")[[key]]
        rec <- switch(parts[[1L]],
            scalar = list(kind = "scalar", name = parts[[2L]],
                          expectation = tracker$expectation, type = tracker$type),
            vector = list(kind = "vector", axis = parts[[2L]], name = parts[[3L]],
                          expectation = tracker$expectation, type = tracker$type),
            matrix = list(kind = "matrix", rows_axis = parts[[2L]],
                          columns_axis = parts[[3L]], name = parts[[4L]],
                          expectation = tracker$expectation, type = tracker$type)
        )
        switch(rec$kind,
            scalar = .verify_scalar_data(cd, rec, is_for_output),
            vector = .verify_vector_data(cd, rec, is_for_output),
            matrix = .verify_matrix_data(cd, rec, is_for_output)
        )
    }
    if (is_for_output) .verify_access(cd)
    invisible()
}

#' @export
verify_input <- function(daf) {
    if (!S7::S7_inherits(daf, ContractDaf)) return(invisible())
    .verify_contract(daf, is_for_output = FALSE)
}

#' @export
verify_output <- function(daf) {
    if (!S7::S7_inherits(daf, ContractDaf)) return(invisible())
    .verify_contract(daf, is_for_output = TRUE)
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/contracts.R tests/testthat/test-contracts-verify.R
git commit -m "feat(contracts): verify_input + verify_output (existence + type + access)"
```

### Task T5: `merge_contracts()` — Julia's `|>`

**Files:**
- Modify: `R/contracts.R`
- Modify: `tests/testthat/test-contracts-class.R`

- [ ] **Step 1: Append tests**

```r
test_that("merge_contracts: left-wins for RequiredInput axis", {
    left <- Contract(axes = list(cell = list(RequiredInput, "d")))
    right <- Contract(axes = list(cell = list(OptionalInput, "d")))
    merged <- merge_contracts(left, right)
    expect_identical(
        S7::prop(merged, "axes")$cell[[1L]],
        RequiredInput
    )
})

test_that("merge_contracts: incompatible output-output raises", {
    left <- Contract(axes = list(cell = list(OptionalOutput, "d")))
    right <- Contract(axes = list(cell = list(OptionalOutput, "d")))
    expect_error(merge_contracts(left, right), "incompatible expectation")
})

test_that("merge_contracts: type compatibility (Int narrower wins)", {
    left <- Contract(data = list(
        contract_vector("cell", "age", RequiredInput, "integer", "d")
    ))
    right <- Contract(data = list(
        contract_vector("cell", "age", RequiredInput, "numeric", "d")
    ))
    merged <- merge_contracts(left, right)
    age <- merged@data[[1L]]
    expect_identical(age$type, "integer")
})
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Implement `merge_contracts` in `R/contracts.R`**

```r
.merge_expectations <- function(what, key, left, right) {
    if (identical(left, RequiredInput) && right %in% c(RequiredInput, OptionalInput)) {
        return(RequiredInput)
    }
    if (identical(left, OptionalInput) && right %in% c(RequiredInput, OptionalInput)) {
        return(right)
    }
    if (identical(left, CreatedOutput) && right %in% c(RequiredInput, OptionalInput)) {
        return(CreatedOutput)
    }
    if (identical(left, GuaranteedOutput) && right %in% c(RequiredInput, OptionalInput)) {
        return(GuaranteedOutput)
    }
    if (identical(left, OptionalOutput) && identical(right, OptionalInput)) {
        return(OptionalOutput)
    }
    stop(sprintf(
        "incompatible expectation: %s and expectation: %s for the contracts %s: %s",
        left, right, what, key
    ), call. = FALSE)
}

.TYPE_WIDTH_ORDER <- c("logical", "integer", "double", "numeric", "character")

.merge_types <- function(key, left, right) {
    if (identical(left, right)) return(left)
    li <- match(left,  .TYPE_WIDTH_ORDER, nomatch = NA_integer_)
    ri <- match(right, .TYPE_WIDTH_ORDER, nomatch = NA_integer_)
    if (is.na(li) || is.na(ri)) {
        stop(sprintf(
            "incompatible type: %s and type: %s for the contracts data: %s",
            left, right, key
        ), call. = FALSE)
    }
    # Prefer the narrower type (smaller index).
    .TYPE_WIDTH_ORDER[[min(li, ri)]]
}

#' Merge two contracts.
#'
#' Mirrors Julia's `Contract |> Contract`.
#' @export
merge_contracts <- function(left, right) {
    merged_name <- if (identical(left@name, "")) right@name else
        if (identical(right@name, "")) left@name else
            paste0(left@name, "_", right@name)
    merged_axes <- left@axes
    for (a in names(right@axes)) {
        spec_r <- right@axes[[a]]
        spec_l <- merged_axes[[a]]
        if (is.null(spec_l)) {
            merged_axes[[a]] <- spec_r
        } else {
            if (!identical(spec_l[[2L]], spec_r[[2L]])) {
                stop(sprintf("different description for the axis: %s", a), call. = FALSE)
            }
            merged_axes[[a]] <- list(
                .merge_expectations("axis", a, spec_l[[1L]], spec_r[[1L]]),
                spec_l[[2L]]
            )
        }
    }
    # Data: match by .data_key()
    merged_data <- list()
    keys_l <- vapply(left@data, .data_key, character(1))
    keys_r <- vapply(right@data, .data_key, character(1))
    for (i in seq_along(left@data)) {
        rec_l <- left@data[[i]]
        j <- match(keys_l[[i]], keys_r)
        if (is.na(j)) {
            merged_data <- c(merged_data, list(rec_l))
        } else {
            rec_r <- right@data[[j]]
            if (!identical(rec_l$description, rec_r$description)) {
                stop(sprintf("different description for the data: %s",
                    keys_l[[i]]
                ), call. = FALSE)
            }
            merged <- rec_l
            merged$expectation <- .merge_expectations(
                "data", keys_l[[i]], rec_l$expectation, rec_r$expectation
            )
            merged$type <- .merge_types(keys_l[[i]], rec_l$type, rec_r$type)
            merged_data <- c(merged_data, list(merged))
        }
    }
    for (j in seq_along(right@data)) {
        if (!(keys_r[[j]] %in% keys_l)) {
            merged_data <- c(merged_data, list(right@data[[j]]))
        }
    }
    Contract(
        name = merged_name,
        is_relaxed = left@is_relaxed || right@is_relaxed,
        axes = merged_axes,
        data = merged_data
    )
}
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/contracts.R tests/testthat/test-contracts-class.R
git commit -m "feat(contracts): merge_contracts — Julia-|> port with expectation/type resolution"
```

### Task T6: End-to-end contract test on MemoryDaf + FilesDaf

**Files:**
- Modify: `tests/testthat/test-contracts-verify.R`

- [ ] **Step 1: Append an end-to-end test**

```r
test_that("end-to-end: contract wraps MemoryDaf; input+output verify; result matches", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "age", c(10L, 20L))
    cn <- Contract(
        axes = list(cell = list(RequiredInput, "cell axis")),
        data = list(
            contract_vector("cell", "age",    RequiredInput, "integer", "age"),
            contract_vector("cell", "doubled", CreatedOutput, "integer", "2x age")
        )
    )
    cd <- contractor("comp", cn, d)
    expect_null(verify_input(cd))
    # Simulate the computation
    age <- get_vector(cd, "cell", "age")
    set_vector(cd, "cell", "doubled", unname(age) * 2L)
    expect_null(verify_output(cd))
    expect_identical(
        unname(get_vector(d, "cell", "doubled")),
        c(20L, 40L)
    )
})

test_that("end-to-end: contract wraps FilesDaf", {
    withr::local_options(dafr.enforce_contracts = TRUE)
    tmp <- tempfile("dafr-fd-")
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    d <- files_daf(tmp, mode = "w", name = "fd")
    add_axis(d, "cell", c("A", "B"))
    set_vector(d, "cell", "age", c(10L, 20L))
    cn <- Contract(
        axes = list(cell = list(RequiredInput, "cell axis")),
        data = list(contract_vector("cell", "age", RequiredInput, "integer", "age"))
    )
    cd <- contractor("comp", cn, d)
    expect_null(verify_input(cd))
    invisible(get_vector(cd, "cell", "age"))
    expect_null(verify_output(cd))
})
```

- [ ] **Step 2: Run — expect pass (assuming T3 + T4 are correct)**

- [ ] **Step 3: Commit (no new source, just test)**

```bash
git add tests/testthat/test-contracts-verify.R
git commit -m "test(contracts): end-to-end contract on MemoryDaf + FilesDaf"
```

---

## Phase Z — Exit gate

### Task Z1: Regenerate NAMESPACE + man pages + NEWS

**Files:**
- Modify: `NAMESPACE`, `man/*.Rd`, `NEWS.md`

- [ ] **Step 1: Run document()**

```
Rscript -e 'devtools::document()'
```

Expected: updates NAMESPACE and man/*.Rd with new exports:
`ReadOnlyChainDaf`, `WriteChainDaf`, `chain_reader`, `chain_writer`,
`Contract`, `ContractDaf`, `contract_scalar`, `contract_vector`,
`contract_matrix`, `contractor`, `verify_input`, `verify_output`,
`merge_contracts`, `RequiredInput`, `OptionalInput`, `CreatedOutput`,
`GuaranteedOutput`, `OptionalOutput`.

- [ ] **Step 2: Update `NEWS.md` — add a Slice 4 entry**

Open `NEWS.md` and prepend:

```markdown
# dafr 0.4.0 (in development)

## New features

- `chain_reader()` / `chain_writer()`: federate an ordered list of
  `DafReader`s into a single read-only (`ReadOnlyChainDaf`) or
  read-write (`WriteChainDaf`) view. Later entries override earlier
  entries on read; writes go to the final writer; deletes only succeed
  when the entry exists solely in the top writer. Axis consistency
  across overlapping axes is validated at construction. (#slice-4)
- `Contract()`, `contractor()`, `verify_input()`, `verify_output()`:
  typed pre/post-condition enforcement for computations consuming Daf
  data. Guards required / optional / created / guaranteed / optional
  outputs, tracks access to required inputs, and validates element
  types. Enforcement is off by default; enable via
  `DAF_ENFORCE_CONTRACTS=1` (env) or
  `options(dafr.enforce_contracts = TRUE)`. (#slice-4)
- `merge_contracts()`: combine two contracts (Julia's `|>` semantics),
  resolving expectations and element types. (#slice-4)
- `IfNot` / `AsAxis` evaluator semantics: single-hop chained lookup
  `@ A : v ?? X =@ : w` is now evaluated end-to-end. `??` bare drops
  empty-value entries; `?? X` substitutes `X`. (#slice-4)
- `ViewDaf`: axis rename (`viewer(d, axes = list(list("obs", "@ cell")))`)
  and axis filter (`viewer(d, axes = list(list("cell", "@ cell [ keep ]")))`)
  now propagate to `get_vector()` / `get_matrix()` reads. (#slice-4)

## Performance

- `% Log eps: 1` on a `dgCMatrix` now preserves sparsity (in-place
  `log1p` on the `@x` slot). Eliminates the multi-GB dense
  intermediate the previous path produced for typical UMI matrices.
  (#slice-4-P2)
- Bare default reductions (`>| Sum`, `>- Mean`, `>| Max`, etc., with
  no parameters) now route to `rowSums` / `Matrix::rowSums` /
  `matrixStats::rowMaxs` instead of `apply()`. (#slice-4-P3)
- `% Log eps: ε >| Sum` and the `Mean` / `>-` variants now dispatch
  to a fused C++ kernel (`kernel_log_reduce_{dense,csc}_cpp`); the
  CSC variant is single-pass over `nnz` with no dense intermediate.
  (#slice-4-P4)
- `dafr.omp_threshold` (declared in Slice 0 but orphaned) is now
  threaded through `kernel_log_add_cpp` and `kernel_csc_colsums_cpp`,
  and through the new fused log-reduce kernel. Defaults to 10000.
  (#slice-4-P1)

## Bug fixes

- `@ A [ v cmp X ]`: NA values in the masked property no longer leak
  into the result; they are dropped silently, matching Julia's boolean
  indexing semantics. (#slice-4)

## Internal changes

- `ViewDaf` now reuses the base daf's cache environment (the
  previously allocated but unused per-view `query` cache bucket was
  removed). (#slice-4)
- New Imports: `matrixStats` (used by Phase P3 fast path for
  `rowMaxs` / `colMins` etc.).
```

- [ ] **Step 3: Run `devtools::check(error_on = "note")` — expect 0/0/0**

```
R_CHECK_SYSTEM_CLOCK_=0 Rscript -e 'devtools::check(error_on = "note")'
```

Expected: 0 ERROR / 0 WARNING / 0 NOTE.

- [ ] **Step 4: Run the full test suite — expect all green**

```
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: all tests passing (baseline 939 + Slice 4 additions).

- [ ] **Step 5: Commit**

```bash
git add NAMESPACE man/ NEWS.md
git commit -m "docs(slice-4): regenerate NAMESPACE + man + NEWS entry"
```

### Task Z2: Exit gate — dev-repo note + feature-branch merge + tag

**Files:**
- Create: `dev/notes/slice-4-exit.md`

- [ ] **Step 1: Write the exit note**

Follow the structure of `dev/notes/slice-3-exit.md`:
- Deliverables per phase (F/C/T/Z).
- Test + build status (paste actual numbers from Z1.4).
- Scope closed vs deferred (call out any unchecked items in this plan).
- Known mines laid in Slice 4 for Slice 5.
- Commit history summary.
- Repo conventions reinforced in Slice 4.
- Ready-to-paste prompt for the next slice.
- Status at session end.

Content skeleton:

```markdown
# Slice 4 exit gate — 2026-04-<DD>

## Deliverables

**Phase P — Perf hot-path fix (lands FIRST)**
- [x] P1: `dafr.omp_threshold` threaded through existing kernels.
- [x] P2: Sparsity-preserving `Log eps: 1` on `dgCMatrix` (in-place log1p).
- [x] P3: Bare default reductions route to `rowSums` / `Matrix::*` / `matrixStats::*`.
- [x] P4: Fused `kernel_log_reduce_{dense,csc}_cpp` for `Log -> Sum|Mean` motif (with per-thread accumulator buffers in the row-axis CSC variant for parallel scaling).
- [x] P5: Benchmark wall-clock + peak RSS, fast paths on/off, 10K^2 and 30K^2 dgCMatrix; CSV committed to `dev/benchmarks/`.

**Phase F — Slice 3 follow-ups**
- [x] F1: View cache bucket removed (reuses base cache env). Side effect: base writes now correctly invalidate view reads.
- [x] F2: NA-in-mask drop semantics aligned with Julia.
- [x] F3: IfNot evaluator sentinel captured. Multi-hop IfNot status documented in NEWS / scope-deferred.
- [x] F4: AsAxis single-hop chained lookup + IfNot substitution.
- [x] F5: ViewDaf axis rename propagation to vector/matrix reads.
- [x] F6: ViewDaf axis filter propagation to vector/matrix reads.

**Phase C — Chains**
- [x] C1: `ReadOnlyChainDaf` + `WriteChainDaf` classes + constructors.
- [x] C2-C5: Scalar / axis / vector / matrix read dispatch.
- [x] C6-C8: Scalar / axis / vector / matrix write dispatch (top-writer
      + delete-earlier-error + auto-add-axis).
- [x] C9: Julia chain fixture round-trip.

**Phase T — Contracts**
- [x] T1: `Contract` class + expectation constants + spec helpers.
- [x] T2: `ContractDaf` + `contractor()` + enforcement gate.
- [x] T3: `format_*` dispatch + access tracking + relaxed mode.
- [x] T4: `verify_input` / `verify_output`.
- [x] T5: `merge_contracts`.
- [x] T6: End-to-end test on MemoryDaf + FilesDaf.

**Phase Z — Docs + exit**
- [x] Z1: NAMESPACE + man pages + NEWS regenerated.
- [x] Z2: Exit note (this document); branch merged to main; tagged `slice-4`.

## Test + build status

- `testthat::test_dir("tests/testthat")` — <paste> PASS / 0 FAIL / 0 SKIP / <N> WARN.
- `devtools::check(error_on = "note")` — 0 ERROR / 0 WARNING / 0 NOTE.
- `pkgbuild::compile_dll(debug = FALSE)` — clean. No new C++.

## Scope closed vs deferred

**Closed in Slice 4:**
<bullets>

**Deferred to Slice 5+:**
- `@examples` roxygen blocks (Z2 polish) — still deferred.
- `complete_chain!` disk-chain helper — not implemented; defer until
  a consumer needs it.
- `@computation` macro equivalent for R — orthogonal to contracts'
  runtime behaviour; defer.
- Tensor keys in contracts (Julia UNTESTED path) — defer.
- Multi-hop chained lookup (`@ A : v =@ : w =@ : u`) — Slice 4 only
  covers single-hop.
- Full kernel buildout per design spec §6 (`kernels_eltwise.cpp`,
  `kernels_reduce.cpp`, `kernels_matvec.cpp`) — Slice 4 ships only the
  one fused motif (P4). Dedicated perf slice follows once a second
  motif justifies it.
- General fusion planner — Slice 4 hand-codes the one motif; revisit
  when the second pattern shows up.
- Per-thread accumulator buffers in `kernel_log_reduce_csc_cpp` for
  the row-axis variant — Slice 4 accepts the serial nnz scan; lift to
  per-thread buffers if profiling shows it pays.
- L2 upstream PR against `tanaylab/DataAxesFormats.jl` docs — declined
  three times. Spec draft remains resolved and ready.

## Known mines laid in Slice 4 for Slice 5

<enumerate — examples:>
- `ContractDaf` inherits its base daf's cache env; a chained
  contract-on-chain-on-memory daf has three layers sharing one cache.
  If one of the layers is explicitly cleared mid-flight, all see it.
- `chain_writer` auto-adds axes from earlier readers when a new
  vector/matrix is first written to the writer for that axis. If the
  earlier axes have diverged from the writer (shouldn't happen since
  we validate at construction, but modification-behind-the-back is an
  anti-pattern), the writer's copy becomes stale.
- `.type_ok` in contracts uses R class-name matching; it does not
  understand S3/S4/R5 inheritance trees beyond `inherits()`. For
  `numeric` vs `integer` etc., we rely on `is.integer` / `is.numeric`.
- The `merge_contracts` type-order `c("logical", "integer", "double",
  "numeric", "character")` is coarse and doesn't match Julia's full
  `<:` lattice. Narrower → wider only; no cross-axis moves.

## Commit history

<paste> ...
```

Fill in the paste-holder markers after all commits land.

- [ ] **Step 2: Merge the branch (if working in a feature branch)**

If this slice lands on a feature branch `slice-4-chains-contracts`:

```bash
cd ~/src/dafr-native
git checkout main
git merge --ff-only slice-4-chains-contracts
git tag -a slice-4 -m "Slice 4: chains + contracts + Slice 3 follow-ups"
git push origin main --tags
git branch -d slice-4-chains-contracts
```

If landing directly on `main` (smaller PR style), omit the merge and just tag.

- [ ] **Step 3: Commit the exit note in the dev repo**

```bash
cd ~/src/dafr-native/dev
git add notes/slice-4-exit.md
git commit -m "docs(slice-4): exit note"
```

- [ ] **Step 4: Confirm CI green**

Wait for GitHub Actions to go green on the tag + main. If any CI job
fails, investigate and land a follow-up fix commit on main (no tag
rewrite).

- [ ] **Step 5: Re-ask user about the deferred L2 upstream PR**

Per Slice 3 exit's instructions, re-ask the user whether to open the
`tanaylab/DataAxesFormats.jl` docs PR for
`dev/specs/filesdaf-on-disk-spec-draft.md`. If declined again,
document in the Slice 4 exit note and carry forward.
