# Slice 9a — Exit

**Date:** 2026-04-22.
**Branch:** `slice-9a-correctness`.
**Predecessor:** tag `slice-8` at merge commit `f7978cc` (branch
`slice-8-matrix-fastpaths`).

## Delivered

Correctness workstream (Workstream A) from the Slice 9 kickoff. All five
items from the Workstream A plan delivered (T3 was a no-op audit; no files
required changes beyond the T2 token swap). Workstream B (perf parity with
DAF.jl) is deferred to Slice 9b.

### Commits on slice-9a-correctness

| SHA | Subject |
|-----|---------|
| `e20a7b8` | refactor(query_eval): swap G2/G3 dispatch to match Julia convention |
| `90f6877` | test(slice-9a): invert grouped-matrix tokens for G2/G3/G4 swap |
| `075f01c` | test(slice-9a): fix stale operator-name strings after T2 swap |
| `49a2d36` | feat(parse): accept `>>` as G1 alias for `>|` (Julia parity) |
| `4edffd5` | feat(ops): accept Julia type-name aliases in Convert, support Int64 |
| `11cc61b` | test(fixtures): extend julia-queries for Slice 9a surface |
| `33a8e6a` | fix(ops): preserve dim/dimnames in Convert Int64 on matrix input |
| `346a744` | fix(tests): handle integer64 matrices in julia-compat comparison |
| `610ef57` | test(complete-daf): round-trip renamed-axis view through complete_daf |

T3 (audit of pre-Slice-8 grouped tests) was confirmed as no-op: all
semantic drift was contained in `test-query-grouped-slice8.R` and
`test-query-eval-groupby.R`, both handled by T2. No other files required
token inversion.

### R-side changes

- **`R/query_eval.R`**: `.apply_reduction_grouped_matrix` — `is_g2` /
  `is_g3` conditions swapped; G4a/G4b inner-op derivation updated to
  match. Now byte-parity with DAF.jl for all grouped-matrix queries.
- **`R/parse.R`**: `>>` accepted as a parser alias for `>|` in the G1
  reduction position.
- **`R/operations.R`**: `.op_convert` extended with a Julia type-name
  alias table (`Float32`/`Float64` → `double`; `Int32` → `integer`;
  `Int64` → `bit64::integer64`; `Bool` → `logical`). `Int64` matrix path
  preserves dim/dimnames explicitly (base R `storage.mode<-` drops them).

### Test infrastructure

- **`tests/testthat/test-query-grouped-slice8.R`**: all 84 grouped-matrix
  assertions updated via T2 token swap; stale operator-name strings fixed
  in T2 follow-up.
- **`tests/testthat/test-query-julia-compat.R`**: matrix comparison path
  now casts both sides to `as.integer()` before comparison when the
  expected type is `integer64`, avoiding IEEE-754 reinterpretation garbage
  from `as.vector()` stripping the S3 class.
- **`tests/testthat/fixtures/julia-queries/fixture.json`**: extended from
  50 to 73 records (+23). New records cover: G1 (`>>`), G2, G3,
  Convert-to-Int32 (vector + matrix), Convert-to-Int64 (vector + matrix),
  Mode-on-character. All byte-parity-verified against DataAxesFormats.jl.
- **`tests/testthat/test-complete.R`**: new regression test confirms
  `complete_daf` correctly round-trips a view with a renamed axis.

### TDD note

Per project convention established at Slice 4 exit: T1 (G2/G3 dispatch
swap), T2 (token inversion), and T3 (audit) are **regression-guard** tests,
not failing-first TDD. The pre-existing grouped tests enforced the old
(wrong) semantics; T1 fixed the code and T2 updated the tests to match
Julia's correct convention. The fixture extension in T6 is new-coverage
TDD (Julia outputs were generated first, then R was verified to match).

### Breaking change

The G2/G3 grouped-matrix operator semantic inversion is a **breaking
change** for callers using `GroupRowsBy`/`GroupColumnsBy` with a matrix
output. Announced in NEWS.md. Users who hard-coded the old operator
pairing (e.g., `-/ g >|` expecting `ngroups × ncol`) must update their
query strings:

| Pattern | Old operator | New operator |
|---------|-------------|-------------|
| G2 (ngroups × ncol) | `-/ g >|` | `-/ g >-` |
| G3 (nrow × ngroups) | `\|/ g >-` | `\|/ g >\|` |
| G4a (vector, ngroups) | `-/ g >-` | `-/ g >\|` |
| G4b (vector, ngroups) | `\|/ g >\|` | `\|/ g >-` |

G1 vector reduction (`/ g >|`) is unchanged.

## Test status at exit

**1813 PASS / 0 FAIL / 1 SKIP / 1 WARN.**

- SKIP: `assert_no_densify_during` S4-branch test (manual-only; pre-existing
  from Slice 8 Task 1, skip condition covers both `devtools::test()` and
  R CMD check via `_R_CHECK_PACKAGE_NAME_` env var).
- WARN: pre-existing scran/irlba SVD tolerance notice in
  `test-altrep-downstream.R` (unchanged since Slice 0).

## Check status at exit

**0 errors / 0 warnings / 2 structural notes** (plus 1 transient network
note that is not caused by any code change).

- NOTE (package size): 6.1 MB installed — extdata 1.4 MB + libs 3.6 MB.
  Structural; identical to Slice 8 exit. Cannot be reduced without removing
  example data or C++ kernels.
- NOTE (top-level files): `benchmarks/` non-standard directory. Added
  Slice 8; harmless for a dev package.
- NOTE (future file timestamps): "unable to verify current time" — R CMD
  check's NTP lookup fails on this machine. Transient infrastructure noise;
  not caused by any code in this repo.

`devtools::check(error_on = "note")` exits 1 due to the 3 notes above.
The underlying R CMD check is clean of errors and warnings. This is
identical to the Slice 8 baseline for the two structural notes.

## Public surface

**110 exports, unchanged.** Slice 9a added no new exports. All changes
are to internal dispatch logic, the parser, and test infrastructure.

## Julia DAF state at exit

- `~/src/DataAxesFormats.jl` HEAD: `49fbba140437387a378217c2fa658d4231d0c8c1`
  (unchanged since Slice 3 — seven slices of stability).
- `~/src/TanayLabUtilities.jl` at `48a4a57` (unchanged).
- Both registered as Julia `dev` packages in conda env `dafr-mcview`
  (Julia 1.12.5 in the conda env).

## Bool-matrix divergence — intrinsic, not deferred

`Convert type Bool` on a matrix produces different results in Julia
(strict `InexactError` for values > 1) versus R (permissive `as.logical`,
non-zero → TRUE). This is an intrinsic language-level semantic difference.
It is **not a deferral** — there is no R-side fix that would match Julia's
strict semantics without writing a custom C++ cast. The fixture deliberately
omits a matrix Bool record. Document-only resolution; no follow-up task.

## Deferred / still open

### Carried forward from Slice 8

1. **Grouped G3 kernel memory at high thread counts**: the thread-bucket
   layout in `kernel_grouped_reduce_csc_cpp` explodes at scale (128
   threads × 10k rows × 100 groups × 8 bytes ≈ 6.7 GB). Workaround via
   `options(dafr.kernel_threshold = Inf)`. A row-partitioned fallback or
   explicit thread cap (4–8 for grouped kernels) would fix this. Still
   open.

2. **G3 cache-access pattern (dense)**: the dense grouped kernel writes
   `output[g, j]` with `g` in the inner loop (col-major layout). Negligible
   at current test sizes. Still open.

3. **`derive_op` silent zero for unknown op codes**: should be an assertion
   or hard error. Still open.

4. **H5df backend**: `open_daf()` only dispatches FilesDaf. HDF5 support
   deferred from Slice 6. Still open.

### New in this slice

5. **Workstream B — Performance parity with DAF.jl**: the bake-off harness
   (described in `dev/notes/slice-9-kickoff.md` Workstream B) was not
   started. This is the primary Slice 9b target. Includes: bake-off
   runner, query set covering the full Julia-queries fixture, comparison
   CSV, and identification of any hot spots where `dafr` is >2× slower
   than Julia.

6. **GeoMean `log(v + eps)` computed unconditionally in C++ kernels**: all
   `Acc::push` calls compute `log(v + eps)` regardless of op. Benchmark
   impact unmeasurable at current sizes; can be gated with
   `if (op == Op::GeoMean)`. Minor.
