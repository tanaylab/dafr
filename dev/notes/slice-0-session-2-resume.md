# Slice 0 — Session 2 resume breadcrumb

**Session ended:** 2026-04-19.
**Status:** Phases A, B, C, D complete. Phase E not started.

## What's on disk

### Package repo (`~/src/dafr-native/`, branch `main`)

Phase D added 8 commits on top of Phase C (`bf8cafc`):

```
aebc80a fix(desc): declare Matrix class imports; add Seurat/scran/SCE to Suggests
2cebd65 test: Seurat/scran ALTREP compatibility smoke tests
ae9cdaa fix(altrep): POC-anchor ALTREP-preservation tests; CSC nnz check; robust tempdir helper
cef0148 feat: mmap_dgCMatrix + Matrix-compat smoke tests
c22139f fix(altrep): file-size validation; error-path, idempotency, serialize-roundtrip tests
27462b3 feat: R-visible mmap_real / mmap_int / mmap_lgl ALTREP constructors
3884b15 fix(altrep): guard get_region bounds; document single-threaded invariant
eff7c53 feat: ALTREP classes for mmap-backed double/int/logical vectors
```

### Dev repo (`~/src/dafr-native/dev/`)

```
b46e133 fix(plan): retrofit three deviations discovered in Phase D execution
12e1893 notes: ALTREP downstream compat findings
18086d8 fix(plan): Phase D ALTREP — scalar length storage, idempotent materialization
8282021 notes: session 1 resume breadcrumb
```

The plan at `plans/2026-04-19-slice-0-scaffold-and-poc.md` has been
retrofitted twice in session 1 + session 2 so it matches what actually
works. Next session can re-execute it without hitting the same bugs.

## Test status

**158 tests passing** across 8 testthat files. `R CMD check` produces
**0 ERRORs**. One WARNING (pre-existing Phase A/B Rd undocumented args
on the S7 classes) and two NOTEs (pre-existing unused-Imports `bit64`,
`cli` — will be addressed when Slice 1 adds real uses; and the
environmental "hedgehog not installed" which is an R library issue,
not a package defect).

Run all: `Rscript -e 'pkgload::load_all("~/src/dafr-native", quiet=TRUE); testthat::test_dir("~/src/dafr-native/tests/testthat")'`

## POC gate — OPEN

The architectural bet — ALTREP-backed slots flowing from FilesDaf into
dgCMatrix and then into downstream single-cell tooling — is empirically
validated:

- `is_altrep(m@x)` is TRUE after `mmap_dgCMatrix(...)` construction.
- `is_altrep(m@x)` is TRUE after `Matrix::colSums(m)` (ALTREP preserved
  through read-only ops).
- scran 1.32 + SingleCellExperiment 1.26: end-to-end `quickCluster`
  works and the `@x/@i/@p` slots stay ALTREP the whole way through.
- Seurat 5.1: `CreateSeuratObject` succeeds but materializes the counts
  slot on ingest. Documented as "Acceptable" in findings — Seurat's
  mutation-heavy pipeline would materialize anyway.

Findings are at `~/src/dafr-native/dev/notes/altrep-compat-findings.md`.

## What's next — Phase E (Benchmark harness)

Two tasks, about 150 lines of R.

- **E1** — `inst/benchmarks/workloads.R`: synthetic sparse-matrix
  workload generator (`make_synthetic_sparse`, `write_csc_slots`).
- **E2** — `inst/benchmarks/bench.R`: main benchmark script that uses
  `bench::mark` to time (1) `mmap_dgCMatrix` construction (open cold),
  (2) `Matrix::colSums` on mmap vs native, (3) `Matrix::t()` on mmap vs
  native. Writes CSV to `dev/benchmarks/`.

Phase E has no production-risk follow-ups. The more interesting Phase F
bake-off (cpp11+BLAS vs RcppEigen) comes after.

## Known open concerns (non-blocking, track for Slice 1)

From the Phase D over-phase review:

- **`bit64` and `cli` are declared in Imports but never used.** Not
  introduced by Phase D but still there. Address when Slice 1 adds
  real uses (`cli` for user messages, `bit64` for int64 column types).
- **`writeBin(..., size=8L)` is native-endian.** Tests happen to work
  on x86_64. FilesDaf format spec (Slice 2) must pin endianness.
- **`is_altrep()` is an unexported helper accessible via `dafr:::`.**
  Slice 1's first vignette might leak it; consider renaming to
  `.is_altrep` or moving to `tests/testthat/helper-*.R`.
- **`mmap_real_dataptr(writeable=FALSE)` returns a `const_cast<void*>`
  into a PROT_READ mmap.** Standard ALTREP convention (caller promises
  not to write), but a comment next to each const_cast would help.
- **Long-vector (>2^31) and "file truncated while R vector live"
  scenarios are untested.** Defer to later slices with real file
  sizes.

## Non-obvious repo conventions (don't re-discover)

- Native headers use `.h`, not `.hpp`. CRAN preference.
- `src/init.cpp` does NOT exist and must NOT exist. cpp11 owns
  `R_init_dafr`. Any subsystem that needs init-time registration does
  it via a `[[cpp11::init]]`-decorated helper (see
  `src/altrep_mmap_r.cpp` for the pattern).
- `helper-tempfiles.R`'s `new_tempdir(envir = parent.frame())` and
  `new_tempfile(envir = parent.frame())` both accept an `envir` arg.
  When calling from a nested helper (fixture function wrapping the
  call), pass `envir = parent.frame()` explicitly so cleanup scopes to
  the `test_that` body rather than the wrapper.
- Dev repo (`~/src/dafr-native/dev/`) is a separate git repo nested
  inside the package repo (ignored). Plan, notes, and findings go in
  the dev repo; package code and tests go in the package repo.
- Commit to the right repo by inferring from file paths, not by
  running `pwd`. When a task says "commit", use
  `cd ~/src/dafr-native && git add ...` for source/test commits and
  `cd ~/src/dafr-native/dev && git add ...` for spec/plan/note
  commits.

## Open TODO for the plan — port DAF R test corpus

The Julia package `DataAxesFormats.jl` has a substantial test suite
that was ported into the existing R-facade package (`DafJuliaWrapper`
at `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr`).
Those tests represent the full semantic contract of the data model —
axis bookkeeping, vector/matrix storage, queries, views, chains,
contracts, adapters, computations — and should be our ground truth for
the native-R reimplementation.

**Goal for some later slice (Slice 2 or Slice 3):** port the DAF R
test suite into the native package, so our final coverage equals the
Julia package's test coverage. The plan should grow a phase (call it
Phase Test-Port, or fold into the MemoryDaf/FilesDaf slices) that
walks the existing DAF R tests, translates any Julia-facade-specific
bits (julia_call, julia_eval) to the native API, and reconciles
behavior differences.

Concretely: `cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr/tests/testthat && ls`
and pick the tests that exercise the data model (not the Julia bridge
mechanics). Make them pass against the native implementation.

**Blockers before this can start:** Slice 1 needs `MemoryDaf` working;
Slice 2 needs `FilesDaf`. Test-porting is most natural once
MemoryDaf+FilesDaf are both in place and the S7 API surface matches
what the tests call.

## How to resume

In a fresh Claude Code session at
`/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr`, say:

> Continue executing Slice 0 of the native-R dafr plan from Phase E
> (benchmark harness). The plan is at
> `~/src/dafr-native/dev/plans/2026-04-19-slice-0-scaffold-and-poc.md`,
> the resume breadcrumb is at
> `~/src/dafr-native/dev/notes/slice-0-session-2-resume.md`. Phase D is
> complete and the POC gate is open. Use subagent-driven development.
> Phase E is two R-only tasks (workload generator + bench script) with
> modest review cadence; full review discipline can resume at Phase F
> where the cpp11+BLAS vs RcppEigen bake-off starts.
