# Slice 10d — Exit note

**Date:** 2026-04-23
**Predecessor:** Slice 10b (tag `slice-10b` on `main`).
**Branch:** `slice-10d` → merged to `main` as commit `ccfe43a` on `main`.
**Kickoff / spec / plan:** dev/notes/slice-10-kickoff.md, dev/notes/2026-04-23-slice-10d-design.md, dev/plans/2026-04-23-slice-10d-implementation.md.

## Scope delivered

- 4 vignettes: `dafr` (getting started), `queries` (DSL), `native-performance`
  (mmap + bench), `anndata` (h5ad round-trip).
- `_pkgdown.yml` covering every export (verified by
  `dev/scripts/check-pkgdown-coverage.R`).
- `README.Rmd` + `README.md` rewritten for native (Julia mentions dropped,
  native advantages added).
- `cran-comments.md` stub for eventual CRAN submission.
- `NEWS.md`: `# dafr 0.1.0` top entry + existing ledger preserved under
  `# dafr 0.1.0 — Development history`.
- `DESCRIPTION`: `Version: 0.0.0.9000` → `0.1.0`; `VignetteBuilder: knitr`
  added.
- `@examples` backfilled on 20 previously-skipped exports (chain/view
  constants, `Contract`, `ContractDaf`, `DafReader`, `DafWriter`,
  `DafReadOnly`, `FilesDaf`, `FilesDafReadOnly`, `MemoryDaf`,
  `ReadOnlyChainDaf`, `WriteChainDaf`, `ViewDaf`).

## Numbers

- Test suite: 2616 PASS / 0 FAIL / 2 SKIP / 1 WARN — unchanged across the
  slice (docs-only; zero behavior change).
- `devtools::check(vignettes = TRUE)`: 0 ERROR, 1 WARNING, 4 NOTEs — all
  pre-existing carry-over, none from 10d surfaces.
  - WARNING: `qpdf` not installed (system-level; vignette PDF size
    check skipped).
  - NOTEs: `.claude/` hidden dir, installed size 6.5 MB, "unable to
    verify current time", non-standard `benchmarks/` top-level.
- `check-pkgdown-coverage.R`: clean — every exported symbol mapped to a
  pkgdown section.

## Per-phase commit SHAs

| Phase | Commit | Subject |
|---|---|---|
| A.1 | `9cbbe90` | backfill @examples on previously-skipped exports |
| A.2 | `0c2d223` | rewrite README for native package |
| A.3 | `4a20032` | fix `Contract` @example — no `name` arg |
| B.1 | `db611d1` | add 4 vignettes + `VignetteBuilder: knitr` in DESCRIPTION |
| B.2 | `b41c096` | add `_pkgdown.yml` with every export categorised |
| C   | `4930814` | NEWS 0.1.0 + version bump + cran-comments stub |

Merge: `ccfe43a  merge(10d): docs + release polish — dafr 0.1.0`.

## Issues encountered / deviations

- README queries that use the `[filter] : vector` form evaluate without
  applying the filter — latent bug surfaced while drafting README
  examples. Logged to the post-slice-10 cleanup bucket (task #20).
- `queries.Rmd` fixture adjustments: `age` lives on the `donor` axis in
  `example_cells_daf()`, not `cell`. Rewrote the filter examples
  accordingly.
- `Contract` constructor does not take a `name` argument (that lives on
  the `contractor()` wrapper); Phase A's initial example was wrong and
  was fixed in `4a20032`.

## Tags

- `v0.1.0` on `ccfe43a` — public release tag.
- `slice-10d` on same commit — consistency with earlier slice tags.
- `git tag --points-at HEAD` at merge commit lists both.

## Post-release work

- Push `main` + tags to origin; watch CI.
- Post-slice-10 cleanup (task #20):
  - mmap S7-ctor floor (4 accept-class breaches).
  - Two-pass flat-storage for mode / quantile.
  - `copy_all` double-write bug.
  - `.Rbuildignore` entries for `.claude/`, `benchmarks/` (CRAN
    pre-submission).
  - tensor `.verify_access` tracking.
  - `verify_contract` proper `is_static` refactor (drop the
    accessed-marker hack).
  - `get_dataframe_query` `columns =` kwarg reinstatement.
  - `format(1e-6)` scientific-notation canonical fix.
  - `[filter] : vector` query bug (new in 10d review).
  - Various README / vignette minor fidelity issues.
- `h5df` HDF5-backed Daf store (new slice).
- CRAN submission (post-cleanup).
