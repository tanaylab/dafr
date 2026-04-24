# Slice 10d — Design: Release Polish + 0.1.0 Tag

**Date:** 2026-04-23
**Predecessor:** Slice 10b (tag `slice-10b` on `main`).
**Parent kickoff:** `dev/notes/slice-10-kickoff.md` §"10d — docs + release polish".
**Scope:** Vignettes, pkgdown site config, README rewrite, `@examples` backfill, NEWS 0.1.0 entry, version bump, `v0.1.0` git tag.

## 1. Goal

Cut the first public release: `dafr 0.1.0`. No new exports. Pure docs + release plumbing. End-state is a tagged commit `v0.1.0` on `main` with a green `devtools::check()` (NOTE-free on everything touched by slice 10, pre-existing CRAN-submission NOTEs acceptable per parent kickoff).

**Done signal.** `DESCRIPTION` shows `Version: 0.1.0`. NEWS top entry is `# dafr 0.1.0`. Vignettes build. pkgdown config covers every export. Tag `v0.1.0` on `main`.

## 2. Out of scope

- CRAN submission. Tag only; CRAN is post-release once the installed-size and benchmarks-dir NOTEs are burned down.
- Any new exports or behavior changes.
- pkgdown site deploy (GitHub Pages / netlify). Only the `_pkgdown.yml` config ships; deployment is a separate CI concern.
- `dafr 0.2.0` roadmap notes.
- The accumulated post-slice-10 cleanup (tracked separately).

## 3. Locked decisions

| # | Topic | Decision |
|---|---|---|
| 1 | Vignettes shipped | 4: `vignettes/dafr.Rmd` (getting started), `vignettes/queries.Rmd` (query DSL incl. pipe-chain builders), `vignettes/native-performance.Rmd` (mmap + bench headline numbers), `vignettes/anndata.Rmd` (h5ad round-trip). Kept concise (~80–120 lines each). |
| 2 | Vignette engine | `knitr::rmarkdown` (default). `VignetteBuilder: knitr` + `Suggests: knitr, rmarkdown` (already in `Suggests`). |
| 3 | pkgdown config | `_pkgdown.yml` at package root. Reference sections grouped by: Core data model / Readers & writers / Query DSL (string form) / Query builders (the 53 DafrQuery exports) / AnnData interop / Mmap readers / Contract UX / Op registry / Class predicates / DataFrame helpers. Drop "Deprecated" — native has none. |
| 4 | README rewrite | Port wrapper README.Rmd structure; rewrite Installation as native-only; drop "Data Transfer" / "JuliaCall copy tax" sections; add "Native advantages" bullet list (no-Julia install, mmap, OpenMP kernels, `register_eltwise` / `register_reduction`). |
| 5 | `@examples` backfill | Run `devtools::check()` once; enumerate all WARNINGs on "missing examples for..." and add runnable blocks. Use `\dontrun{}` for any >1s or fixture-requiring example. Scope: ≤30 functions (matches parent kickoff's ~25 estimate). |
| 6 | NEWS 0.1.0 format | Top entry: `# dafr 0.1.0 (2026-04-23)`. Flat bullets for user-visible surface: new query-builder DSL, AnnData facade + h5ad, wrapper-parity exports. Known gaps: `h5df` deferred. Existing per-slice sections fold under `# dafr 0.1.0 — Development history` below. |
| 7 | Version bump | `DESCRIPTION` `Version: 0.0.0.9000` → `Version: 0.1.0`. Same commit as NEWS rewrite. |
| 8 | Git tag | `v0.1.0` on `main` at slice-10d merge commit. Also keep `slice-10d` tag for consistency with earlier slices. |
| 9 | cran-comments.md | Stub only — no CRAN submit attempt. Minimal file: release summary, R CMD check results noting the two pre-existing NOTEs (installed size, benchmarks dir) as known issues. |
| 10 | Phase ordering | 0 (branch) → A (`@examples` backfill + README) → B (pkgdown + vignettes) → C (NEWS 0.1.0 + version bump + cran-comments stub) → Z (merge + tag v0.1.0 + exit note). |

## 4. Surface contract

No new exports. No new files in `R/`. New files only in:
- `vignettes/` (4 `.Rmd`).
- Package root: `_pkgdown.yml`, `cran-comments.md`.
- `man/` (roxygen regen picks up `@examples` additions).
- `NEWS.md` (rewritten).
- `DESCRIPTION` (version bump + `VignetteBuilder: knitr` if not present).

## 5. Error handling / fallback

This is a docs slice; no runtime code paths. Test mines are limited to:
- Vignette builds without error (`devtools::build_vignettes(quiet = TRUE)`).
- `devtools::check()` remains 0E / 0W / ≤ 4N.
- pkgdown config parses cleanly (`pkgdown::build_reference()` smoke test if `pkgdown` is installed; else skip).

## 6. Test plan

No new testthat assertions. The "tests" for this slice are:

1. `devtools::check(error_on = "never", vignettes = TRUE)` — must pass vignettes too, this time.
2. Running `Rscript -e 'devtools::build_vignettes()'` produces all 4 HTML outputs.
3. Every exported function appears in `_pkgdown.yml`'s `reference:` list (spot-check via a small shell script at Phase B).

## 7. Dependency changes

None hard. `pkgdown` stays out of `Imports` and `Suggests` — it's a developer tool, not a runtime dependency. `knitr` + `rmarkdown` are already in `Suggests` for test helpers.

Optionally add `Config/Needs/website: pkgdown` (pkgdown-specific metadata slot; doesn't affect CRAN).

## 8. Slice execution order

- **Phase 0:** branch `slice-10d` off `main` (currently at `slice-10b` merge).
- **Phase A:** `@examples` backfill + README rewrite.
- **Phase B:** 4 vignettes + `_pkgdown.yml` + reference-coverage spot-check script.
- **Phase C:** NEWS 0.1.0 rewrite + version bump + `cran-comments.md` stub.
- **Phase Z:** `devtools::check` green; merge to `main`; tag `v0.1.0` AND `slice-10d`; exit note.

## 9. Exit criterion

- `DESCRIPTION` shows `Version: 0.1.0`.
- NEWS top entry is `# dafr 0.1.0`.
- 4 vignettes build clean.
- `_pkgdown.yml` references every export.
- `devtools::check(vignettes = TRUE)` 0E / 0W / ≤ 4N (pre-existing).
- Git tags `v0.1.0` and `slice-10d` on `main`.
- Exit note in `dev/`.

## 10. Post-release work (slices after 0.1.0)

- `h5df` HDF5-backed Daf store.
- Full post-slice-10 cleanup (task #20).
- CRAN submission (needs installed-size + benchmarks-dir NOTE burn-down).
- dplyr-style verbs.
- pkgdown site deployment.
