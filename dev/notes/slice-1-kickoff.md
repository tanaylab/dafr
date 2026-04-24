# Slice 1 — Kickoff breadcrumb

**Date:** 2026-04-20.
**Predecessor:** Slice 0 (tag `slice-0` on `tanaylab/dafr`), exit gate at
`dev/notes/slice-0-exit.md`.

## What changed between the end of Slice 0 and now

1. **Package rename landed.** The old JuliaCall-based `dafr` R package was
   renamed to `dafJuliaWrapper` and its GitHub repo renamed to
   `tanaylab/dafJuliaWrapper`. See commit `0c22abe` on branch
   `caching-and-facade` of `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr`.
   - Other long-lived branches of that repo (`main`, `update-to-v0.2.0-api`,
     `feature/jlview-zero-copy`, `code-review-fixes`) still have `Package: dafr`.
     They will pick up the rename when they next rebase/merge onto
     `caching-and-facade`. Dependabot branches will just be closed.
2. **Native-R `dafr` pushed to GitHub.** Created private repo
   `tanaylab/dafr`. The local path `~/src/dafr-native/` tracks `origin/main`.
   Tag `slice-0` pushed. CI triggered on first push: R-CMD-check (linux,
   macOS, windows) + altrep-sanity (linux). Nightly `bench` workflow is
   cron-scheduled and won't fire until 04:17 UTC.
3. **Local directory names intentionally unchanged.** Internal package
   names now match GitHub repo names, but the local checkouts stay at
   `/net/mraid20/.../src/dafr` (the Julia-wrapper, confusingly named)
   and `~/src/dafr-native/` (the native-R one). Don't rename these
   without updating every memory entry, plan file, and breadcrumb that
   references them.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main`, clean,
  tracking `origin/main` at `git@github.com:tanaylab/dafr.git` (private).
  Internal `Package: dafr`.
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`, branch `main`, clean, no remote.
  Contains plans, specs, notes, benchmarks, scratch RcppEigen package,
  bake-off results.
- **Test status**: 8 testthat files, 44 test_that blocks,
  ~158 expectations, 0 failures. One upstream scran warning about SVD
  tolerance — not a dafr defect. Run via
  `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
  from the package root.
- **Build status**: `pkgbuild::compile_dll(debug=FALSE)` is clean. cpp11
  bindings regenerated whenever kernels are added. `src/dafr.so` and
  `*.o` are gitignored in the package repo.
- **Benchmarks**: `inst/benchmarks/bench.R --small` writes
  `dev/benchmarks/slice-0-baseline-*.csv`. `dev/benchmarks/run-bakeoff.R`
  with `SMALL=1` writes `dev/benchmarks/bake-off-results.csv`.
- **Bake-off decision**: cpp11+BLAS (D) overall, with transpose kernel
  reopened — Eigen wins by 51% there; decision deferred to Slice 2 or
  later when transpose usage materializes.

## CI + portability status (updated 2026-04-20)

After the initial push, CI failures surfaced several portability gaps;
all have been fixed on `main` (commits `7bcb958..65a5315`):

- **decor missing from altrep-sanity + bench workflows** → added as
  `any::decor` extra-package. `pkgbuild::compile_dll()` calls
  `cpp11::cpp_register()` which imports it.
- **macOS build: `omp.h file not found`** → hardcoded
  `-DDAFR_HAVE_OPENMP=1` was forcing `#include <omp.h>` on macOS default
  clang which ships without OpenMP. Switched openmp_shim.h to the
  compiler-defined `_OPENMP` macro (defined only when `-fopenmp` is
  effective) and dropped the custom define from both Makevars files.
- **DafReadOnly.Rd undocumented-args WARNING** → documented all six
  properties on the DafReader S7 base class via roxygen `@param`; added
  `@inheritParams DafReader` on DafReadOnly and DafWriter.
- **Windows native mmap** → implemented via Win32
  CreateFileMappingW/MapViewOfFile in `src/mmap_region.cpp`. Paths are
  accepted as UTF-8 and converted to UTF-16 via MultiByteToWideChar.
  `<windows.h>` is confined to the .cpp so its TRUE/FALSE macros don't
  leak into altrep_mmap.cpp and clobber Rboolean. Makevars.win now sets
  `DAFR_HAVE_MMAP=1`.

Result: R-CMD-check (linux/macOS/windows) and altrep-sanity all green
at commit `65a5315`. Fresh CI runs cache deps and complete in ~4 min.

## Still open from Slice 0

These are non-blocking for Slice 1 but should be tracked:

- **Phase G2 upstream PR** against `tanaylab/DataAxesFormats.jl` docs
  (FilesDaf on-disk spec) is deferred. Draft spec is at
  `dev/specs/filesdaf-on-disk-spec-draft.md`. Three `[UNCLEAR]` markers
  in the spec are intended prompts for Oren's review.
- **Transpose kernel** (F3) reopening — decide in Slice 2.
- **CSC colSums bake-off** is inconclusive at SMALL=1 (OpenMP thread-pool
  startup dominates). Re-run at 100M+ nnz before locking the kernel.
- **Two non-obvious findings from the Julia spec extraction** that
  contradict initial plan assumptions for Slice 2:
  - Julia FilesDaf has **no on-disk version counters** — all in-memory,
    reset to 0 on open.
  - Julia FilesDaf has **no atomicity model** — direct overwrite, no
    `.tmp` rename, no fsync. Multi-process concurrent writes unsafe.
  Slice 2 must decide: mirror Julia's behavior or add atomicity.
- **`bit64` / `cli` declared in Imports but never used.** Intentional —
  Slice 1 should close this with real uses (`cli` for user messages,
  `bit64` for int64 column types).
- **Pre-existing Rd undocumented-args warning** on S7 classes — close
  in Slice 1 when those classes get roxygen documentation.
- **`writeBin(..., size=8L)` is native-endian.** FilesDaf format spec
  (Slice 2) must pin endianness. Julia spec says little-endian per
  `dev/specs/filesdaf-on-disk-spec-draft.md`, so this will align.
- **Long-vector (>2^31)** and **"file truncated while R vector live"**
  ALTREP scenarios untested. Defer to later slices.

## What Slice 1 should deliver

Per the top-level plan `dev/plans/2026-04-19-slice-0-scaffold-and-poc.md`
§final-line: *Slice 1 — MemoryDaf + axes + scalars/vectors/matrices
get/set + cache infrastructure.*

Concretely, Slice 1 must implement enough of the API for the downstream
test-port (mentioned in `slice-0-session-2-resume.md`) to begin against
a MemoryDaf backend:

1. **MemoryDaf class**: implement the FormatReader/FormatWriter S7
   methods declared in Phase B (~40 generics at
   `R/format_api.R`) for a pure-in-memory backend. Uses nested named
   environments keyed by axis/name/type.
2. **Axis add/delete/query**: `add_axis`, `delete_axis`, `axes_set`,
   `axis_entries`, `axis_indices`, `axis_length`, `axis_dict`.
3. **Scalars get/set**: `get_scalar`, `set_scalar`, `has_scalar`,
   `delete_scalar`, `scalars_set`.
4. **Vectors get/set**: `get_vector`, `set_vector`, `has_vector`,
   `delete_vector`, `vectors_set`. Dense and sparse variants. Accept
   named vectors, bit64, character.
5. **Matrices get/set**: `get_matrix`, `set_matrix`, `has_matrix`,
   `delete_matrix`, `matrices_set`. Dense (double/int/bool) and CSC
   sparse (`dgCMatrix`, `lgCMatrix`).
6. **Cache infrastructure**: fill in the 3-tier cache skeleton from
   Phase B3 — LRU eviction under a memory cap, query-result cache hits
   on compatible queries, version-counter invalidation wired up.
7. **Test port start**: begin porting the `DafJuliaWrapper` (formerly
   `dafr`) testthat suite at
   `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr/tests/testthat/`
   into the native package, filtering out Julia-bridge-mechanics tests
   (those exercising JuliaCall directly) and keeping data-model tests
   (axes, scalars, vectors, matrices, queries).
8. **`bit64` + `cli` first real uses** — axis int64 entries or progress
   messages respectively.

## Known mines laid in Slice 0 for Slice 1

- `is_altrep()` is unexported but accessed via `dafr:::` in tests.
  Consider moving to `tests/testthat/helper-altrep.R` or renaming to
  `.is_altrep`.
- `mmap_real_dataptr(writeable=FALSE)` returns a `const_cast<void*>`
  into a PROT_READ mmap. Standard ALTREP convention (caller promises
  not to write) — add a comment next to each `const_cast` for future
  readers.
- `DAFR_PARALLEL_FOR` macro now works correctly, but the test coverage
  for OpenMP-enabled code paths is nil. Add at least one unit test per
  kernel that exercises the parallel branch (e.g., `ncol >= 1000`).

## Repo conventions worth re-stating

- Native headers use `.h`, not `.hpp` — CRAN preference.
- `src/init.cpp` does NOT exist and must NOT exist. `cpp11` owns
  `R_init_dafr`. Subsystems that need init-time registration use a
  `[[cpp11::init]]`-decorated helper (see `src/altrep_mmap_r.cpp`).
- `helper-tempfiles.R`'s `new_tempdir(envir = parent.frame())` and
  `new_tempfile(envir = parent.frame())` both accept an `envir` arg.
  When calling from a nested helper, pass `envir = parent.frame()`
  explicitly so cleanup scopes to the `test_that` body.
- Dev repo is a separate nested git repo inside the package repo.
  Plan/note/spec/benchmark commits go to the dev repo
  (`cd ~/src/dafr-native/dev`); source + test commits go to the
  package repo (`cd ~/src/dafr-native`). Infer from file paths, not
  `pwd`.
- Commit to the right repo by inferring from paths: source+tests →
  `cd ~/src/dafr-native`; plans+notes+specs+benches →
  `cd ~/src/dafr-native/dev`.
- Use `/bin/rm` and `/bin/cp` — the shell has `-i` aliases.
- No emojis in code or docs unless explicitly asked.

## Ready-to-paste prompt for the next agent

Copy-paste this when starting the Slice 1 session:

> Start implementing Slice 1 of the native-R `dafr` package:
> MemoryDaf + axes + scalars/vectors/matrices get/set + cache
> infrastructure.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tracking
>   `origin/main` at `git@github.com:tanaylab/dafr.git` (private). Tag
>   `slice-0` marks the Slice 0 exit.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo
>   (gitignored by package repo). Put plans, specs, notes, benchmarks
>   here.
> - Kickoff breadcrumb:
>   `~/src/dafr-native/dev/notes/slice-1-kickoff.md` — read this first,
>   it summarizes Slice 0 deliverables, open risks, conventions, and
>   the Slice 1 scope.
> - Slice 0 exit note:
>   `~/src/dafr-native/dev/notes/slice-0-exit.md`.
> - Slice 0 plan (retrofit-ed twice):
>   `~/src/dafr-native/dev/plans/2026-04-19-slice-0-scaffold-and-poc.md`
>   — read §Self-review at the end for Slice 1 scope boundaries.
> - Existing S7 generics to implement (~40 of them):
>   `R/format_api.R`.
> - Reference test suite for the test-port:
>   `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr/tests/testthat/`
>   (now the renamed `dafJuliaWrapper` package — same tests, new name).
>
> Use `superpowers:writing-plans` first to draft a Slice 1
> implementation plan against the breadcrumb's "What Slice 1 should
> deliver" checklist, then `superpowers:subagent-driven-development`
> to execute it with full two-stage review (spec + quality) per task,
> since Slice 1 is integration-heavy (MemoryDaf binds the whole S7
> API to real storage, cache wires version counters, test-port
> touches the entire data model).
>
> Before writing any plan, scan the open risks in the Slice 1 kickoff
> breadcrumb and decide which ones to close inside Slice 1 vs defer
> to Slice 2.

## Status at session end

- `tanaylab/dafr` (private): `main` at `26dde4d`, tag `slice-0` pushed,
  CI in progress.
- `tanaylab/dafJuliaWrapper` (public, was `tanaylab/dafr`):
  `caching-and-facade` at `0c22abe`, `main` still at pre-rename state.
- Local `~/src/dafr-native/` and
  `/net/mraid20/.../src/dafr`: both clean, nothing to commit.
