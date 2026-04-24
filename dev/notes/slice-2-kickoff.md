# Slice 2 — Kickoff breadcrumb

**Date:** 2026-04-20.
**Predecessor:** Slice 1 (tag `slice-1` on `tanaylab/dafr`, pushed to
GitHub), exit gate at `dev/notes/slice-1-exit.md`.

## What changed between the end of Slice 1 and now

1. **Slice 1 landed.** 34 commits between `slice-0..slice-1` on
   `tanaylab/dafr@main`. Net: +2963 / -19 LoC across 54 files. MemoryDaf
   backend, user-facing read/write API, version-stamp cache with LRU +
   memory cap, ported test coverage. Tag `slice-1` pushed; CI green
   (R-CMD-check 5m12s, altrep-sanity 2m14s). See
   `dev/notes/slice-1-exit.md` for the exit gate deliverables list.
2. **No other side-channel work.** The `slice-1-memory-daf` feature
   branch still exists locally but is merged (fast-forward) into `main`
   — safe to delete with `git branch -d slice-1-memory-daf`.

## Current state (as of this writing)

- **Package repo**: `/home/aviezerl/src/dafr-native/`, branch `main`,
  clean, tracking `origin/main` at `git@github.com:tanaylab/dafr.git`
  (private) at commit `bd8ddc7`. Tag `slice-1` pushed.
- **Dev repo** (nested, gitignored by package repo):
  `/home/aviezerl/src/dafr-native/dev/`, branch `main`, clean, no
  remote. Contains the Slice 1 plan + exit note + this breadcrumb.
- **Test status**: `testthat::test_dir("tests/testthat")` — 22 testthat
  files, 160+ test_that blocks, **470 PASS / 0 FAIL / 0 SKIP / 1
  pre-existing WARN**. The warning is the scran/irlba SVD tolerance
  notice in `test-altrep-downstream.R`, unchanged from Slice 0.
- **Check status**: `devtools::check(error_on = "note")` with
  `_R_CHECK_SYSTEM_CLOCK_=0` — **0 ERROR / 0 WARNING / 0 NOTE**. The
  env-var bypass works around a HTTP 503 from `worldclockapi.com`; the
  package itself is clean. CI in GitHub Actions hits its own network
  and passes without the override.
- **Build status**: `pkgbuild::compile_dll(debug=FALSE)` clean on
  linux-x86_64, macOS, Windows (via CI). C++ sources unchanged in
  Slice 1 apart from the `const_cast<void*>` SAFETY comments added to
  `src/altrep_mmap.cpp` (Task K2).

## Still open from Slice 1 (tracked, non-blocking for Slice 2)

These come from the Slice 1 final code review + the implementer flags:

- **`cache_get` / `cache_put` / `cache_remove`** in `R/cache.R:29-49`
  are vestigial Slice-0 helpers. Production reads/writes now go
  through `cache_store` / `cache_lookup` (stamp + LRU + byte-accounting
  aware). The legacy helpers bypass all three invariants — a future
  backend wiring them in would silently lose correctness. Either
  rewrite `tests/testthat/test-cache.R:27-69` against the new API and
  delete the three helpers, or demote them to a test-only helper file.
  Likely worth handling **early in Slice 2** before the FilesDaf
  backend tempts someone to call them.
- **Arg-name drift: `cols_axis` vs `columns_axis`.** Generics in
  `R/format_api.R:31-42` declare `columns_axis`; method impls in
  `R/memory_daf.R` follow; user-facing wrappers in `R/readers.R` /
  `R/writers.R` and the cache-key builder in `R/cache.R:23` use
  `cols_axis`. Positional dispatch masks the issue, but passing the
  named arg `columns_axis = "gene"` into a user-facing wrapper fails
  with an argument-name error. Pick one — I'd pick `columns_axis` to
  match the Julia parity story — and rename uniformly.
- **`empty_cache(group=, clear=, keep=)` has three mutually-exclusive
  args where two suffice.** `group` is a Slice-0 holdover that
  duplicates `clear`. Drop `group` (ported tests all use `clear`/`keep`
  forms) or document it as an alias.
- **`.memory_matrix_bucket(create = TRUE)` leaks empty bucket envs on
  failed set.** `R/memory_daf.R:306-324` eagerly creates the
  row/col env pair, then the caller validates. If validation throws,
  the new nested env is orphaned. `format_matrices_set(rows, cols)`
  still returns `character(0L)` so no observable bug, but
  `ls(internal$matrices)` leaks. Either validate before
  `create = TRUE` or clean up on catch.
- **`description()` assumes axis names can't contain commas.** Matrix
  rendering at `R/readers.R:324-338` builds `ra,ca` keys and later
  splits on `,`. Today `.assert_name` + `format_add_axis` allow any
  non-NA non-empty string — including commas and colons. If Slice 2's
  FilesDaf reads Julia-produced data where someone stuck a `,` in an
  axis name, `description()` mis-parses. Add a reject-`,`-and-`:` rule
  to `.assert_name` (or rename the internal cache-key/description
  separator to something illegal-in-axis-names).
- **No `@family` / package-level roxygen.** `?memory_daf` doesn't
  link forward to `?add_axis` / `?set_vector` / `?description`. Add
  `@family daf-reader-api` / `@family daf-writer-api` tags and a
  top-level `?dafr` package page. Cheap, navigable.
- **`dafr.omp_threshold` option is declared but unused.** `R/options.R`
  defaults to `10000L`, but both kernels (`src/kernel_eltwise_log_add.cpp`
  and `src/kernel_csc_colsums.cpp`) hardcode their own thresholds
  (10000 and 1000 respectively). Either wire the option into both, or
  rename to a per-kernel threshold. Decide before Slice 3 adds more
  OpenMP-gated kernels.
- **`get_vector(..., default = <axis-length vector>)` recycles via
  `rep()` instead of passing through.** `R/readers.R:177`
  does `rep(default, length(entries))` which correctly expands a
  scalar but wrongly repeats a length-N vector into a length-N*N
  vector for an N-length axis. Julia's DAF accepts a length-N default
  as-is. Align semantics — probably: length-1 → recycle, length-N →
  pass through + name with axis entries, else error.

## Still open from Slice 0 (unchanged, for reference)

- **Phase G2 upstream PR** against `tanaylab/DataAxesFormats.jl` docs
  (FilesDaf on-disk spec) is **not yet opened** — awaiting user
  consent. The draft lives at
  `dev/specs/filesdaf-on-disk-spec-draft.md` with three `[UNCLEAR]`
  markers intended for Oren's review. Slice 2 needs this spec settled
  early, because:
  - Julia FilesDaf has **no on-disk version counters** — all in-memory,
    reset to 0 on open. The native backend should mirror or else
    add version counters to the on-disk format (design question).
  - Julia FilesDaf has **no atomicity model** — direct overwrite, no
    `.tmp` rename, no fsync. Multi-process concurrent writes unsafe.
    Mirror or add atomicity (design question).
- **CSC colSums bake-off** inconclusive at SMALL=1 (100K nnz). OpenMP
  thread-pool startup dominates; re-run at 100M+ nnz before Slice 2
  locks the kernel choice. Scratch package at
  `dev/benchmarks/run-bakeoff.R`.
- **Transpose kernel F3** reopening — Eigen wins by ~51% on CSC→CSR
  transpose at the SMALL size. Decide whether to swap to Eigen for
  this kernel when real transpose usage materializes in Slice 2+.
- **`writeBin(..., size = 8L)` is native-endian.** FilesDaf format spec
  must pin endianness. Julia spec says little-endian per
  `dev/specs/filesdaf-on-disk-spec-draft.md`, so this aligns but needs
  the explicit byte-order write in native code.
- **Long-vector (>2^31)** and **"file truncated while R vector live"**
  ALTREP scenarios still untested. Slice 2's FilesDaf mmap path will
  add opportunities — decide whether to add coverage or defer further.

## What Slice 2 should deliver

Per the top-level plan `dev/plans/2026-04-19-slice-0-scaffold-and-poc.md`
§Decomposition and the Slice 1 exit note: *Slice 2 — `FilesDaf` +
mmap + bidirectional Julia compat; `readBin` fallback path.*

Concretely, Slice 2 must:

1. **FilesDaf class.** Implement the `format_*` S7 methods declared in
   `R/format_api.R` for an on-disk backend. `FilesDaf` is a concrete
   `DafWriter` (or `DafReadOnly` in `"r"` mode). Constructor signature
   `files_daf(path, mode = c("r", "r+", "w", "w+"), name = NULL)`;
   modes mirror `open(2)` semantics.
2. **On-disk format.** Byte-for-byte compatible with Julia's
   `DataAxesFormats.FilesDaf.jl`. Directory layout:
   `<root>/daf.json`, `<root>/scalars/*.json|.txt`,
   `<root>/axes/<name>.txt`, `<root>/vectors/<axis>/<name>.<T>.bin
   + .json`, `<root>/matrices/<rows>/<cols>/<name>_{dense,sparse}.*.bin
   + .json`. See `dev/specs/filesdaf-on-disk-spec-draft.md` for the
   full layout.
3. **Mmap read path via ALTREP** (Slice 0's ALTREP classes are ready
   — see `R/altrep.R` + `src/altrep_mmap.cpp`). `format_get_vector`
   on a FilesDaf reads the `.json` header, mmaps the `.bin`, constructs
   an ALTREP vector. Matrices get `dgCMatrix` with ALTREP `@x` / `@i` /
   `@p` slots for sparse, or `MmapRealAltrep` for dense (via `dim<-`).
4. **`readBin` fallback path.** When `options(dafr.mmap = FALSE)` or
   mmap fails (permissions, platform), eager-read the `.bin` into a
   regular R vector. Same user-facing get_vector/get_matrix return
   semantics.
5. **Write path with atomicity decision.** Given Julia's no-atomicity
   design, decide: (a) mirror (simplest, concurrent-write-unsafe), or
   (b) add `.tmp/` sibling + fsync + atomic rename (safer, divergent
   from Julia). Proposed: **(a) for v1** — match Julia, document the
   constraint, revisit if a user hits it. Matches the deferred
   "no atomicity" finding from Slice 0 G1.
6. **Cache mapped-tier integration.** Mapped-tier entries in
   `cache_env$mapped` hold ALTREP views into mmap regions. Ensure:
   - Version-stamp check still invalidates on write (even though the
     OS page cache doesn't evict).
   - A `FilesDaf` write to disk bumps the version counter so any
     cached ALTREP view becomes stale on next read.
   - The mmap region is released (`shared_ptr` refcount → 0) when the
     cached entry is dropped.
7. **Upstream the on-disk spec.** Open the G2 PR against
   `tanaylab/DataAxesFormats.jl` with the draft at
   `dev/specs/filesdaf-on-disk-spec-draft.md`, after resolving the
   three `[UNCLEAR]` markers with Oren. Target: have the spec merged
   before Slice 2's write path lands so the two packages agree on a
   written contract.
8. **Regression: MemoryDaf ↔ FilesDaf round-trip.** Write a dataset
   to FilesDaf, read it back via MemoryDaf via `copy_all` (not yet
   implemented — build a stub or use manual per-entity copies in
   tests), assert equality. This validates the on-disk spec end-to-end
   against our MemoryDaf contract, which DafJuliaWrapper tests already
   exercise on the Julia side.
9. **Close the "tracked-non-blocking" items above** as drive-by
   fixes. The `cache_get/put/remove` cleanup, the `cols_axis`
   rename, the `description()` separator hardening, and the
   `dafr.omp_threshold` wiring are all natural fits for early Slice 2
   commits — do them before the FilesDaf work piles on top.

## Known mines laid in Slice 1 for Slice 2

- **`matrix_version_counter` keys** use the format `rows:cols:name`.
  If axis names contain `:`, key collisions are possible. `.assert_name`
  currently allows any non-NA non-empty string. Addressed by the
  axis-name character class fix listed above.
- **`get_matrix` cache key uses the STORED orientation**, not the
  requested one. `get_matrix(d, "cell", "gene", ...)` (primary) and
  `get_matrix(d, "gene", "cell", ...)` (flipped, transposed from the
  same stored data) share the entry keyed as
  `matrix:cell:gene:UMIs`. The flipped path transposes AFTER lookup
  and applies dimnames fresh. Tests in
  `tests/testthat/test-memory-matrices.R` pin this — don't break it
  when adding FilesDaf's own matrix retrieval.
- **`@Dimnames <- list(NULL, NULL)`** in `format_set_matrix` strips
  dimnames on sparse inputs via direct slot assignment. Verified
  empirically to be S4 copy-on-modify safe. FilesDaf's read path must
  likewise strip/re-apply dimnames through `@Dimnames` not
  `dimnames(mat) <-`, since the latter would go through `Matrix`'s
  validity checker and may allocate.
- **`.memory_axis_vectors(create = TRUE/FALSE)`** idiom is now
  established. FilesDaf should adopt the same idiom for its own
  per-axis-or-axis-pair bucket resolution, both for code reuse and to
  avoid bucket leaks (see open item above).
- **`.validate_vector_value` and `.validate_matrix_value`** are
  MemoryDaf-internal helpers in `R/memory_daf.R`. Their NULL /
  atomic / length / dim validation logic is generic — FilesDaf's set
  methods will want the same checks. Consider hoisting to a shared
  helper module in Slice 2.
- **`dafr.verbose = TRUE`** gates `.cli_verbose` messages for
  `empty_cache` and `add_axis`. FilesDaf's `open` / `close` / `write`
  entry points are natural additions for the same pattern.

## Repo conventions worth re-stating

- **S7 multi-dispatch needs `list(...)` signature.** The bare-class
  form `S7::method(generic, MemoryDaf)` only works for
  single-dispatch generics (those declared with a single dispatch
  arg). Multi-arg generics like `format_set_vector(daf, axis, name,
  vec, overwrite)` need `list(MemoryDaf, S7::class_character,
  S7::class_character, S7::class_any, S7::class_logical)`. The Slice 1
  plan's code blocks got this wrong; tasks corrected it on the fly.
- **`S7::class_any`** is the universal base for values that can be
  any type (e.g., scalar / matrix payloads). Use when dispatch
  shouldn't reject anything — validation is then the caller's job.
- **`.assert_name(value, arg)` + `.assert_flag(value, arg)`** in
  `R/utils.R` are the shared arg-guards for user-facing wrappers.
  Slice 2 should add `.assert_mode(value, arg)` (or similar) when
  FilesDaf's `mode = "r"|"r+"|"w"|"w+"` enum shows up.
- **`sort(..., method = "radix")`** for all listing returns to keep
  results locale-independent across platforms.
- **Native headers use `.h`, not `.hpp`** — CRAN preference.
- **`src/init.cpp` does NOT exist and must NOT exist.** cpp11 owns
  `R_init_dafr`. Subsystems that need init-time registration use a
  `[[cpp11::init]]`-decorated helper (see `src/altrep_mmap_r.cpp`).
- **`helper-tempfiles.R`'s `new_tempdir(envir = parent.frame())` and
  `new_tempfile(envir = parent.frame())`** both accept an `envir`
  arg. From a nested helper, pass `envir = parent.frame()` explicitly
  so cleanup scopes to the `test_that` body.
- **Dev repo is a separate nested git repo inside the package repo.**
  Plan/note/spec/benchmark commits go to the dev repo
  (`cd ~/src/dafr-native/dev`); source + test commits go to the
  package repo (`cd ~/src/dafr-native`). Infer from file paths, not
  `pwd`.
- **Use `/bin/rm` and `/bin/cp`** — the shell has `-i` aliases.
- **No emojis in code or docs** unless explicitly asked.

## Ready-to-paste prompt for the next agent

Copy-paste this when starting the Slice 2 session:

> Start implementing Slice 2 of the native-R `dafr` package:
> FilesDaf backend + mmap read path + readBin fallback + write path
> bidirectionally compatible with Julia's `DataAxesFormats.FilesDaf`.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tracking
>   `origin/main` at `git@github.com:tanaylab/dafr.git` (private). Tag
>   `slice-1` marks the Slice 1 exit.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo
>   (gitignored by package repo). Put plans, specs, notes, benchmarks
>   here.
> - Kickoff breadcrumb:
>   `~/src/dafr-native/dev/notes/slice-2-kickoff.md` — read this
>   first, it summarizes Slice 1 deliverables, open risks, conventions,
>   and the Slice 2 scope.
> - Slice 1 exit note:
>   `~/src/dafr-native/dev/notes/slice-1-exit.md`.
> - Slice 1 plan (fully executed):
>   `~/src/dafr-native/dev/plans/2026-04-20-slice-1-memory-daf.md`.
> - FilesDaf on-disk spec draft (G1 deliverable, not yet upstreamed):
>   `~/src/dafr-native/dev/specs/filesdaf-on-disk-spec-draft.md` —
>   three `[UNCLEAR]` markers intended for Oren's review. Settle
>   those before writing code.
> - Existing S7 generics to implement (22 of them):
>   `R/format_api.R`. MemoryDaf's implementations in
>   `R/memory_daf.R` are the reference shape.
> - Shared ALTREP classes + `MmapRegion` RAII are ready from Slice 0
>   (see `src/altrep_mmap.cpp` / `src/mmap_region.cpp`,
>   `R/altrep.R` / `R/mmap.R`).
> - User-facing API (`get_vector`, `set_matrix`, `add_axis`, ...)
>   lands in `R/readers.R` / `R/writers.R` and already dispatches
>   through the `format_*` generics — FilesDaf "just needs" the
>   backend methods, plus a `files_daf()` constructor.
>
> Use `superpowers:writing-plans` first to draft a Slice 2
> implementation plan against the breadcrumb's "What Slice 2 should
> deliver" checklist, then `superpowers:subagent-driven-development`
> to execute it with full two-stage review (spec + quality) per task.
>
> Before writing any plan, (a) scan the open risks in the Slice 2
> kickoff breadcrumb, (b) decide which of the Slice-1 tracked-
> non-blocking items to close as drive-by work early in the slice,
> and (c) settle the FilesDaf atomicity-vs-mirror design question.
> The breadcrumb recommends mirroring Julia for v1.

## Status at session end

- `tanaylab/dafr` (private): `main` at `bd8ddc7`, tag `slice-1`
  pushed. CI green (R-CMD-check 5m12s, altrep-sanity 2m14s).
- Local `~/src/dafr-native/`: `main` at `bd8ddc7`, clean. Feature
  branch `slice-1-memory-daf` merged (fast-forward) into `main`;
  safe to delete locally with `git branch -d slice-1-memory-daf`.
- Local `~/src/dafr-native/dev/`: `main` clean with Slice 1 plan +
  exit note + this breadcrumb committed.
- Pre-existing scratch benchmark CSVs in `dev/benchmarks/` are
  untracked — same state as end of Slice 0, untouched by Slice 1.
