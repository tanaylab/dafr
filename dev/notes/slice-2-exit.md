# Slice 2 exit gate — 2026-04-20

## Deliverables

- [x] `FilesDaf` S7 class + `files_daf(path, mode, name)` constructor with
  mode handling (`"r"`, `"r+"`, `"w"`, `"w+"`) and `FilesDafReadOnly` variant
  under `DafReadOnly` (Phase B1/B2).
- [x] Read-only guards on all nine mutating `format_*` generics for
  `FilesDafReadOnly` (B2). Mutation verbs raise
  `"files_daf: store opened read-only; <verb> not permitted"` instead of
  falling through to S7's "method not found".
- [x] I/O helper module `R/files_io.R` — dtype table (R type ↔ Julia
  type string ↔ byte size), path helpers, descriptor read/write, scalar
  JSON read/write, binary read/write for dense + sparse slots, adaptive
  sparsify heuristic helpers (Phases C1/C2/C3).
- [x] `format_*` methods on `FilesDaf` for scalars (D1), axes (E1/E2 with
  delete-cascade), dense/sparse vectors (F1–F5 + G1 with adaptive
  sparsify), dense/sparse matrices (H1–H4, I1–I3 including relayout).
- [x] ALTREP mmap read path for `Float64` + `Int32` dense payloads,
  `readBin` fallback when `options(dafr.mmap = FALSE)` or for
  `Bool`/`String`/`Int64`/`UInt64` (F2, H2, J2).
- [x] Mapped-tier cache integration with version-stamp invalidation on
  writes — `format_get_vector` and `format_get_matrix` hit the `mapped`
  bucket on repeat read; write bumps counter → stamp mismatch → re-open
  (J1).
- [x] Adaptive sparsify on set_vector — numeric / Bool / string heuristics
  match Julia's spec §8 / §8.4 byte thresholds. `Matrix::sparseVector` input
  writes sparse unconditionally (matches Julia's type-driven behaviour).
  Bool all-TRUE omits `.nzval` (§8.3) (F5).
- [x] Sparse CSC matrix read/write with 1-based ↔ 0-based index conversion
  between on-disk `colptr`/`rowval` and `dgCMatrix@p`/`@i`. `lgCMatrix`
  support with all-TRUE `.nzval` elision. (I1, I2).
- [x] `format_relayout_matrix` on FilesDaf — materialises the transpose at
  the flipped axis pair; delete_axis cascade now covers both orientations
  (I3).
- [x] **Julia bidirectional compatibility** — Julia-written FilesDaf
  fixture committed at `tests/testthat/fixtures/julia-filesdaf/`
  (regenerated via `dev/scripts/regen-julia-fixture.jl` run under
  `conda run -n dafr-mcview julia`); static read tests and manual-copy
  round-trip tests always run; live `R → Julia → R` tests gated on
  `.have_julia_env()` and currently green locally (K1/K2/K3).
- [x] **Drive-by Phase A cleanups** — `cols_axis → columns_axis` rename
  across user-facing wrappers and helpers (A1); `.assert_name` hardened
  to reject `/ \ : , \n \r \t \0` and leading/trailing whitespace (A2);
  vestigial `cache_get/put/remove` removed, `empty_cache(group=)` alias
  dropped (A3/A4); `get_vector(default=)` now accepts length-N vector
  (Julia parity) (A5); regression guard added for
  `.memory_matrix_bucket` bucket-leak invariant (A6, confirmed already
  fixed in Slice 1).
- [x] `.validate_vector_value` and `.assert_scalar_value` hoisted from
  `R/memory_daf.R` to `R/utils.R` so both backends reuse them
  (D1, F3).
- [x] **Interop bug caught by live K3 round-trip**: `format_add_axis` on
  FilesDaf now eagerly creates `vectors/<axis>/`, `matrices/<axis>/<other>/`,
  and `matrices/<other>/<axis>/` skeleton directories. Julia FilesDaf's
  reader calls `readdir` on these during `vectors_set`/`matrices_set` and
  errors on missing directories. Without the eager creation, R-written
  stores failed Julia reads unless at least one vector/matrix existed per
  axis-pair.
- [x] On-disk spec draft (`dev/specs/filesdaf-on-disk-spec-draft.md`) —
  all three `[UNCLEAR]` markers resolved inline (Float32 JSON precision,
  single-writer atomicity contract, `Int`/`int` legacy alias) (L1).

## Test + build status

- `testthat::test_dir("tests/testthat")` — **707 PASS / 0 FAIL / 0 SKIP /
  1 WARN** (Julia round-trip actually runs on this machine). The single
  warning is the pre-existing `scran::quickCluster` / `irlba::irlba` SVD
  tolerance notice in `test-altrep-downstream.R`, unchanged since
  Slice 0. From the Slice 1 baseline of 470 tests this is +237 tests
  covering the FilesDaf surface, the drive-by cleanup regression guards,
  and Julia compat.
- `devtools::check(error_on = "note")` with `_R_CHECK_SYSTEM_CLOCK_=0` —
  **0 ERROR / 0 WARNING / 0 NOTE**. Duration 1m 21s. The env-var bypass
  is for the `worldclockapi.com` 503 (unrelated to the package).
- `pkgbuild::compile_dll(debug = FALSE)` — clean on linux-x86_64. C++
  sources unchanged in Slice 2.

## Bake-off / perf items

No new benchmarks this slice — Slice 2 delivers the on-disk backend and
round-trip correctness, not kernel bake-offs. All outstanding bake-off
items from Slice 0/1 remain deferred:

- CSC colSums bake-off re-run at 100M+ nnz (still awaits a larger dataset).
- Transpose kernel B-vs-D decision (no real-world consumer yet).

## Scope closed vs deferred

**Closed in Slice 2** (from the Slice 1 kickoff "Still open" list):

- `cache_get/put/remove` vestigial helpers — deleted (A3).
- `cols_axis → columns_axis` drift — renamed uniformly (A1).
- `empty_cache(group=)` alias — dropped (A4).
- `get_vector(default = <axis-length vector>)` — now passes through (A5).
- `.memory_matrix_bucket(create = TRUE)` leak — regression test added
  (bug already fixed in Slice 1, invariant now pinned) (A6).
- `.assert_name` character-class hardening (also closes the
  `description()`-separator concern from the Slice 1 kickoff) (A2).
- Julia FilesDaf on-disk format documented and implemented with
  bidirectional round-trip coverage (K1/K2/K3).

**Deferred to Slice 3+**:

- **L2 upstream PR against `tanaylab/DataAxesFormats.jl` docs** — skipped
  at user request on 2026-04-20. The spec draft is fully resolved at
  `dev/specs/filesdaf-on-disk-spec-draft.md` and ready to copy into the
  Julia repo whenever a future slice opens the PR.
- `@family` / top-level package roxygen navigation (deferred from Slice 1).
- `dafr.omp_threshold` wiring into C++ kernels (deferred from Slice 1).
- Long-vector (>2^31) and "file truncated while R vector live" ALTREP
  scenarios (deferred from Slice 0).
- `copy_all(src, dst)` end-to-end helper — currently implemented as a
  local test helper `.copy_all_memory_to_files` in
  `test-files-julia-compat.R`. Promote to package API when a real
  consumer appears (likely Slice 4 query layer).
- Multi-writer filesystem locking on FilesDaf root directory. Current
  design mirrors Julia's "single writer, no atomicity" contract; revisit
  only if a user hits the limitation.
- The `UInt32` read arm of `.read_bin_dense` is signed-int32-under-the-hood,
  so indices >= 2^31 come back as negative integers. `.indtype_for_size`
  routes oversized axes to `UInt64` so the pathological case only arises
  when an externally-written fixture uses `UInt32` with values beyond R's
  range. Hardening deferred until a real consumer hits it.

## Commit history

Slice 2 landed as 34 commits on branch `slice-2-files-daf` (off `main` at
tag `slice-1`). Phase A shipped 6 small refactor/test commits; Phase B-C
set up the class + I/O scaffolding; Phases D-I implemented the 22
`format_*` methods (read + write, dense + sparse, vectors + matrices +
scalars + axes); Phase J wired the mapped-tier cache; Phase K generated
the Julia fixture and added both fixture-based and live round-trip
coverage; Phase L resolved the spec. Each task ran through spec-compliance
and code-quality reviewers; a few surfaced legitimate bug-fix follow-ups
(A6 bug already fixed in Slice 1 — converted to regression test; F3
noticed an NA bug in `.should_sparsify_numeric` fixed in F5; K3 caught the
Julia-interop bug in `format_add_axis` directory scaffolding).

Net diff vs `slice-1`: **+2664 / -201** across 70 files.

## Known mines laid in Slice 2 for Slice 3

- **`.read_bin_dense` UInt32 arm is signed.** Values ≥ 2^31 read back as
  negative int32 (no error). The indtype selector routes oversized axes
  to UInt64 so the bug surfaces only when reading externally-written
  stores with oversized UInt32 indices. If a fixture with this property
  shows up, the read path needs a coerce-through-double hop (R has no
  native unsigned int).
- **String dense / sparse matrix paths are unexercised** beyond the single
  unit test. Julia's writer doesn't emit string matrices in the fixture.
  The code path lights up if a future consumer calls
  `set_matrix(d, ..., <character matrix>)` on FilesDaf; tests exist but
  only cover the trivial case.
- **The `mapped` tier is now load-bearing for FilesDaf read latency.** A
  user who clears the mapped tier (`empty_cache(clear = "mapped")`) or
  triggers a write that bumps the counter forces a full mmap re-open on
  the next read. This is cheap (mmap is O(1) and the OS page cache is
  warm) but still measurable on NVMe for multi-GB payloads. The `query`
  tier is still empty in this slice (Slice 4 fills it).
- **`format_add_axis` eager-creates `vectors/<axis>/` and all
  `matrices/<axis|...>/<axis|...>/` permutations.** This is O(A) per
  `add_axis` call, O(A²) cumulative. Fine for A ≪ 100 (single-cell
  atlas scale); document the cost if a user ever needs thousands of
  axes. Symmetric delete cascade in `format_delete_axis`.
- **No persisted version counters.** Matches Julia. Any cache built
  across process restarts (not in v1 scope) would need to revisit this.

## Repo conventions reinforced in Slice 2

- **`@include` directives** are load-bearing when S7 methods are
  registered in files that sort before their generics' definitions.
  `R/files_daf.R`, `R/files_daf_read.R`, and `R/files_daf_write.R` all
  carry `@include format_api.R files_daf.R files_io.R utils.R` tags. New
  files registering methods should follow the pattern.
- **Descriptor JSON writes use `cat(sprintf(...))` not `toJSON`** to
  preserve Julia's byte-exact key order. Keys: `format`, `eltype`,
  (optional) `indtype`.
- **On-disk integer indices are 1-based** (Julia convention). Conversion
  to R's 0-based `dgCMatrix@p`/`@i` happens at read; `+ 1L` at write.
- **Endianness: explicit `endian = "little"`** on every `writeBin` /
  `readBin` call, even though most platforms are little-endian natively.
- **Parameter naming**: user-facing `columns_axis` everywhere. Internal
  helpers also use `columns_axis` for symmetry (no `cols_axis` remaining
  in R/ or tests/).
- **Test file layout**: `tests/testthat/test-files-<subject>.R` for
  FilesDaf-specific coverage; `test-files-julia-compat.R` for round-trip;
  `helper-julia.R` holds `run_julia`, `.have_julia_env`, `fixture_path`.
- **Fixture regeneration script** lives in the dev repo
  (`dev/scripts/regen-julia-fixture.jl`), not the package repo, because
  Julia is a dev dependency only.

## Ready-to-paste prompt for the next agent

Copy-paste this when starting the Slice 3 session:

> Start implementing Slice 3 of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tracking
>   `origin/main` at `git@github.com:tanaylab/dafr.git` (private). Tag
>   `slice-2` marks the Slice 2 exit.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Kickoff breadcrumb: (create new) `dev/notes/slice-3-kickoff.md` —
>   start from this slice-2 exit note.
> - Slice 2 plan (fully executed):
>   `~/src/dafr-native/dev/plans/2026-04-20-slice-2-files-daf.md`.
> - Slice 2 exit note: `~/src/dafr-native/dev/notes/slice-2-exit.md`.
>
> Slice 3 scope (per the top-level plan): query DSL + views + chains +
> contracts. FilesDaf + MemoryDaf are both production-ready; Slice 3 adds
> the read-side composition layer (Julia `QueryData` + `ViewDaf` +
> `ChainDaf`).
>
> Use `superpowers:writing-plans` first to draft a Slice 3 plan, then
> `superpowers:subagent-driven-development` to execute it with full
> two-stage review per task.

## Status at session end

- `tanaylab/dafr` (private): `main` pushed to `96c3bdd`; tag `slice-2`
  moved to `96c3bdd` (CI-green) after a follow-up fix to
  `helper-julia.R`. Original merge at `7529fe4` failed CI on mac +
  windows because `.have_julia_env()` crashed when `conda` was missing
  from the runner PATH; the helper now guards with `Sys.which("conda")`
  + `tryCatch`, falling through to `skip_if_not(...)`. CI on all three
  OSes: **success** after the fix.
- Local `~/src/dafr-native/`: `main` at the slice-2 merge commit, tag
  `slice-2` present, feature branch `slice-2-files-daf` merged
  fast-forward (safe to delete with `git branch -d slice-2-files-daf`).
- Local `~/src/dafr-native/dev/`: `main` clean with Slice 2 plan + exit
  note + Julia fixture regeneration script committed.
- L2 upstream PR (`tanaylab/DataAxesFormats.jl` docs) skipped at user
  request; spec draft at `dev/specs/filesdaf-on-disk-spec-draft.md` is
  fully resolved and ready to submit when Slice 3+ reopens it.
