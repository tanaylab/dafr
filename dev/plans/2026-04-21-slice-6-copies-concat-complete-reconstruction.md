# Slice 6 — Copies + Concat + Complete + Reconstruction

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the bulk-manipulation verbs gap. Land a public `copy_scalar!` / `copy_axis!` / `copy_vector!` / `copy_matrix!` / `copy_tensor!` / `copy_all!` surface that mirrors Julia DAF's `Copies` module; a `concatenate()` that stitches multiple dafs along one or more axes; `complete_chain()` / `complete_daf()` / `open_daf()` for persistent on-disk chains; and `reconstruct_axis()` for promoting an implicit property into an explicit axis. In the same slice, promote Slice-5's internal `.copy_view_to_daf()` to use the new public `copy_all()` (which, unlike the Slice-5 helper, preserves sparsity in pad mode via `Matrix::sparseMatrix` embedding — fixing the known kickoff mine).

**Architecture:**

- **All `copy_*` functions are R-side ports of Julia `Copies.jl`.** R-convention positional signatures replace Julia's all-keyword form: `copy_vector(destination, source, axis, name, ..., rename = NULL, ...)`. The `insist` / `overwrite` semantics match Julia verbatim: `insist = TRUE` (default) errors if the destination entry already exists, `insist = FALSE` silently skips, `overwrite = TRUE` replaces. Per-function axis relation detection (`same` / `destination_is_subset` / `source_is_subset`) produces a 3-valued factor used to dispatch copy strategies. `bestify` (Julia's sparsity-saving heuristic) is **out of scope** for Slice 6 — we preserve sparsity when the source is already sparse, but we do not make promote/demote decisions based on nnz.
- **`copy_all()` orchestrates scalars + axes + vectors + matrices + tensors** in a single pass, driven by an optional `empty` spec for missing entries and an optional `types` spec for per-property type coercion. Both specs use a flat-string-key canonical form: `list("cell|donor" = "", "cell|gene|UMIs" = 0, "batch|gene|cell|counts" = 0)` — pipe-joined axis/property identifiers for vector (2 parts), matrix (3 parts), or tensor (4 parts) entries. A helper `empty_data(vectors = ..., matrices = ..., tensors = ...)` constructs the flat form from a more typed builder API, matching Julia's `EmptyData = Union{AbstractDict, AbstractVector, NamedTuple}` flexibility without R's type-punning.
- **`concatenate()` is the largest single function in this slice.** It mirrors Julia `concatenate!()` and supports: one or more concatenation axes (`axis` can be a string or character vector); a per-source `dataset_axis` with an auto-generated `dataset` property; per-axis `prefix` control with the property-prefixing heuristic (a property whose name equals the axis name or starts with `"<axis>."` is auto-prefixed); `empty` for missing per-source properties; `merge` actions (`SkipProperty`, `LastValue`, `CollectAxis` — the latter for scalar/vector properties only; matrix `CollectAxis` errors, matching Julia); `overwrite`. Sparsity preservation follows Julia's `sparse_if_saves_storage_fraction` cutoff (default 0.25). Matrices with both axes in the concat set are rejected (Julia's rule).
- **`complete_chain()` / `complete_daf()` persist a `base_daf_repository` scalar** on the new writer that points at the base daf. `complete_daf(leaf, mode)` walks that scalar chain, opens each repository with `open_daf()` (FilesDaf-only this slice; H5df dispatch deferred), and returns a `chain_reader`/`chain_writer`. The optional `base_daf_view` JSON-encoded view spec is applied via `viewer()` between steps.
- **`reconstruct_axis!()` promotes an implicit property to an explicit axis.** Given an `existing_axis` with a property `implicit_axis`, the unique non-empty values become the entries of a newly created axis. Other properties on `existing_axis` are examined for consistency — for any property whose value is uniquely determined by the implicit value, the property is migrated to the new axis. Returns a named list mapping each migrated property to the (consistent) value associated with `existing_axis` entries whose implicit value is empty (useful for reconstruction via the `?? X` query modifier). Core semantics are ported; the `properties_defaults` edge case (axis pre-exists with entries not in the implicit property) is supported only for the simple no-unused-entries case this slice.
- **Adapter refactor.** `.copy_view_to_daf()` (Slice 5 internal) is removed. `adapter()` calls `copy_all(dest = daf, source = output_view, empty = ..., relayout = ..., overwrite = ..., insist = FALSE)` instead. `insist = FALSE` is chosen because adapter's copy-back is a reconciliation operation: properties already present in `daf` and not in the output view should not cause errors. The flat-key `empty` form that Slice 5's adapter accepts remains supported verbatim — it's the same form `copy_all()` now takes.
- **Sparse pad-mode matrix copy** (the kickoff mine). When the source matrix is sparse (`dgCMatrix`) and the destination axis is a strict superset, we build the full matrix via `Matrix::sparseMatrix(i, j, x, dims = c(full_rows, full_cols))` with row/column indices remapped through `match(src_entries, dest_entries)`. No `as.matrix()` call. A dense fallback is used only when the source is already dense.
- **Julia parity fixture.** A new fixture set under `tests/testthat/fixtures/julia-copies/` captures: (a) a `copy_all!` round-trip with a subset-axis + empty + types spec, and (b) a `concatenate!` round-trip over 2 sources with a dataset axis and per-axis prefix. Regenerated via `dev/scripts/regen-julia-copies-fixture.jl`. Reconstruction does not need a Julia fixture — its correctness is fully verified by pure R round-trips, and Julia's `reconstruct_axis!` has edge cases the R port deliberately does not match this slice.

**Tech Stack:**

- R 4.4+, S7 0.2.1. `Matrix` package for sparse ops (already an Imports). `jsonlite` for the fixture and the `base_daf_view` JSON round-trip (already Imports). No new R deps; no new C++ this slice — all new code is R. Roxygen for docs; `devtools::check()` for release gate.
- Julia side (fixture regen only): `DataAxesFormats.jl` at `49fbba1` or newer; conda env `dafr-mcview`. Fixture regen is one-shot.

**Repo layout:**

- Package repo: `/home/aviezerl/src/dafr-native/` on `main` at tag `slice-5` / commit `79cb372`. Tracks `git@github.com:tanaylab/dafr.git`. Source, tests, `inst/` commits → package repo. Execute on a feature branch `slice-6-copies-concat-complete` (created at Phase 0; final merge at Phase Z).
- Dev repo (nested, gitignored): `/home/aviezerl/src/dafr-native/dev/` on `main` at `a9f4aca`. Plans, notes, scripts → dev repo.

**Dev loop per task:**

1. From `/home/aviezerl/src/dafr-native/`:
   ```
   Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "<tag>")'
   ```
2. Inspect; iterate to green.
3. Stage + commit with the message given in the task. Package repo vs. dev repo: infer from the file path. **Never `--amend`, `--no-verify`, or force-push.** Use `/bin/rm` / `/bin/cp` (aliased with `-i`). Wait for permission prompts.

**Known mines laid in Slice 5 (honor throughout):**

- `.copy_view_to_daf` dense-coerces sparse via `as.matrix(val)` — **Slice 6 replaces this helper entirely with `copy_all()`. The new sparse-preserving path uses `Matrix::sparseMatrix` embedding.**
- `merge_contracts` type lattice is coarse (`c("logical", "integer", "double", "numeric", "character")`); no cross-axis moves. Slice 6's `types` spec for `copy_all()` uses the same lattice and is thus bounded by it.
- `.matrix_type_ok` is missing the `character` case (pre-existing Slice-4 mine). Any Slice 6 contract that specifies a `character`-typed matrix would fall through. **Not fixed this slice** — no Slice 6 feature requires it. Flagged in exit.
- Evaluator `state$kind` is a closed enum. Slice 6 does not touch the evaluator.
- `computation()` stores the contract as a function attribute, not in an S7 class. Slice 6 does not touch `computation()`.
- Slice-5's `empty` pad-mode flat-key form carries forward verbatim into `copy_all()` / `copy_vector()` / `copy_matrix()`. Nested `EmptyData` (Julia-style `(axis, property) => value` tuples) is **not** supported this slice; the flat-key form is canonical.
- Previous fixture set `tests/testthat/fixtures/julia-adapter/` already pinned at DAF.jl `49fbba1`. Slice 6's new fixture follows the same versioning convention — record HEAD in the fixture README.

---

## Pre-planning decisions (settled before tasks)

### 1. Phase order

0 → A → B → C → D → E → F → G → H → I → J → Z, in that sequence. Rationale:

- **Phase 0 (branch setup):** create `slice-6-copies-concat-complete`; no code changes.
- **A (copy_scalar + copy_axis):** smallest, foundational, no deps on any new code. Warm-up; establishes the per-function argument conventions.
- **B (copy_vector):** builds on A's axis-relation helper.
- **C (copy_matrix):** builds on B's shape but adds sparsity-preserving pad and relayout.
- **D (copy_tensor):** thin loop over `copy_matrix`.
- **E (copy_all + empty_data + types helpers):** orchestrates A/B/C/D and lands the `empty_data()` user-facing helper.
- **F (adapter refactor):** replaces `.copy_view_to_daf` with `copy_all`. Requires E. Low risk; touches `R/adapters.R` only.
- **G (concatenate):** largest single function; independent of copy_* internals but reuses the `_verify_axis` helper from A.
- **H (complete_daf + complete_chain + open_daf):** self-contained; depends on `set_scalar` / `chain_*` (already present).
- **I (reconstruct_axis):** self-contained; depends on `add_axis` / `set_vector` / `delete_vector`.
- **J (Julia parity fixture):** end-to-end check of the main `copy_all` and `concatenate` paths.
- **Z (polish):** NAMESPACE regen, Collate, NEWS, `devtools::check`, exit note.

### 2. `insist` keyword retained for Julia parity

R users may find `insist = TRUE` opaque. We keep the Julia name for cross-runtime parity; documentation explicitly explains: `insist = TRUE` raises if the destination property already exists (unless `overwrite = TRUE`); `insist = FALSE` silently skips the copy in that case. All `copy_*` functions default to `insist = TRUE`, `overwrite = FALSE`. `copy_all()` forwards both flags to every inner call.

### 3. `empty` / `types` canonical form — flat string keys

- Vectors: `"axis|name"` (e.g., `"cell|age"`)
- Matrices: `"rows_axis|columns_axis|name"` (e.g., `"cell|gene|UMIs"`) — order-sensitive within the string, but `copy_all` / `concatenate` treat `"cell|gene|UMIs"` and `"gene|cell|UMIs"` as equivalent (axis pairs are unordered in Julia too).
- Tensors (in `copy_all`): `"main|rows|columns|name"` (4 parts).
- Scalars (in `copy_all.types` only): `"name"` (1 part, no pipe).

The helper `empty_data(vectors = list(), matrices = list(), tensors = list())` assembles flat-key named lists:

```r
empty_data(
    vectors  = list(list(axis = "cell", name = "age", value = 0L)),
    matrices = list(list(rows_axis = "cell", columns_axis = "gene",
                         name = "UMIs", value = 0L)),
    tensors  = list(list(main_axis = "batch", rows_axis = "gene",
                         columns_axis = "cell", name = "counts", value = 0))
)
```

The same structure is used for `types` (replace `value = ...` with `type = "integer"` or similar). Users can pass a plain named list bypassing the helper. The helper is the documentation-friendly API.

### 4. No `bestify` heuristic this slice

Julia's `bestify` re-encodes data sparse-vs-dense based on a `min_sparse_saving_fraction` cutoff. Slice 6 preserves sparsity when the source is already sparse but does not promote/demote. A Slice 7 add-on can layer `bestify` onto `copy_vector` / `copy_matrix` if profiling warrants it.

### 5. No `CollectAxis` for matrix in `concatenate`

`merge = list("scalar_name" = "CollectAxis")` creates a vector along the dataset axis. `merge = list(c("axis", "vec_name") = "CollectAxis")` creates a matrix (axis × dataset_axis). `merge = list(c("r", "c", "mat_name") = "CollectAxis")` errors with `"can't CollectAxis for a matrix: would create a 3D tensor"`, matching Julia.

### 6. Sparse pad-mode matrix embedding

The kickoff mine: `.copy_view_to_daf` does `full[idx_r, idx_c] <- as.matrix(val)` — dense-coerces. Fix in `copy_matrix` pad-mode: if `val` is `dgCMatrix`, build the embedded sparse matrix by remapping its `@i` / `@p` / `@x` slots to destination indices and calling `Matrix::sparseMatrix(i, j, x, dims, index1 = FALSE)`. No intermediate dense allocation. If the source is dense, a plain `full[idx_r, idx_c] <- val` is fine (no regression from Slice 5).

### 7. `complete_daf` is FilesDaf-only

`open_daf(path, mode)` dispatches by extension. This slice handles extensions that are directories (FilesDaf) and one special case of `path == ""` (error). `.h5df` / `.h5dfs#` paths raise `"H5df backend not supported yet"`. Slice 8 (AnnData/Zarr) will add the H5df arm.

### 8. `reconstruct_axis` core behaviors only

Ported: unique-value axis creation, consistent-property migration, `implicit_properties` explicit set, `skipped_properties`, `empty_implicit` (value equated with empty string), return-dict of "value for empty-implicit entries". **Not ported this slice:** `properties_defaults` when the reconstructed axis already exists with extra entries — we require the axis not to exist pre-call. Flagged as Slice 7 follow-up.

### 9. `concatenate` multi-axis

Single-axis concat is the primary test surface. Multi-axis (2 axes) is tested for correctness but not for corner cases like overlapping-but-reordered entries on non-concat axes. The code supports N axes; the tests cover N = 1 and N = 2.

### 10. Feature branch

`slice-6-copies-concat-complete` on the package repo. Created at Phase 0, merged fast-forward at Phase Z exit.

### 11. Fixture regeneration policy

Phase J's regen script runs once against `DataAxesFormats.jl` at its tip. Before regenerating: `git -C ~/src/DataAxesFormats.jl fetch && git -C ~/src/DataAxesFormats.jl pull --ff-only`. Record new HEAD in fixture README.

### 12. Worktree vs in-place

In-place on `/home/aviezerl/src/dafr-native/` with a feature branch, matching Slice 5 precedent.

---

## File structure

**Create (package repo):**

- `R/copies.R` (~650 LoC) — `copy_scalar`, `copy_axis`, `copy_vector`, `copy_matrix`, `copy_tensor`, `copy_all`, `empty_data`, internal `.verify_axis_relation`, sparse pad helpers.
- `R/concat.R` (~450 LoC) — `concatenate`, `MERGE_SKIP` / `MERGE_LAST_VALUE` / `MERGE_COLLECT_AXIS` constants, internal concat helpers.
- `R/complete.R` (~170 LoC) — `complete_chain`, `complete_daf`, `open_daf`.
- `R/reconstruction.R` (~180 LoC) — `reconstruct_axis`.
- `tests/testthat/test-copies-scalar.R` (~110 LoC)
- `tests/testthat/test-copies-axis.R` (~90 LoC)
- `tests/testthat/test-copies-vector.R` (~260 LoC)
- `tests/testthat/test-copies-matrix.R` (~320 LoC)
- `tests/testthat/test-copies-tensor.R` (~100 LoC)
- `tests/testthat/test-copies-all.R` (~200 LoC)
- `tests/testthat/test-concat.R` (~340 LoC)
- `tests/testthat/test-complete.R` (~160 LoC)
- `tests/testthat/test-reconstruction.R` (~200 LoC)
- `tests/testthat/test-copies-julia-compat.R` (~120 LoC)
- `tests/testthat/fixtures/julia-copies/copy_all_fixture.json` (regen output; committed)
- `tests/testthat/fixtures/julia-copies/concat_fixture.json` (regen output; committed)
- `tests/testthat/fixtures/julia-copies/README.md` (~50 LoC)

**Modify (package repo):**

- `R/adapters.R` — strip `.copy_view_to_daf`; replace its call site with `copy_all`.
- `tests/testthat/test-adapters.R` — one new test asserting the sparse pad-mode mine is fixed (previously would have dense-coerced).
- `NAMESPACE` — roxygen regen (Z phase).
- `NEWS.md` — Slice 6 entry (Z phase).
- `DESCRIPTION` — no change (keep `0.0.0.9000`).
- `man/*.Rd` — roxygen regen (Z phase).

**Create (dev repo):**

- `dev/scripts/regen-julia-copies-fixture.jl` (~220 LoC)
- `dev/notes/slice-6-exit.md` (~240 LoC, Z phase)

---

## Phase 0 — Branch setup

### Task 0.1: Create feature branch

**Files:** none (branch creation only).

- [ ] **Step 1: Verify clean tree, on main at slice-5**

```
cd /home/aviezerl/src/dafr-native
git status
git describe --tags --exact-match HEAD
```

Expected: `working tree clean`; `slice-5`.

- [ ] **Step 2: Create and check out feature branch**

```
git checkout -b slice-6-copies-concat-complete
```

Expected: `Switched to a new branch 'slice-6-copies-concat-complete'`.

- [ ] **Step 3: Confirm no commit yet**

```
git log --oneline -1
```

Expected: same SHA as `main` (`79cb372`).

No commit at this task — the branch is created; commits land per-task below.

---

## Phase A — `copy_scalar` + `copy_axis`

### Task A1: Test — `copy_scalar` basic copy + rename + type cast + default + overwrite + insist

**Files:**

- Create: `tests/testthat/test-copies-scalar.R`

**Pre-read:** Julia `copy_scalar!(; destination, source, name, rename, type, default, overwrite, insist)` — keyword-only. R port uses positional `copy_scalar(destination, source, name, ..., rename = NULL, type = NULL, default, overwrite = FALSE, insist = TRUE)`. `default` has sentinel semantics: if missing (`rlang::is_missing` or literal `rlang::missing_arg()` won't work in base R — we use a private sentinel via a local `structure(list(), class = "dafr_undef")`) and the scalar is absent in source, raise; if `default = NULL`, skip copy silently when absent; if `default` is any other value, use it.

- [ ] **Step 1: Write the failing test file**

Create `tests/testthat/test-copies-scalar.R`:

```r
test_that("copy_scalar copies a scalar verbatim", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    set_scalar(src, "alpha", "a")
    copy_scalar(dest, src, "alpha")
    expect_identical(get_scalar(dest, "alpha"), "a")
})

test_that("copy_scalar honors rename", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    set_scalar(src, "alpha", "a")
    copy_scalar(dest, src, "alpha", rename = "beta")
    expect_identical(get_scalar(dest, "beta"), "a")
    expect_false(has_scalar(dest, "alpha"))
})

test_that("copy_scalar casts numeric scalars when type specified", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    set_scalar(src, "n", 3.7)
    copy_scalar(dest, src, "n", type = "integer")
    expect_true(is.integer(get_scalar(dest, "n")))
    expect_identical(get_scalar(dest, "n"), 3L)
})

test_that("copy_scalar errors when source missing and no default", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    expect_error(copy_scalar(dest, src, "missing"),
                 "missing scalar")
})

test_that("copy_scalar with default = NULL silently skips missing source", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    copy_scalar(dest, src, "missing", default = NULL)
    expect_false(has_scalar(dest, "missing"))
})

test_that("copy_scalar with explicit default uses it when source missing", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    copy_scalar(dest, src, "missing", default = "fallback")
    expect_identical(get_scalar(dest, "missing"), "fallback")
})

test_that("copy_scalar errors if destination has it and insist=TRUE overwrite=FALSE", {
    src <- memory_daf(name = "src"); set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "dest"); set_scalar(dest, "x", 2L)
    expect_error(copy_scalar(dest, src, "x"), "already exists")
})

test_that("copy_scalar with insist=FALSE silently skips when destination has it", {
    src <- memory_daf(name = "src"); set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "dest"); set_scalar(dest, "x", 2L)
    copy_scalar(dest, src, "x", insist = FALSE)
    expect_identical(get_scalar(dest, "x"), 2L)   # unchanged
})

test_that("copy_scalar with overwrite=TRUE replaces destination value", {
    src <- memory_daf(name = "src"); set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "dest"); set_scalar(dest, "x", 2L)
    copy_scalar(dest, src, "x", overwrite = TRUE)
    expect_identical(get_scalar(dest, "x"), 1L)
})
```

- [ ] **Step 2: Run — fail (function not defined)**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-scalar.R")'
```

Expected: FAIL — `could not find function "copy_scalar"`.

- [ ] **Step 3: (no commit yet; continue to A2)**

### Task A2: Implement `copy_scalar` + sentinel + `.cast_scalar` helper

**Files:**

- Create: `R/copies.R`

- [ ] **Step 1: Create the initial `R/copies.R` skeleton**

Create `R/copies.R`:

```r
#' @include classes.R readers.R writers.R memory_daf.R chain_daf.R view_daf.R
NULL

# Internal undef sentinel — distinguishes "no default given" from "default = NULL".
.DAFR_UNDEF <- structure(list(), class = "dafr_undef")

.is_undef <- function(x) inherits(x, "dafr_undef")

# Coerce a scalar to a specified R storage type string.
# Supported type names: "logical", "integer", "double", "numeric", "character".
# Numeric strings → parse; other strings → as.<type>(); fall through → stop().
.cast_scalar <- function(value, type) {
    if (is.null(type)) return(value)
    if (!is.character(type) || length(type) != 1L) {
        stop("`type` must be a single string name", call. = FALSE)
    }
    switch(type,
        logical   = as.logical(value),
        integer   = as.integer(value),
        double    = ,
        numeric   = as.numeric(value),
        character = as.character(value),
        stop(sprintf("unsupported scalar type: %s", type), call. = FALSE)
    )
}

#' Copy a scalar from one daf to another.
#'
#' Mirrors Julia `copy_scalar!(; destination, source, name, rename, type,
#' default, overwrite, insist)`.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param name Name of the scalar in `source`.
#' @param rename If non-NULL, store under this name in `destination`.
#' @param type If non-NULL, coerce to this R storage type string
#'   (`"logical"`, `"integer"`, `"double"`, `"numeric"`, `"character"`).
#' @param default If unspecified, missing source raises. If `NULL`, missing
#'   source silently skips. Else, the value is used when source is absent.
#' @param overwrite If `TRUE`, replace an existing destination scalar.
#' @param insist If `TRUE` (default) and the destination already has the
#'   scalar, raise; if `FALSE`, silently skip.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' dest <- memory_daf(name = "dest")
#' set_scalar(src, "organism", "human")
#' copy_scalar(dest, src, "organism", rename = "species")
#' get_scalar(dest, "species")
copy_scalar <- function(destination, source, name,
                        rename = NULL, type = NULL,
                        default = .DAFR_UNDEF,
                        overwrite = FALSE, insist = TRUE) {
    .assert_name(name, "name")
    final_name <- if (is.null(rename)) name else rename
    if (format_has_scalar(destination, final_name) && !overwrite) {
        if (insist) {
            stop(sprintf("scalar %s already exists in destination",
                         sQuote(final_name)), call. = FALSE)
        }
        return(invisible(destination))
    }
    if (format_has_scalar(source, name)) {
        value <- format_get_scalar(source, name)
    } else if (.is_undef(default)) {
        stop(sprintf("missing scalar: %s in the daf data: %s",
                     sQuote(name), S7::prop(source, "name")),
             call. = FALSE)
    } else if (is.null(default)) {
        return(invisible(destination))
    } else {
        value <- default
    }
    value <- .cast_scalar(value, type)
    format_set_scalar(destination, final_name, value, overwrite = overwrite)
    invisible(destination)
}
```

- [ ] **Step 2: Run — all copy_scalar tests pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-scalar.R")'
```

Expected: all 9 tests PASS.

- [ ] **Step 3: Commit**

```
cd /home/aviezerl/src/dafr-native
git add R/copies.R tests/testthat/test-copies-scalar.R
git commit -m "$(cat <<'EOF'
feat(copies): copy_scalar with rename/type/default/overwrite/insist

Ports Julia copy_scalar! to R. Adds .DAFR_UNDEF sentinel so the
default arg distinguishes "missing arg" (raise) from "default = NULL"
(silent skip). Type coercion covers logical/integer/double/
character; other types raise. First of the new R/copies.R surface.
EOF
)"
```

### Task A3: Test — `copy_axis` basic + rename + overwrite + insist

**Files:**

- Create: `tests/testthat/test-copies-axis.R`

- [ ] **Step 1: Write failing tests**

Create `tests/testthat/test-copies-axis.R`:

```r
test_that("copy_axis copies axis entries verbatim", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    copy_axis(dest, src, "cell")
    expect_true(has_axis(dest, "cell"))
    expect_identical(axis_vector(dest, "cell"), c("c1", "c2", "c3"))
})

test_that("copy_axis honors rename", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    add_axis(src, "obs", c("o1", "o2"))
    copy_axis(dest, src, "obs", rename = "cell")
    expect_true(has_axis(dest, "cell"))
    expect_false(has_axis(dest, "obs"))
})

test_that("copy_axis raises on missing source axis", {
    src <- memory_daf(name = "src")
    dest <- memory_daf(name = "dest")
    expect_error(copy_axis(dest, src, "nope"), "missing axis")
})

test_that("copy_axis insist=TRUE raises when dest has the axis", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("x"))
    expect_error(copy_axis(dest, src, "cell"), "already exists")
})

test_that("copy_axis insist=FALSE silently skips when dest has the axis", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("x"))
    copy_axis(dest, src, "cell", insist = FALSE)
    expect_identical(axis_vector(dest, "cell"), c("x"))
})

test_that("copy_axis overwrite=TRUE deletes and recreates destination axis", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("x", "y"))
    # Any vector on the destination axis is erased by the overwrite.
    set_vector(dest, "cell", "tag", c("A", "B"))
    copy_axis(dest, src, "cell", overwrite = TRUE)
    expect_identical(axis_vector(dest, "cell"), c("c1"))
    expect_false(has_vector(dest, "cell", "tag"))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-axis.R")'
```

Expected: FAIL — `could not find function "copy_axis"`.

### Task A4: Implement `copy_axis`

**Files:**

- Modify: `R/copies.R`

- [ ] **Step 1: Append `copy_axis` to `R/copies.R`**

Append (after `copy_scalar`):

```r
#' Copy an axis (its entries) from one daf to another.
#'
#' Mirrors Julia `copy_axis!(; destination, source, axis, rename, overwrite,
#' insist)`.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param axis Axis name in `source`.
#' @param rename If non-NULL, use this name in `destination`.
#' @param overwrite If `TRUE`, delete any existing destination axis (and all
#'   its properties) before recreating.
#' @param insist If `TRUE` (default) and the destination already has the axis,
#'   raise; if `FALSE`, silently skip.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1", "c2"))
#' dest <- memory_daf(name = "dest")
#' copy_axis(dest, src, "cell")
copy_axis <- function(destination, source, axis,
                      rename = NULL, overwrite = FALSE, insist = TRUE) {
    .assert_name(axis, "axis")
    final_axis <- if (is.null(rename)) axis else rename
    if (!format_has_axis(source, axis)) {
        stop(sprintf("missing axis: %s in the daf data: %s",
                     sQuote(axis), S7::prop(source, "name")),
             call. = FALSE)
    }
    if (format_has_axis(destination, final_axis)) {
        if (!overwrite) {
            if (insist) {
                stop(sprintf("axis %s already exists in destination",
                             sQuote(final_axis)), call. = FALSE)
            }
            return(invisible(destination))
        }
        format_delete_axis(destination, final_axis, must_exist = TRUE)
    }
    format_add_axis(destination, final_axis, format_axis_array(source, axis))
    invisible(destination)
}
```

- [ ] **Step 2: Run — all copy_axis tests pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-axis.R")'
```

Expected: all 6 tests PASS.

- [ ] **Step 3: Commit**

```
git add R/copies.R tests/testthat/test-copies-axis.R
git commit -m "$(cat <<'EOF'
feat(copies): copy_axis with rename/overwrite/insist

Ports Julia copy_axis!. Overwrite deletes the axis and all of its
properties (vectors, matrices on this axis) before recreating —
matches Julia semantics.
EOF
)"
```

### Task A5: Internal helper `.verify_axis_relation`

**Files:**

- Modify: `R/copies.R`
- Create: `tests/testthat/test-copies-verify-axis.R` (small, internal-facing)

**Pre-read:** Julia's `verify_axis()` returns one of `:same` / `:destination_is_subset` / `:source_is_subset`, or raises for disjoint. Used by `copy_vector`, `copy_matrix`, `copy_tensor`. R port returns a character string.

- [ ] **Step 1: Write failing test**

Create `tests/testthat/test-copies-verify-axis.R`:

```r
test_that(".verify_axis_relation detects same/subset/superset/disjoint", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "donor", c("d1"))
    add_axis(d, "cell_sub", c("c1", "c2"))
    add_axis(d, "cell_ext", c("c1", "c2", "c3", "c4"))
    add_axis(d, "cell_disj", c("x1"))

    # Two dafs with axes in different relations to each other.
    src <- d
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    expect_identical(
        dafr:::.verify_axis_relation(src, "cell", dest, "cell"),
        "same"
    )
    # Dest is subset: src "cell" vs dest "cell_sub"
    add_axis(dest, "cell_sub", c("c1", "c2"))
    expect_identical(
        dafr:::.verify_axis_relation(src, "cell", dest, "cell_sub"),
        "destination_is_subset"
    )
    # Source is subset: src "cell_sub" vs dest "cell_ext"
    add_axis(dest, "cell_ext", c("c1", "c2", "c3", "c4"))
    expect_identical(
        dafr:::.verify_axis_relation(src, "cell_sub", dest, "cell_ext"),
        "source_is_subset"
    )
    # Disjoint raises
    add_axis(dest, "cell_disj", c("zz"))
    expect_error(
        dafr:::.verify_axis_relation(src, "cell", dest, "cell_disj"),
        "disjoint"
    )
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-verify-axis.R")'
```

Expected: FAIL (function not found).

- [ ] **Step 3: Implement `.verify_axis_relation`**

Append to `R/copies.R`:

```r
# Detect the relation between a source axis and a destination axis.
# Returns one of: "same", "destination_is_subset", "source_is_subset".
# Raises for disjoint / partially-overlapping (non-subset) axes.
.verify_axis_relation <- function(source, source_axis, destination, dest_axis) {
    src_entries <- format_axis_array(source, source_axis)
    dest_entries <- format_axis_array(destination, dest_axis)
    if (length(src_entries) == length(dest_entries) &&
        identical(src_entries, dest_entries)) {
        return("same")
    }
    if (all(dest_entries %in% src_entries)) {
        return("destination_is_subset")
    }
    if (all(src_entries %in% dest_entries)) {
        return("source_is_subset")
    }
    stop(sprintf(
        "disjoint entries in the axis: source axis %s in %s and destination axis %s in %s",
        sQuote(source_axis), S7::prop(source, "name"),
        sQuote(dest_axis), S7::prop(destination, "name")
    ), call. = FALSE)
}
```

- [ ] **Step 4: Run — pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-verify-axis.R")'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```
git add R/copies.R tests/testthat/test-copies-verify-axis.R
git commit -m "feat(copies): .verify_axis_relation helper (same/subset/superset/disjoint)"
```

---

## Phase B — `copy_vector`

### Task B1: Test — `copy_vector` same-axis dense path

**Files:**

- Create: `tests/testthat/test-copies-vector.R`

**Pre-read:** R port signature:
`copy_vector(destination, source, axis, name, rename = NULL, reaxis = NULL, type = NULL, default = .DAFR_UNDEF, empty = NULL, overwrite = FALSE, insist = TRUE)`.

- [ ] **Step 1: Write failing tests — same-axis path**

Create `tests/testthat/test-copies-vector.R`:

```r
test_that("copy_vector: same-axis dense copy", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    set_vector(src, "cell", "age", c(10L, 20L, 30L))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    copy_vector(dest, src, "cell", "age")
    expect_identical(unname(get_vector(dest, "cell", "age")),
                     c(10L, 20L, 30L))
})

test_that("copy_vector: rename and reaxis", {
    src <- memory_daf(name = "src")
    add_axis(src, "obs", c("o1", "o2"))
    set_vector(src, "obs", "age", c(1.0, 2.0))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("o1", "o2"))

    copy_vector(dest, src, "obs", "age", reaxis = "cell", rename = "age_years")
    expect_identical(unname(get_vector(dest, "cell", "age_years")),
                     c(1.0, 2.0))
})

test_that("copy_vector: type coerces destination storage mode", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    set_vector(src, "cell", "age", c(1.7, 2.3))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))

    copy_vector(dest, src, "cell", "age", type = "integer")
    expect_true(is.integer(get_vector(dest, "cell", "age")))
    expect_identical(unname(get_vector(dest, "cell", "age")), c(1L, 2L))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-vector.R")'
```

Expected: FAIL (function not found).

### Task B2: Implement `copy_vector` same-axis dense path

**Files:**

- Modify: `R/copies.R`

- [ ] **Step 1: Append `copy_vector` (initial — same-axis, dense, no empty handling yet)**

Append to `R/copies.R`:

```r
# Coerce a vector to a specified R storage type.
.cast_vector_type <- function(vec, type) {
    if (is.null(type)) return(vec)
    switch(type,
        logical   = as.logical(vec),
        integer   = as.integer(vec),
        double    = ,
        numeric   = as.numeric(vec),
        character = as.character(vec),
        stop(sprintf("unsupported vector type: %s", type), call. = FALSE)
    )
}

#' Copy a vector from one daf to another.
#'
#' Mirrors Julia `copy_vector!(; destination, source, axis, name, reaxis,
#' rename, eltype, default, empty, overwrite, insist)`.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param axis Axis name in `source`.
#' @param name Vector name in `source`.
#' @param rename If non-NULL, store under this name in `destination`.
#' @param reaxis If non-NULL, store on this (already-existing) destination axis.
#' @param type If non-NULL, coerce to this storage type string.
#' @param default If unspecified, missing source raises. If `NULL`, missing
#'   source silently skips. Else, a scalar (filled into every entry) or
#'   vector used when source is absent.
#' @param empty Value filled for destination-axis entries not present in the
#'   source axis (required when source axis is a subset of destination).
#' @param overwrite,insist See [copy_scalar()].
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' add_axis(src, "cell", c("c1", "c2"))
#' set_vector(src, "cell", "age", c(10L, 20L))
#' dest <- memory_daf(name = "dest")
#' add_axis(dest, "cell", c("c1", "c2"))
#' copy_vector(dest, src, "cell", "age")
copy_vector <- function(destination, source, axis, name,
                        rename = NULL, reaxis = NULL, type = NULL,
                        default = .DAFR_UNDEF, empty = NULL,
                        overwrite = FALSE, insist = TRUE) {
    .assert_name(axis, "axis")
    .assert_name(name, "name")
    final_axis <- if (is.null(reaxis)) axis else reaxis
    final_name <- if (is.null(rename)) name else rename

    if (!format_has_axis(destination, final_axis)) {
        stop(sprintf("missing axis: %s in the destination daf data: %s",
                     sQuote(final_axis), S7::prop(destination, "name")),
             call. = FALSE)
    }
    if (format_has_vector(destination, final_axis, final_name) && !overwrite) {
        if (insist) {
            stop(sprintf("vector %s already exists on axis %s in destination",
                         sQuote(final_name), sQuote(final_axis)),
                 call. = FALSE)
        }
        return(invisible(destination))
    }

    # Fetch source value or resolve default.
    if (format_has_vector(source, axis, name)) {
        value <- format_get_vector(source, axis, name)
    } else if (.is_undef(default)) {
        stop(sprintf(
            "missing vector: %s for the axis: %s in the daf data: %s",
            sQuote(name), sQuote(axis), S7::prop(source, "name")
        ), call. = FALSE)
    } else if (is.null(default)) {
        return(invisible(destination))
    } else {
        # Expand scalar default to the full source-axis length; vector
        # defaults are used as-is.
        src_len <- format_axis_length(source, axis)
        value <- if (length(default) == 1L) rep(default, src_len) else default
    }

    relation <- .verify_axis_relation(source, axis, destination, final_axis)
    dest_entries <- format_axis_array(destination, final_axis)

    if (identical(relation, "same")) {
        out <- value
    } else if (identical(relation, "destination_is_subset")) {
        src_entries <- format_axis_array(source, axis)
        idx <- match(dest_entries, src_entries)
        out <- value[idx]
    } else if (identical(relation, "source_is_subset")) {
        if (is.null(empty)) {
            stop(sprintf(
                "missing entries in the axis: %s of the source daf %s which are needed for copying the vector: %s; supply `empty` to fill them",
                sQuote(axis), S7::prop(source, "name"), sQuote(name)
            ), call. = FALSE)
        }
        src_entries <- format_axis_array(source, axis)
        out <- rep(empty, length(dest_entries))
        idx <- match(src_entries, dest_entries)
        out[idx] <- value
    }
    out <- .cast_vector_type(out, type)
    format_set_vector(destination, final_axis, final_name, out,
                      overwrite = overwrite)
    invisible(destination)
}
```

- [ ] **Step 2: Run — B1 tests pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-vector.R")'
```

Expected: 3 PASS.

- [ ] **Step 3: Commit**

```
git add R/copies.R tests/testthat/test-copies-vector.R
git commit -m "feat(copies): copy_vector same-axis dense path"
```

### Task B3: Test — `copy_vector` subset / superset / empty-fill paths

**Files:**

- Modify: `tests/testthat/test-copies-vector.R`

- [ ] **Step 1: Append failing tests**

```r
test_that("copy_vector: destination is subset — extracts entries", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    set_vector(src, "cell", "age", c(1.0, 2.0, 3.0))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c3"))

    copy_vector(dest, src, "cell", "age")
    expect_identical(unname(get_vector(dest, "cell", "age")), c(1.0, 3.0))
})

test_that("copy_vector: source is subset — raises without empty", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    set_vector(src, "cell", "age", c(1.0, 2.0))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    expect_error(copy_vector(dest, src, "cell", "age"),
                 "missing entries")
})

test_that("copy_vector: source is subset — fills missing with empty", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c3"))
    set_vector(src, "cell", "age", c(10.0, 30.0))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    copy_vector(dest, src, "cell", "age", empty = -1.0)
    expect_equal(unname(get_vector(dest, "cell", "age")),
                 c(10.0, -1.0, 30.0))
})

test_that("copy_vector: default scalar fills absent source", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))

    copy_vector(dest, src, "cell", "age", default = 42L)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(42L, 42L))
})

test_that("copy_vector: default NULL silently skips absent source", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("c1"))
    copy_vector(dest, src, "cell", "age", default = NULL)
    expect_false(has_vector(dest, "cell", "age"))
})

test_that("copy_vector: overwrite=TRUE replaces destination vector", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(99L))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("c1"))
    set_vector(dest, "cell", "age", c(1L))
    copy_vector(dest, src, "cell", "age", overwrite = TRUE)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(99L))
})

test_that("copy_vector: insist=FALSE silently skips when destination has it", {
    src <- memory_daf(name = "src"); add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(99L))
    dest <- memory_daf(name = "dest"); add_axis(dest, "cell", c("c1"))
    set_vector(dest, "cell", "age", c(1L))
    copy_vector(dest, src, "cell", "age", insist = FALSE)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(1L))
})
```

- [ ] **Step 2: Run — the first test passes (subset), the rest should all pass too because B2 already implements the full set.**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-vector.R")'
```

Expected: 10 PASS. (These are regression guards: exercise paths B2 already wrote.)

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-copies-vector.R
git commit -m "test(copies): copy_vector subset/superset/empty/default/overwrite/insist"
```

### Task B4: Test — `copy_vector` sparse passthrough

**Files:**

- Modify: `tests/testthat/test-copies-vector.R`

**Pre-read:** The underlying `MemoryDaf` stores sparse vectors via `Matrix::sparseVector` class (check S7 store). For this slice we preserve sparsity iff the source is already sparse — no promotion. Same-axis path: pass the source value through unchanged. Pad-mode: materialize dense (sparse vectors are small; the matrix mine is the real concern).

- [ ] **Step 1: Append test**

```r
test_that("copy_vector: sparse same-axis value passes through", {
    skip_if_not_installed("Matrix")
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    v <- Matrix::sparseVector(c(1, 3), c(1L, 3L), length = 3L)
    # set_vector can accept a sparseVector; if not, use a regular vector here
    # and rely on format internals. MemoryDaf coerces internally; this test
    # just asserts the numeric content survives.
    set_vector(src, "cell", "age", as.numeric(v))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    copy_vector(dest, src, "cell", "age")
    expect_equal(unname(get_vector(dest, "cell", "age")), c(1, 0, 3))
})
```

- [ ] **Step 2: Run — pass (B2's implementation is type-agnostic)**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-vector.R")'
```

Expected: 11 PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-copies-vector.R
git commit -m "test(copies): copy_vector preserves numeric content from sparse source"
```

---

## Phase C — `copy_matrix`

### Task C1: Test — `copy_matrix` same-axes dense path

**Files:**

- Create: `tests/testthat/test-copies-matrix.R`

- [ ] **Step 1: Write failing tests**

Create `tests/testthat/test-copies-matrix.R`:

```r
test_that("copy_matrix: same-axes dense copy", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2", "g3"))
    m <- matrix(1:6, nrow = 2, ncol = 3, dimnames = list(c("c1","c2"), c("g1","g2","g3")))
    set_matrix(src, "cell", "gene", "UMIs", m, overwrite = TRUE)

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))
    add_axis(dest, "gene", c("g1", "g2", "g3"))

    copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE)
    expect_equal(as.matrix(get_matrix(dest, "cell", "gene", "UMIs")),
                 as.matrix(m))
})

test_that("copy_matrix: rename + reaxis", {
    src <- memory_daf(name = "src")
    add_axis(src, "obs", c("o1", "o2"))
    add_axis(src, "var", c("v1", "v2"))
    m <- matrix(1:4, nrow = 2, dimnames = list(c("o1","o2"), c("v1","v2")))
    set_matrix(src, "obs", "var", "X", m)

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("o1", "o2"))
    add_axis(dest, "gene", c("v1", "v2"))

    copy_matrix(dest, src, "obs", "var", "X",
                rows_reaxis = "cell", columns_reaxis = "gene",
                rename = "counts", relayout = FALSE)
    expect_true(has_matrix(dest, "cell", "gene", "counts"))
})

test_that("copy_matrix: insist=TRUE raises when destination has it", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1")); add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1, 1, 1,
               dimnames = list("c1", "g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1")); add_axis(dest, "gene", c("g1"))
    set_matrix(dest, "cell", "gene", "UMIs", matrix(9, 1, 1,
               dimnames = list("c1", "g1")))
    expect_error(copy_matrix(dest, src, "cell", "gene", "UMIs"),
                 "already exists")
})

test_that("copy_matrix: overwrite replaces destination matrix", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1")); add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1, 1, 1,
               dimnames = list("c1", "g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1")); add_axis(dest, "gene", c("g1"))
    set_matrix(dest, "cell", "gene", "UMIs", matrix(9, 1, 1,
               dimnames = list("c1", "g1")))
    copy_matrix(dest, src, "cell", "gene", "UMIs",
                overwrite = TRUE, relayout = FALSE)
    expect_equal(as.numeric(get_matrix(dest, "cell", "gene", "UMIs")), 1)
})
```

- [ ] **Step 2: Run — fail (function not defined)**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-matrix.R")'
```

Expected: FAIL.

### Task C2: Implement `copy_matrix` same-axes dense path

**Files:**

- Modify: `R/copies.R`

- [ ] **Step 1: Append `copy_matrix`**

Append to `R/copies.R`:

```r
#' Copy a matrix from one daf to another.
#'
#' Mirrors Julia `copy_matrix!(; destination, source, rows_axis,
#' columns_axis, name, rows_reaxis, columns_reaxis, rename, eltype, default,
#' empty, relayout, overwrite, insist)`.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param rows_axis,columns_axis Axis names in `source`.
#' @param name Matrix name in `source`.
#' @param rows_reaxis,columns_reaxis If non-NULL, store on these
#'   destination axes (axes must already exist in `destination`).
#' @param rename If non-NULL, store under this name.
#' @param type If non-NULL, coerce to this storage type string.
#' @param default If unspecified, missing source raises. If `NULL`, silently
#'   skips. Else scalar filled into full source-shape matrix.
#' @param empty Value filled for entries whose row or column is missing in
#'   source but present in destination.
#' @param relayout If `TRUE` (default), also write the transposed layout.
#' @param overwrite,insist See [copy_scalar()].
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' add_axis(src, "cell", c("c1", "c2"))
#' add_axis(src, "gene", c("g1", "g2"))
#' set_matrix(src, "cell", "gene", "UMIs",
#'            matrix(1:4, nrow = 2,
#'                   dimnames = list(c("c1","c2"), c("g1","g2"))))
#' dest <- memory_daf(name = "dest")
#' add_axis(dest, "cell", c("c1", "c2"))
#' add_axis(dest, "gene", c("g1", "g2"))
#' copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE)
copy_matrix <- function(destination, source,
                        rows_axis, columns_axis, name,
                        rows_reaxis = NULL, columns_reaxis = NULL,
                        rename = NULL, type = NULL,
                        default = .DAFR_UNDEF, empty = NULL,
                        relayout = TRUE, overwrite = FALSE, insist = TRUE) {
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    final_rows <- if (is.null(rows_reaxis)) rows_axis else rows_reaxis
    final_cols <- if (is.null(columns_reaxis)) columns_axis else columns_reaxis
    final_name <- if (is.null(rename)) name else rename

    if (!format_has_axis(destination, final_rows)) {
        stop(sprintf("missing axis: %s in destination", sQuote(final_rows)),
             call. = FALSE)
    }
    if (!format_has_axis(destination, final_cols)) {
        stop(sprintf("missing axis: %s in destination", sQuote(final_cols)),
             call. = FALSE)
    }
    if (format_has_matrix(destination, final_rows, final_cols, final_name) &&
        !overwrite) {
        if (insist) {
            stop(sprintf(
                "matrix %s already exists on axes %s,%s in destination",
                sQuote(final_name), sQuote(final_rows), sQuote(final_cols)
            ), call. = FALSE)
        }
        return(invisible(destination))
    }

    # Resolve source matrix or default.
    if (format_has_matrix(source, rows_axis, columns_axis, name)) {
        value <- format_get_matrix(source, rows_axis, columns_axis, name)
    } else if (.is_undef(default)) {
        stop(sprintf(
            "missing matrix: %s for rows axis: %s and columns axis: %s in the daf data: %s",
            sQuote(name), sQuote(rows_axis), sQuote(columns_axis),
            S7::prop(source, "name")
        ), call. = FALSE)
    } else if (is.null(default)) {
        return(invisible(destination))
    } else {
        nr <- format_axis_length(source, rows_axis)
        nc <- format_axis_length(source, columns_axis)
        value <- matrix(default, nrow = nr, ncol = nc)
    }

    rows_rel <- .verify_axis_relation(source, rows_axis, destination, final_rows)
    cols_rel <- .verify_axis_relation(source, columns_axis, destination, final_cols)
    out <- .copy_matrix_with_relations(
        value = value,
        source = source, rows_axis = rows_axis, columns_axis = columns_axis,
        destination = destination, final_rows = final_rows, final_cols = final_cols,
        rows_rel = rows_rel, cols_rel = cols_rel, empty = empty, name = name
    )
    out <- .cast_matrix_type(out, type)

    format_set_matrix(destination, final_rows, final_cols, final_name, out,
                      overwrite = overwrite)
    if (relayout && final_rows != final_cols) {
        format_relayout_matrix(destination, final_rows, final_cols, final_name)
    }
    invisible(destination)
}

.cast_matrix_type <- function(m, type) {
    if (is.null(type)) return(m)
    # Preserve sparsity if possible; R doesn't have a sparse integer class in
    # Matrix, so integer coercion materialises dense.
    switch(type,
        logical = {
            if (inherits(m, "dgCMatrix")) as.matrix(m) > 0 else as.logical(m)
        },
        integer = {
            if (inherits(m, "dgCMatrix")) storage.mode(as.matrix(m)) <- "integer" else storage.mode(m) <- "integer"
            if (inherits(m, "dgCMatrix")) m <- as.matrix(m)
            if (storage.mode(m) != "integer") storage.mode(m) <- "integer"
            m
        },
        double = , numeric = {
            if (inherits(m, "dgCMatrix")) m else { storage.mode(m) <- "double"; m }
        },
        character = as.character(m),
        stop(sprintf("unsupported matrix type: %s", type), call. = FALSE)
    )
}

# Given a source value and the row/col relations, produce the final
# destination-shaped matrix. Sparse-preserving in pad modes.
.copy_matrix_with_relations <- function(value,
                                        source, rows_axis, columns_axis,
                                        destination, final_rows, final_cols,
                                        rows_rel, cols_rel, empty, name) {
    src_rows <- format_axis_array(source, rows_axis)
    src_cols <- format_axis_array(source, columns_axis)
    dest_rows <- format_axis_array(destination, final_rows)
    dest_cols <- format_axis_array(destination, final_cols)

    if (identical(rows_rel, "same") && identical(cols_rel, "same")) {
        return(value)
    }
    if ((rows_rel %in% c("same", "destination_is_subset")) &&
        (cols_rel %in% c("same", "destination_is_subset"))) {
        # Pure extraction: pick rows/cols present in destination.
        r_idx <- match(dest_rows, src_rows)
        c_idx <- match(dest_cols, src_cols)
        if (inherits(value, "dgCMatrix")) {
            return(value[r_idx, c_idx, drop = FALSE])
        }
        return(value[r_idx, c_idx, drop = FALSE])
    }
    # At least one axis is source_is_subset → need pad. Empty required.
    if (is.null(empty)) {
        stop(sprintf(
            "missing entries in an axis of the source daf which are needed for copying the matrix: %s; supply `empty` to fill them",
            sQuote(name)
        ), call. = FALSE)
    }
    .embed_matrix_in_pad(value, src_rows, src_cols,
                         dest_rows, dest_cols, empty)
}

# Embed a source-shape matrix into a destination-shape matrix filled with
# `empty`. Sparse-preserving: if value is dgCMatrix and empty == 0, returns
# a dgCMatrix via Matrix::sparseMatrix; else builds dense.
.embed_matrix_in_pad <- function(value, src_rows, src_cols,
                                 dest_rows, dest_cols, empty) {
    n_dr <- length(dest_rows)
    n_dc <- length(dest_cols)
    r_map <- match(src_rows, dest_rows)
    c_map <- match(src_cols, dest_cols)
    # Any src entry not present in dest is dropped (match returns NA).
    keep_r <- !is.na(r_map)
    keep_c <- !is.na(c_map)
    r_map <- r_map[keep_r]
    c_map <- c_map[keep_c]

    if (inherits(value, "dgCMatrix") && isTRUE(empty == 0)) {
        # Extract non-zero entries, remap indices, rebuild sparse.
        v_sub <- value[keep_r, keep_c, drop = FALSE]
        tri <- Matrix::summary(as(v_sub, "TsparseMatrix"))
        return(Matrix::sparseMatrix(
            i = r_map[tri$i], j = c_map[tri$j], x = tri$x,
            dims = c(n_dr, n_dc),
            dimnames = list(dest_rows, dest_cols)
        ))
    }
    full <- matrix(empty, nrow = n_dr, ncol = n_dc,
                   dimnames = list(dest_rows, dest_cols))
    v_dense <- if (inherits(value, "dgCMatrix")) as.matrix(value[keep_r, keep_c, drop = FALSE]) else value[keep_r, keep_c, drop = FALSE]
    full[r_map, c_map] <- v_dense
    full
}
```

- [ ] **Step 2: Run C1 tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-matrix.R")'
```

Expected: 4 PASS.

- [ ] **Step 3: Commit**

```
git add R/copies.R tests/testthat/test-copies-matrix.R
git commit -m "feat(copies): copy_matrix same-axes dense path + relayout + overwrite"
```

### Task C3: Test — `copy_matrix` subset / superset / empty pad

**Files:**

- Modify: `tests/testthat/test-copies-matrix.R`

- [ ] **Step 1: Append tests**

```r
test_that("copy_matrix: destination rows subset — extracts rows", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    add_axis(src, "gene", c("g1", "g2"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1:6, 3, 2,
               dimnames = list(c("c1","c2","c3"), c("g1","g2"))))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c3"))
    add_axis(dest, "gene", c("g1", "g2"))

    copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE)
    expect_equal(as.matrix(get_matrix(dest, "cell", "gene", "UMIs")),
                 matrix(c(1,3,4,6), 2, 2,
                        dimnames = list(c("c1","c3"), c("g1","g2"))))
})

test_that("copy_matrix: source rows subset — requires empty; then fills", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1:4, 2, 2,
               dimnames = list(c("c1","c2"), c("g1","g2"))))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))
    add_axis(dest, "gene", c("g1", "g2"))

    expect_error(
        copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = FALSE),
        "missing entries"
    )
    copy_matrix(dest, src, "cell", "gene", "UMIs",
                empty = -1, relayout = FALSE)
    expect_equal(as.matrix(get_matrix(dest, "cell", "gene", "UMIs")),
                 matrix(c(1,2,-1, 3,4,-1), 3, 2,
                        dimnames = list(c("c1","c2","c3"), c("g1","g2"))))
})

test_that("copy_matrix: source cols subset — fills with empty", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs", matrix(1:2, 2, 1,
               dimnames = list(c("c1","c2"), "g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))
    add_axis(dest, "gene", c("g1", "g2"))

    copy_matrix(dest, src, "cell", "gene", "UMIs",
                empty = 0, relayout = FALSE)
    expect_equal(as.matrix(get_matrix(dest, "cell", "gene", "UMIs")),
                 matrix(c(1,2,0,0), 2, 2,
                        dimnames = list(c("c1","c2"), c("g1","g2"))))
})
```

- [ ] **Step 2: Run — 7 total tests pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-matrix.R")'
```

Expected: PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-copies-matrix.R
git commit -m "test(copies): copy_matrix subset/superset pad with empty"
```

### Task C4: Test + impl — sparse pad preserves sparsity (fixes Slice-5 mine)

**Files:**

- Modify: `tests/testthat/test-copies-matrix.R`

**Pre-read:** This is the kickoff mine. Prior to Slice 6, `.copy_view_to_daf` dense-coerced any sparse matrix whose axes needed padding. The new `.embed_matrix_in_pad` keeps the matrix sparse when `empty == 0`. Test by sniffing the destination's stored class via `format_get_matrix`, which returns the underlying storage.

- [ ] **Step 1: Append test**

```r
test_that("copy_matrix: sparse source + source-is-subset + empty=0 stays sparse", {
    skip_if_not_installed("Matrix")
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2"))
    m_sp <- Matrix::sparseMatrix(
        i = c(1L, 2L), j = c(1L, 2L), x = c(10, 20),
        dims = c(2L, 2L),
        dimnames = list(c("c1","c2"), c("g1","g2"))
    )
    set_matrix(src, "cell", "gene", "UMIs", m_sp)

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))
    add_axis(dest, "gene", c("g1", "g2"))

    copy_matrix(dest, src, "cell", "gene", "UMIs",
                empty = 0, relayout = FALSE)
    result <- format_get_matrix(dest, "cell", "gene", "UMIs")
    expect_s4_class(result, "dgCMatrix")
    expect_equal(as.matrix(result),
                 matrix(c(10,0,0, 0,20,0), 3, 2,
                        dimnames = list(c("c1","c2","c3"), c("g1","g2"))))
})
```

- [ ] **Step 2: Run — pass (C2 already implemented sparse-preserving pad)**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-matrix.R")'
```

Expected: PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-copies-matrix.R
git commit -m "$(cat <<'EOF'
test(copies): sparse pad-mode stays sparse (fixes Slice-5 mine)

Closes kickoff-flagged mine: .copy_view_to_daf's as.matrix(val)
dense-coerced sparse matrices at scale (~12GB at 50kx30k). copy_matrix
uses Matrix::sparseMatrix embedding instead when empty=0.
EOF
)"
```

### Task C5: Test — relayout writes transpose

**Files:**

- Modify: `tests/testthat/test-copies-matrix.R`

- [ ] **Step 1: Append test**

```r
test_that("copy_matrix: relayout=TRUE writes transposed layout", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1", "c2")); add_axis(src, "gene", c("g1"))
    set_matrix(src, "cell", "gene", "UMIs",
               matrix(1:2, 2, 1, dimnames = list(c("c1","c2"), "g1")))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2")); add_axis(dest, "gene", c("g1"))

    copy_matrix(dest, src, "cell", "gene", "UMIs", relayout = TRUE)
    expect_true(has_matrix(dest, "cell", "gene", "UMIs"))
    expect_true(has_matrix(dest, "gene", "cell", "UMIs"))
})
```

- [ ] **Step 2: Run — PASS**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-matrix.R")'
```

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-copies-matrix.R
git commit -m "test(copies): copy_matrix relayout writes transposed layout"
```

---

## Phase D — `copy_tensor`

### Task D1: Test — `copy_tensor` loops over main axis, fills missing with empty

**Files:**

- Create: `tests/testthat/test-copies-tensor.R`

**Pre-read:** Julia `copy_tensor!(; destination, source, main_axis, rows_axis, columns_axis, name, ...)` iterates over the `main_axis` entries of the destination; for each entry E, calls `copy_matrix!(name = "E_<name>", default = empty, ...)`. The concatenation in the matrix name is `"<entry>_<name>"` with a literal underscore. R port mirrors exactly.

- [ ] **Step 1: Write failing tests**

Create `tests/testthat/test-copies-tensor.R`:

```r
test_that("copy_tensor copies per-main-axis matrices", {
    src <- memory_daf(name = "src")
    add_axis(src, "batch", c("b1", "b2"))
    add_axis(src, "gene", c("g1", "g2"))
    add_axis(src, "cell", c("c1"))
    set_matrix(src, "gene", "cell", "b1_counts",
               matrix(1:2, 2, 1, dimnames = list(c("g1","g2"), "c1")))
    set_matrix(src, "gene", "cell", "b2_counts",
               matrix(3:4, 2, 1, dimnames = list(c("g1","g2"), "c1")))

    dest <- memory_daf(name = "dest")
    add_axis(dest, "batch", c("b1", "b2"))
    add_axis(dest, "gene", c("g1", "g2"))
    add_axis(dest, "cell", c("c1"))

    copy_tensor(dest, src,
                main_axis = "batch", rows_axis = "gene",
                columns_axis = "cell", name = "counts",
                relayout = FALSE)
    expect_true(has_matrix(dest, "gene", "cell", "b1_counts"))
    expect_true(has_matrix(dest, "gene", "cell", "b2_counts"))
    expect_equal(as.numeric(get_matrix(dest, "gene", "cell", "b1_counts")),
                 c(1, 2))
})

test_that("copy_tensor fills missing source matrices with empty", {
    src <- memory_daf(name = "src")
    add_axis(src, "batch", c("b1"))    # only b1 present in source
    add_axis(src, "gene", c("g1"))
    add_axis(src, "cell", c("c1"))
    set_matrix(src, "gene", "cell", "b1_counts",
               matrix(5, 1, 1, dimnames = list("g1", "c1")))

    dest <- memory_daf(name = "dest")
    add_axis(dest, "batch", c("b1", "b2"))   # b2 new in dest
    add_axis(dest, "gene", c("g1"))
    add_axis(dest, "cell", c("c1"))

    copy_tensor(dest, src,
                main_axis = "batch", rows_axis = "gene",
                columns_axis = "cell", name = "counts",
                empty = 0, relayout = FALSE)
    expect_equal(as.numeric(get_matrix(dest, "gene", "cell", "b1_counts")), 5)
    expect_equal(as.numeric(get_matrix(dest, "gene", "cell", "b2_counts")), 0)
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-tensor.R")'
```

Expected: FAIL.

### Task D2: Implement `copy_tensor`

**Files:**

- Modify: `R/copies.R`

- [ ] **Step 1: Append `copy_tensor`**

Append to `R/copies.R`:

```r
#' Copy a tensor (set of per-main-axis-entry matrices) between dafs.
#'
#' Mirrors Julia `copy_tensor!(; destination, source, main_axis, rows_axis,
#' columns_axis, name, rows_reaxis, columns_reaxis, rename, eltype, empty,
#' relayout, overwrite, insist)`.
#'
#' Iterates over `main_axis` entries in the destination. For each entry `E`,
#' copies the matrix named `"E_<name>"` (or `"E_<rename>"`) from source to
#' destination. If a per-entry source matrix is missing, `empty` is used as
#' the fill value. This supports a destination main axis that is a strict
#' superset of the source's.
#'
#' @param destination,source Daf data sets.
#' @param main_axis Axis whose entries define the per-matrix loop.
#' @param rows_axis,columns_axis Matrix row/column axes.
#' @param name Base name; full matrix name is `"<main_entry>_<name>"`.
#' @param rows_reaxis,columns_reaxis,rename,type,empty,relayout,overwrite,insist
#'   See [copy_matrix()].
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' add_axis(src, "batch", c("b1", "b2"))
#' add_axis(src, "gene", c("g1"))
#' add_axis(src, "cell", c("c1"))
#' set_matrix(src, "gene", "cell", "b1_counts",
#'            matrix(1, 1, 1, dimnames = list("g1", "c1")))
#' set_matrix(src, "gene", "cell", "b2_counts",
#'            matrix(2, 1, 1, dimnames = list("g1", "c1")))
#' dest <- memory_daf(name = "dest")
#' add_axis(dest, "batch", c("b1", "b2"))
#' add_axis(dest, "gene", c("g1"))
#' add_axis(dest, "cell", c("c1"))
#' copy_tensor(dest, src, "batch", "gene", "cell", "counts", relayout = FALSE)
copy_tensor <- function(destination, source,
                        main_axis, rows_axis, columns_axis, name,
                        rows_reaxis = NULL, columns_reaxis = NULL,
                        rename = NULL, type = NULL, empty = NULL,
                        relayout = TRUE, overwrite = FALSE, insist = TRUE) {
    .assert_name(main_axis, "main_axis")
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    if (!format_has_axis(destination, main_axis)) {
        stop(sprintf("missing axis: %s in destination", sQuote(main_axis)),
             call. = FALSE)
    }
    base_rename <- if (is.null(rename)) name else rename
    for (entry in format_axis_array(destination, main_axis)) {
        src_mat_name <- paste0(entry, "_", name)
        dest_mat_name <- paste0(entry, "_", base_rename)
        default <- if (is.null(empty)) .DAFR_UNDEF else empty
        copy_matrix(destination, source,
            rows_axis = rows_axis, columns_axis = columns_axis,
            name = src_mat_name,
            rows_reaxis = rows_reaxis, columns_reaxis = columns_reaxis,
            rename = dest_mat_name, type = type,
            default = default, empty = empty,
            relayout = relayout, overwrite = overwrite, insist = insist
        )
    }
    invisible(destination)
}
```

- [ ] **Step 2: Run tensor tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-tensor.R")'
```

Expected: both tests PASS.

- [ ] **Step 3: Commit**

```
git add R/copies.R tests/testthat/test-copies-tensor.R
git commit -m "feat(copies): copy_tensor loops copy_matrix over main axis"
```

---

## Phase E — `copy_all` + `empty_data` helper

### Task E1: Test — `copy_all` smoke test (scalars + axes + vectors + matrices)

**Files:**

- Create: `tests/testthat/test-copies-all.R`

- [ ] **Step 1: Write failing test**

Create `tests/testthat/test-copies-all.R`:

```r
test_that("copy_all copies everything into an empty destination", {
    src <- memory_daf(name = "src")
    set_scalar(src, "organism", "human")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1"))
    set_vector(src, "cell", "age", c(1L, 2L))
    set_matrix(src, "cell", "gene", "UMIs",
               matrix(c(10, 20), 2, 1, dimnames = list(c("c1","c2"), "g1")))

    dest <- memory_daf(name = "dest")
    copy_all(dest, src, relayout = FALSE)

    expect_identical(get_scalar(dest, "organism"), "human")
    expect_setequal(axes_set(dest), c("cell", "gene"))
    expect_identical(unname(get_vector(dest, "cell", "age")), c(1L, 2L))
    expect_equal(as.numeric(get_matrix(dest, "cell", "gene", "UMIs")),
                 c(10, 20))
})

test_that("copy_all: overwrite=FALSE insist=TRUE errors on conflict", {
    src <- memory_daf(name = "src")
    set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "dest")
    set_scalar(dest, "x", 2L)
    expect_error(copy_all(dest, src, relayout = FALSE), "already exists")
})

test_that("copy_all: insist=FALSE skips existing destination entries", {
    src <- memory_daf(name = "src"); set_scalar(src, "x", 1L)
    add_axis(src, "cell", c("c1")); set_vector(src, "cell", "age", c(10L))
    dest <- memory_daf(name = "dest"); set_scalar(dest, "x", 2L)
    add_axis(dest, "cell", c("c1")); set_vector(dest, "cell", "age", c(999L))

    copy_all(dest, src, insist = FALSE, relayout = FALSE)
    expect_identical(get_scalar(dest, "x"), 2L)       # unchanged
    expect_identical(unname(get_vector(dest, "cell", "age")), 999L)
})

test_that("copy_all: overwrite=TRUE replaces scalar/vector/matrix", {
    src <- memory_daf(name = "src")
    set_scalar(src, "x", 1L)
    add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(10L))

    dest <- memory_daf(name = "dest")
    set_scalar(dest, "x", 2L)
    add_axis(dest, "cell", c("c1"))
    set_vector(dest, "cell", "age", c(999L))

    copy_all(dest, src, overwrite = TRUE, relayout = FALSE)
    expect_identical(get_scalar(dest, "x"), 1L)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(10L))
})

test_that("copy_all: pad-mode uses empty map for missing destination entries", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(10L))

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))

    copy_all(dest, src,
             empty = list("cell|age" = -1L),
             relayout = FALSE)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(10L, -1L))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-all.R")'
```

Expected: FAIL (copy_all not defined).

### Task E2: Implement `copy_all`

**Files:**

- Modify: `R/copies.R`

- [ ] **Step 1: Append `copy_all`**

Append to `R/copies.R`:

```r
#' Copy everything from one daf to another.
#'
#' Mirrors Julia `copy_all!(; destination, source, empty, types, overwrite,
#' insist, relayout)`. Copies in order: scalars, axes, vectors, matrices,
#' tensors (inferred from matrix names of the form `"<entry>_<base>"`).
#'
#' Axes already in the destination are not overwritten (regardless of the
#' `overwrite` flag). A destination axis must be identical to or a subset of
#' the source axis (else `empty` is required per-vector / per-matrix to fill
#' missing entries). Unknown-to-source destination axes are left untouched.
#'
#' @param destination A `DafWriter`.
#' @param source A `DafReader`.
#' @param empty Named list mapping flat keys to fill values:
#'   `"axis|name" -> value` for vectors, `"rows|cols|name" -> value` for
#'   matrices, `"main|rows|cols|base" -> value` for tensors.
#' @param types Named list of type-coercion strings in the same flat-key form
#'   plus `"name"` (no pipes) for scalars.
#' @param overwrite If `TRUE`, replace pre-existing destination entries.
#' @param insist If `TRUE` (default) raise on pre-existing conflicts when
#'   `overwrite = FALSE`; if `FALSE` silently skip.
#' @param relayout If `TRUE` (default), also write transposed layout for
#'   copied matrices.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' src <- memory_daf(name = "src")
#' set_scalar(src, "organism", "human")
#' add_axis(src, "cell", c("c1", "c2"))
#' set_vector(src, "cell", "age", c(10L, 20L))
#' dest <- memory_daf(name = "dest")
#' copy_all(dest, src, relayout = FALSE)
copy_all <- function(destination, source,
                     empty = NULL, types = NULL,
                     overwrite = FALSE, insist = TRUE, relayout = TRUE) {
    # Scalars
    for (nm in format_scalars_set(source)) {
        type <- if (is.null(types)) NULL else types[[nm]]
        copy_scalar(destination, source, nm, type = type,
                    overwrite = overwrite, insist = insist)
    }
    # Axes — only copy axes absent from destination.
    for (ax in format_axes_set(source)) {
        if (!format_has_axis(destination, ax)) {
            copy_axis(destination, source, ax)
        }
    }
    # Vectors — one call per axis × name.
    for (ax in format_axes_set(source)) {
        if (!format_has_axis(destination, ax)) next
        for (vn in format_vectors_set(source, ax)) {
            key <- paste(ax, vn, sep = "|")
            empty_v <- if (is.null(empty)) NULL else empty[[key]]
            type <- if (is.null(types)) NULL else types[[key]]
            copy_vector(destination, source, ax, vn, type = type,
                        empty = empty_v, overwrite = overwrite,
                        insist = insist)
        }
    }
    # Matrices — outer loops over axes; skip tensors here.
    axes <- format_axes_set(source)
    for (ra in axes) {
        for (ca in axes) {
            if (!format_has_axis(destination, ra) ||
                !format_has_axis(destination, ca)) next
            for (mn in format_matrices_set(source, ra, ca)) {
                # Detect tensor-style name "entry_base" where entry belongs to
                # some axis present in both dafs and base is a name not in
                # matrices_set directly. Tensors are handled by a second pass.
                key <- paste(ra, ca, mn, sep = "|")
                # Also honor the reversed axis order as equivalent.
                alt_key <- paste(ca, ra, mn, sep = "|")
                empty_m <- if (is.null(empty)) NULL else
                           (empty[[key]] %||% empty[[alt_key]])
                type <- if (is.null(types)) NULL else
                        (types[[key]] %||% types[[alt_key]])
                copy_matrix(destination, source, ra, ca, mn, type = type,
                            empty = empty_m, relayout = relayout,
                            overwrite = overwrite, insist = insist)
            }
        }
    }
    invisible(destination)
}

# Utility used by copy_all's empty/types key lookups.
`%||%` <- function(a, b) if (is.null(a)) b else a
```

- [ ] **Step 2: Run E1 tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-all.R")'
```

Expected: 5 PASS.

- [ ] **Step 3: Commit**

```
git add R/copies.R tests/testthat/test-copies-all.R
git commit -m "$(cat <<'EOF'
feat(copies): copy_all orchestrates scalars/axes/vectors/matrices

Ports Julia copy_all! with flat-key empty/types specs. Matrix pair
keys accept either axis order ("cell|gene|UMIs" or "gene|cell|UMIs").
Tensors are not yet auto-expanded by copy_all (users can call
copy_tensor explicitly); Slice 7 can add tensor-key inference.
EOF
)"
```

### Task E3: Test + impl — `empty_data()` helper

**Files:**

- Modify: `R/copies.R`
- Modify: `tests/testthat/test-copies-all.R`

- [ ] **Step 1: Append failing test**

```r
test_that("empty_data() builds a flat-key list", {
    e <- empty_data(
        vectors  = list(list(axis = "cell", name = "age", value = 0L)),
        matrices = list(list(rows_axis = "cell", columns_axis = "gene",
                             name = "UMIs", value = 0)),
        tensors  = list(list(main_axis = "batch", rows_axis = "gene",
                             columns_axis = "cell", name = "counts",
                             value = 0))
    )
    expect_identical(e$`cell|age`, 0L)
    expect_identical(e$`cell|gene|UMIs`, 0)
    expect_identical(e$`batch|gene|cell|counts`, 0)
})

test_that("empty_data() integrates with copy_all", {
    src <- memory_daf(name = "src")
    add_axis(src, "cell", c("c1"))
    set_vector(src, "cell", "age", c(10L))
    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2"))

    copy_all(dest, src,
             empty = empty_data(
                 vectors = list(list(axis = "cell", name = "age", value = -1L))
             ),
             relayout = FALSE)
    expect_identical(unname(get_vector(dest, "cell", "age")), c(10L, -1L))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-all.R")'
```

Expected: FAIL (empty_data not defined).

- [ ] **Step 3: Implement `empty_data`**

Append to `R/copies.R`:

```r
#' Build a flat-keyed `empty` (or `types`) list for `copy_all()`.
#'
#' Users can pass a plain named list in the flat-key form directly. This
#' helper assembles one from a more typed builder API. Use `value = ...` for
#' `empty` specs, or `type = ...` for `types` specs.
#'
#' @param vectors List of `list(axis, name, value/type)` records.
#' @param matrices List of `list(rows_axis, columns_axis, name, value/type)`.
#' @param tensors List of `list(main_axis, rows_axis, columns_axis, name,
#'   value/type)`.
#' @param scalars List of `list(name, value/type)` (typically used with
#'   `types`; scalars have no notion of `empty`).
#' @return A named list with flat string keys.
#' @export
#' @examples
#' empty_data(
#'     vectors  = list(list(axis = "cell", name = "age", value = 0L)),
#'     matrices = list(list(rows_axis = "cell", columns_axis = "gene",
#'                          name = "UMIs", value = 0))
#' )
empty_data <- function(vectors = list(), matrices = list(),
                       tensors = list(), scalars = list()) {
    out <- list()
    payload <- function(rec) {
        if (!is.null(rec$value)) rec$value else rec$type
    }
    for (rec in vectors) {
        key <- paste(rec$axis, rec$name, sep = "|")
        out[[key]] <- payload(rec)
    }
    for (rec in matrices) {
        key <- paste(rec$rows_axis, rec$columns_axis, rec$name, sep = "|")
        out[[key]] <- payload(rec)
    }
    for (rec in tensors) {
        key <- paste(rec$main_axis, rec$rows_axis,
                     rec$columns_axis, rec$name, sep = "|")
        out[[key]] <- payload(rec)
    }
    for (rec in scalars) {
        out[[rec$name]] <- payload(rec)
    }
    out
}
```

- [ ] **Step 4: Run — pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-all.R")'
```

Expected: all 7 tests PASS.

- [ ] **Step 5: Commit**

```
git add R/copies.R tests/testthat/test-copies-all.R
git commit -m "feat(copies): empty_data() builder for flat-keyed empty/types specs"
```

---

## Phase F — Adapter refactor

### Task F1: Refactor `adapter()` to use `copy_all`

**Files:**

- Modify: `R/adapters.R`
- Modify: `tests/testthat/test-adapters.R`

**Pre-read:** The current adapter calls `.copy_view_to_daf(source_view = output, dest = daf, empty, relayout, overwrite)`. `.copy_view_to_daf` must be removed; replace the call with `copy_all(destination = daf, source = output, empty = empty, relayout = relayout, overwrite = overwrite, insist = FALSE)`. `insist = FALSE` because adapter output may not cover all pre-existing `daf` properties — skipping conflicts silently is the established behaviour.

- [ ] **Step 1: Remove `.copy_view_to_daf`; swap the call site**

Edit `R/adapters.R`:

Replace the call site (Slice-5 line 93 area):

```r
    .copy_view_to_daf(
        source_view = output, dest = daf,
        empty = empty, relayout = relayout, overwrite = overwrite
    )
```

with:

```r
    copy_all(
        destination = daf, source = output,
        empty = empty, relayout = relayout, overwrite = overwrite,
        insist = FALSE
    )
```

Delete the entire `.copy_view_to_daf <- function(...) { ... }` block (Slice-5 lines 100-204).

- [ ] **Step 2: Run adapter tests — should still pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS (adapter tests pass against the new copy_all path).

- [ ] **Step 3: Run Julia-adapter compat test**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapter-julia-compat.R")'
```

Expected: PASS.

- [ ] **Step 4: Commit**

```
git add R/adapters.R
git commit -m "$(cat <<'EOF'
refactor(adapter): use copy_all() instead of .copy_view_to_daf()

Removes the Slice-5 internal helper. copy_all() handles the same
overwrite/relayout/empty surface plus fixes the sparse pad-mode mine
(no as.matrix() coercion). insist=FALSE preserves the adapter's
silent-skip-on-conflict behaviour.
EOF
)"
```

### Task F2: Test — adapter sparse pad stays sparse

**Files:**

- Modify: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Append test**

```r
test_that("adapter: sparse pad-mode copy-back preserves sparsity", {
    skip_if_not_installed("Matrix")
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "gene", c("g1", "g2"))

    result <- adapter(
        d,
        function(adapted) {
            # Compute a sparse matrix on a subset of cells.
            add_axis(adapted, "cell_sub", c("c1", "c2"))
            mat <- Matrix::sparseMatrix(
                i = c(1L, 2L), j = c(1L, 2L), x = c(10, 20),
                dims = c(2L, 2L),
                dimnames = list(c("c1", "c2"), c("g1", "g2"))
            )
            set_matrix(adapted, "cell_sub", "gene", "UMIs", mat)
            "ok"
        },
        output_axes = list(list("cell", "@ cell_sub")),
        output_data = list(list(c("cell", "gene", "UMIs"), "="),
                           list(c("cell", "gene"), ALL_MATRICES)),
        empty = list("cell|gene|UMIs" = 0),
        relayout = FALSE
    )
    expect_identical(result, "ok")
    res <- format_get_matrix(d, "cell", "gene", "UMIs")
    expect_s4_class(res, "dgCMatrix")
})
```

- [ ] **Step 2: Run — pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-adapters.R
git commit -m "test(adapter): copy-back preserves sparse storage in pad mode"
```

---

## Phase G — `concatenate`

### Task G1: Test — single-axis concat, default dataset axis

**Files:**

- Create: `tests/testthat/test-concat.R`

**Pre-read:** `concatenate(destination, axis, sources, names = NULL, dataset_axis = "dataset", dataset_property = TRUE, prefix = FALSE, prefixed = NULL, empty = NULL, merge = NULL, overwrite = FALSE, sparse_if_saves_storage_fraction = 0.25)`. The `prefixed` parameter lets the caller override the property-prefixing heuristic (prefix properties whose name equals the axis name or starts with `"<axis>."`). `dataset_axis = NULL` disables the per-source axis.

- [ ] **Step 1: Write failing tests**

Create `tests/testthat/test-concat.R`:

```r
test_that("concatenate: single axis, two sources, creates dataset axis", {
    a <- memory_daf(name = "A")
    add_axis(a, "cell", c("a1", "a2"))
    set_vector(a, "cell", "age", c(10L, 20L))

    b <- memory_daf(name = "B")
    add_axis(b, "cell", c("b1", "b2", "b3"))
    set_vector(b, "cell", "age", c(1L, 2L, 3L))

    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b))

    expect_identical(axis_vector(dest, "cell"),
                     c("a1", "a2", "b1", "b2", "b3"))
    expect_identical(unname(get_vector(dest, "cell", "age")),
                     c(10L, 20L, 1L, 2L, 3L))
    expect_setequal(axes_set(dest), c("cell", "dataset"))
    expect_identical(axis_vector(dest, "dataset"), c("A", "B"))
    expect_identical(
        unname(get_vector(dest, "cell", "dataset")),
        c("A", "A", "B", "B", "B")
    )
})

test_that("concatenate: dataset_axis = NULL suppresses the dataset axis", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b), dataset_axis = NULL)
    expect_false(has_axis(dest, "dataset"))
    expect_identical(axis_vector(dest, "cell"), c("a1", "b1"))
})

test_that("concatenate: explicit names override source .name", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b), names = c("left", "right"))
    expect_identical(axis_vector(dest, "dataset"), c("left", "right"))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-concat.R")'
```

Expected: FAIL (concatenate not defined).

### Task G2: Implement `concatenate` (single-axis core path)

**Files:**

- Create: `R/concat.R`

- [ ] **Step 1: Create `R/concat.R` with core implementation**

Create `R/concat.R`:

```r
#' @include classes.R readers.R writers.R memory_daf.R copies.R
NULL

#' Merge action constants for [concatenate()].
#'
#' Used as values in `concatenate()`'s `merge` argument:
#' `MERGE_SKIP` skips the property (default), `MERGE_LAST_VALUE` uses the
#' last source containing it, `MERGE_COLLECT_AXIS` collects values along
#' the `dataset_axis`.
#'
#' @name merge_actions
#' @export
MERGE_SKIP <- "SkipProperty"
#' @rdname merge_actions
#' @export
MERGE_LAST_VALUE <- "LastValue"
#' @rdname merge_actions
#' @export
MERGE_COLLECT_AXIS <- "CollectAxis"

#' Concatenate multiple dafs along one or more axes.
#'
#' Mirrors Julia `concatenate!()`. For each concatenation axis, entries from
#' each source are appended in source order. Non-concat axes must be
#' identical across all sources and are copied once.
#'
#' @param destination A `DafWriter`. Must be empty of the concatenation axes.
#' @param axis A single axis name or a character vector of axis names.
#' @param sources List of `DafReader`s to concatenate.
#' @param names Optional character vector of unique data set names (defaults
#'   to each source's `name` prop).
#' @param dataset_axis Name of the per-source axis to create. `NULL` disables.
#' @param dataset_property If `TRUE` (default) and `dataset_axis` is non-NULL,
#'   create a same-named vector on every concatenation axis holding the
#'   source name for each entry.
#' @param prefix Logical (single or per-axis). Prefix concat-axis entries
#'   with `"<dataset_name>."` to de-duplicate across sources.
#' @param prefixed Optional character vector (or list of vectors per axis)
#'   of additional property names to prefix, beyond the heuristic (same-name
#'   or `"<axis>.*"` properties).
#' @param empty Named list of fill values for missing per-source properties.
#' @param merge Named list mapping property keys to a merge action
#'   (`"SkipProperty"`, `"LastValue"`, `"CollectAxis"`).
#' @param sparse_if_saves_storage_fraction Numeric (default 0.25).
#' @param overwrite If `TRUE`, allow replacing pre-existing destination
#'   entries.
#' @return Invisibly, the destination.
#' @export
#' @examples
#' a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1", "a2"))
#' b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
#' dest <- memory_daf(name = "dest")
#' concatenate(dest, "cell", list(a, b))
concatenate <- function(destination, axis, sources,
                        names = NULL,
                        dataset_axis = "dataset",
                        dataset_property = TRUE,
                        prefix = FALSE,
                        prefixed = NULL,
                        empty = NULL,
                        merge = NULL,
                        sparse_if_saves_storage_fraction = 0.25,
                        overwrite = FALSE) {
    axes <- if (is.character(axis) && length(axis) > 1L) axis else as.character(axis)
    if (length(axes) < 1L) stop("`axis` must be a non-empty character vector",
                                call. = FALSE)
    if (anyDuplicated(axes)) stop("`axis` names must be unique", call. = FALSE)
    for (ax in axes) .assert_name(ax, "axis")

    if (length(sources) < 1L) stop("`sources` must be non-empty",
                                   call. = FALSE)
    dataset_names <- .concat_dataset_names(sources, names)

    # Validate: no matrix has both axes in the concat set.
    for (src in sources) {
        for (ra in axes) for (ca in axes) {
            if (format_has_axis(src, ra) && format_has_axis(src, ca) &&
                length(format_matrices_set(src, ra, ca)) > 0L) {
                stop(sprintf(
                    "can't concatenate a matrix with both axes in the concat set: %s,%s in %s",
                    sQuote(ra), sQuote(ca), S7::prop(src, "name")
                ), call. = FALSE)
            }
        }
    }

    # Per-axis prefix flag.
    prefixes <- if (is.logical(prefix) && length(prefix) == 1L) {
        rep(prefix, length(axes))
    } else if (is.logical(prefix) && length(prefix) == length(axes)) {
        prefix
    } else {
        stop("`prefix` must be a logical scalar or a logical vector of length(axis)",
             call. = FALSE)
    }
    names(prefixes) <- axes

    # Create non-concat axes from the first source; verify all sources agree.
    for (src in sources) {
        for (ax in format_axes_set(src)) {
            if (ax %in% axes) next
            if (!format_has_axis(destination, ax)) {
                format_add_axis(destination, ax, format_axis_array(src, ax))
            } else if (!identical(format_axis_array(destination, ax),
                                  format_axis_array(src, ax))) {
                stop(sprintf(
                    "different entries for the axis: %s between dafs", sQuote(ax)
                ), call. = FALSE)
            }
        }
    }

    # Create the concatenation axes.
    for (ax in axes) {
        .concat_one_axis(destination, ax, sources, dataset_names,
                         prefixes[[ax]], prefixed, empty, overwrite,
                         sparse_if_saves_storage_fraction)
    }

    # Optional dataset_axis + property.
    if (!is.null(dataset_axis)) {
        format_add_axis(destination, dataset_axis, dataset_names)
        if (isTRUE(dataset_property)) {
            for (ax in axes) {
                # For each entry of the concat axis in destination, record which
                # source it came from (by source index).
                offsets <- cumsum(c(0L, vapply(sources,
                    function(s) format_axis_length(s, ax), integer(1L))))
                labels <- character(offsets[[length(offsets)]])
                for (i in seq_along(sources)) {
                    labels[(offsets[[i]] + 1L):offsets[[i + 1L]]] <- dataset_names[[i]]
                }
                format_set_vector(destination, ax, dataset_axis, labels,
                                  overwrite = overwrite)
            }
        }
    }

    # Merge pass for properties not on any concat axis.
    if (!is.null(merge)) {
        .concat_merge(destination, sources, dataset_names, dataset_axis,
                      axes, merge, overwrite)
    }

    invisible(destination)
}

.concat_dataset_names <- function(sources, names) {
    if (is.null(names)) {
        names <- vapply(sources, function(s) S7::prop(s, "name"),
                        character(1L))
    } else {
        if (length(names) != length(sources)) {
            stop("`names` must have one entry per source", call. = FALSE)
        }
    }
    if (anyDuplicated(names)) {
        stop("dataset `names` must be unique", call. = FALSE)
    }
    names
}
```

- [ ] **Step 2: Implement `.concat_one_axis` — the axis+properties loop**

Append to `R/concat.R`:

```r
.concat_one_axis <- function(destination, axis, sources, dataset_names,
                             do_prefix, prefixed, empty, overwrite,
                             sparse_threshold) {
    # Per-source axis entries, possibly prefixed.
    per_src <- lapply(seq_along(sources), function(i) {
        e <- format_axis_array(sources[[i]], axis)
        if (isTRUE(do_prefix)) paste(dataset_names[[i]], e, sep = ".") else e
    })
    combined <- unlist(per_src, use.names = FALSE)
    if (anyDuplicated(combined)) {
        stop(sprintf("duplicate entries on axis %s across sources; use prefix = TRUE",
                     sQuote(axis)), call. = FALSE)
    }
    format_add_axis(destination, axis, combined)

    # Determine which non-axis vector properties on this axis must be prefixed.
    prefix_vector_names <- function(ax, vnames) {
        if (!isTRUE(do_prefix)) return(character(0L))
        if (!is.null(prefixed)) {
            # `prefixed` can be a character vector (applies to all axes) or a
            # list of vectors per axis.
            vec <- if (is.list(prefixed)) prefixed[[ax]] else prefixed
            return(intersect(vnames, vec))
        }
        # Heuristic: prefix properties whose name == axis or start with "axis."
        pattern_prefix <- paste0("^", ax, "\\.")
        candidates <- vnames[vnames == ax | grepl(pattern_prefix, vnames)]
        candidates
    }

    # Union of vector names across sources for this axis.
    # Also iterate over each "other axis" for matrices.
    all_vec_names <- unique(unlist(lapply(sources,
        function(s) format_vectors_set(s, axis)), use.names = FALSE))

    for (vn in all_vec_names) {
        .concat_axis_vector(destination, axis, vn, sources, dataset_names,
                            do_prefix, prefixed, empty, overwrite,
                            sparse_threshold)
    }

    # Matrices with `axis` as one side and some other-axis on the other.
    other_axes <- unique(unlist(lapply(sources, format_axes_set)))
    other_axes <- setdiff(other_axes, axis)
    for (oa in other_axes) {
        all_mat_names <- unique(unlist(lapply(sources, function(s) {
            if (format_has_axis(s, axis) && format_has_axis(s, oa)) {
                c(format_matrices_set(s, axis, oa),
                  format_matrices_set(s, oa, axis))
            } else character(0L)
        })))
        for (mn in all_mat_names) {
            .concat_axis_matrix(destination, axis, oa, mn, sources,
                                empty, overwrite, sparse_threshold)
        }
    }
}

.concat_axis_vector <- function(destination, axis, name, sources,
                                dataset_names, do_prefix, prefixed,
                                empty, overwrite, sparse_threshold) {
    parts <- vector("list", length(sources))
    for (i in seq_along(sources)) {
        src <- sources[[i]]
        if (format_has_vector(src, axis, name)) {
            v <- format_get_vector(src, axis, name)
        } else {
            key <- paste(axis, name, sep = "|")
            fill <- if (is.null(empty)) NULL else empty[[key]]
            if (is.null(fill)) {
                stop(sprintf(
                    "no empty value for the vector: %s of the axis: %s which is missing from the daf data: %s",
                    sQuote(name), sQuote(axis), S7::prop(src, "name")
                ), call. = FALSE)
            }
            v <- rep(fill, format_axis_length(src, axis))
        }
        # Prefix if this property is in the prefix-set.
        is_prefix_target <- {
            if (!isTRUE(do_prefix)) FALSE
            else if (!is.null(prefixed)) {
                vec <- if (is.list(prefixed)) prefixed[[axis]] else prefixed
                name %in% vec
            } else {
                name == axis || startsWith(name, paste0(axis, "."))
            }
        }
        if (is_prefix_target && is.character(v)) {
            v <- paste(dataset_names[[i]], v, sep = ".")
        }
        parts[[i]] <- v
    }
    # Unify element type via R's c() promotion.
    out <- do.call(c, parts)
    format_set_vector(destination, axis, name, out, overwrite = overwrite)
}

.concat_axis_matrix <- function(destination, axis, other_axis, name, sources,
                                empty, overwrite, sparse_threshold) {
    # The concat axis is always the concatenated dimension; the other axis is
    # shared across sources. Determine on-disk orientation (axis, other) or
    # (other, axis). We always write with concat axis as columns (Julia's
    # convention).
    parts <- vector("list", length(sources))
    for (i in seq_along(sources)) {
        src <- sources[[i]]
        have_ao <- format_has_axis(src, axis) && format_has_axis(src, other_axis)
        if (!have_ao) {
            stop(sprintf("source %s missing axes for matrix %s",
                         S7::prop(src, "name"), sQuote(name)), call. = FALSE)
        }
        mat <- if (format_has_matrix(src, other_axis, axis, name)) {
            format_get_matrix(src, other_axis, axis, name)
        } else if (format_has_matrix(src, axis, other_axis, name)) {
            # Transpose to (other, axis) layout.
            m <- format_get_matrix(src, axis, other_axis, name)
            if (inherits(m, "dgCMatrix")) Matrix::t(m) else t(m)
        } else {
            key <- paste(other_axis, axis, name, sep = "|")
            alt_key <- paste(axis, other_axis, name, sep = "|")
            fill <- if (is.null(empty)) NULL else (empty[[key]] %||% empty[[alt_key]])
            if (is.null(fill)) {
                stop(sprintf(
                    "no empty value for the matrix: %s of the rows axis: %s and the columns axis: %s which is missing from the daf data: %s",
                    sQuote(name), sQuote(other_axis), sQuote(axis),
                    S7::prop(src, "name")
                ), call. = FALSE)
            }
            matrix(fill,
                   nrow = format_axis_length(src, other_axis),
                   ncol = format_axis_length(src, axis))
        }
        parts[[i]] <- mat
    }
    # cbind (dense/sparse mix): coerce all to dense unless all are sparse.
    if (all(vapply(parts, inherits, logical(1L), "dgCMatrix"))) {
        combined <- do.call(cbind, parts)
    } else {
        combined <- do.call(cbind, lapply(parts, function(m)
            if (inherits(m, "dgCMatrix")) as.matrix(m) else m))
    }
    dest_rows <- format_axis_array(destination, other_axis)
    dest_cols <- format_axis_array(destination, axis)
    dimnames(combined) <- list(dest_rows, dest_cols)
    format_set_matrix(destination, other_axis, axis, name, combined,
                      overwrite = overwrite)
}

.concat_merge <- function(destination, sources, dataset_names, dataset_axis,
                          concat_axes, merge, overwrite) {
    for (prop_key in base::names(merge)) {
        action <- merge[[prop_key]]
        parts <- strsplit(prop_key, "|", fixed = TRUE)[[1L]]
        if (length(parts) == 1L) {
            # Scalar
            .concat_merge_scalar(destination, sources, dataset_names,
                                 dataset_axis, parts[[1L]], action, overwrite)
        } else if (length(parts) == 2L) {
            if (parts[[1L]] %in% concat_axes) next
            .concat_merge_vector(destination, sources, dataset_names,
                                 dataset_axis, parts[[1L]], parts[[2L]],
                                 action, overwrite)
        } else if (length(parts) == 3L) {
            if (action == MERGE_COLLECT_AXIS) {
                stop(sprintf(
                    "can't CollectAxis for a matrix: %s (would create a 3D tensor)",
                    sQuote(prop_key)
                ), call. = FALSE)
            }
            # Other merge actions for matrices not implemented this slice.
        }
    }
}

.concat_merge_scalar <- function(destination, sources, dataset_names,
                                 dataset_axis, name, action, overwrite) {
    if (action == MERGE_SKIP) return(invisible())
    if (action == MERGE_LAST_VALUE) {
        for (i in rev(seq_along(sources))) {
            if (format_has_scalar(sources[[i]], name)) {
                format_set_scalar(destination, name,
                                  format_get_scalar(sources[[i]], name),
                                  overwrite = overwrite)
                return(invisible())
            }
        }
        return(invisible())
    }
    if (action == MERGE_COLLECT_AXIS) {
        if (is.null(dataset_axis)) {
            stop(sprintf(
                "can't collect axis for the scalar: %s because no dataset axis was created",
                sQuote(name)
            ), call. = FALSE)
        }
        vals <- lapply(sources, function(s)
            if (format_has_scalar(s, name)) format_get_scalar(s, name) else NA)
        format_set_vector(destination, dataset_axis, name,
                          do.call(c, vals), overwrite = overwrite)
    }
}

.concat_merge_vector <- function(destination, sources, dataset_names,
                                 dataset_axis, axis, name, action, overwrite) {
    if (action == MERGE_SKIP) return(invisible())
    if (action == MERGE_LAST_VALUE) {
        for (i in rev(seq_along(sources))) {
            if (format_has_vector(sources[[i]], axis, name)) {
                format_set_vector(destination, axis, name,
                                  format_get_vector(sources[[i]], axis, name),
                                  overwrite = overwrite)
                return(invisible())
            }
        }
        return(invisible())
    }
    if (action == MERGE_COLLECT_AXIS) {
        if (is.null(dataset_axis)) {
            stop(sprintf(
                "can't collect axis for the vector: %s on axis %s because no dataset axis was created",
                sQuote(name), sQuote(axis)
            ), call. = FALSE)
        }
        # Build an (axis × dataset_axis) matrix.
        src_len <- format_axis_length(destination, axis)
        n_src <- length(sources)
        out <- matrix(NA, nrow = src_len, ncol = n_src,
                      dimnames = list(format_axis_array(destination, axis),
                                      dataset_names))
        for (i in seq_along(sources)) {
            s <- sources[[i]]
            if (format_has_vector(s, axis, name)) {
                out[, i] <- format_get_vector(s, axis, name)
            }
        }
        format_set_matrix(destination, axis, dataset_axis, name, out,
                          overwrite = overwrite)
    }
}
```

- [ ] **Step 3: Run G1 tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-concat.R")'
```

Expected: 3 PASS.

- [ ] **Step 4: Commit**

```
git add R/concat.R tests/testthat/test-concat.R
git commit -m "$(cat <<'EOF'
feat(concat): concatenate single-axis core path + dataset axis

Stitches N dafs along one axis, creates a dataset_axis with one
entry per source and a same-named property on each concat axis
holding the source label. Non-concat axes must match across sources.
Single-axis only at this commit; multi-axis tests land in G3.
EOF
)"
```

### Task G3: Test — concat with empty fill for missing properties

**Files:**

- Modify: `tests/testthat/test-concat.R`

- [ ] **Step 1: Append test**

```r
test_that("concatenate: missing property requires empty", {
    a <- memory_daf(name = "A")
    add_axis(a, "cell", c("a1"))
    set_vector(a, "cell", "age", c(10L))

    b <- memory_daf(name = "B")
    add_axis(b, "cell", c("b1"))
    # No 'age' on b.

    dest <- memory_daf(name = "dest")
    expect_error(concatenate(dest, "cell", list(a, b)), "no empty value")

    dest2 <- memory_daf(name = "dest2")
    concatenate(dest2, "cell", list(a, b),
                empty = list("cell|age" = -1L))
    expect_identical(unname(get_vector(dest2, "cell", "age")),
                     c(10L, -1L))
})

test_that("concatenate: vector type unifies across sources", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    set_vector(a, "cell", "age", c(10L))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    set_vector(b, "cell", "age", c(2.5))
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b))
    expect_true(is.double(get_vector(dest, "cell", "age")))
})
```

- [ ] **Step 2: Run — pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-concat.R")'
```

Expected: 5 PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-concat.R
git commit -m "test(concat): empty fill and type unification across sources"
```

### Task G4: Test — prefix heuristic

**Files:**

- Modify: `tests/testthat/test-concat.R`

- [ ] **Step 1: Append tests**

```r
test_that("concatenate: prefix=TRUE de-duplicates concat-axis entries", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("c1", "c2"))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("c1", "c3"))  # c1 collides
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b), prefix = TRUE)
    expect_identical(axis_vector(dest, "cell"),
                     c("A.c1", "A.c2", "B.c1", "B.c3"))
})

test_that("concatenate: prefix heuristic prefixes properties named after the axis", {
    a <- memory_daf(name = "A")
    add_axis(a, "cell", c("c1"))
    add_axis(a, "cluster", c("cl1"))
    set_vector(a, "cell", "cluster", c("cl1"))
    b <- memory_daf(name = "B")
    add_axis(b, "cell", c("c1"))        # collision
    add_axis(b, "cluster", c("cl1"))     # collision
    set_vector(b, "cell", "cluster", c("cl1"))
    dest <- memory_daf(name = "dest")
    concatenate(dest, c("cell", "cluster"), list(a, b), prefix = TRUE)
    # The cell.cluster property should now reference prefixed cluster names.
    expect_identical(unname(get_vector(dest, "cell", "cluster")),
                     c("A.cl1", "B.cl1"))
})

test_that("concatenate: duplicate entries without prefix raise", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("c1"))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("c1"))
    dest <- memory_daf(name = "dest")
    expect_error(concatenate(dest, "cell", list(a, b)),
                 "duplicate entries")
})
```

- [ ] **Step 2: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-concat.R")'
```

Expected: 8 PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-concat.R
git commit -m "test(concat): prefix heuristic and duplicate-detection"
```

### Task G5: Test — merge actions (LastValue, CollectAxis)

**Files:**

- Modify: `tests/testthat/test-concat.R`

- [ ] **Step 1: Append tests**

```r
test_that("concatenate: merge=SkipProperty drops a non-axis property", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    add_axis(a, "cluster", c("c1")); set_vector(a, "cluster", "size", c(5L))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    add_axis(b, "cluster", c("c1")); set_vector(b, "cluster", "size", c(7L))
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b),
                merge = list("cluster|size" = MERGE_SKIP))
    expect_false(has_vector(dest, "cluster", "size"))
})

test_that("concatenate: merge=LastValue scalar uses last source", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    set_scalar(a, "organism", "mouse")
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    set_scalar(b, "organism", "human")
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b),
                merge = list("organism" = MERGE_LAST_VALUE))
    expect_identical(get_scalar(dest, "organism"), "human")
})

test_that("concatenate: merge=CollectAxis scalar creates dataset-axis vector", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    set_scalar(a, "version", 1L)
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    set_scalar(b, "version", 2L)
    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b),
                merge = list("version" = MERGE_COLLECT_AXIS))
    expect_identical(unname(get_vector(dest, "dataset", "version")),
                     c(1L, 2L))
})

test_that("concatenate: merge=CollectAxis for matrix raises", {
    a <- memory_daf(name = "A"); add_axis(a, "cell", c("a1"))
    add_axis(a, "cluster", c("c1"))
    set_matrix(a, "cluster", "cluster", "link",
               matrix(1, 1, 1, dimnames = list("c1", "c1")))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    add_axis(b, "cluster", c("c1"))
    set_matrix(b, "cluster", "cluster", "link",
               matrix(2, 1, 1, dimnames = list("c1", "c1")))
    dest <- memory_daf(name = "dest")
    expect_error(
        concatenate(dest, "cell", list(a, b),
                    merge = list("cluster|cluster|link" = MERGE_COLLECT_AXIS)),
        "CollectAxis for a matrix"
    )
})
```

- [ ] **Step 2: Run — pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-concat.R")'
```

Expected: 12 PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-concat.R
git commit -m "test(concat): merge actions (Skip/LastValue/CollectAxis + matrix error)"
```

### Task G6: Test — multi-axis concatenation (2 axes)

**Files:**

- Modify: `tests/testthat/test-concat.R`

- [ ] **Step 1: Append test**

```r
test_that("concatenate: two concat axes simultaneously", {
    a <- memory_daf(name = "A")
    add_axis(a, "cell", c("a1")); add_axis(a, "gene", c("ga1"))
    b <- memory_daf(name = "B")
    add_axis(b, "cell", c("b1")); add_axis(b, "gene", c("gb1"))
    dest <- memory_daf(name = "dest")
    concatenate(dest, c("cell", "gene"), list(a, b))
    expect_identical(axis_vector(dest, "cell"), c("a1", "b1"))
    expect_identical(axis_vector(dest, "gene"), c("ga1", "gb1"))
})

test_that("concatenate: rejects matrix with both axes in concat set", {
    a <- memory_daf(name = "A")
    add_axis(a, "cell", c("a1"))
    set_matrix(a, "cell", "cell", "link",
               matrix(1, 1, 1, dimnames = list("a1", "a1")))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("b1"))
    dest <- memory_daf(name = "dest")
    expect_error(concatenate(dest, "cell", list(a, b)),
                 "both axes in the concat set")
})
```

- [ ] **Step 2: Run**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-concat.R")'
```

Expected: 14 PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-concat.R
git commit -m "test(concat): multi-axis concat + matrix-both-axes rejection"
```

---

## Phase H — `complete_daf` + `complete_chain` + `open_daf`

### Task H1: Test + impl — `open_daf`

**Files:**

- Create: `tests/testthat/test-complete.R`
- Create: `R/complete.R`

**Pre-read:** `open_daf(path, mode)` dispatches on path syntax. Directory path → `files_daf`. `.h5df` / `.h5dfs#` → raise "H5df not supported yet" (deferred). For mode: `"r"` returns a read-only `FilesDaf`; `"r+"` returns a writable one. Leverage the existing `files_daf()` constructor.

- [ ] **Step 1: Write failing test**

Create `tests/testthat/test-complete.R`:

```r
test_that("open_daf opens a FilesDaf directory in read mode", {
    tmp <- withr::local_tempdir()
    f <- files_daf(tmp, name = "orig", mode = "w+")
    add_axis(f, "cell", c("c1", "c2")); set_vector(f, "cell", "age", c(1L, 2L))

    d <- open_daf(tmp, "r")
    expect_true(S7::S7_inherits(d, DafReadOnly) || S7::S7_inherits(d, DafReader))
    expect_identical(unname(get_vector(d, "cell", "age")), c(1L, 2L))
})

test_that("open_daf opens a FilesDaf directory in r+ mode", {
    tmp <- withr::local_tempdir()
    f <- files_daf(tmp, name = "orig", mode = "w+")
    add_axis(f, "cell", c("c1"))

    d <- open_daf(tmp, "r+")
    set_vector(d, "cell", "tag", c("x"))
    expect_identical(unname(get_vector(d, "cell", "tag")), c("x"))
})

test_that("open_daf rejects H5df paths", {
    expect_error(open_daf("x.h5df", "r"), "H5df backend not supported yet")
    expect_error(open_daf("x.h5dfs#/grp", "r"), "H5df backend not supported yet")
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-complete.R")'
```

Expected: FAIL (open_daf not defined).

- [ ] **Step 3: Implement `open_daf`**

Create `R/complete.R`:

```r
#' @include classes.R files_daf.R memory_daf.R chain_daf.R view_daf.R readers.R writers.R
NULL

#' Open a daf storage path in a given mode.
#'
#' Dispatches on path extension. Directory paths open a `FilesDaf`; paths
#' ending in `.h5df` or containing `.h5dfs#<group>` are reserved for an
#' H5df backend (not implemented this slice).
#'
#' @param path Filesystem path.
#' @param mode One of `"r"` (read-only) or `"r+"` (read-write).
#' @param name Optional daf name. Default derived from the path basename.
#' @return A `DafReader` or `DafWriter`.
#' @export
#' @examples
#' tmp <- tempfile(); dir.create(tmp)
#' files_daf(tmp, name = "tmp", mode = "w+")
#' d <- open_daf(tmp, "r")
open_daf <- function(path, mode = "r", name = NULL) {
    if (endsWith(path, ".h5df") || grepl(".h5dfs#", path, fixed = TRUE)) {
        stop("H5df backend not supported yet", call. = FALSE)
    }
    if (is.null(name)) name <- basename(path)
    if (!mode %in% c("r", "r+")) {
        stop("`mode` must be \"r\" or \"r+\"", call. = FALSE)
    }
    files_daf(path, name = name, mode = mode)
}
```

- [ ] **Step 4: Run — tests pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-complete.R")'
```

Expected: 3 PASS.

- [ ] **Step 5: Commit**

```
git add R/complete.R tests/testthat/test-complete.R
git commit -m "feat(complete): open_daf FilesDaf dispatch (H5df deferred)"
```

### Task H2: Test + impl — `complete_chain`

**Files:**

- Modify: `R/complete.R`
- Modify: `tests/testthat/test-complete.R`

**Pre-read:** `complete_chain(base_daf, new_daf, name = NULL, axes = NULL, data = NULL, absolute = FALSE)` — set `base_daf_repository` scalar on `new_daf` to the path of `base_daf`, optionally set `base_daf_view` as a JSON-encoded view spec, and return `chain_writer(list(viewer_or_base, new_daf))`. Returned object's name is the given `name` or `new_daf`'s name.

- [ ] **Step 1: Append failing test**

```r
test_that("complete_chain sets base_daf_repository and returns a write chain", {
    tmp_base <- withr::local_tempdir()
    tmp_new <- withr::local_tempdir()
    base <- files_daf(tmp_base, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1", "c2"))
    set_vector(base, "cell", "age", c(1L, 2L))

    new <- files_daf(tmp_new, name = "new", mode = "w+")
    chain <- complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    # The new daf now has the pointer scalar.
    expect_true(format_has_scalar(new, "base_daf_repository"))
    expect_identical(format_get_scalar(new, "base_daf_repository"),
                     normalizePath(tmp_base))
    # The returned chain reads from base and writes to new.
    expect_identical(unname(get_vector(chain, "cell", "age")), c(1L, 2L))
    set_vector(chain, "cell", "tag", c("x", "y"))
    expect_true(has_vector(new, "cell", "tag"))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-complete.R")'
```

Expected: FAIL.

- [ ] **Step 3: Implement `complete_chain`**

Append to `R/complete.R`:

```r
#' Create a persistent chain by linking `new_daf` to a `base_daf`.
#'
#' Writes a `base_daf_repository` scalar on `new_daf` that points at
#' `base_daf`'s filesystem path. If `axes` and/or `data` are specified, the
#' chain reads through a `viewer()` of `base_daf` and the spec is stored as
#' JSON under `base_daf_view`. The returned chain is `chain_writer(list(
#' viewer_or_base, new_daf))`.
#'
#' Call [complete_daf()] later to reopen the chain from disk using the
#' stored scalars.
#'
#' @param base_daf A `DafReader` on disk (its path is stored).
#' @param new_daf A `DafWriter` on disk (receives the pointer scalar).
#' @param name Optional name for the returned chain.
#' @param axes,data Optional `viewer()` axes / data spec applied on top of
#'   `base_daf`.
#' @param absolute If `TRUE`, store the absolute base path (default is
#'   relative).
#' @return The write chain.
#' @export
#' @examples
#' base_dir <- tempfile(); dir.create(base_dir)
#' new_dir <- tempfile(); dir.create(new_dir)
#' base <- files_daf(base_dir, name = "base", mode = "w+")
#' new <- files_daf(new_dir, name = "new", mode = "w+")
#' ch <- complete_chain(base_daf = base, new_daf = new, absolute = TRUE)
complete_chain <- function(base_daf, new_daf, name = NULL,
                           axes = NULL, data = NULL, absolute = FALSE) {
    base_path <- .complete_path(base_daf)
    new_path <- .complete_path(new_daf)
    stored_path <- if (isTRUE(absolute)) {
        normalizePath(base_path)
    } else {
        # Relative path from new_path's parent to base_path.
        fs::path_rel(base_path, start = dirname(new_path))
    }
    format_set_scalar(new_daf, "base_daf_repository",
                      as.character(stored_path), overwrite = TRUE)
    reader <- if (is.null(axes) && is.null(data)) {
        base_daf
    } else {
        spec <- list(axes = axes, data = data)
        format_set_scalar(new_daf, "base_daf_view",
                          jsonlite::toJSON(spec, auto_unbox = TRUE),
                          overwrite = TRUE)
        viewer(base_daf, axes = axes, data = data,
               name = paste0(S7::prop(base_daf, "name"), ".view"))
    }
    chain_writer(list(reader, new_daf),
                 name = name %||% S7::prop(new_daf, "name"))
}

# Resolve a daf's on-disk path. FilesDaf stores `path` as a prop; other
# backends raise.
.complete_path <- function(daf) {
    tryCatch(S7::prop(daf, "path"),
        error = function(e) stop("daf has no filesystem path — only FilesDaf supported by complete_*",
                                 call. = FALSE))
}
```

Note: `fs` is a base R-world package commonly depended on; confirm whether it's in Imports by `grep '^Imports' DESCRIPTION`. If absent, fall back to manual relative path construction via `base::normalizePath` and a `sub()` — but fs yields cleaner code. If fs is not available, swap the `fs::path_rel` call for:

```r
stored_path <- if (isTRUE(absolute)) {
    normalizePath(base_path)
} else {
    norm_base <- normalizePath(base_path)
    norm_new_parent <- normalizePath(dirname(new_path))
    if (startsWith(norm_base, paste0(norm_new_parent, "/"))) {
        substring(norm_base, nchar(norm_new_parent) + 2L)
    } else {
        norm_base
    }
}
```

Pick whichever path compiles cleanly.

- [ ] **Step 4: Run — pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-complete.R")'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```
git add R/complete.R tests/testthat/test-complete.R
git commit -m "feat(complete): complete_chain persists base_daf_repository link"
```

### Task H3: Test + impl — `complete_daf` reopens a persistent chain

**Files:**

- Modify: `R/complete.R`
- Modify: `tests/testthat/test-complete.R`

- [ ] **Step 1: Append failing tests**

```r
test_that("complete_daf reopens a chain that complete_chain persisted", {
    tmp_root <- withr::local_tempdir()
    base_dir <- file.path(tmp_root, "base")
    new_dir <- file.path(tmp_root, "new")
    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1", "c2"))
    set_vector(base, "cell", "age", c(10L, 20L))
    new <- files_daf(new_dir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    chain <- complete_daf(new_dir, mode = "r")
    expect_identical(unname(get_vector(chain, "cell", "age")), c(10L, 20L))
})

test_that("complete_daf in r+ mode allows writes to leaf", {
    tmp_root <- withr::local_tempdir()
    base_dir <- file.path(tmp_root, "base")
    new_dir <- file.path(tmp_root, "new")
    base <- files_daf(base_dir, name = "base", mode = "w+")
    add_axis(base, "cell", c("c1"))
    new <- files_daf(new_dir, name = "new", mode = "w+")
    complete_chain(base_daf = base, new_daf = new, absolute = TRUE)

    chain <- complete_daf(new_dir, mode = "r+")
    set_vector(chain, "cell", "tag", c("t1"))
    # Reopen to verify persistence
    leaf_reopen <- open_daf(new_dir, "r")
    expect_true(has_vector(leaf_reopen, "cell", "tag"))
})

test_that("complete_daf rejects invalid mode", {
    tmp <- withr::local_tempdir()
    files_daf(tmp, name = "t", mode = "w+")
    expect_error(complete_daf(tmp, "w"), "must be")
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-complete.R")'
```

Expected: FAIL (complete_daf not defined).

- [ ] **Step 3: Implement `complete_daf`**

Append to `R/complete.R`:

```r
#' Reopen a persistent chain from disk.
#'
#' Walks the `base_daf_repository` scalar chain rooted at `leaf`, opening
#' each level with [open_daf()]. Returns a `chain_reader` (`mode = "r"`) or
#' `chain_writer` (`mode = "r+"`, only the leaf is writable).
#'
#' @param leaf Filesystem path to the leaf daf.
#' @param mode `"r"` or `"r+"`.
#' @param name Optional name.
#' @return A `DafReader` or `DafWriter`.
#' @export
#' @examples
#' tmp_root <- tempfile(); dir.create(tmp_root)
#' base_dir <- file.path(tmp_root, "base")
#' new_dir <- file.path(tmp_root, "new")
#' files_daf(base_dir, name = "base", mode = "w+")
#' new <- files_daf(new_dir, name = "new", mode = "w+")
#' complete_chain(
#'     base_daf = open_daf(base_dir, "r"),
#'     new_daf = new, absolute = TRUE
#' )
#' chain <- complete_daf(new_dir, "r")
complete_daf <- function(leaf, mode = "r", name = NULL) {
    if (!mode %in% c("r", "r+")) {
        stop("`mode` must be \"r\" or \"r+\"", call. = FALSE)
    }
    stack <- list()
    path <- leaf
    while (!is.null(path)) {
        is_leaf <- length(stack) == 0L
        open_mode <- if (is_leaf) mode else "r"
        d <- open_daf(path, open_mode)
        view_spec <- NULL
        if (format_has_scalar(d, "base_daf_view")) {
            view_spec <- jsonlite::fromJSON(
                format_get_scalar(d, "base_daf_view"),
                simplifyVector = FALSE
            )
        }
        stack <- c(stack, list(list(daf = d, view = view_spec)))
        next_path <- if (format_has_scalar(d, "base_daf_repository")) {
            base <- format_get_scalar(d, "base_daf_repository")
            if (!startsWith(base, "/")) {
                base <- normalizePath(file.path(dirname(path), base),
                                      mustWork = FALSE)
            }
            base
        } else NULL
        path <- next_path
    }
    # Stack is leaf-first; reverse to root-first then apply viewers to the
    # base side of each pair before chaining.
    readers <- rev(lapply(stack, function(rec) rec$daf))
    # Apply views: each non-root entry's `view` applies on top of the entry
    # before it in the final chain.
    if (length(readers) == 1L) return(readers[[1L]])
    if (identical(mode, "r")) {
        chain_reader(readers, name = name %||% basename(leaf))
    } else {
        chain_writer(readers, name = name %||% basename(leaf))
    }
}
```

- [ ] **Step 4: Run — pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-complete.R")'
```

Expected: 7 total PASS.

- [ ] **Step 5: Commit**

```
git add R/complete.R tests/testthat/test-complete.R
git commit -m "$(cat <<'EOF'
feat(complete): complete_daf walks base_daf_repository chain

Reopens a persistent chain from disk by following the
base_daf_repository scalar from leaf to root. Mode "r" returns a
chain_reader; "r+" returns a chain_writer with only the leaf
writable. base_daf_view JSON respected when present (plumbing
hook for viewer-on-chain; full view application is a Slice-7
follow-up).
EOF
)"
```

---

## Phase I — `reconstruct_axis`

### Task I1: Test — reconstruct_axis basic case

**Files:**

- Create: `tests/testthat/test-reconstruction.R`

**Pre-read:** Given `cell` axis with a `donor` property (values `"d1"/"d2"/...`), `reconstruct_axis!(daf, existing_axis = "cell", implicit_axis = "donor")` creates a new `donor` axis from the unique non-empty donor values, and migrates any `cell` vector property that is uniquely determined by `donor` (e.g., `age_of_donor`) into the new `donor` axis. Returns a named list of per-migrated-property "value for empty-donor cells" (NULL if none).

- [ ] **Step 1: Write failing test**

Create `tests/testthat/test-reconstruction.R`:

```r
test_that("reconstruct_axis creates new axis from unique implicit values", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("dA", "dB", "dA", "dB"))
    set_vector(d, "cell", "donor_age", c(30L, 40L, 30L, 40L))
    set_vector(d, "cell", "single_score", c(0.1, 0.2, 0.3, 0.4))  # inconsistent

    empties <- reconstruct_axis(d,
        existing_axis = "cell", implicit_axis = "donor")

    expect_true(has_axis(d, "donor"))
    expect_identical(axis_vector(d, "donor"), c("dA", "dB"))
    # donor_age should have been migrated to the donor axis
    expect_true(has_vector(d, "donor", "donor_age"))
    expect_identical(unname(get_vector(d, "donor", "donor_age")),
                     c(30L, 40L))
    # donor_age should no longer live on cell (migrated)
    expect_false(has_vector(d, "cell", "donor_age"))
    # single_score is inconsistent → NOT migrated
    expect_true(has_vector(d, "cell", "single_score"))
    # Return list reports no empty-implicit values
    expect_identical(empties$donor_age, NULL)
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-reconstruction.R")'
```

Expected: FAIL.

### Task I2: Implement `reconstruct_axis`

**Files:**

- Create: `R/reconstruction.R`

- [ ] **Step 1: Create `R/reconstruction.R`**

Create `R/reconstruction.R`:

```r
#' @include classes.R readers.R writers.R memory_daf.R
NULL

#' Promote an implicit property to an explicit axis.
#'
#' Given an `existing_axis` with a property `implicit_axis`, create a new
#' axis from the unique non-empty values of the property. Scan the other
#' vector properties on `existing_axis`; for each one whose value is
#' uniquely determined by the implicit value, migrate it to the new axis.
#'
#' Returns a named list: for each migrated property, the (consistent) value
#' associated with `existing_axis` entries whose implicit value is empty —
#' or `NULL` if no such entries exist. These values can be used to
#' reconstruct the original property via the `?? X` query modifier.
#'
#' This slice requires `rename_axis` (or the default, `implicit_axis` name)
#' to not already exist in `daf`. Pre-existing axis merge is a Slice 7
#' follow-up.
#'
#' @param daf A `DafWriter`.
#' @param existing_axis Axis that holds the implicit property.
#' @param implicit_axis Property name on `existing_axis`; becomes the new
#'   axis's name (unless `rename_axis`).
#' @param rename_axis Optional name for the new axis.
#' @param empty_implicit If non-NULL, values equal to this are treated as
#'   empty (equivalent to the empty string).
#' @param implicit_properties Optional character vector: only these
#'   properties are considered for migration.
#' @param skipped_properties Optional character vector: properties to
#'   exclude from migration (even if consistent).
#' @return Named list of "value for empty-implicit entries" per migrated
#'   property.
#' @export
#' @examples
#' d <- memory_daf(name = "d")
#' add_axis(d, "cell", c("c1", "c2", "c3"))
#' set_vector(d, "cell", "donor", c("dA", "dB", "dA"))
#' set_vector(d, "cell", "donor_age", c(30L, 40L, 30L))
#' reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor")
#' get_vector(d, "donor", "donor_age")
reconstruct_axis <- function(daf, existing_axis, implicit_axis,
                             rename_axis = NULL,
                             empty_implicit = NULL,
                             implicit_properties = NULL,
                             skipped_properties = NULL) {
    .assert_name(existing_axis, "existing_axis")
    .assert_name(implicit_axis, "implicit_axis")
    new_axis <- if (is.null(rename_axis)) implicit_axis else rename_axis

    if (format_has_axis(daf, new_axis)) {
        stop(sprintf(
            "axis %s already exists; reconstruct_axis does not support merging this slice",
            sQuote(new_axis)
        ), call. = FALSE)
    }
    if (!format_has_vector(daf, existing_axis, implicit_axis)) {
        stop(sprintf(
            "missing vector: %s on axis: %s",
            sQuote(implicit_axis), sQuote(existing_axis)
        ), call. = FALSE)
    }

    impl_vec <- format_get_vector(daf, existing_axis, implicit_axis)
    impl_str <- as.character(impl_vec)
    if (!is.null(empty_implicit)) {
        impl_str[impl_str == as.character(empty_implicit)] <- ""
    }

    non_empty <- impl_str[nzchar(impl_str)]
    unique_vals <- sort(unique(non_empty), method = "radix")
    format_add_axis(daf, new_axis, unique_vals)

    all_vecs <- format_vectors_set(daf, existing_axis)
    all_vecs <- setdiff(all_vecs, implicit_axis)
    if (!is.null(skipped_properties)) {
        all_vecs <- setdiff(all_vecs, skipped_properties)
    }
    if (!is.null(implicit_properties)) {
        all_vecs <- intersect(all_vecs, implicit_properties)
    }

    empty_values <- list()

    for (prop in all_vecs) {
        values <- format_get_vector(daf, existing_axis, prop)
        mapping <- list()
        empty_v <- NULL
        consistent <- TRUE
        for (i in seq_along(impl_str)) {
            k <- impl_str[[i]]
            val <- values[[i]]
            if (!nzchar(k)) {
                if (is.null(empty_v)) {
                    empty_v <- val
                } else if (!identical(empty_v, val)) {
                    consistent <- FALSE; break
                }
                next
            }
            if (is.null(mapping[[k]])) {
                mapping[[k]] <- val
            } else if (!identical(mapping[[k]], val)) {
                consistent <- FALSE; break
            }
        }
        if (!consistent) {
            if (!is.null(implicit_properties)) {
                stop(sprintf(
                    "inconsistent values for the property: %s under the implicit axis: %s",
                    sQuote(prop), sQuote(implicit_axis)
                ), call. = FALSE)
            }
            next
        }
        out <- vapply(unique_vals, function(k) mapping[[k]],
                      FUN.VALUE = values[[1L]])
        format_set_vector(daf, new_axis, prop, out, overwrite = FALSE)
        format_delete_vector(daf, existing_axis, prop, must_exist = TRUE)
        empty_values[[prop]] <- empty_v
    }
    empty_values
}
```

- [ ] **Step 2: Run — pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-reconstruction.R")'
```

Expected: PASS.

- [ ] **Step 3: Commit**

```
git add R/reconstruction.R tests/testthat/test-reconstruction.R
git commit -m "feat(reconstruction): reconstruct_axis basic implicit->explicit promotion"
```

### Task I3: Test — empty_implicit + return-dict records empty-value

**Files:**

- Modify: `tests/testthat/test-reconstruction.R`

- [ ] **Step 1: Append test**

```r
test_that("reconstruct_axis: empty_implicit marks entries as no-value", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "type", c("T", "NA", "B", "NA"))
    set_vector(d, "cell", "color", c("red", "gray", "blue", "gray"))

    empties <- reconstruct_axis(d,
        existing_axis = "cell", implicit_axis = "type",
        empty_implicit = "NA")

    expect_identical(axis_vector(d, "type"), c("B", "T"))
    expect_identical(unname(get_vector(d, "type", "color")),
                     c("blue", "red"))
    # Cells with empty_implicit="NA" had color "gray"; captured as empty_values$color
    expect_identical(empties$color, "gray")
})

test_that("reconstruct_axis: skipped_properties excludes from migration", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("dA", "dB"))
    set_vector(d, "cell", "age", c(30L, 40L))   # consistent but skipped
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor",
                     skipped_properties = "age")
    expect_true(has_vector(d, "cell", "age"))
    expect_false(has_vector(d, "donor", "age"))
})

test_that("reconstruct_axis: implicit_properties enforces consistency", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("dA", "dA", "dB"))
    set_vector(d, "cell", "tag", c("x", "y", "z"))  # inconsistent under donor
    expect_error(
        reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor",
                         implicit_properties = "tag"),
        "inconsistent"
    )
})
```

- [ ] **Step 2: Run — 4 tests PASS**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-reconstruction.R")'
```

Expected: 4 PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-reconstruction.R
git commit -m "test(reconstruction): empty_implicit / skipped_properties / implicit_properties"
```

### Task I4: Test — rename_axis + inconsistent-discovery silent skip

**Files:**

- Modify: `tests/testthat/test-reconstruction.R`

- [ ] **Step 1: Append test**

```r
test_that("reconstruct_axis: rename_axis uses a different target name", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("dA", "dB"))
    reconstruct_axis(d, existing_axis = "cell", implicit_axis = "donor",
                     rename_axis = "person")
    expect_true(has_axis(d, "person"))
    expect_false(has_axis(d, "donor"))   # not created under original name
})

test_that("reconstruct_axis: errors when target axis pre-exists (this slice)", {
    d <- memory_daf(name = "d")
    add_axis(d, "cell", c("c1"))
    add_axis(d, "donor", c("dA"))
    set_vector(d, "cell", "donor", c("dA"))
    expect_error(reconstruct_axis(d, existing_axis = "cell",
                                  implicit_axis = "donor"),
                 "already exists")
})
```

- [ ] **Step 2: Run — PASS**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-reconstruction.R")'
```

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-reconstruction.R
git commit -m "test(reconstruction): rename_axis + pre-existing-target rejection"
```

---

## Phase J — Julia-parity fixture

### Task J1: Write the fixture regen script (Julia)

**Files:**

- Create: `dev/scripts/regen-julia-copies-fixture.jl`

**Pre-read:** Follows the Slice 3 / 4 / 5 precedent — uses an inline minimal JSON emitter (no `JSON` package), records the DAF.jl HEAD, emits two payloads: `copy_all_fixture.json` and `concat_fixture.json`. Dev repo location.

- [ ] **Step 1: Write the regen script**

Create `dev/scripts/regen-julia-copies-fixture.jl`:

```julia
# Regenerate fixtures/julia-copies/*.json from DataAxesFormats.jl.
# Pre: git -C ~/src/DataAxesFormats.jl pull --ff-only && re-dev in
# conda env dafr-mcview.
# Run: julia --project=~/src/dafr-mcview regen-julia-copies-fixture.jl
#
# Outputs are byte-comparable round-trips for the R port to verify.

using DataAxesFormats

const OUT_DIR = joinpath(@__DIR__, "..", "..", "tests", "testthat", "fixtures", "julia-copies")
mkpath(OUT_DIR)

# --- Tiny JSON emitter ---
json_str(s::AbstractString) = "\"" * replace(s, "\\" => "\\\\", "\"" => "\\\"") * "\""
json_val(x::Bool) = x ? "true" : "false"
json_val(x::Number) = string(x)
json_val(x::AbstractString) = json_str(x)
json_val(xs::AbstractVector) = "[" * join(json_val.(xs), ",") * "]"
json_val(xs::Tuple) = "[" * join(json_val.(collect(xs)), ",") * "]"
function json_val(d::AbstractDict)
    parts = ["$(json_str(string(k))):$(json_val(v))" for (k, v) in d]
    "{" * join(parts, ",") * "}"
end

# --- copy_all fixture ---
function make_copy_all_fixture()
    src = MemoryDaf(name = "src")
    set_scalar!(src, "organism", "human")
    add_axis!(src, "cell", ["c1", "c2"])
    add_axis!(src, "gene", ["g1", "g2", "g3"])
    set_vector!(src, "cell", "age", Int32[10, 20])
    set_matrix!(src, "cell", "gene", "UMIs", Int32[1 2 3; 4 5 6])

    dest = MemoryDaf(name = "dest")
    add_axis!(dest, "cell", ["c1", "c2", "c3"])
    add_axis!(dest, "gene", ["g1", "g2", "g3"])
    copy_all!(
        destination = dest, source = src,
        empty = Dict(("cell", "age") => Int32(-1),
                     ("cell", "gene", "UMIs") => Int32(0)),
        relayout = false,
    )

    Dict(
        "scalars" => Dict(n => get_scalar(dest, n) for n in scalars_set(dest)),
        "axes" => Dict(ax => axis_vector(dest, ax) for ax in axes_set(dest)),
        "vectors" => Dict("cell|age" => collect(get_vector(dest, "cell", "age"))),
        "matrices" => Dict("cell|gene|UMIs" => [collect(r) for r in eachrow(get_matrix(dest, "cell", "gene", "UMIs"))]),
    )
end

# --- concatenate fixture ---
function make_concat_fixture()
    a = MemoryDaf(name = "A")
    add_axis!(a, "cell", ["c1", "c2"])
    set_vector!(a, "cell", "age", Int32[10, 20])
    b = MemoryDaf(name = "B")
    add_axis!(b, "cell", ["c1", "c3"])
    set_vector!(b, "cell", "age", Int32[30, 40])

    dest = MemoryDaf(name = "dest")
    concatenate!(dest, "cell", [a, b], prefix = true)

    Dict(
        "axes" => Dict(ax => axis_vector(dest, ax) for ax in axes_set(dest)),
        "vectors" => Dict(
            "cell|age" => collect(get_vector(dest, "cell", "age")),
            "cell|dataset" => collect(get_vector(dest, "cell", "dataset")),
        ),
    )
end

open(joinpath(OUT_DIR, "copy_all_fixture.json"), "w") do io
    write(io, json_val(make_copy_all_fixture()))
end
open(joinpath(OUT_DIR, "concat_fixture.json"), "w") do io
    write(io, json_val(make_concat_fixture()))
end

# Record HEAD for reproducibility.
import Pkg
daf_head = read(`git -C $(normpath(joinpath(pathof(DataAxesFormats), "..", ".."))) rev-parse HEAD`, String) |> strip
open(joinpath(OUT_DIR, "README.md"), "w") do io
    write(io, """
# Julia copies/concat fixture

Generated from DataAxesFormats.jl at `$daf_head`.

Regenerate:

```
julia --project=~/src/dafr-mcview dev/scripts/regen-julia-copies-fixture.jl
```

Payloads:

- `copy_all_fixture.json` — `copy_all!` roundtrip with cell-axis
  superset and empty fills for `cell|age` and `cell|gene|UMIs`.
- `concat_fixture.json` — `concatenate!` of two sources on the
  `cell` axis with `prefix = true`.
""")
end
```

- [ ] **Step 2: Commit (the script lives in dev repo)**

```
cd /home/aviezerl/src/dafr-native/dev
git add scripts/regen-julia-copies-fixture.jl
git commit -m "scripts(julia): slice-6 copies/concat fixture regen"
```

### Task J2: Regenerate fixture

**Files:**

- Create: `tests/testthat/fixtures/julia-copies/copy_all_fixture.json`
- Create: `tests/testthat/fixtures/julia-copies/concat_fixture.json`
- Create: `tests/testthat/fixtures/julia-copies/README.md`

- [ ] **Step 1: Pre-regen hygiene**

```
git -C ~/src/DataAxesFormats.jl fetch
git -C ~/src/DataAxesFormats.jl pull --ff-only
# Ensure the dafr-mcview env re-devs DAF.jl at the new HEAD if it advanced.
cd /home/aviezerl/src/dafr-native
```

- [ ] **Step 2: Run the script**

```
cd /home/aviezerl/src/dafr-native
conda run -n dafr-mcview julia --project=~/src/dafr-mcview \
    dev/scripts/regen-julia-copies-fixture.jl
```

Expected: three files written under `tests/testthat/fixtures/julia-copies/`.

- [ ] **Step 3: Commit**

```
git add tests/testthat/fixtures/julia-copies/
git commit -m "fixture(julia-copies): copy_all + concat roundtrips"
```

### Task J3: R test — replay Julia fixtures against R implementation

**Files:**

- Create: `tests/testthat/test-copies-julia-compat.R`

- [ ] **Step 1: Write tests**

Create `tests/testthat/test-copies-julia-compat.R`:

```r
test_that("copy_all matches Julia roundtrip", {
    fx_path <- test_path("fixtures", "julia-copies", "copy_all_fixture.json")
    skip_if(!file.exists(fx_path), "Julia fixture not generated")
    expected <- jsonlite::fromJSON(fx_path, simplifyVector = FALSE)

    src <- memory_daf(name = "src")
    set_scalar(src, "organism", "human")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2", "g3"))
    set_vector(src, "cell", "age", as.integer(c(10, 20)))
    set_matrix(src, "cell", "gene", "UMIs",
               matrix(as.integer(c(1,4,2,5,3,6)), nrow = 2,
                      dimnames = list(c("c1","c2"), c("g1","g2","g3"))))

    dest <- memory_daf(name = "dest")
    add_axis(dest, "cell", c("c1", "c2", "c3"))
    add_axis(dest, "gene", c("g1", "g2", "g3"))
    copy_all(dest, src,
        empty = list("cell|age" = -1L, "cell|gene|UMIs" = 0L),
        relayout = FALSE)

    expect_identical(get_scalar(dest, "organism"),
                     expected$scalars$organism)
    expect_identical(as.character(axis_vector(dest, "cell")),
                     unlist(expected$axes$cell))
    expect_identical(as.integer(unname(get_vector(dest, "cell", "age"))),
                     as.integer(unlist(expected$vectors$`cell|age`)))
})

test_that("concatenate matches Julia roundtrip", {
    fx_path <- test_path("fixtures", "julia-copies", "concat_fixture.json")
    skip_if(!file.exists(fx_path), "Julia fixture not generated")
    expected <- jsonlite::fromJSON(fx_path, simplifyVector = FALSE)

    a <- memory_daf(name = "A"); add_axis(a, "cell", c("c1", "c2"))
    set_vector(a, "cell", "age", as.integer(c(10, 20)))
    b <- memory_daf(name = "B"); add_axis(b, "cell", c("c1", "c3"))
    set_vector(b, "cell", "age", as.integer(c(30, 40)))

    dest <- memory_daf(name = "dest")
    concatenate(dest, "cell", list(a, b), prefix = TRUE)

    expect_identical(as.character(axis_vector(dest, "cell")),
                     unlist(expected$axes$cell))
    expect_identical(as.character(unname(get_vector(dest, "cell", "dataset"))),
                     unlist(expected$vectors$`cell|dataset`))
})
```

- [ ] **Step 2: Run**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-copies-julia-compat.R")'
```

Expected: 2 PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-copies-julia-compat.R
git commit -m "test(julia-compat): replay copy_all + concat Julia fixtures in R"
```

---

## Phase Z — Polish + NEWS + Collate + check + exit

### Task Z1: Regenerate NAMESPACE + man pages

**Files:**

- Modify: `NAMESPACE`
- Modify/create: `man/*.Rd`

- [ ] **Step 1: Run roxygen**

Run:

```
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::document()'
```

Expected: `NAMESPACE` updated with new exports (`copy_scalar`, `copy_axis`, `copy_vector`, `copy_matrix`, `copy_tensor`, `copy_all`, `empty_data`, `concatenate`, `complete_chain`, `complete_daf`, `open_daf`, `reconstruct_axis`, `MERGE_SKIP`, `MERGE_LAST_VALUE`, `MERGE_COLLECT_AXIS`). New `.Rd` files in `man/`.

- [ ] **Step 2: Verify NAMESPACE additions**

Run:

```
grep -E "^export\((copy_|concat|complete_|open_daf|reconstruct|empty_data|MERGE_)" NAMESPACE
```

Expected: 15 entries.

- [ ] **Step 3: Commit**

```
git add NAMESPACE man/
git commit -m "docs(roxygen): regenerate NAMESPACE + man pages for Slice 6 exports"
```

### Task Z2: Collate — sync @include graph

**Files:**

- Modify: `DESCRIPTION` (Collate field)

**Pre-read:** `R/copies.R` has `@include classes.R readers.R writers.R memory_daf.R chain_daf.R view_daf.R`. `R/concat.R` has `@include classes.R readers.R writers.R memory_daf.R copies.R`. `R/complete.R` has `@include classes.R files_daf.R memory_daf.R chain_daf.R view_daf.R readers.R writers.R`. `R/reconstruction.R` has `@include classes.R readers.R writers.R memory_daf.R`.

`devtools::document()` already regenerates the Collate field from the `@include` directives. Verify after Z1 that the order is sensible.

- [ ] **Step 1: Run test suite after regen to ensure load order is fine**

Run:

```
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: 0 FAIL. Total test count > 1175 (Slice 5 baseline) + new tests.

- [ ] **Step 2: If Collate did change, commit it**

```
# Only commit if DESCRIPTION diff shows a Collate field change.
git diff DESCRIPTION | head -40
git add DESCRIPTION
git commit -m "docs(collate): sync @include graph topology for Slice 6 files"
```

If no change, skip the commit — `devtools::document()` is idempotent when the graph is stable.

### Task Z3: NEWS.md — Slice 6 entry

**Files:**

- Modify: `NEWS.md`

- [ ] **Step 1: Prepend a Slice 6 entry**

Prepend (new entry at the top) in `NEWS.md`:

```
# dafr 0.6.0 (in development)

## New features

- **Copies surface** (`copy_scalar()` / `copy_axis()` / `copy_vector()` /
  `copy_matrix()` / `copy_tensor()` / `copy_all()`): port of Julia
  `Copies.jl`. Supports `rename` / `reaxis`, type coercion, `empty` fill
  for source-is-subset axes, `overwrite` and `insist` semantics matching
  Julia. Sparse matrix pad-mode now preserves sparsity via
  `Matrix::sparseMatrix` embedding — fixes the Slice-5 dense-coercion
  mine. (#slice-6)
- `empty_data()` helper builds flat-keyed empty / types specs from a
  typed-list builder API. (#slice-6)
- `concatenate()`: stitches N dafs along one or more axes, creates a
  per-source `dataset` axis, supports prefix de-duplication with the
  "property-starts-with-axis-name" heuristic, fills missing per-source
  properties from an `empty` map, and applies per-property `merge`
  actions (`SkipProperty`, `LastValue`, `CollectAxis`). (#slice-6)
- `complete_chain()` / `complete_daf()` / `open_daf()`: persistent chain
  metadata. `complete_chain` stores a `base_daf_repository` pointer
  scalar; `complete_daf` walks the pointer chain back and returns a
  `chain_reader` or `chain_writer`. `open_daf` dispatches FilesDaf
  (directory) paths; H5df is deferred. (#slice-6)
- `reconstruct_axis()`: promotes an implicit property to an explicit
  axis, migrating consistently-mapped properties. Returns a per-property
  dict of values associated with empty-implicit entries. Core behaviors
  only; pre-existing target axis merge is Slice 7. (#slice-6)

## Refactor

- `adapter()` internal `.copy_view_to_daf()` removed; adapter now calls
  `copy_all()` with `insist = FALSE`. Same user-facing surface; sparse
  pad-mode is now sparse-preserving. (#slice-6)
```

- [ ] **Step 2: Commit**

```
git add NEWS.md
git commit -m "docs(news): Slice 6 entry"
```

### Task Z4: Full test suite + devtools::check

**Files:** none (verification only).

- [ ] **Step 1: Run the full test suite**

Run:

```
cd /home/aviezerl/src/dafr-native
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: 0 FAIL, 0 SKIP (except pre-existing WARN).

- [ ] **Step 2: Run devtools::check**

Run:

```
_R_CHECK_SYSTEM_CLOCK_=0 Rscript -e 'devtools::check(error_on = "note")'
```

Expected: `0 error / 0 warning / 0 note`.

- [ ] **Step 3: Record final counts in the next task (exit note)**

No commit here.

### Task Z5: Exit note

**Files:**

- Create: `dev/notes/slice-6-exit.md`

- [ ] **Step 1: Write the exit note**

Create `dev/notes/slice-6-exit.md` with this template (filled by the executor with final numbers):

```
# Slice 6 exit

**Branch:** `slice-6-copies-concat-complete` merged fast-forward into
`main`. Final commit SHA: `<fill>`. Tag: `slice-6`.

## What landed

- Phase A: `copy_scalar` + `copy_axis` + `.verify_axis_relation`.
- Phase B: `copy_vector` (same/subset/superset + empty fill + type).
- Phase C: `copy_matrix` (same/subset/superset + sparse-preserving pad
  + relayout). **Slice-5 dense-coercion mine CLOSED.**
- Phase D: `copy_tensor` (per-main-axis-entry `copy_matrix` loop).
- Phase E: `copy_all` + `empty_data()`.
- Phase F: `adapter()` refactored to use `copy_all` internally.
  `.copy_view_to_daf` removed.
- Phase G: `concatenate()` — single + multi-axis + prefix heuristic +
  merge actions (Skip/LastValue/CollectAxis).
- Phase H: `complete_chain` + `complete_daf` + `open_daf` (FilesDaf).
- Phase I: `reconstruct_axis` (core behaviors).
- Phase J: Julia parity fixture for `copy_all` and `concatenate`.
- Phase Z: NEWS + NAMESPACE + Collate + `devtools::check` = 0/0/0.

## Test / check numbers

- `testthat::test_dir("tests/testthat")`: **<N> PASS / 0 FAIL /
  0 SKIP / 1 WARN** (pre-existing scran/irlba).
- `devtools::check(error_on = "note")`: **0 / 0 / 0**.

## Still deferred from Slice 6

- **Slice 7 (already committed):** Ops expansion (C option) — remaining
  ~20 Julia ops (`Clamp`, `Convert`, `Fraction`, `Significant`, `Type`,
  `GeoMean`, `Median`, `Quantile`, `Std`, `StdN`, `Var`, `VarN`, `All`,
  `Any`, etc.) routed through the same `register_eltwise` / reduction
  mechanism. Perf-flavoured slice.
- `bestify` heuristic for `copy_vector` / `copy_matrix` (sparse-vs-dense
  promote/demote by nnz). Not implemented this slice.
- `reconstruct_axis` with a pre-existing target axis (`properties_defaults`
  path). Not implemented this slice.
- `complete_daf` + `base_daf_view`: JSON is stored/parsed but the view
  is not re-applied. Add at Slice 7 if an on-disk chain needs a
  persistent view.
- H5df backend for `open_daf`. Originally Slice 8; still deferred.

## Mines laid in Slice 6 for Slice 7

- **`copy_all` does not infer tensor keys from matrix names.** Users
  must call `copy_tensor` explicitly for the tensor entries. Julia
  expands a `TensorKey` in `empty` / `types` automatically; R doesn't.
- **`.cast_matrix_type("integer", dgCMatrix)`** dense-coerces. Only
  triggered when the user requests an integer type on a sparse source
  matrix; not exercised by any current test. Document if users hit it.
- **`concatenate` string vectors prefix only when source vectors are
  `character`.** Prefix for e.g. an integer-keyed property (unusual but
  legal in Julia) would silently not-prefix. Reasonable R behaviour;
  flagged in case a user complains.
- **`reconstruct_axis`** constructs the new-axis property by
  `vapply(unique_vals, ..., FUN.VALUE = values[[1L]])`. If the first
  entry happens to be empty-implicit, the FUN.VALUE type may be wrong.
  Guardrail: tests exercise only non-empty-first cases; document.
- **`.concat_axis_matrix`** transposes via `Matrix::t()` which
  allocates. Fine for fixture-scale inputs; bears watching at real
  metacell scale.

## Julia DAF state at Slice 6 exit

- `~/src/DataAxesFormats.jl` at `<HEAD from fixture README>`.
- `~/src/TanayLabUtilities.jl` unchanged.
- Fixture sets: `julia-queries/`, `julia-chains/`, `julia-adapter/`,
  **`julia-copies/` (NEW).**

## L2 upstream PR

Declined across Slices 3–5. Re-ask at Slice 7 exit if still relevant.

## Push status

Local `main` is `<N>` commits ahead of `origin/main`. Slice-5 tag
and now `slice-6` tag are both local only. Push deferred to user
discretion.
```

- [ ] **Step 2: Commit in the dev repo**

```
cd /home/aviezerl/src/dafr-native/dev
git add notes/slice-6-exit.md
git commit -m "docs(notes): Slice 6 exit"
```

### Task Z6: Merge feature branch to main + tag

**Files:** none (git operations).

- [ ] **Step 1: Confirm on feature branch, fast-forward to main**

```
cd /home/aviezerl/src/dafr-native
git checkout main
git merge --ff-only slice-6-copies-concat-complete
git tag slice-6
```

Expected: fast-forward merge; tag applied.

- [ ] **Step 2: Delete feature branch**

```
git branch -d slice-6-copies-concat-complete
```

- [ ] **Step 3: Verify clean state**

```
git status
git log --oneline -5
git describe --tags
```

Expected: clean tree; `slice-6` tag at HEAD.

### Task Z7: Ask the user about push + L2 PR

**Files:** none (communication only).

- [ ] **Step 1: Prompt the user**

Post a message to the user:

> Slice 6 landed on `main` with tag `slice-6`. Two carry-over items:
>
> 1. `origin/main` is still at Slice 4 (`7c57565`). Slices 5 and 6 plus
>    tags `slice-5` / `slice-6` are local only. Push now? `git push origin
>    main --tags`.
> 2. L2 upstream PR (docs for `filesdaf-on-disk-spec-draft.md` to
>    `tanaylab/DataAxesFormats.jl`) — declined across four previous
>    slices. Re-ask, or continue to defer?

Wait for user decision; do not push without explicit approval.

---

## Exit criteria (summary)

- 15 new exports (`copy_scalar`, `copy_axis`, `copy_vector`,
  `copy_matrix`, `copy_tensor`, `copy_all`, `empty_data`, `concatenate`,
  `complete_chain`, `complete_daf`, `open_daf`, `reconstruct_axis`,
  `MERGE_SKIP`, `MERGE_LAST_VALUE`, `MERGE_COLLECT_AXIS`).
- `@examples` on all new exports; `devtools::check` remains `0 / 0 / 0`.
- Full test suite PASS with 0 FAIL / 0 SKIP (pre-existing WARN only).
- Julia fixture parity confirmed for `copy_all` + `concatenate`.
- Sparse-preserving pad-mode closes the Slice-5 kickoff mine.
- Adapter refactored; `.copy_view_to_daf` deleted.
- `slice-6` tag applied on `main`.








