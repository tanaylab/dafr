# Slice 5 — Computations + Adapters + ExampleData + Multi-hop Chained Lookup

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the contracts user-facing story by landing `computation()` (contract HOF), `adapter()` (renaming-view → capture → copy-back), and byte-parity `example_cells_daf()` / `example_metacells_daf()` / `example_chain_daf()`. In the same slice, extend the single-hop `=@` chained lookup (Slice 4) to multi-hop so `/ cell : donor =@ : lab =@ : country` resolves, matching Julia. Prove cross-runtime parity with a Julia fixture for a computation+adapter roundtrip.

**Architecture:**

- **`computation(contract, fn)` is a higher-order function.** It returns a wrapped closure that on each invocation calls `contractor(name, contract, daf)` on the first argument, runs `verify_input(wrapped_daf)`, calls the user's `fn(wrapped_daf, ...)`, runs `verify_output(wrapped_daf)`, and returns the original result. The bound `contract` is stashed as an attribute on the wrapped closure so `function_contract(wrapped_fn)` can retrieve it. Julia's `@computation` macro does the same binding via `FunctionMetadata`; R's equivalent is plain attributes on the closure. Scope choice: **single-contract only**. Julia's dual- and triple-contract macros are marked UNTESTED upstream; we defer them with a short guard in `computation()` that errors on anything other than a single `Contract` positional argument. Matches the Slice-5 kickoff scope ("`computation(contract, fn)` HOF").
- **`adapter()` mirrors Julia `adapter()` verbatim.** It is the Julia idiom for applying a generic-named computation to specifically-named data:
  1. Wrap `daf` (a `DafWriter`) in a read-only `viewer()` using `input_axes` / `input_data` — exposes the subset the computation consumes, possibly under renamed axes/names.
  2. Construct a fresh writable `capture` (default: `memory_daf()`) for the computation's outputs.
  3. Chain `[input_view, capture]` via `chain_writer()` → `adapted`. Reads fall through to the input view; writes land in the capture.
  4. Call `result <- fn(adapted)`.
  5. Wrap `adapted` in a `viewer()` using `output_axes` / `output_data` — exposes the outputs under the final user-visible names.
  6. Copy the output view into `daf` via a new internal `.copy_view_to_daf()` helper (the minimal subset of Julia's `copy_all!` the adapter needs).
  7. Return `result`.
  No public `copy_all!` is exported this slice; the helper is `.copy_view_to_daf()` and stays internal to `R/adapters.R`. Exporting a full `copy_all!`/`copies` surface is Option B (Slice 6+ tail helpers).
- **`example_cells_daf()` / `example_metacells_daf()` / `example_chain_daf()` port Julia's `ExampleData` module.** Julia ships the raw data as `test/example_data/{axes,vectors,matrices}/*.{txt,csv}` — a total of 2.1 MB — and the loader parses + type-casts at call time. R does the same: the raw files are copied (as-is) into `inst/extdata/example_data/`; an R loader replicates `load_axis` / `load_vector` / `load_matrix` with the exact same `cast_vector` (Bool → UInt32/Int32 → Float32) and `cast_matrix` (UInt8 → UInt16 → Float32) promotion order. Both datasets are `MemoryDaf`s; `example_chain_daf()` is `chain_writer([cells, metacells])`. No Julia process is required at test time — parity is checked against the existing FilesDaf dump already present at `tests/testthat/fixtures/julia-queries/example-daf/`, not regenerated per run.
- **Multi-hop chained lookup is a single targeted patch.** Slice 4's `.apply_chained_lookup_vector` returns `list(kind = "vector", value = out, axis = base_axis)` — it does not set `$property`. Consequence: a second `=@` in a bare form cannot infer the next target axis. Fix: stamp `$property = node$name` on the return, and clear `$if_not_present` / `$if_not_value` / `$chain_target_axis` so the next hop starts clean. That single return-list change unlocks `/ cell : donor =@ : lab =@ : country` end-to-end. Explicit-axis multi-hop (`=@ donor =@ lab`) already works because `.apply_as_axis` accepts `kind == "vector"`. New tests verify: 2-hop bare, 2-hop explicit, 3-hop, IfNot at intermediate hop, wrong-type pivot error. No new nodes in the AST; no new parser productions. **The closed `state$kind` enum is unchanged** (Slice-4 kickoff mine F4 is respected: no new kinds added).
- **Julia parity fixture for adapter+computation.** A new fixture set under `tests/testthat/fixtures/julia-adapter/` captures a roundtrip the R side then replays. Regenerated via `dev/scripts/regen-julia-adapter-fixture.jl`, following the Slice-3/Slice-4 pattern (minimal inline JSON emitter, no `JSON` dependency). Coverage: one simple computation (squares a vector), wrapped in `adapter()` with axis rename `{obs → cell, var → gene}` and vector rename for an input + output, `overwrite = true`, `relayout = true` on a small matrix. The fixture records the expected output state (scalars/vectors/matrices under final names). R replays using `example_cells_daf()` as the starting `daf`, the same adapter config, and an identical `squares_compute` function — asserts byte-equivalence on outputs.

**Tech Stack:**

- R 4.4+, S7 0.2.1. No new R dependencies; no new C++. All new code is R plus one static-data copy (Julia `test/example_data` → `inst/extdata/example_data`). `jsonlite` already present for fixture parsing. `Matrix` for sparse handling in `.copy_view_to_daf()`. Roxygen for docs; `devtools::check()` for release gate.
- Julia side (fixture regeneration only): `DataAxesFormats.jl` at `49fbba1` or newer; `TanayLabUtilities.jl`; conda env `dafr-mcview`. Fixture regen is one-shot — not part of the CI path.

**Repo layout:**

- Package repo: `/home/aviezerl/src/dafr-native/` on `main` at tag `slice-4` / commit `7c57565`. Tracks `git@github.com:tanaylab/dafr.git`. Source, tests, `inst/` commits → package repo. Execute on a feature branch `slice-5-computations-adapters` (created at Phase 0; final merge at Phase Z).
- Dev repo (nested, gitignored): `/home/aviezerl/src/dafr-native/dev/` on `main` at `c461755`. Plans, notes, scripts → dev repo. Dev repo commits stand separate from package commits.

**Dev loop per task:**

1. From `/home/aviezerl/src/dafr-native/`:
   ```
   Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "<tag>")'
   ```
2. Inspect; iterate to green.
3. Stage + commit with the message given in the task. Package repo vs. dev repo: infer from the file path. **Never `--amend`, `--no-verify`, or force-push.** Use `/bin/rm` / `/bin/cp` (aliased with `-i`). Wait for permission prompts; do not retry the same denied action.

**Known mines laid in Slice 4 (honor throughout):**

- `ContractDaf` shares its base daf's `cache` env. Three-layer stacks (`contractor → chain_writer → memory_daf`) all see the same cache. The computation wrapper uses `contractor()` unchanged — so the same cache-sharing applies. This is fine for Slice 5 but worth recording in exit if a computation triggers a surprise via stale-cache on a deleted output.
- `chain_writer` auto-adds axes from earlier readers on first vector/matrix write. Adapter's `adapted = chain_writer([input_view, capture])` depends on this: output writes to `capture` auto-add axes from the input view. Do not test behind-the-back modification of `daf` after `adapter()` is called.
- `.type_ok` / `.vector_type_ok` / `.matrix_type_ok` use R class-name matching. `character` is missing from `.matrix_type_ok`'s switch (falls through to `inherits()`). Slice 5 does not add or fix; only the existing contract paths are exercised.
- `merge_contracts` type lattice is `c("logical", "integer", "double", "numeric", "character")`; narrower wins. Not exercised by this slice.
- Evaluator `state$kind` is a closed enum including `"vector_axis"` (Slice 4 F4). Phase D does NOT extend it. All multi-hop changes stay within existing kinds (`vector` + `vector_axis`).
- `Contract()` validator uses a mix of `stop()` and `return(string)`. Unchanged; Phase A uses the existing constructor.
- `Contract` accepts unnamed/partial-named `axes` silently. Not added in this slice; flagged only.

---

## Pre-planning decisions (settled before tasks)

### 1. Phase order

D → E → A → B → J → Z, in that sequence. Rationale:

- **D first (multi-hop):** smallest, independent, zero-file new code (one `.apply_chained_lookup_vector` return-list change + tests). Good warm-up; reduces risk of a late-slice rebase if the evaluator needs follow-up.
- **E next (ExampleData):** enables A/B tests to exercise realistic data. Independent of all other phases.
- **A then B:** B depends on A (adapter's test coverage includes a `computation()`-wrapped inner function).
- **J last functional phase:** end-to-end Julia parity of A+B+D+E.
- **Z polish:** docs, NEWS, NAMESPACE, `devtools::check`.

### 2. No `copy_all!` public surface this slice

`adapter()` needs the effect of Julia's `copy_all!` to project the output view into `daf`. Slice 5 ships an INTERNAL helper `.copy_view_to_daf()` that supports exactly what the adapter needs: iterating scalars, axes, vectors, matrices visible in the source view and setting them into the destination, with `overwrite` + `relayout` + minimal `empty` (NULL default; per-property default only when the source axis is a strict subset of the destination axis). Full `copy_all!` / `copy_scalar!` / `copy_vector!` public surface is deferred (Slice 6 tail helpers Option B).

### 3. No `@computation` macro

R has no macro system. `computation()` is a plain HOF. The Julia docstring-splice for `CONTRACT` is replaced by an exported helper `contract_description(contract)` that returns a formatted string, which users can interpolate manually in roxygen.

### 4. No multi-contract computations

Julia's dual-/triple-contract `@computation` variants are UNTESTED upstream. Slice 5 errors on them with a clear message pointing to single-contract usage. Revisit if users hit the limitation.

### 5. `empty` parameter scope for `adapter()`

Julia's `EmptyData` lets copy-back fill entries for rows/cols not in the source view. Slice 5 accepts `empty` as a named list `list("<axis>|<vector>" = default_value, "<rows>|<cols>|<matrix>" = default_value)` and only supports the flat-key form exercised by the Julia-parity fixture. More elaborate structures defer.

### 6. Fixture regeneration policy

Phase J's fixture regen script runs once against `DataAxesFormats.jl` at its tip. Before regenerating, the agent must `git -C ~/src/DataAxesFormats.jl fetch && git -C ~/src/DataAxesFormats.jl pull`, then record the new HEAD in the fixture's README so future parity checks are reproducible.

### 7. Feature branch

`slice-5-computations-adapters` on the package repo. Created at Phase 0, merged fast-forward at Phase Z exit.

### 8. Worktree vs in-place

Following Slice 4 precedent, we work in-place on `/home/aviezerl/src/dafr-native/` with a feature branch. The `superpowers:using-git-worktrees` skill is optional and may be used if the user prefers an isolated worktree; the instructions below assume in-place.

---

## File structure

**Create (package repo):**

- `R/computations.R` (~120 LoC) — `computation(contract, fn)` HOF, `function_contract(fn)`, `contract_description(contract)`.
- `R/adapters.R` (~260 LoC) — `adapter()` + internal `.copy_view_to_daf()` + helpers.
- `R/example_data.R` (~150 LoC) — `example_cells_daf()`, `example_metacells_daf()`, `example_chain_daf()`, `.load_axis_file()`, `.load_vector_file()`, `.load_matrix_file()`, `.cast_vector()`, `.cast_matrix()`, `.example_data_dir()`.
- `tests/testthat/test-computations.R` (~180 LoC)
- `tests/testthat/test-adapters.R` (~320 LoC)
- `tests/testthat/test-example-data.R` (~100 LoC)
- `tests/testthat/test-adapter-julia-compat.R` (~80 LoC)
- `tests/testthat/fixtures/julia-adapter/fixture.json` (regen output; committed)
- `tests/testthat/fixtures/julia-adapter/README.md` (~40 LoC)
- `inst/extdata/example_data/axes/*.txt` (copy from Julia: 6 files)
- `inst/extdata/example_data/vectors/*.txt` (copy from Julia: 9 files)
- `inst/extdata/example_data/matrices/*.csv` (copy from Julia: 3 files)

**Modify (package repo):**

- `R/query_eval.R:369` — `.apply_chained_lookup_vector` return-list change (D phase).
- `tests/testthat/test-query-eval-chains.R` — append multi-hop tests (D phase).
- `NAMESPACE` — roxygen regen (Z phase).
- `NEWS.md` — Slice 5 entry (Z phase).
- `DESCRIPTION` — bump Version to `0.0.1.9000`? **No** — stay at `0.0.0.9000` until a real tagged release. No change.
- `man/*.Rd` — roxygen regen (Z phase).

**Create (dev repo):**

- `dev/scripts/regen-julia-adapter-fixture.jl` (~180 LoC)
- `dev/notes/slice-5-exit.md` (~200 LoC, Z phase)

---

## Phase D — Multi-hop chained lookup

### Task D1: Test — bare 2-hop chained lookup

**Files:**

- Modify: `tests/testthat/test-query-eval-chains.R`

**Pre-read:** The existing file already covers single-hop cases. Look at the last `test_that` block and append the new ones after it.

- [ ] **Step 1: Add failing test for 2-hop bare form**

Append this block to `tests/testthat/test-query-eval-chains.R`:

```r
test_that("bare 2-hop '=@:x =@:y' chains through two axes", {
    d <- memory_daf(name = "multi-hop")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))
    set_vector(d, "donor", "lab", c("lA", "lB"))
    set_vector(d, "lab", "country", c("IL", "US"))

    out <- get_query(d, "/ cell : donor =@ : lab =@ : country")
    expect_equal(unname(out), c("IL", "US", "IL"))
    expect_equal(names(out), c("c1", "c2", "c3"))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R", filter = NULL)'
```

Expected: new block fails with an error about `'=@' requires a vector in scope` or similar — the second `=@` sees the wrong state from the first hop's return.

- [ ] **Step 3: (no implementation yet; continue to D2)**

### Task D2: Fix `.apply_chained_lookup_vector` to support multi-hop

**Files:**

- Modify: `R/query_eval.R:369`

**Pre-read:** Current return:

```r
list(kind = "vector", value = out, axis = base_axis)
```

- [ ] **Step 1: Replace the return value**

Change line 369 from:

```r
    list(kind = "vector", value = out, axis = base_axis)
}
```

to:

```r
    list(
        kind     = "vector",
        value    = out,
        axis     = base_axis,
        property = node$name
    )
}
```

The `$property` field tells a subsequent bare `=@` which property name to use for inferring the next-hop target axis (same logic `.apply_as_axis` uses for the initial hop).

- [ ] **Step 2: Run tests — all chain-eval tests pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R")'
```

Expected: all tests pass, including the D1 test.

- [ ] **Step 3: Run the full eval suite (regression guard)**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "query")'
```

Expected: 0 failures.

- [ ] **Step 4: Commit**

```
cd /home/aviezerl/src/dafr-native
# Branch slice-5-computations-adapters was created up-front by the controller; just stage + commit.
git add R/query_eval.R tests/testthat/test-query-eval-chains.R
git commit -m "$(cat <<'EOF'
feat(evaluator): multi-hop chained lookup via '=@' stacking

.apply_chained_lookup_vector now returns $property in its state,
letting a subsequent bare '=@' infer the next-hop target axis from
the just-looked-up property (mirrors the initial hop's behaviour).
Tests cover 2-hop bare form; additional multi-hop coverage lands in
subsequent tasks.
EOF
)"
```

### Task D3: Test — explicit-axis 2-hop

**Files:**

- Modify: `tests/testthat/test-query-eval-chains.R`

- [ ] **Step 1: Append failing test**

```r
test_that("explicit-axis 2-hop '=@axis:x =@:y' chains through two axes", {
    d <- memory_daf(name = "multi-hop-explicit")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    set_vector(d, "cell", "d_alias", c("d1", "d2"))    # not a valid axis name itself
    set_vector(d, "donor", "l_alias", c("lA", "lB"))   # not a valid axis name itself
    set_vector(d, "lab", "country", c("IL", "US"))

    # Explicit axis names at each hop
    out <- get_query(d, "/ cell : d_alias =@ donor : l_alias =@ lab : country")
    expect_equal(unname(out), c("IL", "US"))
    expect_equal(names(out), c("c1", "c2"))
})
```

- [ ] **Step 2: Run test**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R", filter = NULL)'
```

Expected: PASS. The fix in D2 is sufficient (`.apply_as_axis` already accepts `kind == "vector"`, and `$chain_target_axis` is set from the explicit node).

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-query-eval-chains.R
git commit -m "test(evaluator): explicit-axis 2-hop chained lookup"
```

### Task D4: Test — 3-hop chain

**Files:**

- Modify: `tests/testthat/test-query-eval-chains.R`

- [ ] **Step 1: Append failing test (should pass post-D2)**

```r
test_that("3-hop chain '=@:x =@:y =@:z' resolves", {
    d <- memory_daf(name = "3-hop")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    add_axis(d, "country", c("IL", "US"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    set_vector(d, "donor", "lab", c("lA", "lB"))
    set_vector(d, "lab", "country", c("IL", "US"))
    set_vector(d, "country", "language", c("Hebrew", "English"))

    out <- get_query(d, "/ cell : donor =@ : lab =@ : country =@ : language")
    expect_equal(unname(out), c("Hebrew", "English"))
})
```

- [ ] **Step 2: Run test**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R", filter = NULL)'
```

Expected: PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-query-eval-chains.R
git commit -m "test(evaluator): 3-hop chained lookup"
```

### Task D5: Test — IfNot (`??`) at intermediate hop drops empty rows

**Files:**

- Modify: `tests/testthat/test-query-eval-chains.R`

**Pre-read:** Current single-hop `IfNot` semantics: `??` with no argument drops empty pivot rows; `?? <value>` substitutes. We want the same behaviour at an intermediate hop, and — critical — the `$if_not_present` / `$if_not_value` state must NOT leak into the next hop.

- [ ] **Step 1: Append failing test for leaky state**

```r
test_that("intermediate '??' drops missing rows but does not leak to next hop", {
    d <- memory_daf(name = "if-not-leak")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    set_vector(d, "cell", "donor", c("d1", "", "d2"))   # c2 has empty donor
    set_vector(d, "donor", "lab", c("lA", "lB"))
    set_vector(d, "lab", "country", c("IL", "US"))

    # `??` at the first hop only: drop c2; next hop has no empties.
    out <- get_query(d, "/ cell : donor ?? =@ : lab =@ : country")
    expect_equal(unname(out), c("IL", "US"))
    expect_equal(names(out), c("c1", "c3"))
})
```

- [ ] **Step 2: Run test**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R", filter = NULL)'
```

Expected: FAIL. The existing `.apply_chained_lookup_vector` leaves `$if_not_present = TRUE` in state. The second hop re-applies it — but since no rows are empty at that hop, the condition that would trigger the drop logic is unreached. However, the real bug is `$chain_target_axis` and `$if_not_present` staying set after the first hop completes. If the second hop has empties, the leftover `$if_not_value` / `$if_not_present` is consulted *by the second hop's `.apply_chained_lookup_vector`*, which is wrong. Add also a positive test:

```r
test_that("intermediate '??' state is cleared after the hop consumes it", {
    d <- memory_daf(name = "if-not-clear")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "donor", c("d1", "d2"))
    add_axis(d, "lab", c("lA", "lB"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    set_vector(d, "donor", "lab", c("lA", ""))   # d2 has empty lab
    set_vector(d, "lab", "country", c("IL", "US"))

    # `??` applied only at hop 1; hop 2 has an empty and MUST raise because no sentinel.
    expect_error(
        get_query(d, "/ cell : donor ?? =@ : lab =@ : country"),
        "empty pivot values"
    )
})
```

Without clearing, the leftover `$if_not_present` would mistakenly drop/sentinel the empty `d2 -> ""` lab at hop 2 instead of raising. Both tests together check the clearing invariant.

- [ ] **Step 3: Fix — clear leaky state in the hop return**

Modify `R/query_eval.R` inside `.apply_chained_lookup_vector`, immediately before the final `return(list(...))`, set explicit NULLs if needed, but since we are building a fresh list, there is no state to clear beyond what we put in the return list. **The return list already excludes `if_not_present` / `chain_target_axis` etc.** So the fix in D2 (building a fresh list) is sufficient. Rerun to confirm.

Actually, re-read D2's return list:

```r
list(
    kind     = "vector",
    value    = out,
    axis     = base_axis,
    property = node$name
)
```

This does NOT forward `if_not_present` / `if_not_value` / `chain_target_axis`. Good. Run the tests.

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R", filter = NULL)'
```

Expected: both new tests PASS. If the positive-drop test passes but the expect_error test fails with "empty pivot values" raised at the wrong hop, inspect the state dict returned between hops and add explicit `NULL` stripping to `.apply_chained_lookup_vector`.

- [ ] **Step 5: Commit**

```
git add tests/testthat/test-query-eval-chains.R
git commit -m "test(evaluator): '??' state cleared between chained hops"
```

### Task D6: Test — wrong-type pivot at hop 2 raises

**Files:**

- Modify: `tests/testthat/test-query-eval-chains.R`

- [ ] **Step 1: Append failing test**

```r
test_that("hop 2 raises when the pivot property names a non-axis", {
    d <- memory_daf(name = "bad-hop")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "donor", c("d1", "d2"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    set_vector(d, "donor", "not_an_axis", c("x", "y"))

    expect_error(
        get_query(d, "/ cell : donor =@ : not_an_axis =@ : anything"),
        "AsAxis target axis"
    )
})
```

- [ ] **Step 2: Run test**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-eval-chains.R", filter = NULL)'
```

Expected: PASS. The existing `.apply_chained_lookup_vector` already `stop()`s when `format_has_axis(daf, target_axis)` is false.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-query-eval-chains.R
git commit -m "test(evaluator): wrong-type pivot on chained hop raises"
```

### Task D7: Regression — full test suite green

- [ ] **Step 1: Run the full test suite**

Run:

```
cd /home/aviezerl/src/dafr-native
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: pass count >= 1055 (Slice 4 baseline) + 5 new tests = 1060+; 0 FAIL; 0 SKIP (the pre-existing scran/irlba WARN is OK).

- [ ] **Step 2: (no commit if no changes)**

---

## Phase E — ExampleData (cells + metacells + chain)

### Task E1: Copy Julia example_data/ into inst/extdata/example_data/

**Files:**

- Create: `inst/extdata/example_data/axes/c.cell.txt`, `inst/extdata/example_data/axes/c.donor.txt`, ... (18 files total)

**Pre-read:** Julia shipping path: `~/src/DataAxesFormats.jl/test/example_data/{axes,vectors,matrices}/`. Files:

- axes: `c.cell.txt`, `c.donor.txt`, `c.experiment.txt`, `c.gene.txt` (cells-only); `m.metacell.txt`, `m.type.txt` (metacells-only); `mc.cell.txt`, `mc.gene.txt` (both-shared)
- vectors: `c.cell.donor.txt`, `c.cell.experiment.txt`, `c.donor.age.txt`, `c.donor.sex.txt`, `c.gene.is_lateral.txt` (cells); `m.cell.metacell.txt`, `m.gene.is_marker.txt`, `m.metacell.type.txt`, `m.type.color.txt` (metacells)
- matrices: `c.cell.gene.UMIs.csv` (cells); `m.metacell.gene.fraction.csv`, `m.metacell.metacell.edge_weight.csv` (metacells)

The Julia `which in kind` check matches the `c`/`m` prefix against file name's first token; `mc.*` files are loaded for BOTH. We preserve that logic exactly.

- [ ] **Step 1: Copy the files**

```
cd /home/aviezerl/src/dafr-native
mkdir -p inst/extdata/example_data/axes inst/extdata/example_data/vectors inst/extdata/example_data/matrices
/bin/cp ~/src/DataAxesFormats.jl/test/example_data/axes/*.txt inst/extdata/example_data/axes/
/bin/cp ~/src/DataAxesFormats.jl/test/example_data/vectors/*.txt inst/extdata/example_data/vectors/
/bin/cp ~/src/DataAxesFormats.jl/test/example_data/matrices/*.csv inst/extdata/example_data/matrices/
```

- [ ] **Step 2: Verify file counts**

Run:

```
ls inst/extdata/example_data/axes/ | wc -l
ls inst/extdata/example_data/vectors/ | wc -l
ls inst/extdata/example_data/matrices/ | wc -l
```

Expected: 8, 9, 3.

- [ ] **Step 3: Commit**

```
git add inst/extdata/example_data
git commit -m "chore(example-data): import Julia test/example_data verbatim"
```

### Task E2: Scaffold R/example_data.R with loader helpers (test-first)

**Files:**

- Create: `R/example_data.R`
- Create: `tests/testthat/test-example-data.R`

- [ ] **Step 1: Write failing test for `.example_data_dir()`**

Create `tests/testthat/test-example-data.R`:

```r
test_that(".example_data_dir resolves to installed extdata", {
    d <- dafr:::.example_data_dir()
    expect_true(dir.exists(d))
    expect_true(dir.exists(file.path(d, "axes")))
    expect_true(dir.exists(file.path(d, "vectors")))
    expect_true(dir.exists(file.path(d, "matrices")))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: FAIL with "could not find function .example_data_dir".

- [ ] **Step 3: Implement `.example_data_dir()` + @include header**

Create `R/example_data.R`:

```r
#' @include classes.R memory_daf.R writers.R chain_daf.R
NULL

.example_data_dir <- function() {
    path <- system.file("extdata", "example_data", package = "dafr")
    if (!nzchar(path)) {
        # devtools::load_all path
        path <- file.path(
            rprojroot::find_package_root_file(),
            "inst", "extdata", "example_data"
        )
    }
    path
}
```

Note: `rprojroot` is a transitive testthat dep so it loads under `load_all`. If CI flags it, fall back to `testthat::test_path(...)` — but the `system.file` path works post-install.

- [ ] **Step 4: Run test to verify it passes**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: PASS.

- [ ] **Step 5: (no commit yet; continue to E3)**

### Task E3: `.cast_vector()` matches Julia's Bool → UInt32 → Int32 → Float32 lattice

**Files:**

- Modify: `R/example_data.R`
- Modify: `tests/testthat/test-example-data.R`

**Pre-read — Julia `cast_vector`:**

```julia
try return parse.(Bool, vector)  catch end
try
    vector = parse.(Float32, vector)
    for type in (UInt32, Int32)
        try return type.(vector)  catch end
    end
    return vector
catch end
return vector
```

Boolean wins first; else try integer promotions from a Float32 parse; else keep as string. We replicate this.

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-example-data.R`:

```r
test_that(".cast_vector parses Bool first", {
    expect_identical(dafr:::.cast_vector(c("true", "false", "true")),
                     c(TRUE, FALSE, TRUE))
})

test_that(".cast_vector promotes to UInt32 / integer", {
    v <- dafr:::.cast_vector(c("1", "2", "3"))
    expect_true(is.integer(v))
    expect_identical(v, c(1L, 2L, 3L))
})

test_that(".cast_vector keeps negative as integer", {
    v <- dafr:::.cast_vector(c("-1", "0", "5"))
    expect_true(is.integer(v))
})

test_that(".cast_vector falls through to character", {
    v <- dafr:::.cast_vector(c("apple", "banana"))
    expect_identical(v, c("apple", "banana"))
})

test_that(".cast_vector keeps numeric when not integer-valued", {
    v <- dafr:::.cast_vector(c("1.5", "2.5"))
    expect_true(is.double(v))
    expect_equal(v, c(1.5, 2.5))
})
```

- [ ] **Step 2: Run tests — they fail with "could not find function .cast_vector"**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: FAIL.

- [ ] **Step 3: Implement `.cast_vector()`**

Append to `R/example_data.R`:

```r
.cast_vector <- function(strs) {
    # Bool lattice — case-insensitive "true"/"false" only (matches Julia parse).
    if (all(tolower(strs) %in% c("true", "false"))) {
        return(tolower(strs) == "true")
    }
    # Float path: parse to double; if every value is integer-valued AND non-negative
    # and fits in UInt32 → return integer. Else if integer-valued → integer.
    # Else → double.
    num <- suppressWarnings(as.numeric(strs))
    if (anyNA(num)) {
        # Fall through to character.
        return(strs)
    }
    if (all(num == as.integer(num)) && all(!is.na(suppressWarnings(as.integer(num))))) {
        return(as.integer(num))
    }
    num
}
```

- [ ] **Step 4: Run tests — all cast_vector tests pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: PASS.

- [ ] **Step 5: (no commit yet; continue to E4)**

### Task E4: `.cast_matrix()` matches Julia's UInt8 → UInt16 → Float32 lattice

**Files:**

- Modify: `R/example_data.R`
- Modify: `tests/testthat/test-example-data.R`

**Pre-read — Julia `cast_matrix`:**

```julia
for type in (UInt8, UInt16)
    try return Matrix{type}(matrix) catch end
end
return matrix  # Float32
```

Try UInt8 first (0..255); UInt16 next (0..65535); else keep Float32. Matrix shape preserved.

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-example-data.R`:

```r
test_that(".cast_matrix promotes to integer when all values fit UInt8", {
    m <- matrix(c(0, 1, 2, 3, 4, 5), nrow = 2)
    out <- dafr:::.cast_matrix(m)
    expect_true(is.integer(out))
    expect_equal(dim(out), dim(m))
    expect_identical(out, matrix(c(0L, 1L, 2L, 3L, 4L, 5L), nrow = 2))
})

test_that(".cast_matrix keeps double for non-integer-valued entries", {
    m <- matrix(c(0.1, 1.2, 2.3, 3.4), nrow = 2)
    out <- dafr:::.cast_matrix(m)
    expect_true(is.double(out))
    expect_equal(out, m)
})

test_that(".cast_matrix keeps double for values above UInt16", {
    m <- matrix(c(70000, 80000, 90000, 100000), nrow = 2)
    out <- dafr:::.cast_matrix(m)
    expect_true(is.double(out))
})
```

- [ ] **Step 2: Run tests — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: FAIL (".cast_matrix not found").

- [ ] **Step 3: Implement `.cast_matrix()`**

Append to `R/example_data.R`:

```r
.cast_matrix <- function(m) {
    if (!is.double(m)) return(m)
    if (all(m == floor(m)) && all(m >= 0) && all(m <= 65535)) {
        storage.mode(m) <- "integer"
    }
    m
}
```

Note: R has no distinct UInt8/UInt16 — we collapse both to `integer`. Tests guarantee the same observable class as Julia would be observed through `inherits(m, "integer")` after dafr's matrix type-check (`.matrix_type_ok`).

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: PASS.

- [ ] **Step 5: (no commit yet; continue to E5)**

### Task E5: Per-file loaders — `.load_axis_file()`, `.load_vector_file()`, `.load_matrix_file()`

**Files:**

- Modify: `R/example_data.R`
- Modify: `tests/testthat/test-example-data.R`

**Pre-read:** The Julia loader inspects the filename, splits on `.`, checks whether the first part contains the `which` character (`c` or `m`), and dispatches. We do the same.

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-example-data.R`:

```r
test_that(".load_axis_file respects the cells/metacells/shared kind filter", {
    d <- memory_daf(name = "cells-axes")
    dafr:::.load_axis_file(d, "c", file.path(dafr:::.example_data_dir(), "axes", "c.cell.txt"))
    expect_true(has_axis(d, "cell"))
    expect_gt(axis_length(d, "cell"), 0L)

    d2 <- memory_daf(name = "m-skips-cells-only")
    dafr:::.load_axis_file(d2, "m", file.path(dafr:::.example_data_dir(), "axes", "c.cell.txt"))
    expect_false(has_axis(d2, "cell"))  # 'c' not in 'm'

    d3 <- memory_daf(name = "m-takes-shared")
    dafr:::.load_axis_file(d3, "m", file.path(dafr:::.example_data_dir(), "axes", "mc.cell.txt"))
    expect_true(has_axis(d3, "cell"))   # 'm' in 'mc'
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: FAIL (function not found).

- [ ] **Step 3: Implement all three loaders**

Append to `R/example_data.R`:

```r
.load_axis_file <- function(daf, which, path) {
    file <- basename(path)
    parts <- strsplit(file, ".", fixed = TRUE)[[1L]]
    stopifnot(length(parts) == 3L)
    kind <- parts[[1L]]; axis <- parts[[2L]]; suffix <- parts[[3L]]
    stopifnot(identical(suffix, "txt"))
    if (grepl(which, kind, fixed = TRUE)) {
        entries <- readLines(path, warn = FALSE)
        entries <- entries[nzchar(entries)]
        add_axis(daf, axis, entries)
    }
    invisible()
}

.load_vector_file <- function(daf, which, path) {
    file <- basename(path)
    parts <- strsplit(file, ".", fixed = TRUE)[[1L]]
    stopifnot(length(parts) == 4L)
    kind <- parts[[1L]]; axis <- parts[[2L]]; prop <- parts[[3L]]; suffix <- parts[[4L]]
    stopifnot(identical(suffix, "txt"))
    if (grepl(which, kind, fixed = TRUE)) {
        raw <- readLines(path, warn = FALSE)
        # Julia uses mmap_file_lines which keeps empty trailing lines out.
        raw <- raw[nzchar(raw) | seq_along(raw) < length(raw)]
        # Actually Julia's parse.(String, …) preserves every line; strip a
        # trailing empty from a trailing newline only.
        if (length(raw) > 0L && !nzchar(raw[[length(raw)]])) {
            raw <- raw[-length(raw)]
        }
        vec <- .cast_vector(raw)
        set_vector(daf, axis, prop, vec)
    }
    invisible()
}

.load_matrix_file <- function(daf, which, path) {
    file <- basename(path)
    parts <- strsplit(file, ".", fixed = TRUE)[[1L]]
    stopifnot(length(parts) == 5L)
    kind <- parts[[1L]]; lines_axis <- parts[[2L]]; vals_axis <- parts[[3L]]
    prop <- parts[[4L]]; suffix <- parts[[5L]]
    stopifnot(identical(suffix, "csv"))
    if (grepl(which, kind, fixed = TRUE)) {
        lines <- readLines(path, warn = FALSE)
        if (length(lines) > 0L && !nzchar(lines[[length(lines)]])) {
            lines <- lines[-length(lines)]
        }
        # Julia: matrix[:, line_index] = parse.(Float32, split(line, ","))
        # i.e. each line is a column. We build a Float32 matrix with shape
        # (n_values, n_lines) — rows = vals_axis entries, cols = lines_axis.
        n_lines <- length(lines)
        first_row <- strsplit(lines[[1L]], ",", fixed = TRUE)[[1L]]
        n_values <- length(first_row)
        m <- matrix(0, nrow = n_values, ncol = n_lines)
        for (j in seq_along(lines)) {
            m[, j] <- as.numeric(strsplit(lines[[j]], ",", fixed = TRUE)[[1L]])
        }
        m <- .cast_matrix(m)
        # Julia: set_matrix!(daf, values_axis, lines_axis, property, matrix; relayout = eltype <: Integer)
        # In R: rows_axis = vals_axis, columns_axis = lines_axis.
        set_matrix(daf, vals_axis, lines_axis, prop, m)
        if (is.integer(m)) {
            relayout_matrix(daf, vals_axis, lines_axis, prop)
        }
    }
    invisible()
}
```

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: PASS.

- [ ] **Step 5: (no commit yet; continue to E6)**

### Task E6: `example_cells_daf()` — public entry, byte-parity with Julia

**Files:**

- Modify: `R/example_data.R`
- Modify: `tests/testthat/test-example-data.R`

- [ ] **Step 1: Write failing test**

Append to `tests/testthat/test-example-data.R`:

```r
test_that("example_cells_daf produces the expected Julia-parity MemoryDaf", {
    d <- example_cells_daf()
    expect_s3_class(d, "dafr::MemoryDaf")
    expect_identical(S7::prop(d, "name"), "cells!")

    expect_identical(get_scalar(d, "organism"), "human")
    expect_identical(get_scalar(d, "reference"), "test")

    expect_setequal(axes_set(d), c("cell", "donor", "experiment", "gene"))
    expect_identical(axis_length(d, "cell"), 856L)
    expect_identical(axis_length(d, "gene"), 683L)
    expect_identical(axis_length(d, "donor"), 95L)
    expect_identical(axis_length(d, "experiment"), 23L)

    # Donor vectors
    expect_true(is.integer(get_vector(d, "donor", "age")))
    expect_true(is.character(get_vector(d, "donor", "sex")))
    # Cell vectors
    expect_true(is.character(get_vector(d, "cell", "donor")))
    expect_true(is.character(get_vector(d, "cell", "experiment")))
    # Gene vectors
    expect_true(is.logical(get_vector(d, "gene", "is_lateral")))

    # UMIs matrix: cell x gene (integer, relayouted)
    m <- get_matrix(d, "cell", "gene", "UMIs")
    expect_equal(dim(m), c(856L, 683L))
    expect_true(is.integer(m[1L]))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: FAIL (function not found).

- [ ] **Step 3: Implement `.example_daf()` + `example_cells_daf()`**

Append to `R/example_data.R`:

```r
.example_daf <- function(which, name) {
    daf <- memory_daf(name = name)
    if (identical(which, "c")) {
        set_scalar(daf, "organism", "human")
        set_scalar(daf, "reference", "test")
    }
    dir <- .example_data_dir()

    # Axes: load in sorted order for determinism.
    axes_files <- sort(list.files(file.path(dir, "axes"), full.names = TRUE),
                       method = "radix")
    for (f in axes_files) {
        .load_axis_file(daf, which, f)
    }

    vec_files <- sort(list.files(file.path(dir, "vectors"), full.names = TRUE),
                      method = "radix")
    for (f in vec_files) {
        .load_vector_file(daf, which, f)
    }

    mat_files <- sort(list.files(file.path(dir, "matrices"), full.names = TRUE),
                      method = "radix")
    for (f in mat_files) {
        .load_matrix_file(daf, which, f)
    }
    daf
}

#' Load the cells example data into a `MemoryDaf`.
#'
#' Matches Julia `DataAxesFormats.ExampleData.example_cells_daf()` byte-for-byte:
#' 856 cells, 683 genes, 95 donors, 23 experiments; cell × gene UMIs matrix;
#' per-axis vectors (`donor.age`, `donor.sex`, `cell.donor`, `cell.experiment`,
#' `gene.is_lateral`); scalars `organism = "human"` / `reference = "test"`.
#'
#' @param name Name of the returned daf. Default `"cells!"`.
#' @return A `MemoryDaf`.
#' @export
#' @examples
#' d <- example_cells_daf()
#' axes_set(d)
example_cells_daf <- function(name = "cells!") {
    .example_daf("c", name)
}
```

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: PASS.

- [ ] **Step 5: (no commit yet; continue to E7)**

### Task E7: `example_metacells_daf()` + `example_chain_daf()`

**Files:**

- Modify: `R/example_data.R`
- Modify: `tests/testthat/test-example-data.R`

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-example-data.R`:

```r
test_that("example_metacells_daf produces the expected MemoryDaf", {
    d <- example_metacells_daf()
    expect_s3_class(d, "dafr::MemoryDaf")
    expect_identical(S7::prop(d, "name"), "metacells!")

    expect_setequal(axes_set(d), c("cell", "gene", "metacell", "type"))
    expect_identical(axis_length(d, "metacell"), 7L)
    expect_identical(axis_length(d, "type"), 4L)

    # Expected vectors
    expect_true(is.character(get_vector(d, "cell", "metacell")))
    expect_true(is.logical(get_vector(d, "gene", "is_marker")))
    expect_true(is.character(get_vector(d, "metacell", "type")))
    expect_true(is.character(get_vector(d, "type", "color")))

    # Matrices
    m1 <- get_matrix(d, "gene", "metacell", "fraction")
    expect_equal(dim(m1), c(683L, 7L))
    expect_true(is.double(m1[1L]))  # Float32 in Julia → double in R

    m2 <- get_matrix(d, "metacell", "metacell", "edge_weight")
    expect_equal(dim(m2), c(7L, 7L))
})

test_that("example_chain_daf chains both datasets via chain_writer", {
    d <- example_chain_daf()
    expect_s3_class(d, "dafr::WriteChainDaf")
    expect_identical(S7::prop(d, "name"), "chain!")

    # Union of both axes
    expect_setequal(
        axes_set(d),
        c("cell", "donor", "experiment", "gene", "metacell", "type")
    )
    # Scalars from cells (metacells has none)
    expect_identical(get_scalar(d, "organism"), "human")
    # Vectors from both
    expect_true(is.logical(get_vector(d, "gene", "is_lateral")))  # from cells
    expect_true(is.logical(get_vector(d, "gene", "is_marker")))   # from metacells
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: FAIL.

- [ ] **Step 3: Implement**

Append to `R/example_data.R`:

```r
#' Load the metacells example data into a `MemoryDaf`.
#'
#' Matches Julia `DataAxesFormats.ExampleData.example_metacells_daf()`:
#' 7 metacells, 4 types, plus the shared `cell` and `gene` axes; fraction +
#' edge_weight matrices; `gene.is_marker`, `cell.metacell`, `metacell.type`,
#' `type.color`.
#'
#' @param name Name of the returned daf. Default `"metacells!"`.
#' @return A `MemoryDaf`.
#' @export
example_metacells_daf <- function(name = "metacells!") {
    .example_daf("m", name)
}

#' Chain the cells and metacells example data via `chain_writer`.
#'
#' Equivalent to `chain_writer(list(example_cells_daf(), example_metacells_daf()))`.
#'
#' @param name Name of the returned chain. Default `"chain!"`.
#' @return A `WriteChainDaf`.
#' @export
example_chain_daf <- function(name = "chain!") {
    chain_writer(
        list(example_cells_daf(), example_metacells_daf()),
        name = name
    )
}
```

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: PASS.

- [ ] **Step 5: Commit Phase E in one logical bundle**

```
git add R/example_data.R tests/testthat/test-example-data.R
git commit -m "$(cat <<'EOF'
feat(example-data): port Julia ExampleData module

example_cells_daf() / example_metacells_daf() / example_chain_daf()
load byte-parity versions of the Julia example datasets from
inst/extdata/example_data (copied from DataAxesFormats.jl). Loader
replicates Julia's .cast_vector / .cast_matrix promotion lattice
(Bool → UInt32/Int32 → Float32; UInt8 → UInt16 → Float32).
EOF
)"
```

### Task E8: Parity spot-check against Slice 3's FilesDaf dump

**Files:**

- Modify: `tests/testthat/test-example-data.R`

**Pre-read:** `tests/testthat/fixtures/julia-queries/example-daf/` is a FilesDaf dump of Julia's `example_cells_daf()`, written during Slice 3. We spot-check byte-parity by opening both and comparing a handful of exactly-known properties.

- [ ] **Step 1: Append parity test**

```r
test_that("example_cells_daf matches Slice-3 FilesDaf dump entry-for-entry", {
    skip_if_not(
        dir.exists("fixtures/julia-queries/example-daf"),
        "Julia FilesDaf fixture absent"
    )
    fdd <- files_daf("fixtures/julia-queries/example-daf", mode = "r")
    mdd <- example_cells_daf()

    expect_identical(axes_set(mdd), axes_set(fdd))
    for (ax in axes_set(fdd)) {
        expect_identical(axis_vector(mdd, ax), axis_vector(fdd, ax),
            info = sprintf("axis %s", ax))
    }
    expect_identical(get_scalar(mdd, "organism"), get_scalar(fdd, "organism"))
    expect_identical(get_scalar(mdd, "reference"), get_scalar(fdd, "reference"))

    # Vector parity (unname — FilesDaf may carry dim-names)
    expect_identical(
        unname(get_vector(mdd, "donor", "age")),
        unname(get_vector(fdd, "donor", "age"))
    )
    expect_identical(
        unname(get_vector(mdd, "gene", "is_lateral")),
        unname(get_vector(fdd, "gene", "is_lateral"))
    )

    # Matrix parity
    expect_equal(
        unname(get_matrix(mdd, "cell", "gene", "UMIs")),
        unname(get_matrix(fdd, "cell", "gene", "UMIs"))
    )
})
```

- [ ] **Step 2: Run test**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-example-data.R")'
```

Expected: PASS — if it fails, the loader has a cast or parse divergence from Julia. Debug: `.cast_vector` for the specific axis/property that differs.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-example-data.R
git commit -m "test(example-data): parity with Slice-3 FilesDaf dump"
```

---

## Phase A — Computations HOF

### Task A1: Scaffold R/computations.R + test file

**Files:**

- Create: `R/computations.R`
- Create: `tests/testthat/test-computations.R`

- [ ] **Step 1: Write failing test — `computation()` exists**

Create `tests/testthat/test-computations.R`:

```r
test_that("computation() wraps a function and returns a callable", {
    c <- Contract()
    fn <- function(daf) daf
    wrapped <- computation("noop", c, fn)
    expect_true(is.function(wrapped))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-computations.R")'
```

Expected: FAIL ("could not find function \"computation\"").

- [ ] **Step 3: Create the file + stub**

Create `R/computations.R`:

```r
#' @include classes.R contracts.R
NULL

#' Wrap a function so a contract is enforced on every call.
#'
#' Returns a closure that, on each call, wraps its first (`daf`) argument in
#' a `contractor()`, runs `verify_input()`, executes `fn`, then runs
#' `verify_output()`, and returns `fn`'s result. When contract enforcement is
#' disabled (env `DAF_ENFORCE_CONTRACTS` / option `dafr.enforce_contracts`
#' are both falsy), the wrapping is still performed but verification is a
#' no-op, so the wrapper is cheap to leave in place.
#'
#' Scope: single contract (matching Julia's exported `@computation Contract`
#' form). Two-contract / three-contract variants (Julia UNTESTED) are not
#' supported in this slice; pass a single `Contract` or use a merged one via
#' `merge_contracts`.
#'
#' @param name Human-readable computation name (character scalar). Used in
#'   contract violation messages and as a suffix on the wrapper's daf name.
#' @param contract A `Contract()` describing the inputs and outputs.
#' @param fn The function to wrap. Must accept a `DafReader`/`DafWriter` as
#'   its first argument.
#' @return A function with the same signature as `fn`. The bound `contract`
#'   and `name` are attached as attributes (`dafr_contract`, `dafr_computation_name`).
#' @seealso [function_contract()], [contract_description()], [contractor()].
#' @export
computation <- function(name, contract, fn) {
    stop("computation() not yet implemented")  # placeholder for A2
}
```

- [ ] **Step 4: Run test — it runs but fails**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-computations.R")'
```

Expected: FAIL with the placeholder "not yet implemented".

- [ ] **Step 5: (no commit yet; continue to A2)**

### Task A2: Implement `computation()` — no-enforcement path

**Files:**

- Modify: `R/computations.R`
- Modify: `tests/testthat/test-computations.R`

- [ ] **Step 1: Replace placeholder with implementation**

Replace the `computation()` body in `R/computations.R` with:

```r
computation <- function(name, contract, fn) {
    stopifnot(is.character(name), length(name) == 1L, nzchar(name))
    if (!S7::S7_inherits(contract, Contract)) {
        stop("`contract` must be a Contract()", call. = FALSE)
    }
    if (!is.function(fn)) {
        stop("`fn` must be a function", call. = FALSE)
    }

    wrapped <- function(daf, ...) {
        if (!S7::S7_inherits(daf, DafReader)) {
            stop(sprintf(
                "first argument to computation %s must be a DafReader",
                sQuote(name)
            ), call. = FALSE)
        }
        wrapped_daf <- contractor(
            computation = name, contract = contract, daf = daf
        )
        verify_input(wrapped_daf)
        result <- fn(wrapped_daf, ...)
        verify_output(wrapped_daf)
        result
    }
    attr(wrapped, "dafr_contract") <- contract
    attr(wrapped, "dafr_computation_name") <- name
    wrapped
}
```

- [ ] **Step 2: Extend test**

Replace the test in `tests/testthat/test-computations.R`:

```r
test_that("computation() wraps and is callable; returns fn's result", {
    c <- Contract()
    fn <- function(daf) "result-sentinel"
    wrapped <- computation("noop", c, fn)
    expect_true(is.function(wrapped))

    d <- memory_daf(name = "t1")
    expect_identical(wrapped(d), "result-sentinel")
})

test_that("computation() rejects non-Contract second argument", {
    expect_error(
        computation("bad", list(), function(d) d),
        "must be a Contract"
    )
})

test_that("computation() rejects non-DafReader first call-arg", {
    c <- Contract()
    w <- computation("typed", c, function(d) d)
    expect_error(w(42), "must be a DafReader")
})
```

- [ ] **Step 3: Run tests — all pass**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-computations.R")'
```

Expected: PASS.

- [ ] **Step 4: Commit**

```
git add R/computations.R tests/testthat/test-computations.R
git commit -m "$(cat <<'EOF'
feat(computations): computation() HOF — base path + typecheck guards

Wraps a function with contractor() + verify_input/output around each
call. Contract is bound as an attribute (dafr_contract) for later
retrieval via function_contract(). No-enforcement path verified;
enforcement tests land in A3.
EOF
)"
```

### Task A3: `computation()` under enforcement — verify fires

**Files:**

- Modify: `tests/testthat/test-computations.R`

- [ ] **Step 1: Write failing test**

Append to `tests/testthat/test-computations.R`:

```r
test_that("computation() raises on missing RequiredInput under enforcement", {
    withr::local_options(list(dafr.enforce_contracts = TRUE))
    c <- Contract(
        data = list(contract_vector(
            axis = "cell", name = "donor",
            expectation = RequiredInput, type = "character",
            description = "donor id"
        ))
    )
    fn <- function(daf) get_vector(daf, "cell", "donor")
    w <- computation("needs-donor", c, fn)

    d <- memory_daf(name = "empty")
    add_axis(d, "cell", c("c1", "c2"))
    expect_error(w(d), "missing input vector: donor")
})

test_that("computation() raises on missing CreatedOutput after call", {
    withr::local_options(list(dafr.enforce_contracts = TRUE))
    c <- Contract(
        data = list(contract_vector(
            axis = "cell", name = "score",
            expectation = CreatedOutput, type = "double",
            description = "per-cell score"
        ))
    )
    fn <- function(daf) "did-nothing"  # does not set the vector
    w <- computation("produces-score", c, fn)

    d <- memory_daf(name = "e")
    add_axis(d, "cell", c("c1"))
    expect_error(w(d), "missing output vector: score")
})

test_that("computation() succeeds when contract is honored", {
    withr::local_options(list(dafr.enforce_contracts = TRUE))
    c <- Contract(
        data = list(
            contract_vector("cell", "donor", RequiredInput, "character", "in"),
            contract_vector("cell", "age", CreatedOutput, "integer", "out")
        )
    )
    fn <- function(daf) {
        donors <- get_vector(daf, "cell", "donor")
        set_vector(daf, "cell", "age", rep(0L, length(donors)))
        "ok"
    }
    w <- computation("ages", c, fn)
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("d1", "d2"))
    expect_identical(w(d), "ok")
    expect_identical(unname(get_vector(d, "cell", "age")), c(0L, 0L))
})
```

- [ ] **Step 2: Run tests — all pass (the plumbing is already in place)**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-computations.R")'
```

Expected: PASS. If any fail, the issue is almost certainly in the enforcement branch of `contractor()` (cache-sharing, for instance). Inspect `contractor()` in `R/contracts.R`.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-computations.R
git commit -m "test(computations): enforcement path — missing input/output + happy path"
```

### Task A4: `function_contract()` + `contract_description()`

**Files:**

- Modify: `R/computations.R`
- Modify: `tests/testthat/test-computations.R`

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-computations.R`:

```r
test_that("function_contract() retrieves the bound contract", {
    c <- Contract(
        axes = list(cell = list(RequiredInput, "per-cell axis")),
        data = list(contract_vector("cell", "donor", RequiredInput, "character", "id"))
    )
    w <- computation("demo", c, function(d) d)
    got <- function_contract(w)
    expect_true(S7::S7_inherits(got, Contract))
    expect_identical(got, c)
})

test_that("function_contract() errors on an unwrapped function", {
    expect_error(function_contract(function(x) x), "no dafr contract bound")
})

test_that("contract_description() produces a non-empty string", {
    c <- Contract(
        axes = list(cell = list(RequiredInput, "per-cell axis")),
        data = list(contract_vector("cell", "donor", RequiredInput, "character", "id"))
    )
    s <- contract_description(c)
    expect_true(is.character(s) && length(s) == 1L && nzchar(s))
    expect_true(grepl("RequiredInput", s))
    expect_true(grepl("cell", s))
    expect_true(grepl("donor", s))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-computations.R")'
```

Expected: FAIL (functions not found).

- [ ] **Step 3: Implement both functions**

Append to `R/computations.R`:

```r
#' Retrieve the contract bound to a wrapped computation.
#'
#' Mirror of Julia `function_contract(fn)`. Errors if `fn` was not wrapped
#' by `computation()` in this session.
#'
#' @param fn A function returned by `computation()`.
#' @return A `Contract`.
#' @seealso [computation()].
#' @export
function_contract <- function(fn) {
    contract <- attr(fn, "dafr_contract")
    if (is.null(contract)) {
        stop("no dafr contract bound to this function", call. = FALSE)
    }
    contract
}

#' Render a contract as a human-readable multi-line string.
#'
#' Intended for splicing into roxygen docstrings of functions created by
#' `computation()`. Sections rendered: axes (with expectation + description),
#' scalars, vectors (per axis), matrices (per axis pair).
#'
#' @param contract A `Contract`.
#' @return A character scalar.
#' @export
contract_description <- function(contract) {
    if (!S7::S7_inherits(contract, Contract)) {
        stop("`contract` must be a Contract", call. = FALSE)
    }
    lines <- character()
    if (length(contract@axes) > 0L) {
        lines <- c(lines, "Axes:")
        for (a in names(contract@axes)) {
            spec <- contract@axes[[a]]
            lines <- c(lines, sprintf("  %s (%s): %s", a, spec[[1L]], spec[[2L]]))
        }
    }
    scalars <- Filter(function(r) identical(r$kind, "scalar"), contract@data)
    vectors <- Filter(function(r) identical(r$kind, "vector"), contract@data)
    matrices <- Filter(function(r) identical(r$kind, "matrix"), contract@data)
    if (length(scalars) > 0L) {
        lines <- c(lines, "Scalars:")
        for (r in scalars) {
            lines <- c(lines, sprintf("  %s (%s, %s): %s",
                r$name, r$expectation, r$type, r$description
            ))
        }
    }
    if (length(vectors) > 0L) {
        lines <- c(lines, "Vectors:")
        for (r in vectors) {
            lines <- c(lines, sprintf("  %s / %s (%s, %s): %s",
                r$axis, r$name, r$expectation, r$type, r$description
            ))
        }
    }
    if (length(matrices) > 0L) {
        lines <- c(lines, "Matrices:")
        for (r in matrices) {
            lines <- c(lines, sprintf("  %s, %s / %s (%s, %s): %s",
                r$rows_axis, r$columns_axis, r$name,
                r$expectation, r$type, r$description
            ))
        }
    }
    paste(lines, collapse = "\n")
}
```

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-computations.R")'
```

Expected: PASS.

- [ ] **Step 5: Commit**

```
git add R/computations.R tests/testthat/test-computations.R
git commit -m "feat(computations): function_contract + contract_description"
```

### Task A5: Regression — full suite

- [ ] **Step 1: Run**

```
cd /home/aviezerl/src/dafr-native
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: at least 1060 PASS + all A tests; 0 FAIL; the one pre-existing scran/irlba WARN.

---

## Phase B — Adapter + internal copy_view_to_daf

### Task B1: Scaffold R/adapters.R + test file

**Files:**

- Create: `R/adapters.R`
- Create: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Write failing scaffold test**

Create `tests/testthat/test-adapters.R`:

```r
test_that("adapter() exists and requires a DafWriter", {
    expect_error(adapter(memory_daf(), function(d) d), "no-op adapter")
    # no-op adapter: neither input_* nor output_* supplied
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: FAIL ("could not find function adapter").

- [ ] **Step 3: Create R/adapters.R skeleton**

Create `R/adapters.R`:

```r
#' @include classes.R view_daf.R chain_daf.R memory_daf.R writers.R format_api.R
NULL

#' Apply a computation to a renaming view of a DafWriter.
#'
#' Mirrors Julia `adapter(computation, daf; input_axes, input_data, capture,
#' output_axes, output_data, empty, relayout, overwrite, name)`. The typical
#' use is to run a `@computation` (R: `computation()`-wrapped) function whose
#' expected property names differ from the names stored in `daf`.
#'
#' Flow:
#' 1. `input = viewer(daf, axes = input_axes, data = input_data)` exposes the
#'    subset the computation consumes, possibly under renamed axes / names.
#' 2. `capture = capture_factory(name = "<base>.capture")` is a fresh writable.
#' 3. `adapted = chain_writer(list(input, capture))` — reads fall through to
#'    `input`, writes go to `capture`.
#' 4. `result = fn(adapted)` — the computation's return value.
#' 5. `output = viewer(adapted, axes = output_axes, data = output_data)` —
#'    selects + renames the outputs.
#' 6. Copy `output` into `daf` via an internal helper. Honors `overwrite`,
#'    `relayout`, and `empty`.
#' 7. Return `result`.
#'
#' @param daf A `DafWriter` — the base data to read from and write into.
#' @param fn A function taking a single `DafWriter` argument (the `adapted`
#'   chain). Return value passes through.
#' @param input_axes,input_data Passed through to [viewer()] for the input
#'   view. At least one of these or `output_axes` / `output_data` must be
#'   non-NULL (otherwise `adapter()` degenerates to `fn(daf)`).
#' @param output_axes,output_data Passed through to [viewer()] for the
#'   output view.
#' @param capture Factory function returning a fresh `DafWriter`. Default
#'   `memory_daf`.
#' @param empty Named list `list("<axis>|<vector>" = default, "<r>|<c>|<m>" =
#'   default)` supplying default values for entries present in `daf`'s axis
#'   but absent from the source view's. NULL (default) disables the feature.
#' @param relayout If `TRUE` (default), matrix copies also write the
#'   transposed layout.
#' @param overwrite If `TRUE`, pre-existing destination entries are replaced.
#' @param name Human-readable name for the input/capture/adapted dafs. Default
#'   `".adapter"`.
#' @return The return value of `fn(adapted)`.
#' @seealso [viewer()], [chain_writer()], [computation()].
#' @export
adapter <- function(daf, fn,
                    input_axes = NULL, input_data = NULL,
                    output_axes = NULL, output_data = NULL,
                    capture = memory_daf,
                    empty = NULL, relayout = TRUE, overwrite = FALSE,
                    name = ".adapter") {
    stop("adapter() not yet implemented")
}
```

- [ ] **Step 4: (no commit yet; continue to B2)**

### Task B2: Internal `.copy_view_to_daf()` — scalars

**Files:**

- Modify: `R/adapters.R`
- Modify: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Write failing test**

Append to `tests/testthat/test-adapters.R`:

```r
test_that(".copy_view_to_daf copies scalars from a view into dest", {
    src <- memory_daf(name = "src")
    set_scalar(src, "alpha", 1L)
    set_scalar(src, "beta", "b")
    v <- viewer(src)
    dest <- memory_daf(name = "dest")

    dafr:::.copy_view_to_daf(source_view = v, dest = dest,
                             empty = NULL, relayout = FALSE, overwrite = FALSE)
    expect_identical(get_scalar(dest, "alpha"), 1L)
    expect_identical(get_scalar(dest, "beta"), "b")
})

test_that(".copy_view_to_daf errors on pre-existing scalars unless overwrite", {
    src <- memory_daf(name = "s")
    set_scalar(src, "x", 1L)
    dest <- memory_daf(name = "d")
    set_scalar(dest, "x", 2L)

    expect_error(
        dafr:::.copy_view_to_daf(viewer(src), dest, NULL, FALSE, FALSE),
        "already exists"
    )
    dafr:::.copy_view_to_daf(viewer(src), dest, NULL, FALSE, TRUE)
    expect_identical(get_scalar(dest, "x"), 1L)
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: FAIL (function not found).

- [ ] **Step 3: Implement scalar-copy path**

Append to `R/adapters.R`:

```r
.copy_view_to_daf <- function(source_view, dest, empty = NULL,
                              relayout = TRUE, overwrite = FALSE) {
    # 1. Scalars
    for (nm in format_scalars_set(source_view)) {
        val <- format_get_scalar(source_view, nm)
        format_set_scalar(dest, nm, val, overwrite)
    }
    invisible()
}
```

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS.

- [ ] **Step 5: (no commit yet; continue to B3)**

### Task B3: `.copy_view_to_daf()` — axes and vectors

**Files:**

- Modify: `R/adapters.R`
- Modify: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Write failing tests**

Append to `tests/testthat/test-adapters.R`:

```r
test_that(".copy_view_to_daf copies axes + vectors (view axis names preserved)", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1", "c2", "c3"))
    set_vector(src, "cell", "donor", c("d1", "d2", "d3"))
    v <- viewer(src)
    dest <- memory_daf(name = "d")

    dafr:::.copy_view_to_daf(v, dest, NULL, FALSE, FALSE)
    expect_identical(axis_vector(dest, "cell"), c("c1", "c2", "c3"))
    expect_identical(unname(get_vector(dest, "cell", "donor")),
                     c("d1", "d2", "d3"))
})

test_that(".copy_view_to_daf copies renamed axes under their view name", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1", "c2"))
    set_vector(src, "cell", "donor", c("d1", "d2"))
    # Rename cell -> obs via a view query; data falls through unchanged.
    v <- viewer(src, axes = list(list("obs", "cell"), list("cell", NULL)))
    dest <- memory_daf(name = "d")

    dafr:::.copy_view_to_daf(v, dest, NULL, FALSE, FALSE)
    expect_true(has_axis(dest, "obs"))
    expect_false(has_axis(dest, "cell"))
    expect_identical(axis_vector(dest, "obs"), c("c1", "c2"))
    expect_identical(unname(get_vector(dest, "obs", "donor")),
                     c("d1", "d2"))
})

test_that(".copy_view_to_daf errors on axis collision unless overwrite", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1"))
    dest <- memory_daf(name = "d")
    add_axis(dest, "cell", c("c1"))

    expect_error(
        dafr:::.copy_view_to_daf(viewer(src), dest, NULL, FALSE, FALSE),
        "already exists"
    )
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: FAIL (axes/vectors not handled).

- [ ] **Step 3: Extend `.copy_view_to_daf` with axes + vectors**

Replace the existing body with:

```r
.copy_view_to_daf <- function(source_view, dest, empty = NULL,
                              relayout = TRUE, overwrite = FALSE) {
    # 1. Scalars
    for (nm in format_scalars_set(source_view)) {
        val <- format_get_scalar(source_view, nm)
        format_set_scalar(dest, nm, val, overwrite)
    }
    # 2. Axes
    for (ax in format_axes_set(source_view)) {
        if (format_has_axis(dest, ax)) {
            if (!overwrite) {
                stop(sprintf("axis %s already exists in destination",
                    sQuote(ax)
                ), call. = FALSE)
            }
            format_delete_axis(dest, ax, must_exist = TRUE)
        }
        entries <- format_axis_array(source_view, ax)
        format_add_axis(dest, ax, entries)
    }
    # 3. Vectors
    for (ax in format_axes_set(source_view)) {
        for (vn in format_vectors_set(source_view, ax)) {
            val <- format_get_vector(source_view, ax, vn)
            format_set_vector(dest, ax, vn, val, overwrite)
        }
    }
    invisible()
}
```

Note: `format_delete_axis` cascades to vectors/matrices, so the overwrite path re-uses that invariant. The `must_exist = TRUE` raises if the axis was already pruned.

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS.

- [ ] **Step 5: (no commit yet; continue to B4)**

### Task B4: `.copy_view_to_daf()` — matrices (with relayout)

**Files:**

- Modify: `R/adapters.R`
- Modify: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Write failing test**

Append to `tests/testthat/test-adapters.R`:

```r
test_that(".copy_view_to_daf copies matrices + relayouts when asked", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1", "c2"))
    add_axis(src, "gene", c("g1", "g2", "g3"))
    m <- matrix(1:6, nrow = 2, dimnames = list(c("c1","c2"), c("g1","g2","g3")))
    set_matrix(src, "cell", "gene", "UMIs", m)
    v <- viewer(src)
    dest <- memory_daf(name = "d")

    dafr:::.copy_view_to_daf(v, dest, NULL, relayout = TRUE, overwrite = FALSE)
    got <- get_matrix(dest, "cell", "gene", "UMIs")
    expect_equal(unname(got), unname(m))

    # Relayout should have stored the transpose as well
    expect_true(format_has_matrix(dest, "gene", "cell", "UMIs"))
})

test_that(".copy_view_to_daf matrix without relayout skips transpose store", {
    src <- memory_daf(name = "s")
    add_axis(src, "cell", c("c1"))
    add_axis(src, "gene", c("g1", "g2"))
    set_matrix(src, "cell", "gene", "UMIs",
        matrix(c(5, 9), nrow = 1,
               dimnames = list("c1", c("g1","g2"))))
    v <- viewer(src)
    dest <- memory_daf(name = "d")

    dafr:::.copy_view_to_daf(v, dest, NULL, relayout = FALSE, overwrite = FALSE)
    expect_true(format_has_matrix(dest, "cell", "gene", "UMIs"))
    # Without relayout, transpose is NOT physically stored.
    # (Readers may still synthesize it on demand; check the raw format_* path.)
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: FAIL.

- [ ] **Step 3: Extend `.copy_view_to_daf` with matrix loop**

After the vectors loop in `.copy_view_to_daf`, add:

```r
    # 4. Matrices: iterate axis pairs; set + optional relayout.
    axes <- format_axes_set(source_view)
    for (ra in axes) {
        for (ca in axes) {
            for (mn in format_matrices_set(source_view, ra, ca)) {
                val <- format_get_matrix(source_view, ra, ca, mn)
                format_set_matrix(dest, ra, ca, mn, val, overwrite)
                if (relayout) {
                    # Store the transpose too if the destination can do so.
                    if (ra != ca) {
                        format_relayout_matrix(dest, ra, ca, mn)
                    }
                }
            }
        }
    }
```

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS.

- [ ] **Step 5: Commit Phase B helpers so far**

```
git add R/adapters.R tests/testthat/test-adapters.R
git commit -m "$(cat <<'EOF'
feat(adapters): internal .copy_view_to_daf — scalars/axes/vectors/matrices

Copies a view's observable state into a destination DafWriter. Honors
overwrite on every per-entry write, cascades axis-overwrite through
format_delete_axis, and optionally relayouts matrices (row-major
transpose stored) when relayout = TRUE. Kept internal — public
copy_all! surface deferred to Slice 6.
EOF
)"
```

### Task B5: `.copy_view_to_daf()` — `empty` per-entry default

**Files:**

- Modify: `R/adapters.R`
- Modify: `tests/testthat/test-adapters.R`

**Pre-read:** Minimal `empty` support: when the source view exposes a SUBSET of the destination axis's entries (by index), the copied vector / matrix is padded at entries not in the source, using the default value keyed by `"<axis>|<vector>"` / `"<rows>|<cols>|<matrix>"`. This is the bare-minimum needed for the Julia-parity fixture in Phase J.

- [ ] **Step 1: Write failing test**

Append to `tests/testthat/test-adapters.R`:

```r
test_that(".copy_view_to_daf honors `empty` for a subset-axis vector", {
    # Source has only {c1, c3}; dest has {c1, c2, c3}. Copy should pad c2
    # via the `empty` default. We skip the filter-view route here (query
    # semantics are tested separately in test-view-filter-propagation.R)
    # and go straight to viewer() on a smaller memory_daf — same code path
    # in .copy_view_to_daf.
    src <- memory_daf(name = "src-subset")
    add_axis(src, "cell", c("c1", "c3"))
    set_vector(src, "cell", "donor", c("d1", "d3"))

    dest <- memory_daf(name = "dest-full")
    add_axis(dest, "cell", c("c1", "c2", "c3"))

    dafr:::.copy_view_to_daf(viewer(src), dest,
        empty = list("cell|donor" = "MISSING"),
        relayout = FALSE, overwrite = FALSE
    )
    expect_identical(
        unname(get_vector(dest, "cell", "donor")),
        c("d1", "MISSING", "d3")
    )
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: FAIL (the current copy overwrites the full axis, which the dest already has → raises).

- [ ] **Step 3: Extend copy helper to support subset-axis with `empty`**

Replace the body of `.copy_view_to_daf` with a version that checks axis pre-existence on dest and, when the dest axis is a superset, pads via `empty`:

```r
.copy_view_to_daf <- function(source_view, dest, empty = NULL,
                              relayout = TRUE, overwrite = FALSE) {
    for (nm in format_scalars_set(source_view)) {
        format_set_scalar(dest, nm, format_get_scalar(source_view, nm),
                          overwrite)
    }

    # For each axis in source: if dest doesn't have it, add it verbatim;
    # if dest has it as a SUPERSET (source entries all in dest), pad mode
    # is enabled — per-vector / per-matrix `empty` defaults fill gaps.
    # Otherwise (collision) — overwrite gate.
    axis_mode <- list()  # axis -> "new" | "pad" | "replace"
    for (ax in format_axes_set(source_view)) {
        src_entries <- format_axis_array(source_view, ax)
        if (!format_has_axis(dest, ax)) {
            format_add_axis(dest, ax, src_entries)
            axis_mode[[ax]] <- "new"
            next
        }
        dest_entries <- format_axis_array(dest, ax)
        if (length(src_entries) == length(dest_entries) &&
            identical(src_entries, dest_entries)) {
            axis_mode[[ax]] <- "replace"
            next
        }
        if (all(src_entries %in% dest_entries)) {
            axis_mode[[ax]] <- "pad"
            next
        }
        if (!overwrite) {
            stop(sprintf(
                "axis %s already exists in destination (not a superset of the source)",
                sQuote(ax)
            ), call. = FALSE)
        }
        format_delete_axis(dest, ax, must_exist = TRUE)
        format_add_axis(dest, ax, src_entries)
        axis_mode[[ax]] <- "new"
    }

    # Vectors
    for (ax in format_axes_set(source_view)) {
        src_entries <- format_axis_array(source_view, ax)
        dest_entries <- format_axis_array(dest, ax)
        for (vn in format_vectors_set(source_view, ax)) {
            val <- format_get_vector(source_view, ax, vn)
            mode_ <- axis_mode[[ax]]
            if (identical(mode_, "pad")) {
                key <- paste(ax, vn, sep = "|")
                default <- if (is.null(empty)) NULL else empty[[key]]
                if (is.null(default)) {
                    stop(sprintf(
                        "missing empty value for pad-mode vector: %s",
                        key
                    ), call. = FALSE)
                }
                full <- rep(default, length(dest_entries))
                names(full) <- dest_entries
                idx <- match(src_entries, dest_entries)
                full[idx] <- val
                format_set_vector(dest, ax, vn, full, overwrite)
            } else {
                format_set_vector(dest, ax, vn, val, overwrite)
            }
        }
    }

    # Matrices
    axes <- format_axes_set(source_view)
    for (ra in axes) {
        for (ca in axes) {
            for (mn in format_matrices_set(source_view, ra, ca)) {
                val <- format_get_matrix(source_view, ra, ca, mn)
                mode_ra <- axis_mode[[ra]] %||% "new"
                mode_ca <- axis_mode[[ca]] %||% "new"
                if (identical(mode_ra, "pad") || identical(mode_ca, "pad")) {
                    key <- paste(ra, ca, mn, sep = "|")
                    default <- if (is.null(empty)) NULL else empty[[key]]
                    if (is.null(default)) {
                        stop(sprintf(
                            "missing empty value for pad-mode matrix: %s",
                            key
                        ), call. = FALSE)
                    }
                    dest_ra_entries <- format_axis_array(dest, ra)
                    dest_ca_entries <- format_axis_array(dest, ca)
                    full <- matrix(default,
                        nrow = length(dest_ra_entries),
                        ncol = length(dest_ca_entries),
                        dimnames = list(dest_ra_entries, dest_ca_entries)
                    )
                    idx_r <- match(
                        format_axis_array(source_view, ra),
                        dest_ra_entries
                    )
                    idx_c <- match(
                        format_axis_array(source_view, ca),
                        dest_ca_entries
                    )
                    full[idx_r, idx_c] <- as.matrix(val)
                    format_set_matrix(dest, ra, ca, mn, full, overwrite)
                } else {
                    format_set_matrix(dest, ra, ca, mn, val, overwrite)
                }
                if (relayout && ra != ca) {
                    format_relayout_matrix(dest, ra, ca, mn)
                }
            }
        }
    }
    invisible()
}
```

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS (including all earlier B tests — regression check).

- [ ] **Step 5: Commit**

```
git add R/adapters.R tests/testthat/test-adapters.R
git commit -m "$(cat <<'EOF'
feat(adapters): .copy_view_to_daf — pad mode for subset-axis views

When the source view's axis is a strict subset of the destination's,
padding defaults are pulled from `empty` (keyed by axis|vector and
rows|cols|matrix) and gaps are filled at the destination indices. New
axis (absent from destination) still adds verbatim; equal axes use
per-entry overwrite; true collisions fail or delete+add under
overwrite.
EOF
)"
```

### Task B6: `adapter()` — signature + no-op guard

**Files:**

- Modify: `R/adapters.R`
- Modify: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Extend the scaffold test so it now expects a specific no-op message**

Already covered by `tests/testthat/test-adapters.R` test 1 ("no-op adapter"). No change.

- [ ] **Step 2: Implement the early checks in `adapter()`**

Replace the `adapter()` body:

```r
adapter <- function(daf, fn,
                    input_axes = NULL, input_data = NULL,
                    output_axes = NULL, output_data = NULL,
                    capture = memory_daf,
                    empty = NULL, relayout = TRUE, overwrite = FALSE,
                    name = ".adapter") {
    if (!S7::S7_inherits(daf, DafWriter)) {
        stop("`daf` must be a DafWriter", call. = FALSE)
    }
    if (!is.function(fn)) stop("`fn` must be a function", call. = FALSE)
    if (is.null(input_axes) && is.null(input_data) &&
        is.null(output_axes) && is.null(output_data)) {
        stop("no-op adapter: at least one of input_axes/input_data/output_axes/output_data required",
             call. = FALSE)
    }
    stop("adapter() body not yet implemented")   # B7 lands the real thing
}
```

- [ ] **Step 3: Run — no-op test passes; other tests still fail the not-implemented**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: the B1 no-op test passes; later tests still error on the placeholder.

### Task B7: `adapter()` — full flow (viewer → chain → fn → viewer → copy)

**Files:**

- Modify: `R/adapters.R`
- Modify: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Write failing full-flow test**

Append to `tests/testthat/test-adapters.R`:

```r
test_that("adapter() runs fn on an input view and copies output back", {
    # Base daf has cell axis + donor vector. Computation expects 'obs' axis and
    # reads vector 'donor'; produces a new vector 'squared_idx' of same length.
    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("d1", "d2", "d1"))

    fn <- function(adapted) {
        entries <- axis_vector(adapted, "obs")
        set_vector(adapted, "obs", "squared_idx",
            as.integer(seq_along(entries))^2L
        )
        "done"
    }

    result <- adapter(d, fn,
        input_axes = list(list("obs", "cell"), list("cell", NULL)),
        input_data = VIEW_ALL_VECTORS,
        output_axes = list(list("cell", "obs"), list("obs", NULL)),
        output_data = list(list(c("cell", "squared_idx"), "="))
    )
    expect_identical(result, "done")
    expect_identical(
        unname(get_vector(d, "cell", "squared_idx")),
        c(1L, 4L, 9L)
    )
})

test_that("adapter() returns fn's value untouched (passthrough)", {
    d <- memory_daf(name = "b")
    add_axis(d, "cell", c("c1"))
    fn <- function(adapted) list(value = 42L, note = "hi")
    res <- adapter(d, fn,
        input_axes = list(list("obs", "cell"), list("cell", NULL)),
        input_data = VIEW_ALL_DATA,
        output_axes = list(list("cell", "obs"), list("obs", NULL)),
        output_data = list()
    )
    expect_identical(res, list(value = 42L, note = "hi"))
})
```

- [ ] **Step 2: Run — fail**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: FAIL (placeholder stops it).

- [ ] **Step 3: Implement full flow**

Replace `adapter()`:

```r
adapter <- function(daf, fn,
                    input_axes = NULL, input_data = NULL,
                    output_axes = NULL, output_data = NULL,
                    capture = memory_daf,
                    empty = NULL, relayout = TRUE, overwrite = FALSE,
                    name = ".adapter") {
    if (!S7::S7_inherits(daf, DafWriter)) {
        stop("`daf` must be a DafWriter", call. = FALSE)
    }
    if (!is.function(fn)) stop("`fn` must be a function", call. = FALSE)
    if (is.null(input_axes) && is.null(input_data) &&
        is.null(output_axes) && is.null(output_data)) {
        stop("no-op adapter: at least one of input_axes/input_data/output_axes/output_data required",
             call. = FALSE)
    }

    base_name <- S7::prop(daf, "name")
    input <- viewer(daf,
        name = paste0(base_name, name, ".input"),
        axes = input_axes, data = input_data
    )
    captured <- capture(name = paste0(base_name, name, ".capture"))
    adapted <- chain_writer(
        list(input, captured),
        name = paste0(base_name, name, ".adapted")
    )

    result <- fn(adapted)

    output <- viewer(adapted,
        name = paste0(base_name, name, ".output"),
        axes = output_axes, data = output_data
    )
    .copy_view_to_daf(
        source_view = output, dest = daf,
        empty = empty, relayout = relayout, overwrite = overwrite
    )
    result
}
```

- [ ] **Step 4: Run tests**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS. If it fails on the passthrough test, suspect that `.copy_view_to_daf` with an empty `output_data` list still tries to iterate scalars from `adapted` — in which case iteration must be a no-op on an empty axes/data list.

- [ ] **Step 5: Commit**

```
git add R/adapters.R tests/testthat/test-adapters.R
git commit -m "$(cat <<'EOF'
feat(adapters): adapter() — viewer→chain→fn→viewer→copy_view_to_daf

Public entry point mirroring Julia adapter() semantics: input view
(renaming + subset), fresh capture writable via `capture` factory,
chain_writer composing the two, fn runs on the adapted chain, output
view selects the results back out, and .copy_view_to_daf projects
them into the caller's daf under the final names. `empty`, `relayout`,
`overwrite` pass through.
EOF
)"
```

### Task B8: `adapter()` + `computation()` integration

**Files:**

- Modify: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Write failing test — contract enforcement under an adapter**

Append:

```r
test_that("adapter() + computation() enforces contract on the adapted chain", {
    withr::local_options(list(dafr.enforce_contracts = TRUE))

    c <- Contract(
        axes = list(obs = list(RequiredInput, "renamed cell axis")),
        data = list(
            contract_vector("obs", "donor",      RequiredInput, "character", "id"),
            contract_vector("obs", "squared_idx", CreatedOutput, "integer", "out")
        )
    )
    inner <- function(adapted) {
        entries <- get_vector(adapted, "obs", "donor")
        set_vector(adapted, "obs", "squared_idx",
                   as.integer(seq_along(entries))^2L)
        "ok"
    }
    comp <- computation("adapt-demo", c, inner)

    d <- memory_daf(name = "base")
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("d1", "d2"))

    res <- adapter(d, comp,
        input_axes = list(list("obs", "cell"), list("cell", NULL)),
        input_data = VIEW_ALL_DATA,
        output_axes = list(list("cell", "obs"), list("obs", NULL)),
        output_data = list(list(c("cell", "squared_idx"), "="))
    )
    expect_identical(res, "ok")
    expect_identical(unname(get_vector(d, "cell", "squared_idx")), c(1L, 4L))
})
```

- [ ] **Step 2: Run — pass (if everything upstream is correct)**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS. Failure mode: cache-sharing between `ContractDaf` → `WriteChainDaf` → `MemoryDaf` could show as a stale read. Debug: inspect `cache` props on each layer.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-adapters.R
git commit -m "test(adapters): adapter + computation integration"
```

### Task B9: `adapter()` — real-data smoke with `example_cells_daf`

**Files:**

- Modify: `tests/testthat/test-adapters.R`

- [ ] **Step 1: Write test**

Append:

```r
test_that("adapter() on example_cells_daf computes a per-cell total UMI count", {
    d <- example_cells_daf()
    fn <- function(adapted) {
        m <- get_matrix(adapted, "obs", "var", "UMIs")
        totals <- as.integer(rowSums(as.matrix(m)))
        set_vector(adapted, "obs", "total_umis", totals)
        totals
    }
    totals <- adapter(d, fn,
        input_axes = list(
            list("obs", "cell"), list("var", "gene"),
            list("cell", NULL), list("gene", NULL)
        ),
        input_data = VIEW_ALL_DATA,
        output_axes = list(
            list("cell", "obs"), list("gene", "var"),
            list("obs", NULL), list("var", NULL)
        ),
        output_data = list(list(c("cell", "total_umis"), "="))
    )
    expect_identical(length(totals), 856L)
    expect_true(all(totals >= 0L))
    expect_identical(
        unname(get_vector(d, "cell", "total_umis")),
        totals
    )
})
```

- [ ] **Step 2: Run**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapters.R")'
```

Expected: PASS.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-adapters.R
git commit -m "test(adapters): smoke test on example_cells_daf"
```

### Task B10: Regression — full suite

- [ ] **Step 1: Run**

```
cd /home/aviezerl/src/dafr-native
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: all prior phases still green; new phase-B tests added; 0 FAIL.

---

## Phase J — Julia-compat fixture for adapter + computation

### Task J1: Refresh DataAxesFormats.jl and verify the env

**Files:**

- External: `~/src/DataAxesFormats.jl`, `~/src/TanayLabUtilities.jl`

- [ ] **Step 1: Fetch + pull Julia DAF**

Run:

```
cd ~/src/DataAxesFormats.jl
git fetch origin
git pull --ff-only origin main
git rev-parse HEAD
```

Record the HEAD commit — it will go into the fixture README so parity runs are reproducible.

- [ ] **Step 2: Verify Julia env is usable**

Run:

```
conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
    -e 'using DataAxesFormats; println(joinpath(@__DIR__))'
```

Expected: no error; prints a module path. If it fails with `Pkg` issues, `conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl -e 'using Pkg; Pkg.instantiate()'`.

- [ ] **Step 3: (no commit)**

### Task J2: Write `dev/scripts/regen-julia-adapter-fixture.jl`

**Files:**

- Create: `dev/scripts/regen-julia-adapter-fixture.jl`

**Pre-read:** The Slice-4 `regen-julia-chains-fixture.jl` is the template. Our fixture needs to record a computation+adapter roundtrip: starting state (in the R test we re-create this by calling `example_cells_daf()`), the renaming / subsetting config, and the expected final state of the outputs. Since the R test re-runs the computation locally, the fixture mainly records (a) the config and (b) the expected output vector values for a reproducible assertion.

- [ ] **Step 1: Create the script**

Create `dev/scripts/regen-julia-adapter-fixture.jl`:

```julia
# Regenerate the Julia adapter+computation fixture for dafr Slice 5.
#
# Run via:
#   conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
#     dev/scripts/regen-julia-adapter-fixture.jl
#
# Produces:
#   tests/testthat/fixtures/julia-adapter/fixture.json
#
# The computation being tested: sum UMIs per cell into a new vector
# `total_umis` on the (renamed) obs axis. We pass through adapter() with:
#   input_axes = ["obs" => "=", "cell" => nothing, "var" => "=", "gene" => nothing]
# (Julia uses "=" to mean "same as the axis named, with no rename"; for our
# R port we go obs<-cell / var<-gene and drop the originals.)
# For parity the fixture records the total_umis vector (856-long UInt32).

using DataAxesFormats
using DataAxesFormats.Adapters
using DataAxesFormats.Computations
using DataAxesFormats.Contracts
using DataAxesFormats.ExampleData
using DataAxesFormats.Views
using DataAxesFormats.MemoryFormat

const DAFR_ROOT = dirname(dirname(@__DIR__))
const FIX_DIR = joinpath(DAFR_ROOT, "tests", "testthat", "fixtures", "julia-adapter")
mkpath(FIX_DIR)

daf = example_cells_daf()

contract = Contract(;
    axes = ["obs" => (RequiredInput, "renamed cell axis"),
            "var" => (RequiredInput, "renamed gene axis")],
    data = [
        ("obs", "var", "UMIs")   => (RequiredInput, UInt8, "input"),
        ("obs", "total_umis")    => (CreatedOutput, UInt32, "total UMIs per cell"),
    ],
)

@computation Contract(;
    axes = ["obs" => (RequiredInput, "renamed cell axis"),
            "var" => (RequiredInput, "renamed gene axis")],
    data = [
        ("obs", "var", "UMIs")   => (RequiredInput, UInt8, "input"),
        ("obs", "total_umis")    => (CreatedOutput, UInt32, "total UMIs per cell"),
    ],
) function sum_umis(adapted::DafWriter)
    m = get_matrix(adapted, "obs", "var", "UMIs")
    totals = UInt32.(sum(m; dims = 2)[:])
    set_vector!(adapted, "obs", "total_umis", totals)
    return "ok"
end

result = adapter(daf;
    input_axes = ["obs" => "= cell", "var" => "= gene",
                  "cell" => nothing, "gene" => nothing],
    input_data = [("*", "*")         => "=",
                  ("*", "*", "*")    => "="],
    output_axes = ["cell" => "= obs", "gene" => "= var",
                   "obs" => nothing, "var" => nothing],
    output_data = [("cell", "total_umis") => "="],
) do adapted
    sum_umis(adapted)
end

# Minimal JSON emitter (same style as prior regen scripts).
function json_str(s::AbstractString)
    io = IOBuffer()
    write(io, '"')
    for c in s
        c == '"'  && (write(io, "\\\""); continue)
        c == '\\' && (write(io, "\\\\"); continue)
        c == '\n' && (write(io, "\\n"); continue)
        write(io, c)
    end
    write(io, '"')
    return String(take!(io))
end
json_val(x::AbstractString) = json_str(x)
json_val(x::Integer) = string(x)
json_val(x::AbstractFloat) = isfinite(x) ? string(x) : "null"
json_val(x::AbstractVector) = "[" * join(json_val.(x), ", ") * "]"

totals_stored = get_vector(daf, "cell", "total_umis")

jaf_head = read(`git -C /home/aviezerl/src/DataAxesFormats.jl rev-parse HEAD`, String) |> strip

payload = """{
  "daf_jl_head": $(json_str(jaf_head)),
  "result_returned": $(json_val(result)),
  "total_umis_length": $(json_val(length(totals_stored))),
  "total_umis_values": $(json_val(collect(totals_stored)))
}
"""
open(joinpath(FIX_DIR, "fixture.json"), "w") do io
    write(io, payload)
end

println("wrote ", joinpath(FIX_DIR, "fixture.json"))
```

**Important caveat — Julia syntax must be cross-checked before running.** The `@computation` macro, `Contract()` constructor, and `adapter()` keyword shapes in DataAxesFormats.jl have evolved across versions. BEFORE running the regen, the executing agent MUST open `~/src/DataAxesFormats.jl/src/contracts.jl`, `computations.jl`, `adapters.jl`, `views.jl` and at least one test file exercising `@computation` + `adapter` (grep under `~/src/DataAxesFormats.jl/test/` for `@computation` and `adapter(`) to confirm:

- The exact `Contract(; axes = [...], data = [...])` keyword form — whether axes entries are `"name" => (expectation, description)` (tuple) or a dict or something else.
- Whether data records are `(axis, name) => (expectation, type, description)` tuples, or constructed via `ContractAxis(...)` / `ContractScalar(...)` helpers.
- The `input_axes` / `input_data` keyword shapes in `adapter()` — especially whether `"obs" => "= cell"` is the right rename syntax (the space-prefixed form may or may not be needed).
- Whether `@computation` requires the function's first argument to be typed `DafWriter` vs. `AbstractDaf`.

Fix the script to match before running the regen. If blocked, fall back to manually constructing the expected output (R-side computation + expected UMI sums) and writing a fixture by hand — the parity target then becomes "R matches Julia-computed byte stream" rather than "R replays a Julia regen output". A hand-written fixture is acceptable if regen proves intractable; document the deviation in the fixture README.

- [ ] **Step 2: (no commit yet; continue to J3)**

### Task J3: Run the regen, commit the fixture

**Files:**

- Create: `tests/testthat/fixtures/julia-adapter/fixture.json`
- Create: `tests/testthat/fixtures/julia-adapter/README.md`

- [ ] **Step 1: Run the regen**

Run:

```
cd /home/aviezerl/src/dafr-native
conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
    dev/scripts/regen-julia-adapter-fixture.jl
```

Expected: `wrote .../fixture.json`. If Julia errors on the contract/adapter syntax, debug by running the script fragment-by-fragment via `conda run -n dafr-mcview julia --project=...` interactively.

- [ ] **Step 2: Create the fixture README**

Create `tests/testthat/fixtures/julia-adapter/README.md`:

```markdown
# Julia adapter fixture

Fixture for testing R/Julia compatibility of adapter() + computation().

Regenerate with:

    conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
      dev/scripts/regen-julia-adapter-fixture.jl

## Contents

- `fixture.json` — records:
  - `daf_jl_head` — HEAD commit of DataAxesFormats.jl at regen time.
  - `result_returned` — the value the computation returned from `adapter()`.
  - `total_umis_length` — length of the stored `total_umis` vector (856).
  - `total_umis_values` — the entries, which the R test must reproduce
    bit-identically when running the same computation on
    `example_cells_daf()` via `adapter()`.
```

- [ ] **Step 3: Commit fixture + README + regen script**

```
# Dev repo: regen script
cd /home/aviezerl/src/dafr-native/dev
git add scripts/regen-julia-adapter-fixture.jl
git commit -m "scripts: Julia adapter+computation fixture regenerator"

# Package repo: fixture + README
cd /home/aviezerl/src/dafr-native
git add tests/testthat/fixtures/julia-adapter
git commit -m "test(fixtures): Julia adapter+computation roundtrip fixture"
```

### Task J4: Write R parity test

**Files:**

- Create: `tests/testthat/test-adapter-julia-compat.R`

- [ ] **Step 1: Create the test**

Create `tests/testthat/test-adapter-julia-compat.R`:

```r
test_that("R adapter()+computation() matches Julia fixture bit-identically", {
    skip_if_not(file.exists("fixtures/julia-adapter/fixture.json"),
                "Julia adapter fixture absent")
    fx <- jsonlite::fromJSON("fixtures/julia-adapter/fixture.json",
                             simplifyVector = TRUE)

    withr::local_options(list(dafr.enforce_contracts = TRUE))
    d <- example_cells_daf()

    c <- Contract(
        axes = list(
            obs = list(RequiredInput, "renamed cell axis"),
            var = list(RequiredInput, "renamed gene axis")
        ),
        data = list(
            contract_matrix("obs", "var", "UMIs",
                RequiredInput, "integer", "input"),
            contract_vector("obs", "total_umis",
                CreatedOutput, "integer", "total UMIs per cell")
        )
    )
    inner <- function(adapted) {
        m <- get_matrix(adapted, "obs", "var", "UMIs")
        totals <- as.integer(rowSums(as.matrix(m)))
        set_vector(adapted, "obs", "total_umis", totals)
        "ok"
    }
    comp <- computation("sum_umis", c, inner)

    res <- adapter(d, comp,
        input_axes = list(
            list("obs", "cell"), list("var", "gene"),
            list("cell", NULL), list("gene", NULL)
        ),
        input_data = VIEW_ALL_DATA,
        output_axes = list(
            list("cell", "obs"), list("gene", "var"),
            list("obs", NULL), list("var", NULL)
        ),
        output_data = list(list(c("cell", "total_umis"), "="))
    )
    expect_identical(res, as.character(fx$result_returned))
    got <- unname(get_vector(d, "cell", "total_umis"))
    expect_identical(length(got), as.integer(fx$total_umis_length))
    expect_identical(got, as.integer(fx$total_umis_values))
})
```

- [ ] **Step 2: Run the parity test**

Run:

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-adapter-julia-compat.R")'
```

Expected: PASS. If values diverge, the UMI type in Julia is `UInt8` / summed to `UInt32`; in R, `rowSums` on an integer matrix returns a double. Coerce explicitly; compare on integer cast.

- [ ] **Step 3: Commit**

```
git add tests/testthat/test-adapter-julia-compat.R
git commit -m "test(adapter): Julia-parity roundtrip"
```

### Task J5: Regression — full suite

- [ ] **Step 1: Run**

```
cd /home/aviezerl/src/dafr-native
Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat")'
```

Expected: all prior phases still green; Phase J adds 1 test; 0 FAIL.

---

## Phase Z — Docs + NAMESPACE + NEWS + check

### Task Z1: roxygen regen

**Files:**

- Modify: `NAMESPACE`, `man/*.Rd`

- [ ] **Step 1: Run devtools::document()**

Run:

```
cd /home/aviezerl/src/dafr-native
Rscript -e 'devtools::document(roclets = c("collate","namespace","rd"))'
```

Expected: no errors; new manpages for `computation`, `function_contract`, `contract_description`, `adapter`, `example_cells_daf`, `example_metacells_daf`, `example_chain_daf`.

- [ ] **Step 2: Review diff**

Run:

```
git diff --stat NAMESPACE man
```

Expected: new entries for all of the above; no deletions except for pre-existing alias reshuffles.

- [ ] **Step 3: Commit**

```
git add NAMESPACE man
git commit -m "docs: regenerate NAMESPACE + man for Slice 5 exports"
```

### Task Z2: `@examples` roxygen blocks on new exports

**Files:**

- Modify: `R/computations.R`, `R/adapters.R`, `R/example_data.R`

- [ ] **Step 1: Add `@examples` to `computation`**

Append this block to the roxygen of `computation()` in `R/computations.R`, just before the `@export` line:

```r
#' @examples
#' c <- Contract()
#' noop <- computation("noop", c, function(daf) daf)
#' d <- memory_daf(name = "ex")
#' noop(d)
```

- [ ] **Step 2: `@examples` for `function_contract` and `contract_description`**

```r
#' @examples
#' c <- Contract()
#' w <- computation("demo", c, function(daf) daf)
#' identical(function_contract(w), c)
```

```r
#' @examples
#' c <- Contract(
#'     axes = list(cell = list(RequiredInput, "per-cell")),
#'     data = list(contract_vector("cell", "donor",
#'         RequiredInput, "character", "id"))
#' )
#' cat(contract_description(c))
```

- [ ] **Step 3: `@examples` for `adapter`**

In `R/adapters.R`:

```r
#' @examples
#' d <- memory_daf(name = "base")
#' add_axis(d, "cell", c("c1","c2"))
#' set_vector(d, "cell", "donor", c("a","b"))
#' adapter(d,
#'     function(adapted) {
#'         set_vector(adapted, "obs", "squared",
#'                    seq_along(axis_vector(adapted, "obs"))^2L)
#'     },
#'     input_axes = list(list("obs", "cell"), list("cell", NULL)),
#'     input_data = VIEW_ALL_DATA,
#'     output_axes = list(list("cell", "obs"), list("obs", NULL)),
#'     output_data = list(list(c("cell", "squared"), "="))
#' )
#' get_vector(d, "cell", "squared")
```

- [ ] **Step 4: `@examples` for `example_*_daf`**

In `R/example_data.R`:

```r
#' @examples
#' d <- example_cells_daf()
#' axes_set(d)
#' get_scalar(d, "organism")
```

```r
#' @examples
#' d <- example_metacells_daf()
#' axis_length(d, "metacell")
```

```r
#' @examples
#' d <- example_chain_daf()
#' length(axes_set(d))
```

- [ ] **Step 5: Re-run document**

Run:

```
Rscript -e 'devtools::document(roclets = c("collate","namespace","rd"))'
```

- [ ] **Step 6: Run examples**

Run:

```
Rscript -e 'devtools::run_examples(quiet = TRUE)' 2>&1 | tail -30
```

Expected: no errors. Warnings on missing system fonts etc. are acceptable.

- [ ] **Step 7: Commit**

```
git add R/computations.R R/adapters.R R/example_data.R NAMESPACE man
git commit -m "docs: @examples on Slice 5 exports"
```

### Task Z3: NEWS entry

**Files:**

- Modify: `NEWS.md`

- [ ] **Step 1: Read the Slice 4 entry format**

Run:

```
head -40 NEWS.md
```

- [ ] **Step 2: Add Slice 5 entry at the top**

Prepend to `NEWS.md`:

```markdown
# dafr (development) — Slice 5

* `computation(name, contract, fn)` — HOF wrapping a function with
  contract enforcement (verify_input / verify_output) on each call.
  `function_contract(fn)` retrieves the bound contract;
  `contract_description(c)` renders it for roxygen docstrings.
* `adapter(daf, fn, ...)` — run a computation against a renaming view
  of `daf` and project the outputs back under the destination names.
  Mirrors Julia `DataAxesFormats.Adapters.adapter`.
* `example_cells_daf()` / `example_metacells_daf()` / `example_chain_daf()`
  ship Julia-parity example data (856 cells × 683 genes) under
  `inst/extdata/example_data`.
* Multi-hop chained lookup: `/ cell : donor =@ : lab =@ : country`
  resolves through an arbitrary number of hops.
```

- [ ] **Step 3: Commit**

```
git add NEWS.md
git commit -m "docs(news): Slice 5 entry"
```

### Task Z4: `devtools::check()` — zero notes

**Files:**

- (none — gate)

- [ ] **Step 1: Run check**

Run:

```
cd /home/aviezerl/src/dafr-native
Rscript -e 'Sys.setenv(`_R_CHECK_SYSTEM_CLOCK_` = "0"); devtools::check(error_on = "note")'
```

Expected: **0 ERROR / 0 WARNING / 0 NOTE**. If a note fires on examples runtime, split long examples with `\dontrun{}`. If a note fires on undocumented S7 properties, add `@param` stubs.

- [ ] **Step 2: Commit any fixup required**

```
git add R man
git commit -m "docs: <summary of the fixup>"
```

### Task Z5: Slice 5 exit note

**Files:**

- Create: `dev/notes/slice-5-exit.md`

- [ ] **Step 1: Write the exit note**

Create `dev/notes/slice-5-exit.md` (dev repo). Structure follows Slice 4 exit:
1. Scope delivered (D, E, A, B, J, Z)
2. Test count + check status
3. Known mines laid in Slice 5 for Slice 6 (e.g. character matrices untested; `empty` only supports the flat-key form; adapter() does not reset `state$kind` of the captured chain — contract+cache-sharing with chain_writer)
4. Deferred items — explicitly re-confirm which Option B / C / D pieces remain open
5. Next-slice prompt

- [ ] **Step 2: Commit dev repo**

```
cd /home/aviezerl/src/dafr-native/dev
git add notes/slice-5-exit.md
git commit -m "notes: Slice 5 exit gate"
```

### Task Z6: Feature-branch merge and tag

**Files:**

- External: `main`, tag `slice-5`

- [ ] **Step 1: Verify branch state**

Run:

```
cd /home/aviezerl/src/dafr-native
git status
git log --oneline main..HEAD | wc -l
```

Expected: clean working tree; ~25-30 commits on the feature branch.

- [ ] **Step 2: Fast-forward main**

Run:

```
git checkout main
git merge --ff-only slice-5-computations-adapters
```

If --ff-only fails, the branch diverged (shouldn't happen if we didn't pull main mid-slice). Investigate before retrying.

- [ ] **Step 3: Tag and push (after user confirmation)**

Confirm with the user before `git push`:

```
git tag slice-5
```

Do NOT push without explicit user approval.

---

## Self-review checklist (run before handoff)

- [ ] Every task from D1..Z6 has a file path, failing test, exact command, and expected output.
- [ ] All types/signatures are internally consistent: `computation(name, contract, fn)`, `function_contract(fn)`, `contract_description(contract)`, `adapter(daf, fn, input_axes, ...)`, `.copy_view_to_daf(source_view, dest, empty, relayout, overwrite)`, `example_{cells,metacells,chain}_daf(name)`.
- [ ] Contract records reuse Slice-4 constructors (`contract_scalar/vector/matrix`) — no renamed kind fields.
- [ ] Multi-hop patch does NOT add a new `state$kind` value (respects F4 mine).
- [ ] `.copy_view_to_daf` is internal (dot-prefix); no export.
- [ ] No Phase introduces new C++ or new R dependencies.
- [ ] Phase J's Julia script records `daf_jl_head` into the fixture for reproducibility.
- [ ] `withr::local_options` used to scope `dafr.enforce_contracts` per test so enforcement does not leak into other tests.
- [ ] `/bin/cp` used (not `cp`) when the agent copies files; `/bin/rm` if anything must be removed.
- [ ] No `--no-verify`, `--amend`, or force-push anywhere. All commits are fresh.
- [ ] Package-repo vs dev-repo commits are correctly separated per file path.
- [ ] No new emojis anywhere.
