# P1 Feature-Blocked Parity Backlog Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the P1 feature-blocked parity gaps from `dev/notes/2026-06-10-post-0.4.2-kickoff.md`: Chains C1, Reorder R6, Concat M5/M1/M4/M2, Queries E10/E5/E4/E3/E6/E8/E7, Views V5-fix + V1 tensors. Julia DAF.jl (~/src/DataAxesFormats.jl @ 0.3.0) is the parity reference.

**Architecture:** Each task unskips existing `tests/testthat/test-*-jl-parity.R` tests (TDD: unskip -> red -> implement -> green -> commit). One divergence, one commit. R5 is already implemented (verify only). E9 (auto-relayout) and E11/E1 are explicitly OUT of this plan: E11/E1 are accepted divergences; E9 is XLARGE and deferred to its own slice.

**Tech Stack:** R + S7, testthat, devtools (`devtools::test(filter = ...)`, never R CMD INSTALL in the dev loop), Matrix package for sparse.

**Verification commands:**
- Single file: `R --quiet --no-save -e 'devtools::test(filter = "chains-jl-parity")'`
- Full suite at the end: `R --quiet --no-save -e 'devtools::test()'`

---

### Task 1: C1 — singleton-chain identity optimization

**Files:**
- Modify: `R/chain_daf.R` (chain_reader ~line 62-88, read_only ~line 106-111, chain_writer ~line 124-159)
- Test: `tests/testthat/test-chains-jl-parity.R:57-67` (2 skipped blocks: "chains / one / reader", "chains / one / writer")

Julia semantics (`src/chains.jl:83-85, 116-118`; `src/read_only.jl:42-60`):
- `chain_reader(list(d), name=NULL)` with a single daf returns `read_only(d)` (identity when d is already read-only).
- `chain_writer(list(d), name=NULL)` with a single daf returns `d` itself. A non-NULL `name` forces wrapping in both.
- `read_only(x)` on an already-read-only daf returns `x` unchanged when `name` is NULL; a name forces a new wrapper.

- [ ] **Step 1:** Port the Julia test bodies (test/chains.jl:28-39) into the two skipped blocks, replacing the `skip()` calls. Assertions: `identical(chain_reader(list(read_only(d))), read_only(d))` and `identical(chain_writer(list(d)), d)`; with explicit `name=` the result must NOT be identical to the input.
- [ ] **Step 2:** Run `devtools::test(filter = "chains-jl-parity")` — the two blocks must FAIL (wrapper always created).
- [ ] **Step 3:** Implement: in `chain_reader`, after the empty-list check add `if (length(dafs) == 1L && is.null(name)) return(read_only(dafs[[1L]]))`. In `read_only`, before delegating: `if (S7::S7_inherits(daf, DafReadOnly) && is.null(name)) return(daf)` (check what class read_only results have — match Julia's idempotence contract). In `chain_writer`, after the empty-list check, before is_frozen: `if (length(dafs) == 1L && is.null(name)) return(dafs[[1L]])`.
- [ ] **Step 4:** Run chains-jl-parity + test-chain-readers + test-chain-writers — all green.
- [ ] **Step 5:** Commit: `fix(parity): C1 singleton chain_reader/chain_writer/read_only return input unwrapped` (body references chains.jl:83-85,116-118).

---

### Task 2: R6 — memory_daf reorder crash-recovery atomicity (+ verify R5)

**Files:**
- Modify: `R/memory_daf.R` (`format_replace_reorder` MemoryDaf method, ~line 448-496)
- Test: `tests/testthat/test-reorder-jl-parity.R:288-298` (memory crash_recovery after_1/after_4) and 422-455 (multi-writer R5 — verify only)

Julia semantics (`src/memory_format.jl:488-595`): backup-before-mutation; on mid-reorder error, `reset_reorder_axes!` restores from backup and returns TRUE. dafr already has `.memory_snapshot_for_reorder` / `.memory_restore_from_snapshot`; what's missing is exercising restore on the crash path. CAUTION: the research note claims current tests "pass" while the code comments say memory reset is a no-op — first establish actual current behavior.

- [ ] **Step 1:** Run `devtools::test(filter = "reorder-jl-parity")`; read the crash_recovery memory tests + the `.test_crash_recovery` helper. Determine: does `reset_reorder_axes()` on memory actually restore? Do the tests assert pre-reorder data after crash, as Julia test/reorder.jl:69-77 does? If the tests are weaker than Julia's, strengthen them first (assert reset returns TRUE and data is restored byte-for-byte), see them fail.
- [ ] **Step 2:** Implement: ensure the snapshot is taken BEFORE the first crash-counter tick, and that `format_reset_reorder` (MemoryDaf) restores from `internal$reorder_backup` and returns TRUE when a backup exists. Mirror Julia: backup holds references to old vectors/matrices; replace stages new arrays.
- [ ] **Step 3:** Run reorder-jl-parity — crash_recovery memory tests green, multi-writer tests (R5) still green.
- [ ] **Step 4:** Commit: `fix(parity): R6 memory_daf reorder crash recovery restores from snapshot` (body references memory_format.jl:488-595). Note in the commit body that R5 was verified already-working.

---

### Task 3: M5 — memory_daf sparseVector round-trip

**Files:**
- Modify: `R/memory_daf.R` (`format_set_vector`, ~line 260-272), possibly `R/utils.R` (`.attach_vector_axis_names`, ~line 99-117)
- Test: `tests/testthat/test-concat-jl-parity.R:256-276` (sparse vector tests) + a new round-trip test in the same file or test-memory-*.

Julia stores SparseVector natively in MemoryDaf. dafr's gap: `.attach_vector_axis_names()` densifies on read (`as.numeric(vec)`) because S4 sparseVector can't carry names. M2 (Task 6) needs to detect source sparsity, so memory_daf must PRESERVE the sparseVector in storage and expose it at the format layer; densification (with names) happens only at the public `get_vector` boundary, same as files_daf.

- [ ] **Step 1:** Write a failing test: `set_vector` a `Matrix::sparseVector` into memory_daf, then check the format-layer/raw storage still holds a sparseVector (mirror however the files_daf sparse round-trip test asserts this, see test-files-vectors.R:333). Public `get_vector` keeps returning a named dense vector (names contract is non-negotiable).
- [ ] **Step 2:** Run — confirm where it fails (storage may already preserve; the read path densifies).
- [ ] **Step 3:** Implement minimal change so storage preserves sparseVector and whatever internal accessor M2 will use can see it. Do NOT strip names at lower layers.
- [ ] **Step 4:** Run concat-jl-parity + memory tests — green.
- [ ] **Step 5:** Commit: `fix(parity): M5 memory_daf preserves sparseVector storage`.

---

### Task 4: M1 — concat merge wildcards (ALL_SCALARS/ALL_VECTORS/ALL_MATRICES)

**Files:**
- Modify: `R/concat.R` (`.concat_expand_merge_wildcards`, ~line 337-399 — skeleton exists, expansion loops stubbed)
- Test: `tests/testthat/test-concat-jl-parity.R:466-483` (scalar collect/!collect — currently "translated to explicit key"; restore the wildcard form) + add vector/matrix wildcard cases mirroring Julia test/concat.jl:292,304,332,346,390,420,434,453,469.

Julia semantics (`src/concat.jl:157-221`, get_merge_action at 1187-1221): wildcard keys `"*"`, `c("*","*")`, `c("*","*","*")` expand to every property not matched by an explicit key; LAST matching entry wins (reverse iteration); explicit keys override wildcards; vector/matrix expansion skips the concatenation axes.

- [ ] **Step 1:** Rewrite the wildcard-translated tests to use the real `ALL_SCALARS`/`ALL_VECTORS`/`ALL_MATRICES` constants (exported from R/view_daf.R); run — FAIL.
- [ ] **Step 2:** Complete the three expansion loops in `.concat_expand_merge_wildcards()`: enumerate `format_scalars_set` / `format_vectors_set(axis)` / `format_matrices_set(rows,cols)` across sources, skipping concat axes and keys already explicit. Preserve last-wins ordering.
- [ ] **Step 3:** Run concat-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): M1 concat merge wildcards ALL_SCALARS/ALL_VECTORS/ALL_MATRICES` (body references concat.jl:157-221).

---

### Task 5: M4 — `prefixed=` overrides the heuristic

**Files:**
- Modify: `R/concat.R` (`.concat_axis_vector`, gate at ~line 224-230) + `concatenate()` docstring (~line 39-40)
- Test: `tests/testthat/test-concat-jl-parity.R:227-250` ("concat / prefix / prefixes")

Julia semantics (`src/concat.jl:360-373`): when `prefixed` is non-NULL it REPLACES the heuristic entirely - a property listed in `prefixed[[axis]]` is prefixed even when that axis's `prefix` flag is FALSE. Julia test/concat.jl:142-160: `prefix=c(FALSE,TRUE)`, `prefixed=list(c("metacell","!metacell"), character(0))` must yield `source.1!.M1`-style values on the unprefixed cell axis.

- [ ] **Step 1:** Align the dafr test with the Julia case exactly (prefix flag FALSE but property in prefixed list); run — FAIL.
- [ ] **Step 2:** Drop the `isTRUE(do_prefix) &&` gate when `prefixed` is non-NULL; update docstring from "additional ... beyond the heuristic" to "explicit list ... overriding the heuristic".
- [ ] **Step 3:** Run concat-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): M4 prefixed= overrides the prefix heuristic` (body references concat.jl:360-373).

---

### Task 6: M2 — sparse-preserving MERGE_COLLECT_AXIS for vectors

**Files:**
- Modify: `R/concat.R` (`.concat_merge_vector`, ~line 429-490; dense alloc at ~line 471)
- Test: `tests/testthat/test-concat-jl-parity.R:566-568` (skipped: "concat / merge / vector / collect / sparse", asserts dgCMatrix output)

Julia semantics (`src/concat.jl:949-1027`): collect-axis turns N source vectors into an (axis x dataset) matrix. Sparse path taken when storage savings >= `sparse_if_saves_storage_fraction` (default 0.25), computed from source nnz (`sparse_vectors_storage_fraction`, concat.jl:1259-1286). Sparse output assembled per-column CSC (concatenate_merge_sparse_vector, concat.jl:1029-1068). Missing sources = zero columns (empty_value != 0 forces dense).

- [ ] **Step 1:** Unskip the test; run — FAIL (dense matrix returned).
- [ ] **Step 2:** Implement: read source vectors via the M5-preserving path to know nnz; compute the savings fraction mirroring Julia; when sparse wins, build a `Matrix::sparseMatrix` (dgCMatrix) column-by-column instead of the dense `matrix()` alloc. Keep dimnames contract (axis entries x dataset names).
- [ ] **Step 3:** Run concat-jl-parity full file — green, including the dense-path tests (no regression on mixed/dense sources).
- [ ] **Step 4:** Commit: `fix(parity): M2 concat collect-axis preserves sparse vectors as dgCMatrix` (body references concat.jl:949-1068).

---

### Task 7: E10 — regex escape handling in query tokenizer

**Files:**
- Modify: `R/query_tokens.R` (regex value scan)
- Test: `tests/testthat/test-queries-jl-parity.R` (vector / compare / ~ and !~ with `\[`-escapes; find the deferred cases via the divergence note `dev/notes/2026-05-03-queries-jl-parity-divergences.md` E10 section)

Julia: tokenizer honors backslash escapes inside `[ ... ]` mask values, so `[ type ~ \^\[A-U\] ]` parses; dafr splits on the unescaped `\]`.

- [ ] **Step 1:** Add/unskip the escape-regex tests per the divergence note; run — FAIL.
- [ ] **Step 2:** Fix the tokenizer's value scan to treat `\X` as literal X (consume the escape) before bracket matching.
- [ ] **Step 3:** Run queries-jl-parity + query tokenizer tests — green.
- [ ] **Step 4:** Commit: `fix(parity): E10 query tokenizer honors backslash escapes in regex values`.

---

### Task 8: E5 — top-level `:` and `::` lookups

**Files:**
- Modify: `R/query_parse.R` (`.parse_next` — allow `:`/`::` as the first token)
- Test: `tests/testthat/test-queries-jl-parity.R` (scalar / vector / () and scalar / matrix / () cases per divergence note E5)

Julia: `: vec @ axis = E` (vector then entry-pick -> scalar) and `:: M @ rows = R @ cols = C` are valid whole queries without a leading `@ axis`.

- [ ] **Step 1:** Unskip/add the tests; run — FAIL (parser rejects leading `:`).
- [ ] **Step 2:** Allow the lookup operators at position 1; the evaluator's pending-axis states must resolve via the `@ axis = entry` suffixes.
- [ ] **Step 3:** Run queries-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): E5 top-level : and :: lookups`.

---

### Task 9: E4 — top-level comparators on vectors/matrices

**Files:**
- Modify: `R/query_parse.R` (.parse_lookup) + `R/query_eval.R` (comparator application to in-scope vector/matrix)
- Test: queries-jl-parity vector/compare and matrix/compare deferred cases (divergence note E4)

Julia (`src/queries.jl:765-787`): `@ cell : type ~ [UV` yields a named logical vector (comparator as terminal op, not just inside masks).

- [ ] **Step 1:** Unskip/add tests; run — FAIL.
- [ ] **Step 2:** Implement comparator dispatch when a comparison token follows a vector/matrix in scope; reuse the existing mask comparator kernels.
- [ ] **Step 3:** Run queries-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): E4 top-level comparators return boolean vectors/matrices`.

---

### Task 10: E3 — matrix-slice-as-mask

**Files:**
- Modify: `R/query_parse.R` (`.parse_begin_mask` — accept `@ axis = entry` inside mask) + `R/query_eval.R` (two-axis mask state)
- Test: queries-jl-parity vector/mask/matrix cases (divergence note E3)

Julia (`src/queries.jl:649-651,686-689`): `[ UMIs @ gene = A > 0 ]` slices a matrix at a row/col entry and uses the resulting vector as the mask.

- [ ] **Step 1:** Unskip/add tests; run — FAIL.
- [ ] **Step 2:** Implement: inside mask parsing, a matrix lookup followed by `@ axis = entry` resolves to a vector before the comparator applies.
- [ ] **Step 3:** Run queries-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): E3 matrix-slice masks`.

---

### Task 11: E6 — lookup chains after matrix

**Files:**
- Modify: `R/query_eval.R` (`.apply_lookup_matrix` — allow chained `: prop` walks after a matrix result)
- Test: queries-jl-parity matrix/lookup, vector/lookup/as_axis, vector/lookup/if_not deferred cases (divergence note E6 — the biggest unskip count of the medium items)

Julia: `@ rows @ cols :: M : V : prop` — after a matrix entry-lookup produces values that index another axis, further `:` lookups chain-walk.

- [ ] **Step 1:** Unskip/add tests; run — FAIL.
- [ ] **Step 2:** Extract/reuse the existing vector chain-walk helper so post-matrix states accept `: prop` (and the if_not/as_axis modifiers).
- [ ] **Step 3:** Run queries-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): E6 lookup chains after matrix lookups`.

---

### Task 12: E8 — CountBy (`*` operator)

**Files:**
- Modify: `R/query_ast.R` (new `.qop_count_by` node), `R/query_parse.R` (`*` token), `R/query_eval.R` (cross-tabulation evaluator)
- Test: queries-jl-parity matrix/count deferred cases (all variants; divergence note E8)

Julia (`src/queries.jl:2717-2730` CountBy struct; 4382-4530 compute_count_matrix etc.): `: vec * other` produces a 2D contingency matrix (unique values of vec x unique values of other), as_axis variants included.

- [ ] **Step 1:** Unskip/add the count tests; run — FAIL at parse.
- [ ] **Step 2:** Implement parser + AST node; evaluator builds the count matrix (R `table()` semantics but matching Julia's ordering/dtype/dimnames exactly — check Julia tests for expected entry order).
- [ ] **Step 3:** Run queries-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): E8 CountBy (*) cross-tabulation queries`.

---

### Task 13: E7 — group-by with matrix-slice / `=@` group source

**Files:**
- Modify: `R/query_parse.R` (group-by grammar disambiguation) + `R/query_eval.R` (matrix-slice group dispatch)
- Test: queries-jl-parity vector/group/vector/matrix|square and matrix/group slice cases (divergence note E7)

Julia: `/ kind @ axis = E` (group key from a matrix slice) and `* type =@` forms.

- [ ] **Step 1:** Unskip/add tests; run — FAIL.
- [ ] **Step 2:** Implement, reusing E3's matrix-slice resolution inside the group-by parser states.
- [ ] **Step 3:** Run queries-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): E7 group-by matrix-slice sources`.

---

### Task 14: V5 — `__axis__` substitutes the full axis QUERY, not the base axis name

**Files:**
- Modify: `R/view_daf.R` (`.view_query_for_vector` ~line 567-577; `.view_query_for_matrix` ~line 579-601 for `__rows_axis__`/`__columns_axis__`)
- Test: `tests/testthat/test-views-jl-parity.R:234-248,266-279` pass today only because the cases are simple; add a case with a FILTERED view axis (Julia test/views.jl:184-196: axis `"@ cell [ batch = V ]"`, query `"@ gene @ __axis__ :: UMIs >- Sum"`).

Julia (`src/views.jl:1132-1133,1164-1169`): the placeholder expands to the axis's full query including filters.

- [ ] **Step 1:** Add the filtered-axis test mirroring views.jl:184-196; run — FAIL (filter lost).
- [ ] **Step 2:** Substitute the stored axis query string instead of `override$base_axis`; add `__rows_axis__`/`__columns_axis__` handling in the matrix path.
- [ ] **Step 3:** Run views-jl-parity — green.
- [ ] **Step 4:** Commit: `fix(parity): V5 __axis__ placeholders expand to the full axis query`.

---

### Task 15: V1 — view tensor support

**Files:**
- Modify: `R/view_daf.R` (ViewDaf struct ~line 134-147; `.parse_view_item`; `.resolve_view_matrices` ~line 413-511; description rolldown)
- Test: `tests/testthat/test-views-jl-parity.R:424-426` (single stub covering 8 Julia leaves; Julia test/views.jl:427-525)

Julia (`src/views.jl:69,314-340,1045-1088`): matrices named `<entry>_<suffix>` (entries of a "main" axis) group into virtual tensors; viewer accepts 4-tuple keys `(main_axis, rows_axis, cols_axis, property)`; description emits a `tensors:` block; `matrices_set(...; tensors=false)` toggles. dafr's contracts layer already has partial tensor naming concepts — reuse.

- [ ] **Step 1:** Expand the stub into the 8 ported leaves; run — FAIL.
- [ ] **Step 2:** Implement in order: 4-tuple key parsing -> tensor detection pass in matrix resolution -> description rolldown -> collection logic.
- [ ] **Step 3:** Run views-jl-parity full file — green.
- [ ] **Step 4:** Commit: `fix(parity): V1 view tensor support` (likely the largest single commit; split parse/resolve/description if natural).

---

### Task 16: Wrap-up

- [ ] **Step 1:** Full suite: `R --quiet --no-save -e 'devtools::test()'` — no regressions (known pre-existing: test-helpers.R:26 FAILs under Rscript only).
- [ ] **Step 2:** `R --quiet --no-save -e 'devtools::document()'` if any roxygen changed; re-run tests if NAMESPACE changed.
- [ ] **Step 3:** Update `dev/notes/2026-05-03-queries-jl-parity-divergences.md`, the concat/chains/reorder/views divergence notes, and `dev/notes/2026-06-10-post-0.4.2-kickoff.md` P1 section: mark closed ids, note R5/V2/V3/V6/V7 were verified already-done, note E9 deferred to its own slice.
- [ ] **Step 4:** Write `dev/notes/2026-06-10-slice-p1-parity-backlog-exit.md` summarizing closed ids + remaining (E9, V relayout-tracking if untouched, R4/zarr).
- [ ] **Step 5:** Commit docs: `dev: P1 parity backlog exit notes`.

---

## Self-review notes

- Spec coverage: kickoff P1 lists Builder API (C3) FIRST — **C3 is intentionally split out**: it is a cross-backend API (8 format generics x 4 backends) and per the research the value in R is semantic-only (no zero-copy). It blocks 10 tests across 3 files. DECISION: do C3 after Task 13 if context allows, else defer to its own slice with the research report (saved in this plan's session transcript) as the spec. The kickoff explicitly says "Pick from:", so partial completion with documented remainder is in-contract.
- E9 deferred (XLARGE, needs format-API relayout metadata). E1/E11 accepted, untouched.
- Line numbers come from agent research on 2026-06-10; verify on read, don't trust blindly.
- Test counts: queries divergence note claims 68+ blocked rows across E3-E11; the per-task unskip lists must be derived from the note + grep, not assumed.
