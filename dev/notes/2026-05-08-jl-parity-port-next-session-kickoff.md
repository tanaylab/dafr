# Kickoff - port the rest of DAF.jl's test suite (next session)

**Date:** 2026-05-08
**Predecessor:** `dev/notes/2026-05-05-jl-parity-port-rest-kickoff.md`
(the original Slice A-G plan; this doc is the resume point after
Slices B + first half of C shipped, plus contracts/add committed but
not yet shipped).

## State at session boundary

### What's on `main` (origin/tanaylab, pushed)

Single ship commit `c071bad` on `main` covering:

- Slice B in full: `concat.jl`, `reorder.jl`, `chains.jl` parity ports
  with 5 inline fixes (B1, B2, R0, R7, C0) and 13 divergences
  documented.
- Slice C, first half: `views.jl` parity port. No inline fixes; 8
  divergences documented (V1-V7 + C2 carryover).

NEWS.md entry covering the 5 inline behavior fixes is on main.

### What's on `dev` only (private/aviezerl, not yet shipped to main)

Single commit `25e14cd` ahead of main:

- contracts.jl `add` sub-group parity port (14 tests, 1 SKIP). No R/
  changes; dafr's `merge_contracts` already passes the Julia
  composition tests verbatim. Skip is the C1 type-lattice divergence
  (R has no Int32/Int64 sibling concept).

This commit is shippable to main standalone; held back because the
caller wanted to bundle with the next sub-slice or two before
shipping.

### Test state

Full suite: `FAIL 0 | WARN 1 | SKIP 49 | PASS 5024`. The 1 warning is
a pre-existing scran SVD warning unrelated to parity work. Baseline
at session start was `4626 PASS / 6 SKIP`; net delta is +398 PASS /
+43 SKIP across the session's 6 slice commits.

## Remaining contracts.jl scope

The full file is 1639 lines / ~188 nested_tests across 6 sub-groups.
**1 of 6 done (add).** Remaining:

| Sub-group | Julia line range | Approx leaves | Notes |
|-----------|-----------------:|--------------:|-------|
| scalar | 116-285 (~170) | ~30 | Cross-product loops over (name × overwrite × direction × accessed). Smallest remaining; mechanical port. |
| axis | 286-425 (~140) | ~30 | Same shape as scalar. |
| vector | 426-663 (~238) | ~50 | Same shape + axis-prerequisite handling. |
| matrix | 664-1392 (~728) | ~80 | Largest; includes a `fill` sub-block (lines 1393+) testing the `empty_*` builder API dafr lacks (already filed as C3 in chains slice). Recommend splitting `matrix` itself into 2 sub-slices. |
| tensor | 1492-1638 (~147) | ~10 | Tensor-specific wrapper variants; dafr has tensor in contracts but views are blocked (V1). Likely most blocked sub-group. |

Each sub-group's cross-product structure suggests defining a few
parameterized helpers (`.scalar_test_for_direction(...)`, etc.) the
way the chains parity slice did for its access tests. ~10-15 helpers
per sub-group.

### Recommended order for next session

1. **`contracts/scalar`** (smallest remaining; mechanical). Will give
   a sense of cross-product unrolling cost.
2. **`contracts/axis`** (same shape as scalar; reuse helper patterns).
3. **`contracts/vector`** (similar but ~2x leaves).
4. Defer `matrix` and `tensor` — both warrant their own sessions.

Bundle slices 1+2+3 into one ship to main when they all green.

## Other slices the original kickoff still owes

After contracts.jl, the kickoff plan still has:

- **Slice D**: `operations.jl` (576 lines, 213 @tests) +
  `computations.jl` (434 lines, 58 @tests). Algorithm-level tests +
  the `@computation` API for contract-defined functions.
  `operations.jl` likely surfaces T-class type-error wording
  divergences (R has no Int32 etc.); `computations.jl` is mostly
  net-new for dafr.
- **Slice E**: `copies.jl` (1397 lines, 376 @tests). Copy semantics.
  Lower expected yield since dafr already has 8 `test-copies-*.R`
  files.
- **Slice F**: `data.jl` (4329 lines, 1160 @tests). The big one.
  Storage-API stress test. Already split-recommended in the original
  kickoff.
- **Slice G**: `cache_groups.jl` + `anndata.jl` + `reconstruction.jl`
  (451 lines, ~110 @tests total). Three small files; one slice. Low
  risk.

## Alternative pivot (lower complexity)

If contracts.jl fatigue sets in, **Slice G is a clean break**:
451 lines combined, mostly assertion-tightening on existing R-side
tests. Probably one session, ~3-5 inline fixes if any. Closes 3
files toward the "every Julia file has a sister parity test" goal.

## Important context for the next session

### Divergence-ID namespace already used

To avoid collision when filing new divergences:

- `B1, B2` (concat behavior bugs, fixed)
- `M1, M2, M4, M5` (concat divergences open)
- `R0, R7` (reorder behavior bugs, fixed)
- `R2, R3, R4, R5, R6` (reorder divergences open)
- `C0` (chains behavior bug, fixed - broad cache impact)
- `C1, C2, C3, C5` (chains divergences open)
- `V1-V7` (views divergences open)
- `C1` (contracts/add divergence open) — name collision with chains
  C1 but in different doc; reader should disambiguate by file
- `T1, T2` (recurring R-fundamental: tempdir idiom, error wording)

For contracts/scalar onward, prefix new IDs with `CS-` for clarity
(CS1, CS2, ...). Same for contracts/axis (`CA-`), vector (`CV-`),
matrix (`CM-`), tensor (`CT-`). This keeps each sub-slice's audit
self-contained.

### Recurring patterns from prior slices

- **Sparse matrix construction**: use
  `methods::as(matrix(.., dimnames=...), "dgCMatrix")` not
  `Matrix::Matrix(., sparse=TRUE)` (which can produce ddiMatrix /
  dsCMatrix on diagonal-shaped or symmetric inputs).
- **memory_daf rejects `Matrix::sparseVector`** (`.validate_vector_value`
  atomic-only gate). Filed as M5; use `files_daf` if sparse vector
  storage needs exercising.
- **Error-text relax**: regex on tokens (axis name, operation name,
  "no empty value", "incompatible expectation", etc.) rather than
  exact Julia chomp-formatted multi-line strings.
- **Set-equality**: `expect_setequal(scalars_set(d), c("v"))` to
  mirror Julia's `Set(["v"]) == Set(...)`.
- **Cross-product loop unrolling**: parameterize as
  `.assert_*(factory)` helpers (see chains parity file for the
  template; the access group there unrolls Julia's
  `for (name, type_name, chain) in [...]` into 32 explicit
  test_thats sharing 14 helpers).

### Workflow

- Branch: `dev` (commit directly, matching prior parity-slice
  practice).
- Test invocation: `cd tests && NOT_CRAN=true Rscript testthat.R` for
  full suite (saw "user-memory" entry on this).
- Per-file iteration:
  `Rscript -e 'library(testthat); library(dafr);
  testthat::test_file("tests/testthat/test-X.R")'`.
- Reinstall dafr after R/ changes:
  `R CMD INSTALL --no-docs --no-help --no-test-load <repo-root>`.
- Ship to main via `dev/skills/dafr-ship/ship.sh "msg"` — but note it
  fails when main is checked out at the parallel
  `/net/mraid20/.../src/dafr-main` worktree. Workaround: cd into the
  main worktree, run `git read-tree --reset -u dev`, manually `git
  rm` dev-only paths (`dev/`, `CLAUDE.md`, `.claude/` etc.), commit.
  See this session for the exact sequence.

## Acceptance per sub-slice

`FAIL 0 | PASS ≥ <ported test count>`, every skip keyed to a
divergence-note ID, no skip reads just `# TODO`.

## Out of scope for next session

- Closing E11 (kernel-level int-promotion type-strictness) - filed in
  `2026-05-03-queries-jl-parity-divergences.md` as the only open
  E-class item from the queries.jl port.
- Lifting C2 (description deep parameter) - design choice, not bug.
- Lifting V3 (viewer default-all-vector visibility) - behavioral
  semantic change, needs user decision before flipping.
- Lifting R6 (memory_daf reorder atomicity) - design comment in
  `R/memory_daf.R:427-428` justifies the current non-atomic
  behavior; lifting requires re-arguing the design.
- New features (compute helpers, h5ad ergonomics, etc.).
