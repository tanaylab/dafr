# Slice exit: contracts.jl / scalar subgroup parity port

Date: 2026-05-08
Branch: `dev`
Commit: <to be filled by ship step>
Predecessors:
- `2026-05-07-slice-contracts-add-jl-parity-exit.md` (sibling sub-slice)
- `2026-05-08-jl-parity-port-next-session-kickoff.md` (kickoff doc;
  this slice is item 1 of the recommended order)

## Scope

Literal port of contracts.jl's `scalar` subgroup (Julia lines 105-272,
48 nested_test leaves) into
`tests/testthat/test-contracts-scalar-jl-parity.R`.

## Result

- **Test counts.** New file: `FAIL 0 | SKIP 4 | PASS 44`. Full suite:
  `FAIL 0 | WARN 1 | SKIP 53 | PASS 5068` (delta from session-start
  baseline of `5024 PASS / 49 SKIP`: +44 PASS, +4 SKIP, no new FAIL,
  WARN unchanged - the 1 warning remains the pre-existing scran SVD).
- **R/ changes.** None. dafr's existing contractor / verify_input /
  verify_output / contract_scalar machinery already covers the
  cross-product, with one documented gap (CS1).
- **Inline fixes.** None.
- **Open divergences.** 1 (CS1).

## Divergences

See `dev/notes/2026-05-08-contracts-scalar-jl-parity-divergences.md`.

- **CS1.** dafr's `.is_forbidden` only fires for `CreatedOutput`;
  Julia's also fires for `GuaranteedOutput` and `OptionalOutput`.
  When a scalar is declared `OptionalOutput` and pre-exists with
  `overwrite=FALSE`, Julia rejects it in `verify_input` (token
  "pre-existing OptionalOutput scalar"); dafr returns silently.
  Affects 4 cells of `scalar / ()` (the `contingent / !overwrite`
  arm). Likely to recur in vector/matrix/tensor sub-slices; will be
  named `CV1`/`CM1`/`CT1` there.

The Julia GuaranteedOutput parity tests pass against R's
`CreatedOutput` enum, since CreatedOutput's verify_* enforcement is
the semantic equivalent. dafr's separate `GuaranteedOutput` enum is
not exercised by this slice; it's not enforced in verify_*, so
testing it directly would document "no enforcement" rather than
checking parity.

## Acceptance per kickoff

> `FAIL 0 | PASS >= <ported test count>`, every skip keyed to a
> divergence-note ID, no skip reads just `# TODO`.

Met: 0 FAIL, 44 PASS (>= 44 ported), 4 SKIP all keyed to CS1 with the
divergence note text inline.

## Cumulative state on dev

This commit is on `dev` only, alongside the earlier `25e14cd`
(contracts/add). Per the kickoff plan, the recommendation is to
bundle scalar + axis + vector into one ship to main when all three
are green. **Not yet shipping.**

## Next sub-slice

Per the kickoff order:
2. **`contracts/axis`** (Julia lines 286-425, ~30 leaves). Same shape
   as scalar - one cross-product over expectation x overwrite x
   direction x accessed, plus a `missing` block. The `.is_forbidden`
   gap (CS1) reappears for axes; new ID `CA1` to keep audit
   self-contained.
3. **`contracts/vector`** (lines 426-663, ~50 leaves).

After scalar+axis+vector, ship the bundle. Matrix and tensor each
warrant their own sessions.

## Workflow notes

- Reinstalled dafr via `R CMD INSTALL --no-docs --no-help --no-test-load`
  before running tests. Reinstall is required because the parity
  test depends on `library(dafr)` resolving to the installed copy
  (memory entry: bake-off install requirement applies here too).
- Per-file iteration: `Rscript -e 'library(testthat); library(dafr);
  testthat::test_file("tests/testthat/test-contracts-scalar-jl-parity.R")'`.
- Full suite: `cd tests && NOT_CRAN=true Rscript testthat.R`.
- Each test_that wraps its body in
  `withr::with_options(list(dafr.enforce_contracts = TRUE), { ... })`
  - matches the existing test-contract-ux.R convention; without it,
  `contractor()` short-circuits and returns the underlying daf.

## Out of scope (carried forward)

Same as kickoff "Out of scope": E11, C2, V3, R6, new features.
