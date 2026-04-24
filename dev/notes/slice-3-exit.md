# Slice 3 exit gate — 2026-04-20

## Deliverables

**Phase O — Operations registry**

- [x] Registry scaffolding for reductions + eltwise via `register_reduction()` /
  `register_eltwise()` with explicit `overwrite` flag and function guard (O1).
- [x] Default reductions: `Sum`, `Mean`, `Max`, `Min`, `Count` (O2).
- [x] Default eltwise ops: `Log` (with `eps` + `base` params), `Abs`, `Exp`,
  `Sqrt`, `Round` (O3).

**Phase Q — Query DSL**

- [x] Tokenizer: operator regex + value splitting with quoted-escape handling (Q1).
- [x] AST: lookup node constructors + `canonicalise_ast` (Q2).
- [x] AST: mask, slice, reduction, grouping, eltwise nodes + canonicalisation (Q3).
- [x] Parser: lookup tokens (`@ . : :: ?`) with error-position reporting (Q4).
- [x] Parser: bracketed masks, comparators (`< <= = != > >= ~ !~`), logical
  combinators (`& | ^ !`) (Q5).
- [x] Parser: slicing (`@- @|`), grouping (`@>- @>|`), reductions (`>- >|`),
  eltwise (`%`), `IfMissing`, `IfNot`, `AsAxis` (Q6).
- [x] Evaluator: scalar + axis lookups, `Names`, `IfMissing` via lookahead (Q7).
- [x] Evaluator: vector + matrix lookups + two-axes scope transition (Q8).
- [x] Evaluator: bracketed mask chains with comparators (Q9).
- [x] Evaluator: logical mask combinators (`AND`, `OR`, `XOR` + negated) (Q10).
- [x] Evaluator: square slicing `@- / @|` → `SquareRowIs / SquareColumnIs` (Q11).
- [x] Evaluator: eltwise (`%`) + reductions (`>| >-`) via operations registry (Q12).
- [x] Evaluator: `GroupBy`, `CountBy`, `GroupRowsBy`, `GroupColumnsBy` +
  grouped reductions (Q13).
- [x] `get_frame()`: axis-query + named columns → `data.frame` (Q14).
- [x] Query-tier cache with version-counter invalidation (Q15).
- [x] `parse_query()` user-facing constructor; `is_axis_query()`,
  `query_axis_name()`, `query_result_dimensions()`, `has_query()` introspection
  helpers; `q()` removed to avoid shadowing `base::q` (Q16).
- [x] Julia query fixture + regeneration script
  (`dev/scripts/regen-julia-query-fixture.jl`) producing
  `tests/testthat/fixtures/julia-queries/` (Q17).
- [x] Julia-compat end-to-end tests: 17/17 fixture queries parse + evaluate to
  matching values; canonical-string byte-for-byte match (Q18).

**Phase V — ViewDaf**

- [x] `ViewDaf` S7 class (under `DafReadOnly`) + `viewer()` constructor with
  query-string override maps for axes, scalars, vectors, matrices (V1).
- [x] `format_*` methods on `ViewDaf` rewriting into `get_query` on base daf;
  no copies (V2).
- [x] Axis / scalar / vector / matrix overrides via query strings (V3).
- [x] `ALL_AXES` / `ALL_SCALARS` / `ALL_VECTORS` / `ALL_MATRICES` wildcards
  with last-wins override semantics; `NULL` hide (V4).
- [x] Julia-compat view tests via Julia-produced `FilesDaf` fixture (V5).

**Phase Z1 — Docs + integration**

- [x] Regenerated `NAMESPACE`, all man pages, `NEWS.md` entry (Z1).

**Phase Z4 — Exit gate (this document)**

- [x] Full verification suite passing (Z4, this note).
- [x] Dev-repo exit note committed (Z4).
- [x] `slice-3-queries-views` merged → `main` fast-forward and tagged `slice-3` (Z4).

## Test + build status

- `testthat::test_dir("tests/testthat")` — **939 PASS / 0 FAIL / 0 SKIP /
  1 WARN**. The single warning is the pre-existing
  `scran::quickCluster` / `irlba::irlba` SVD tolerance notice in
  `test-altrep-downstream.R`, unchanged since Slice 0. From the Slice 2
  baseline of 707 tests this is +232 tests covering the operations registry,
  query tokeniser/parser/AST/evaluator, `get_frame()`, query cache, ViewDaf,
  and Julia compat for both queries and views.
- `devtools::check(error_on = "note")` with `_R_CHECK_SYSTEM_CLOCK_=0` —
  **0 ERROR / 0 WARNING / 0 NOTE**. Duration 1m 22.9s.
- `pkgbuild::compile_dll(debug = FALSE)` — clean. No new C++ in Slice 3.

## Scope closed vs deferred

**Closed in Slice 3:**

- Operations registry (O1–O3): `Sum / Mean / Max / Min / Count` reductions,
  `Log / Abs / Exp / Sqrt / Round` eltwise.
- Full query DSL tokeniser → parser → AST → evaluator (Q1–Q18), including
  Julia canonical-string parity.
- `get_frame()` (Q14) and `has_query()` / `is_axis_query()` / etc. introspection
  helpers (Q16).
- Query-tier cache with version-counter invalidation (Q15).
- `ViewDaf` class, `viewer()`, `format_*` dispatch via query rewrites,
  `ALL_*` wildcards, last-wins resolution, NULL hide (V1–V5, Z1).

**Deferred to Slice 4:**

- **Chains + Contracts** — the other half of the original Slice 3 proposal,
  split at user request 2026-04-20. Julia's `ChainDaf` / `chain_reader` /
  `chain_writer` (federation) and `Contract` / `verify_input` / `verify_output`
  (typed pre/post-conditions) both scheduled for Slice 4.
- **`@examples` roxygen blocks** (Z2) — explicit user instruction to defer
  2026-04-20.
- **`alutil::sad()` styler pass** (Z3) — explicit user instruction to defer
  2026-04-20.
- **L2 upstream PR** (`tanaylab/DataAxesFormats.jl` docs) — user declined at
  Slice 2 exit, re-declined 2026-04-20. Spec draft at
  `dev/specs/filesdaf-on-disk-spec-draft.md` remains resolved and ready.
- **ViewDaf axis rename does not propagate to vector/matrix reads** —
  `viewer(d, axes = list(list("obs", "@ cell")))` exposes the renamed axis
  via `axis_vector()` but `get_vector(v, "obs", ...)` does not resolve.
  Documented in NEWS. Deferred.
- **ViewDaf axis filter does not propagate to vector/matrix reads** —
  `viewer(d, axes = list(list("cell", "@ cell [ keep ]")))` exposes the
  filtered entries via `axis_vector()` but `get_vector(v, "cell", ...)`
  returns the full base vector. Documented in NEWS. Deferred.
- **`IfNot` / `AsAxis` are no-op stubs in the evaluator** — the parser emits
  them correctly; the evaluator treats both as identity in Slice 3. Real
  semantics deferred to Slice 4.
- **Performance**: evaluator uses `apply()` for reductions and eltwise ops —
  several times slower than a vectorized `colSums` / `rowMeans` path on large
  matrices. Vectorized default-op path planned.
- **View cache is scaffolded but dead** — per-`ViewDaf` `query` cache bucket
  is created by the constructor but `format_get_*` methods route to the
  base-daf cache via `get_query`. The view bucket is never populated. Not
  incorrect (base cache is still version-gated), but the per-view isolation
  intended by the scaffold never fires. Slice 4 chains work should decide
  whether to populate it or remove it.
- **Long-vector (>2^31) ALTREP scenarios** — still untested (inherited from
  Slice 0/1/2).
- **UInt32 > 2^31 read arm** — inherited from Slice 2; signed-int32
  under-the-hood for oversized UInt32 indices.
- **Multi-writer filesystem locking on FilesDaf** — inherited from Slice 2.

## Known mines laid in Slice 3 for Slice 4

- **Query cache key is canonical string via `.canonicalise_ast()`**. Views do
  not canonicalise into the base daf's cache key-space — a
  `chain_reader(view_daf, writer)` may produce cache key collisions with
  base-daf entries. Slice 4's chain wrapper should use its own namespace
  prefix (e.g., `"view:<n>:<canon>"`) or a separate cache bucket.
- **Evaluator state `kind` values are a closed enum**: `"init"`, `"scalar"`,
  `"axis"`, `"two_axes"`, `"matrix"`, `"vector"`, `"mask"`,
  `"grouped_vector"`, `"grouped_matrix_rows"`, `"grouped_matrix_cols"`,
  `"names"`, `"scalar_names_ready"`, `"vector_names_ready"`,
  `"matrix_names_ready"`. Any new handler added in Slice 4 must respect
  this set and must not introduce a `kind` that the existing handlers
  implicitly fall through.
- **`.apply_axis` is load-bearing for the first→second-axis transition** in
  the evaluator state machine. Restructuring it will break `>|` / `>-` axis
  semantics. Do not touch without a full evaluator test run.
- **`IfMissing` lookahead in `.eval_query` is specific to `IfMissing`**.
  `IfNot` and `AsAxis` evaluator stubs are no-ops in Slice 3. Both need
  lookahead or AST-rewrite handling in Slice 4 to match Julia's semantics.
- **Matrix cache version key uses `"rows:cols"` (colon-separated, axis-order
  dependent)**. Queries on the same data under a flipped orientation do not
  share cache invalidation. If Slice 4 exposes relayout through views, the
  cache key will need to normalise axis order (e.g., sort alphabetically).
- **NA in mask comparators produces NA in result via `entries[mask]`**. Julia
  drops `NA` silently. This divergence is latent in Slice 3 and untested.
  Document or fix in Slice 4 before contracts rely on mask correctness.

## Commit history

Slice 3 landed as 31 commits on branch `slice-3-queries-views` (off `main`
at tag `slice-2`). Phase O shipped 4 commits (registry + default ops); Phase Q
shipped 18 commits (tokeniser through Julia compat); Phase V shipped 5 commits
(class + dispatch + wildcards + view Julia compat); Phase Z1 shipped 2 commits
(docs + fix).

```
f3bcc24 fix(slice-3): reduction axis semantics, dimension count, no-op IfNot/AsAxis, NEWS accuracy
7db1c1d docs(slice-3): regenerate NAMESPACE + man + NEWS entry
61652ac test(view_daf): viewer against Julia-produced FilesDaf fixture
13219f4 feat(view_daf): ALL_* wildcards + NULL hide + last-wins resolution
e82056e feat(view_daf): axis / scalar / vector / matrix overrides via query strings
7849066 feat(view_daf): format_* methods dispatching via get_query on base daf
b2a824c feat(view_daf): ViewDaf S7 class + viewer() constructor
570bbe3 test(queries): end-to-end Julia fixture parity
70b244a fixture(queries): Julia-generated query fixture + example daf
74ede49 fix(queries): drop q() shadow of base::q — use parse_query() directly
a494a81 feat(queries): q() alias + is_axis_query + query_axis_name + query_result_dimensions + has_query
6665214 feat(queries): query-tier cache with version-counter invalidation
50d2472 feat(queries): get_frame() — axis-query + columns -> data.frame
7856bfe feat(query-eval): GroupBy, CountBy, GroupRowsBy, GroupColumnsBy + grouped reductions
8db6e1a feat(query-eval): eltwise (%) + reductions (>|, >-) via operations registry
2b8938b feat(query-eval): SquareRowIs / SquareColumnIs matrix slicing
b4c1917 feat(query-eval): logical mask combinators (AND, OR, XOR + negated)
79d1b12 feat(query-eval): mask chains with comparators (<, =, >, ~, and negated)
70d2412 feat(query-eval): vector + matrix lookups + two_axes scope transition
88b6df8 feat(query-eval): scalar + axis lookups + Names + IfMissing via lookahead
c0025c8 feat(query-parse): slicing, grouping, reductions, eltwise, if-missing, if-not, as-axis
022144f feat(query-parse): bracketed masks, comparators, logical combinators
4425202 feat(query-parse): lookups (@ . : :: ?) with error-position reporting
caec66d feat(query-ast): mask, slice, reduction, grouping, eltwise nodes + canonicalisation
b1ebcaa feat(query-ast): lookup node constructors + canonicalise_ast
5c4c5ef feat(query-tokens): operator + value tokenizer with quoted-escape handling
2e5ae67 scaffold(queries): empty source files with @include directives
6f4e369 feat(operations): default eltwise ops (Log, Abs, Exp, Sqrt, Round)
5d1542d feat(operations): default reductions (Sum, Mean, Max, Min, Count)
04d9037 fix(operations): explicit overwrite flag + non-function guard + roxygen bodies
c1389a6 feat(operations): registry scaffolding for reductions + eltwise
```

Net diff vs `slice-2`: +2374 / -94 across 48 files (R source + tests + fixtures + docs).

## Repo conventions reinforced in Slice 3

- **Tagged AST nodes via `.qop(op, ...)`** produce lists with class
  `c(paste0("qop_", op), "qop")` — lightweight alternative to S7 for small
  records. Pattern is established; Slice 4 can add new node types by following
  `.qop(op, ...)` + a case in `.QOP_DISPATCH`.
- **State stack-machine pattern** (`state$kind` discriminator). Handlers test
  `identical(state$kind, "...")` before mutating. Adding a handler without
  an `identical()` guard breaks the machine silently.
- **S7 multi-dispatch ALWAYS needs `list(ClassA, ClassB, ...)` signatures**,
  never the bare class form. This caused a V2 failure during development;
  reinforced in the ViewDaf method stubs.
- **Julia fixture regeneration uses an inlined JSON emitter** (not
  `JSON3.jl`) because `Manifest.toml` for the `DataAxesFormats.jl` project
  is broken in the `dafr-mcview` conda env. The script is self-contained.
- **`format_get_*` returns values WITHOUT dimnames (plain arrays)**.
  `get_*` user-facing wrappers add names. This contract is relied upon by
  the query evaluator when constructing named result vectors from mask output.

## Ready-to-paste prompt for Slice 4

> Start implementing Slice 4 of the native-R `dafr` package.
>
> - Package repo: `~/src/dafr-native/` on branch `main`, tracking
>   `origin/main` at `git@github.com:tanaylab/dafr.git` (private). Tag
>   `slice-3` marks the Slice 3 exit.
> - Dev repo: `~/src/dafr-native/dev/` — separate nested git repo.
> - Kickoff breadcrumb: (create new) `dev/notes/slice-4-kickoff.md` —
>   start from this slice-3 exit note.
> - Slice 3 plan (fully executed):
>   `~/src/dafr-native/dev/plans/2026-04-20-slice-3-queries-views.md`.
> - Slice 3 exit note: `~/src/dafr-native/dev/notes/slice-3-exit.md`.
>
> Slice 4 scope:
>
> **Primary (chains + contracts):**
> - `ChainDaf` S7 class + `chain_reader()` / `chain_writer()` constructors
>   (federation: read falls through ordered list of dafs, write goes to top).
> - `Contract` + `verify_input()` / `verify_output()` typed pre/post-conditions
>   (check required axes, vectors, matrices exist and have correct dtypes).
>
> **Follow-up work from Slice 3 deferred items:**
> - ViewDaf axis rename propagation to vector/matrix reads.
> - ViewDaf axis filter propagation to vector/matrix reads.
> - `IfNot` / `AsAxis` evaluator stubs → real semantics.
> - NA in mask comparators: align with Julia's drop-NA behaviour or document
>   the divergence.
> - View cache dead machinery (decide: populate or remove).
>
> **Polish deferred from earlier slices:**
> - `@examples` roxygen blocks (Z2) — still deferred.
> - `alutil::sad()` styler pass (Z3) — **DONE** post-tag as commit
>   `af842d7` on `main` (4-space indent is now the convention).
>
> Known mines to brief the Slice 4 agent: see "Known mines" section of
> `dev/notes/slice-3-exit.md`.
>
> **Julia DAF state at handoff:** `~/src/DataAxesFormats.jl` is at
> `49fbba1` (origin/main as of handoff). `~/src/TanayLabUtilities.jl` at
> `48a4a57`. Both registered as Julia `dev` packages in conda env
> `dafr-mcview`. The 17-query fixture at
> `tests/testthat/fixtures/julia-queries/` was regenerated against this
> state — **bytes unchanged** vs Slice 3's tagged fixture, so the 6 new
> DAF.jl commits (cache_group per-item, named-tuple params, reorder_axes
> stubs) don't perturb our query-DSL output for the existing fixture.
> Slice 4's chain/contract work may want to extend the fixture to
> exercise new DAF.jl features once reorder_axes lands.
>
> Use `superpowers:writing-plans` first to draft a Slice 4 plan, then
> `superpowers:subagent-driven-development` to execute it with full
> two-stage review per task.

## Status at session end

- `tanaylab/dafr` (private): `main` at `af842d7` (styler commit on top of
  slice-3 tag at `f3bcc24`). Tag `slice-3` pushed. CI green on both the
  slice-3 tag and the styler commit (altrep-sanity + R-CMD-check).
- Local `~/src/dafr-native/`: `main` at `af842d7`, tag `slice-3`
  present, feature branch `slice-3-queries-views` deleted.
- Local `~/src/dafr-native/dev/`: `main` clean with Slice 3 plan + exit
  note + Julia query fixture regeneration script committed.
- Local `~/src/DataAxesFormats.jl`: `main` at `49fbba1` (pulled at
  handoff). Local `~/src/TanayLabUtilities.jl`: `main` at `48a4a57`.
  Both registered via `Pkg.develop` in conda env `dafr-mcview`. Fixture
  regeneration confirmed byte-identical; no Slice 3 regression from the
  Julia-side update.
- L2 upstream PR (`tanaylab/DataAxesFormats.jl` docs) stays deferred at
  user request; spec draft at `dev/specs/filesdaf-on-disk-spec-draft.md`
  remains resolved.
- `alutil::sad()` styler pass landed as commit `af842d7` on `main`
  post-tag; the codebase is now 4-space indent. Future R edits should
  match.
