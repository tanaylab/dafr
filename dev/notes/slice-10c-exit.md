# Slice 10c — Exit note

**Date:** 2026-04-23
**Predecessor:** Slice 9d-N (tag `slice-9d-n` on `main`).
**Branch:** `slice-10c` → merged to `main` as `slice-10c`.
**Parent kickoff:** `dev/notes/slice-10-kickoff.md` §"10c — small ports".
**Design:** `dev/notes/2026-04-23-slice-10c-design.md`.
**Plan:** `dev/plans/2026-04-23-slice-10c-implementation.md`.

## Scope delivered

25 new user-facing exports across 7 groups + 1 removal (`get_frame`
renamed to `get_dataframe_query`). Seven TDD phases, each landing as
one commit on the feature branch.

| Phase | Commit | Group | Exports |
|---|---|---|---|
| A | `e197e2b` | Handler constants + shim | 4 (`ERROR_HANDLER`, `WARN_HANDLER`, `IGNORE_HANDLER`, `inefficient_action_handler`) |
| B | `9940e50` | Query introspection | 3 (`escape_value`, `unescape_value`, `query_requires_relayout`) |
| C | `5d52cd1` | Version counters | 3 (`axis_version_counter`, `vector_version_counter`, `matrix_version_counter`) |
| D | `c68980f` | Group helpers | 3 (`compact_groups`, `collect_group_members`, `group_names`) |
| E | `9dd07f6` | Class-surface sugar | 4 (`is_daf`, `daf_name`, `complete_path`, `read_only`) |
| F | `f23a0f8` | DataFrame helpers | 3 (`get_dataframe`, `get_dataframe_query`, `get_tidy`); `get_frame` removed |
| G | `f52ee05` | Contract UX | 5 (`create_contract`, `axis_contract`, `tensor_contract`, `contract_docs`, `verify_contract`) |

**Post-merge commit hash:** filled at merge time below.

## Numbers

**Test suite:** 1932 (pre-slice baseline) → 2075 PASS post-slice
(Phase G commit); +143 assertions across 7 new testthat files.
Budget was ~130; modest overshoot driven by the expanded escape/
unescape round-trip table (30 cases) and the Contract UX tensor
regression guards.

**Per-phase assertion growth:**

| Phase | Baseline → Post | Δ |
|---|---:|---:|
| 0 | 1932 → 1932 | 0 (branch-only) |
| A | 1932 → 1941 | +9 |
| B | 1941 → 1983 | +42 |
| C | 1983 → 2000 | +17 |
| D | 2000 → 2019 | +19 |
| E | 2019 → 2036 | +17 |
| F | 2036 → 2047 | +11 |
| G | 2047 → 2075 | +28 |

## 5 wrapper deviations (locked at design time, documented in NEWS)

1. `get_frame` → `get_dataframe_query` rename; no compat shim (pre-1.0 package).
2. `create_contract` takes typed per-category args (`scalars` /
   `vectors` / `matrices` / `tensors` / `axes`); no `name` field on the
   contract (computation label lives on `contractor()`).
3. `tensor_contract` parameter is `type`, not `dtype` — aligns with
   native's existing `contract_scalar` / `contract_vector` /
   `contract_matrix`.
4. Version counters return `integer(1)`, not stringified UInt32.
5. `read_only(daf)` is a 1-element `chain_reader` wrap; no new S7 class.

## Issues encountered mid-slice

### Phase B: pre-existing `.escape_value` bug

The original `.escape_value` (inherited from slice 4) used
`gsub("[\\\\\"]", "\\\\\\0", s, perl = TRUE)`. The `\\0` in
`perl = TRUE` mode produces a literal backslash-zero sequence, not a
backreference to the matched character. This meant the escape was
producing unparseable output for any string containing `"` or `\`.
Slice 4 tests never exercised that path — the first time it was
exercised was by Phase B's 30-case round-trip identity test.

Fix: use a capture group: `gsub("([\\\\\"])", "\\\\\\1", s, perl = TRUE)`.
Also extended the "needs quoting" detection regex to include `\\`
itself so bare backslash strings round-trip correctly. Confirmed no
downstream breakage — no existing canonical query or cache key used
quote/backslash characters.

### Phase G: `verify_contract` static-check false-positive

The plan's literal `verify_contract(c, d) = { verify_input(cd); verify_output(cd) }`
fails its own green-path test because `verify_output` dispatches to
`.verify_access` which flags `RequiredInput` trackers whose
`$accessed` is `FALSE`. Since `verify_contract` doesn't execute a
computation between input and output, no trackers are marked
accessed, and every `RequiredInput` falsely errors.

Fix: between `verify_input` and `verify_output`, the implementer
walks the axes env and the data env setting `$accessed <- TRUE` on
each tracker. This is a minimal-scope patch that preserves the
wrapper's "static existence + type check" semantics of
`verify_contract`. A cleaner refactor (an `is_static` flag threaded
through `.verify_contract`) was deliberately deferred as
out-of-scope.

### Phase D: R-parser NUL rejection in `group_names` hash

The plan specified a `"\x00"` NUL separator for the FNV-32 input. R's
parser rejects NUL in string literals. Replaced with `"\x01"` (SOH),
which is equally effective as a separator (neither appears in valid
axis entry names). Also fixed an overflow issue in the FNV-32 XOR
step — `bitwXor` in R requires int32 inputs, and the FNV running
hash is a double in `[0, 2^32)`. Added explicit int32 casts around
the XOR. Algorithm is standard FNV-32.

### Phase F: `get_frame`'s `columns` kwarg dropped

The wrapper's `get_frame(daf, axis_or_query, columns = NULL, cache)`
accepted both a query string AND a `columns` subset. The split into
`get_dataframe(axis, columns)` + `get_dataframe_query(query)` loses
the ability to combine mask-filtered queries with column selection.
Two pre-existing tests had to be rewritten to use the axis form for
column selection and the query form for mask-only assertion. No
coverage loss; minor expressivity loss. Post-1.0 reconsideration
slot.

## Carry-over

### Into slice 10a (query builders)
- `escape_value` / `unescape_value` ready; `DafrQuery` builders will
  use `escape_value` for canonical-string construction.

### Into slice 10b (AnnData + h5ad)
- `WARN_HANDLER` constant ready for `h5ad_as_daf(..., unsupported_handler = WARN_HANDLER)`.
- `is_daf` ready for `DafAnnData` facade guards.
- `get_dataframe` ready for `obs` / `var` active bindings on `DafAnnData`.

### Into slice 10d (release polish)
- `@examples` blocks already present on all 10c new exports.
- NEWS entry in place; slice-10d will add the `# dafr 0.1.0` heading
  and the version bump.

### Orthogonal / unchanged from 9d-N
- mmap S7-ctor floor (4 accept-class breaches).
- Two-pass flat-storage optimisation for mode/quantile.
- Acc-struct slimming.
- `copy_all` double-write bug.
- 9d-M code-review minor items.

## `devtools::check` (post-merge)

```
Status: 4 NOTEs
0 errors ✔ | 0 warnings ✔ | 4 notes ✖
```

All 4 NOTEs are pre-existing carry-over, **none are 10c-new**:

1. `.claude` hidden directory in top-level (Claude Code session state
   — gitignored in `3b26548` but not `.Rbuildignore`'d; housekeeping
   slot).
2. Installed package size 6.1 MB (`libs/` 3.5 MB + `extdata/` 1.4 MB)
   — parent kickoff §"Future post-release work" names this as a
   CRAN-submission blocker to be burned down separately.
3. Non-standard top-level `benchmarks/` directory — same CRAN-blocker
   carry-over bucket. Fix is a `.Rbuildignore` entry.
4. "unable to verify current time" — build-environment flake,
   non-actionable.

Exit-criterion "NOTE-free on the new exports" is met (no NOTE
mentions any 10c surface).

## Post-release follow-up slots

- Tensor `.verify_access` tracking — tensors declared `RequiredInput`
  are currently never flagged as unused. Low priority; wrapper has
  the same limitation.
- `get_dataframe_query` `columns` kwarg (see Phase F issue).
- Smarter `verify_contract` (proper static flag; drop the
  accessed-marker hack).
- `.Rbuildignore` entries for `.claude/` and `benchmarks/` (CRAN
  pre-submission housekeeping).
