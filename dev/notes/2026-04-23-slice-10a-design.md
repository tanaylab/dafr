# Slice 10a — Design: Query Builders

**Date:** 2026-04-23
**Predecessor:** Slice 10c (tag `slice-10c` on `main`).
**Parent kickoff:** `dev/notes/slice-10-kickoff.md` §"10a — query builders".
**Scope:** 53 exported builder functions producing `DafrQuery` S7 objects; 5 factory helpers; `DafrQuery` class + methods; `get_query` / `has_query` / `[.DafReader` dispatch extended to accept `DafrQuery`. Pure R, no C++.

## 1. Goal

Port the wrapper's pipe-chain query DSL. Users write:
```r
daf[Axis("cell") |> LookupVector("age") |> IsGreater(2)]
```
and get the same result as the string form `daf["@ cell : age > 2"]`.

**Done signal.** 53 builders exported; ≥250 new testthat assertions; full existing suite green; `parse_query(canonical_query(q))@ast == q@ast` identity holds per-builder and under composition; merged to `main` with tag `slice-10a`.

## 2. Out of scope

- Deprecated wrapper builders: `And`, `AndNot`, `Or`, `OrNot`, `Xor`, `XorNot`, `Fetch`, `Lookup`, `MaskSlice`, `SquareMaskColumn`, `SquareMaskRow`. Not ported.
- Any reduction-formula logic. Formula authority stays in `R/operations.R` + `R/query_eval.R`.
- No C++ / kernel changes.
- No new dependencies.

## 3. Locked decisions

| # | Topic | Decision |
|---|---|---|
| 1 | `DafrQuery` class shape | S7 class with two properties: `ast` (list, as `parse_query` returns) and `canonical` (character scalar). Builders compute both at construction. |
| 2 | `DafrQuery` methods | `print.DafrQuery` prints the canonical string (single line, plus a label "<DafrQuery>"), returns invisibly. `format.DafrQuery` and `as.character.DafrQuery` return the canonical string. `length.DafrQuery` returns `length(ast)`. |
| 3 | Factory helpers | 5 helpers in `R/query_builders.R`, mirroring wrapper `R/query_factories.R`: `.make_nullary(op_name)`, `.make_string_op(op_name, param_name)`, `.make_value_op(op_name, param_name)`, `.make_optional_string_op(op_name, param_name)`, `.make_typed_reduction(op_name)`. All use `force()` on captured symbols. |
| 4 | Composition detection | Port wrapper's `extract_query_and_value` verbatim as `.extract_query_and_value` in `R/query_builders.R`. The DafrQuery-vs-value check uses `S7::S7_inherits(x, DafrQuery)` instead of `inherits(x, "JuliaObject")`. |
| 5 | File layout | 3 new files: `R/query_class.R` (class + methods), `R/query_builders.R` (factory helpers + `.extract_query_and_value`), `R/query_builders_exports.R` (one-liner per export). |
| 6 | `[.DafReader` dispatch | New S7 method via `S7::method(\`[\`, DafReader) <- function(x, i) { ... }`. Accepts character scalar (→ existing `get_query`) or `DafrQuery` (→ `.eval_query(x, i@ast)`). Errors on anything else. |
| 7 | `get_query` / `has_query` dispatch | Extend to accept either a character scalar or a `DafrQuery`. Character path unchanged. `DafrQuery` path: `.eval_query(daf, q@ast)`, with cache key from `q@canonical`. |
| 8 | Phase ordering | 7 TDD phases: 0 (branch), A (infrastructure: class + factories + dispatch), B (eltwise 7), C (reductions 19), D (selection/axis 13), E (logical masks 6), F (comparison 8), Z (polish). |
| 9 | Test budget | ≥250 assertions. Per builder: construct, pipe-compose, AST identity, error on bad input. Plus cross-cutting: print/format/as.character methods, canonical-equality under composition order, reduction type dispatch. |

## 4. Class & factory specifications

### 4.1 `DafrQuery` class (R/query_class.R)

```r
#' @export
DafrQuery <- S7::new_class(
    name = "DafrQuery",
    package = "dafr",
    properties = list(
        ast       = S7::class_list,
        canonical = S7::class_character
    ),
    validator = function(self) {
        if (length(self@canonical) != 1L) {
            return("canonical must be a character scalar")
        }
        NULL
    }
)

#' @export
format.DafrQuery <- function(x, ...) x@canonical
#' @export
as.character.DafrQuery <- function(x, ...) x@canonical
#' @export
print.DafrQuery <- function(x, ...) {
    cat("<DafrQuery>", x@canonical, "\n", sep = " ")
    invisible(x)
}
#' @export
length.DafrQuery <- function(x) length(x@ast)
```

### 4.2 `.extract_query_and_value` (R/query_builders.R)

Port from wrapper `R/utils.R:65`. Swap `inherits(x, "JuliaObject")` → `S7::S7_inherits(x, DafrQuery)`. Signature:
```r
.extract_query_and_value <- function(arg_val, arg_missing, dots,
                                     required = FALSE, default = NULL) {
    # ...
    list(query = q_or_NULL, value = v_or_default, provided = bool)
}
```

### 4.3 Factory helpers (R/query_builders.R)

Each factory:
1. Captures `op_name` (and optionally `param_name`) via `force()`.
2. Returns a closure that accepts `value`/`type`/`...`.
3. Uses `.extract_query_and_value` to split pipe-target vs real value.
4. Validates the value type.
5. Constructs an AST fragment via the appropriate `.qop_<op>` internal constructor (existing; lives in `R/query_ast.R`).
6. Computes the canonical string for the fragment via `.canonicalise_node`.
7. If there's a prior `DafrQuery`, concatenates both `ast` (list-concat) and `canonical` (space-separated).
8. Returns a fresh `DafrQuery(ast = ..., canonical = ...)`.

### 4.4 Dispatch (R/queries.R modifications)

```r
get_query <- function(daf, query_string) {
    if (S7::S7_inherits(query_string, DafrQuery)) {
        ast <- query_string@ast
        canon <- query_string@canonical
    } else if (is.character(query_string) && length(query_string) == 1L) {
        ast <- parse_query(query_string)
        canon <- .canonicalise_ast(ast)
    } else {
        stop("`query_string` must be a character scalar or DafrQuery", call. = FALSE)
    }
    # ... existing cache + eval path with `ast` and `canon` ...
}
```

Similar for `has_query`. Add `S7::method(\`[\`, DafReader) <- function(x, i) { ... }` in `R/queries.R` (or `R/classes.R`).

## 5. Builder export table

All 53 builders are one-line assignments in `R/query_builders_exports.R`:

| Category | Factory | Builders |
|---|---|---|
| Nullary | `.make_nullary` | `Abs`, `Count`, `EndMask`, `Max`, `Mean`, `Median`, `Min`, `Names`, `ReduceToColumn`, `ReduceToRow`, `Round`, `Significant`, `Sum` |
| String op (property/name) | `.make_string_op` | `AndMask`, `AndNegatedMask`, `Axis`, `BeginMask`, `BeginNegatedMask`, `CountBy`, `GroupBy`, `GroupColumnsBy`, `GroupRowsBy`, `IsMatch`, `IsNotMatch`, `OrMask`, `OrNegatedMask`, `XorMask`, `XorNegatedMask` |
| Value op | `.make_value_op` | `IfMissing`, `IsEqual`, `IsGreater`, `IsGreaterEqual`, `IsLess`, `IsLessEqual`, `IsNotEqual`, `SquareColumnIs`, `SquareRowIs` |
| Optional string | `.make_optional_string_op` | `AsAxis`, `IfNot`, `LookupMatrix`, `LookupScalar`, `LookupVector` |
| Typed reduction | `.make_typed_reduction` | `Clamp`, `Convert`, `Fraction`, `GeoMean`, `Log`, `Mode`, `Quantile`, `Std`, `StdN`, `Var`, `VarN` |

Total: 13 + 15 + 9 + 5 + 11 = 53. Exact categorisation may need minor adjustment per wrapper's `R/query.R` — the plan will verify against the wrapper's actual factories list.

## 6. Error handling

| Scenario | Response |
|---|---|
| Builder missing required value | `cli::cli_abort("`value`/`property` is missing with no default")` |
| Builder receives wrong type | `cli::cli_abort` with expected type |
| `get_query(daf, 42)` | Error: "must be a character scalar or DafrQuery" |
| `daf[TRUE]` | Error: same message |
| `Axis("name with / slash")` | Passes through: `escape_value` quotes the name, canonicalises as `@ "name with / slash"` |
| Pipe two masks without EndMask | Passes through to AST; parse-level error surfaces at eval time |

## 7. Test plan

7 new test files:

| File | Assertions |
|---|---|
| `test-dafrquery-class.R` | ~15 (constructor, accessors, print/format/as.character/length, validator errors) |
| `test-builders-eltwise.R` | ~30 (4 × 7 eltwise + cross-cutting) |
| `test-builders-reductions.R` | ~85 (4 × 19 reductions + type-dispatch + GroupBy/CountBy semantics) |
| `test-builders-selection.R` | ~60 (4 × 13 + escape round-trip for Axis) |
| `test-builders-masks.R` | ~28 (4 × 6 + composition order) |
| `test-builders-comparison.R` | ~36 (4 × 8 + IsMatch regex + Sparse NA handling) |
| `test-query-dispatch.R` | ~10 (get_query/has_query/[.DafReader accepting both forms; error on invalid) |

Total ≥ 264 assertions. Budget target: ≥250. Overshoot acceptable.

**Non-negotiable test mines:**
- `parse_query(canonical_query(q))@ast` equals `q@ast` for every builder in isolation AND under composition.
- `Axis("name with spaces")` canonical is `@ "name with spaces"` and round-trips.
- `daf[Axis("cell") |> LookupVector("age")]` equals `daf["@ cell : age"]`.
- `length(Axis("x") |> LookupVector("y"))` equals 2.

## 8. Slice execution order

- **Phase 0**: branch setup (`slice-10a` off `main`); baseline-green test run.
- **Phase A**: DafrQuery class + 5 factories + `.extract_query_and_value` + dispatch extension (`get_query`, `has_query`, `[.DafReader`). Plus `test-dafrquery-class.R` and `test-query-dispatch.R`.
- **Phase B**: 7 eltwise builders. `test-builders-eltwise.R`.
- **Phase C**: 19 reduction builders. `test-builders-reductions.R`. Biggest phase.
- **Phase D**: 13 selection/axis builders. `test-builders-selection.R`.
- **Phase E**: 6 logical mask builders. `test-builders-masks.R`.
- **Phase F**: 8 comparison builders. `test-builders-comparison.R`.
- **Phase Z**: NEWS entry; merge; tag `slice-10a`; exit note.

## 9. Exit criterion

- `R CMD INSTALL .` clean.
- `cd tests && NOT_CRAN=true Rscript testthat.R` green; +260 assertions.
- `devtools::check()` 0 ERROR, 0 WARNING, 0 NEW NOTES.
- 53 builders in NAMESPACE.
- `daf[Axis("cell") |> LookupVector("age") |> IsGreater(2)]` returns the same object as the string equivalent.
- Merged to `main`; tag `slice-10a`.

## 10. Post-slice carry-over

Unchanged from 10c exit: mmap ctor floor; mode/quantile two-pass; `copy_all` double-write; 9d-M code-review minor items.

New for 10a:
- Builder coverage for Sparse-specific comparison semantics (IsMatch on factor columns) — may surface during testing, defer if orthogonal.
