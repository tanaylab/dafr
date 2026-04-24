# Slice 3 — Query DSL + Views over DafReader backends

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` (recommended) or `superpowers:executing-plans` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Port Julia `DataAxesFormats.Queries` + `DataAxesFormats.Views` to the native-R `dafr` package, giving users a text-based query DSL (`q("@ cell : UMIs % log(eps: 1)")`) that composes over both `MemoryDaf` and `FilesDaf`, and a `ViewDaf` lazy wrapper that exposes a renamed / filtered view of a base store without copying data.

**Architecture:**
- **Queries are lazy, parse-once, cache-on-canonical-string.** A query string is tokenised, parsed into a vector of AST nodes (one per operation token in the left-to-right pipeline), re-serialised into a canonical form, and then evaluated against a `DafReader` through the existing 22 `format_*` generics. Evaluation uses a small stack machine over `QueryState` that mirrors Julia's `get_query_final_state`. Results land in the pre-scaffolded `query` cache tier (`R/cache.R:6`) keyed by the canonical string + the axis/vector/matrix version counters they touch.
- **Operations registry (reductions + eltwise) ships as a prerequisite.** The query grammar has `>-` / `>|` tokens taking named reductions (`Sum`, `Mean`, `Max`, `Min`, `Count`) and `%` tokens taking named eltwise ops (`Log`, `Abs`, `Exp`, `Sqrt`, `Round`). These are small, pure functions with a registry table; we port the minimum set that unblocks the query fixture round-trip.
- **Views are a new S7 class** `ViewDaf` (under `DafReadOnly`) carrying a reference to a base `DafReader` and a list of renamed axis / scalar / vector / matrix specs (name-in-view → `NULL | "=" | query-string`). Every `format_*` generic on `ViewDaf` rewrites the call into a query against the base. No copies.
- **Julia compatibility is verified by a fixture of query strings + expected results**, generated via the existing conda-gated Julia env (`tests/testthat/helper-julia.R:.have_julia_env`). The fixture lives at `tests/testthat/fixtures/julia-queries/` (committed JSON of inputs + outputs). Live Julia parity runs when the env is available; static fixture runs always.

**Tech Stack:** R 4.4+, S7 0.2.1, `jsonlite`, `Matrix` (dgCMatrix / lgCMatrix), `bit64` (Int64 vectors), `stringi` for regex + escape handling. No new Imports beyond what Slice 2 already carries.

**Repo layout:**
- Package repo: `/home/aviezerl/src/dafr-native/` (`main`, tracks `origin/main` at `git@github.com:tanaylab/dafr.git`, tag `slice-2` at `96c3bdd`).
- Dev repo (nested, gitignored): `/home/aviezerl/src/dafr-native/dev/`. Plans + notes + specs live here.
- Source + tests commits → package repo. Plan + notes + spec commits → dev repo. Infer from file path; use `cd ~/src/dafr-native` or `cd ~/src/dafr-native/dev` explicitly.

**Dev loop per task:**
1. From the package root:
   ```
   Rscript -e 'pkgbuild::compile_dll(debug=FALSE); devtools::load_all("."); testthat::test_dir("tests/testthat", filter = "<tag>")'
   ```
2. Inspect output; iterate until green.
3. Stage + commit with the provided message.

---

## Pre-planning decisions (settled before tasks)

### 1. Scope split — queries + views only, chains + contracts deferred to Slice 4

Confirmed with user on 2026-04-20. Reason: porting all four Julia modules (queries 5738 + views 1100 + chains 828 + contracts 1634 = 9300 lines) in one slice is 2–3× Slice 2. Landing queries + views standalone lets the parser / cache-key story stabilise against real workloads before layering federation (chains) and typed pre/post-conditions (contracts) on top.

### 2. L2 upstream PR (`tanaylab/DataAxesFormats.jl` docs) — stays deferred

Confirmed with user on 2026-04-20. Spec draft at `dev/specs/filesdaf-on-disk-spec-draft.md` remains resolved and ready; we do not open the Julia-repo PR in Slice 3. Re-ask at the Slice 3 exit.

### 3. AST representation — tagged lists, not S7 classes

Julia uses `struct X <: QueryOperation` + multi-dispatch. R S7 would be overkill for ~30 small AST node types. We use tagged lists:

```r
.qop_axis <- function(axis_name) {
  structure(list(op = "Axis", axis_name = axis_name), class = c("qop_Axis", "qop"))
}
```

Dispatch on `$op` via a small lookup table (`.QOP_DISPATCH`) for the parser and evaluator. This keeps AST nodes cheap to allocate, easy to pretty-print, and avoids S7 method-table churn for every token.

### 4. Operations registry — vendored minimum set, not the full Julia registry

Julia's `Registry` module supports **all** registered eltwise + reduction operations with parameter validation. For Slice 3 we ship:

- **Reductions:** `Sum`, `Mean`, `Max`, `Min`, `Count` (these are the ones used in jldoctests + fixture).
- **Eltwise:** `Log` (with `eps` + `base` named params), `Abs`, `Exp`, `Sqrt`, `Round`.

Additional operations can be registered later via `register_reduction()` / `register_eltwise()`. Not every Julia op needs to land in v1.

### 5. Query canonicalisation — exact byte match with Julia's `canonical_query`

For `query` cache tier key stability across R and Julia (which reuse the same canonical-string for interop), we serialise the AST back to a string in the same form as Julia's `canonical_query()` (queries.jl line-range is documented in the task). This means: single-space operator separators, no leading/trailing whitespace, escape sequences matching `Tokens.escape_value`. The Julia fixture test compares canonical-strings byte-for-byte.

### 6. Cache key structure — reuse `cache_key_query(canon)` from Slice 1

`R/cache.R:26` already defines `cache_key_query(canon) -> "query:<canon>"`. We use this verbatim. The stored value is the evaluated result (scalar / vector / matrix / NULL on missing). Invalidation piggy-backs on the existing version counters: query cache entries are tagged with the (axis_version, vector_version, matrix_version) tuples they touched at eval time; on read the tuple is compared and stale entries are evicted. This is new logic — see Task Q12.

### 7. Named parameters in operations — R list via `@param_name = value` at call site

Julia's `Sum type Int64` or `Log eps: 1 base: 2` syntax maps to R named-list arguments at the operation call site. The parser collects `identifier: value` pairs into a list; the evaluator passes them to the op function as `do.call(op, c(list(x), params))`.

### 8. No new native (C++) code in Slice 3

All of queries + views is pure R. The ALTREP mmap integration from Slice 2 is already exercised through `format_get_vector` / `format_get_matrix`; queries call those, so mmap-backed data flows through queries unchanged. Reductions and eltwise operations that are performance-sensitive (future work) can be lifted into C++ in a later slice.

### 9. Deferred polish — explicitly tracked, not silently dropped

Two items the user called out on 2026-04-20 that are deferred to late-slice / post-slice cleanup, not skipped entirely:

- **Roxygen `@examples` sections for all new exported functions.** Every task below specifies roxygen headers with `@param` / `@return` / `@description`; `@examples` blocks are intentionally omitted from per-task roxygen and added in Phase Z task Z2.
- **Run `alutil::sad()` (= `styler::style_pkg(indent_by = 4); devtools::document()`) on the package before the exit gate.** This reformats the entire codebase to 4-space indent — a large but mechanical diff. Do it as a **single separate commit** in Phase Z task Z3, not mixed with substantive work. Flag to user before running: this will touch ALL R files, not just Slice 3 additions.

### 10. Live Julia env reuse

The Julia-fixture generation script (`dev/scripts/regen-julia-queries-fixture.jl`) runs under the same `dafr-mcview` conda env used by Slice 2 (`conda run -n dafr-mcview julia ...`). The gate is `tests/testthat/helper-julia.R::.have_julia_env()` — added in Slice 2 and already guards against missing conda on CI. Reuse the same helper; do NOT re-implement gating.

---

## File structure

### New R source files (package repo)

| File | Responsibility |
|------|---------------|
| `R/query_tokens.R` | Tokenizer: split a query string into a vector of `{type, value, pos}` records. Port of `Tokens.tokenize` + operator regex + escape handling. ~350 lines. |
| `R/query_ast.R` | AST node constructors (`.qop_axis`, `.qop_lookup_scalar`, `.qop_mask`, …) + `.QOP_DISPATCH` table + canonical-string serialiser. ~500 lines. |
| `R/query_parse.R` | `.parse_query_tokens(tokens) -> list(qop, ...)` — hand-rolled state machine consuming tokens left-to-right, emitting AST nodes. ~400 lines. |
| `R/query_eval.R` | `QueryState` stack machine; `.eval_query(daf, ast) -> scalar / vector / matrix / names_set / NULL`. ~900 lines. |
| `R/operations.R` | Operations registry (reductions + eltwise) + vendored default ops. ~200 lines. |
| `R/queries.R` | User-facing entry points: `parse_query()`, `get_query()`, `has_query()`, `is_axis_query()`, `query_axis_name()`, `query_result_dimensions()`, `get_frame()`, `q()` convenience wrapper. ~250 lines. |
| `R/view_daf.R` | `ViewDaf` S7 class + `viewer()` constructor + all 22 `format_*` method registrations over base daf via query rewrite. ~500 lines. |

### New test files (package repo)

| File | Responsibility |
|------|---------------|
| `tests/testthat/test-query-tokens.R` | Tokeniser unit tests. |
| `tests/testthat/test-query-parse.R` | Parser unit tests (string → AST → canonical string). |
| `tests/testthat/test-query-eval-lookups.R` | Scalar / axis / vector / matrix lookup evaluator. |
| `tests/testthat/test-query-eval-masks.R` | `[ … ]` mask chains + comparators + logical combinators. |
| `tests/testthat/test-query-eval-slicing.R` | `SquareRowIs` / `SquareColumnIs`. |
| `tests/testthat/test-query-eval-reductions.R` | `>-` / `>|` with registered reductions. |
| `tests/testthat/test-query-eval-eltwise.R` | `%` with registered eltwise ops. |
| `tests/testthat/test-query-eval-groupby.R` | `GroupBy` / `CountBy` / `GroupRowsBy` / `GroupColumnsBy`. |
| `tests/testthat/test-query-frames.R` | `get_frame()` returning a tibble of vectors. |
| `tests/testthat/test-query-cache.R` | Query cache tier hit/miss + invalidation on writes. |
| `tests/testthat/test-query-julia-compat.R` | Run every query in the Julia fixture; compare canonical strings + eval results. |
| `tests/testthat/test-operations-registry.R` | Reduction + eltwise op unit tests. |
| `tests/testthat/test-view-daf.R` | `ViewDaf` class + `viewer()` smoke + axis/scalar/vector/matrix override. |
| `tests/testthat/test-view-wildcards.R` | `ALL_AXES` / `ALL_SCALARS` / `ALL_VECTORS` / `ALL_MATRICES` expansion. |
| `tests/testthat/test-view-julia-compat.R` | Round-trip view specs through Julia + compare. |

### New fixtures

- `tests/testthat/fixtures/julia-queries/fixture.json` — array of `{query, canonical, result_kind, result_value}` records. Generated by `dev/scripts/regen-julia-queries-fixture.jl`.
- `tests/testthat/fixtures/julia-queries/example-daf/` — serialized `example_cells_daf()` at FilesDaf format (reused across all query tests).

### New dev-repo artefacts

- `dev/scripts/regen-julia-queries-fixture.jl` — Julia script regenerating the fixture.
- `dev/notes/slice-3-exit.md` — exit gate (written in Phase Z).

---

## Phase O — Operations registry (prerequisite)

### Task O1: Scaffold `R/operations.R` + registry

**Files:**
- Create: `R/operations.R`
- Create: `tests/testthat/test-operations-registry.R`

- [ ] **Step 1: Write failing test `tests/testthat/test-operations-registry.R`**

```r
test_that("register_reduction stores a function retrievable by name", {
  f <- function(x, ...) sum(x)
  register_reduction("TestSum", f)
  expect_identical(get_reduction("TestSum"), f)
  expect_true("TestSum" %in% registered_reductions())
})

test_that("register_eltwise stores a function retrievable by name", {
  f <- function(x, ...) x + 1
  register_eltwise("TestAdd1", f)
  expect_identical(get_eltwise("TestAdd1"), f)
  expect_true("TestAdd1" %in% registered_eltwise())
})

test_that("get_reduction raises for unknown name", {
  expect_error(get_reduction("NoSuchOp"), "unknown reduction operation")
})

test_that("get_eltwise raises for unknown name", {
  expect_error(get_eltwise("NoSuchOp"), "unknown eltwise operation")
})
```

- [ ] **Step 2: Run test — expect failure**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-registry.R")'
```

Expected: FAIL — `register_reduction` not found.

- [ ] **Step 3: Implement `R/operations.R`**

```r
#' @include utils.R
NULL

.ops_env <- new.env(parent = emptyenv())
.ops_env$reductions <- list()
.ops_env$eltwise <- list()

#' Register a reduction operation.
#' @param name Op name (character scalar, matches token in query strings).
#' @param fn Function `function(x, ...)` where `x` is a numeric vector /
#'   matrix column and `...` collects named parameters.
#' @return Invisibly `NULL`.
#' @export
register_reduction <- function(name, fn) {
  .assert_name(name, "reduction name")
  stopifnot(is.function(fn))
  .ops_env$reductions[[name]] <- fn
  invisible(NULL)
}

#' Register an eltwise operation.
#' @inheritParams register_reduction
#' @export
register_eltwise <- function(name, fn) {
  .assert_name(name, "eltwise name")
  stopifnot(is.function(fn))
  .ops_env$eltwise[[name]] <- fn
  invisible(NULL)
}

#' @export
get_reduction <- function(name) {
  fn <- .ops_env$reductions[[name]]
  if (is.null(fn)) {
    stop(sprintf("unknown reduction operation: %s", sQuote(name)), call. = FALSE)
  }
  fn
}

#' @export
get_eltwise <- function(name) {
  fn <- .ops_env$eltwise[[name]]
  if (is.null(fn)) {
    stop(sprintf("unknown eltwise operation: %s", sQuote(name)), call. = FALSE)
  }
  fn
}

#' @export
registered_reductions <- function() sort(names(.ops_env$reductions))

#' @export
registered_eltwise <- function() sort(names(.ops_env$eltwise))
```

Add `@include operations.R` to `R/queries.R` header when created in Q0.

- [ ] **Step 4: Run test — expect pass**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-operations-registry.R")'
```

Expected: PASS 4/4.

- [ ] **Step 5: Commit**

```bash
git add R/operations.R tests/testthat/test-operations-registry.R
git commit -m "feat(operations): registry scaffolding for reductions + eltwise"
```

---

### Task O2: Vendored reductions (Sum / Mean / Max / Min / Count)

**Files:**
- Modify: `R/operations.R` (append default reductions + init hook)
- Modify: `tests/testthat/test-operations-registry.R` (append tests for each)
- Modify: `R/zzz.R` (call `.register_default_ops()` in `.onLoad`)

- [ ] **Step 1: Write failing tests (append)**

```r
test_that("default reductions are registered on load", {
  for (op in c("Sum", "Mean", "Max", "Min", "Count")) {
    expect_true(op %in% registered_reductions(), info = op)
  }
})

test_that("Sum reduces a numeric vector", {
  expect_equal(get_reduction("Sum")(c(1, 2, 3)), 6)
  expect_equal(get_reduction("Sum")(c(1, NA, 3)), NA_real_)
  expect_equal(get_reduction("Sum")(c(1, NA, 3), na_rm = TRUE), 4)
})

test_that("Mean reduces a numeric vector", {
  expect_equal(get_reduction("Mean")(c(1, 2, 3)), 2)
})

test_that("Max/Min reduce a numeric vector", {
  expect_equal(get_reduction("Max")(c(3, 1, 4, 1, 5)), 5)
  expect_equal(get_reduction("Min")(c(3, 1, 4, 1, 5)), 1)
})

test_that("Count returns length of input", {
  expect_equal(get_reduction("Count")(c(1, 2, 3)), 3L)
  expect_equal(get_reduction("Count")(character(5)), 5L)
})
```

- [ ] **Step 2: Run test — expect failure**

Expected: FAIL on "default reductions are registered" (none registered yet).

- [ ] **Step 3: Append to `R/operations.R`**

```r
.op_sum <- function(x, ..., na_rm = FALSE) sum(x, na.rm = na_rm)
.op_mean <- function(x, ..., na_rm = FALSE) mean(x, na.rm = na_rm)
.op_max <- function(x, ..., na_rm = FALSE) max(x, na.rm = na_rm)
.op_min <- function(x, ..., na_rm = FALSE) min(x, na.rm = na_rm)
.op_count <- function(x, ...) length(x)

.register_default_ops <- function() {
  register_reduction("Sum",   .op_sum)
  register_reduction("Mean",  .op_mean)
  register_reduction("Max",   .op_max)
  register_reduction("Min",   .op_min)
  register_reduction("Count", .op_count)
  invisible(NULL)
}
```

- [ ] **Step 4: Hook `.register_default_ops()` from `.onLoad` in `R/zzz.R`**

Append to existing `.onLoad` body:

```r
.register_default_ops()
```

- [ ] **Step 5: Run test — expect pass**

Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add R/operations.R R/zzz.R tests/testthat/test-operations-registry.R
git commit -m "feat(operations): default reductions (Sum, Mean, Max, Min, Count)"
```

---

### Task O3: Vendored eltwise (Log / Abs / Exp / Sqrt / Round)

**Files:**
- Modify: `R/operations.R` (append eltwise ops)
- Modify: `tests/testthat/test-operations-registry.R`

- [ ] **Step 1: Write failing tests (append)**

```r
test_that("default eltwise ops are registered on load", {
  for (op in c("Log", "Abs", "Exp", "Sqrt", "Round")) {
    expect_true(op %in% registered_eltwise(), info = op)
  }
})

test_that("Log applies log with eps + base", {
  fn <- get_eltwise("Log")
  expect_equal(fn(c(1, 10, 100)), log(c(1, 10, 100)))
  expect_equal(fn(c(1, 10, 100), base = 10), log10(c(1, 10, 100)))
  expect_equal(fn(c(0, 9, 99), eps = 1, base = 10), log10(c(1, 10, 100)))
})

test_that("Abs / Exp / Sqrt / Round behave as expected", {
  expect_equal(get_eltwise("Abs")(c(-1, 2, -3)), c(1, 2, 3))
  expect_equal(get_eltwise("Exp")(c(0, 1)), c(1, exp(1)))
  expect_equal(get_eltwise("Sqrt")(c(0, 4, 9)), c(0, 2, 3))
  expect_equal(get_eltwise("Round")(c(1.4, 1.5, 1.6)), c(1, 2, 2))
  expect_equal(get_eltwise("Round")(c(1.44, 1.55), digits = 1), c(1.4, 1.6))
})
```

- [ ] **Step 2: Run test — expect failure**

- [ ] **Step 3: Append to `R/operations.R`**

```r
.op_log <- function(x, ..., eps = 0, base = exp(1)) {
  log(x + eps, base = base)
}
.op_abs   <- function(x, ...) abs(x)
.op_exp   <- function(x, ...) exp(x)
.op_sqrt  <- function(x, ...) sqrt(x)
.op_round <- function(x, ..., digits = 0) round(x, digits = digits)
```

Extend `.register_default_ops()`:

```r
.register_default_ops <- function() {
  register_reduction("Sum",   .op_sum)
  register_reduction("Mean",  .op_mean)
  register_reduction("Max",   .op_max)
  register_reduction("Min",   .op_min)
  register_reduction("Count", .op_count)

  register_eltwise("Log",   .op_log)
  register_eltwise("Abs",   .op_abs)
  register_eltwise("Exp",   .op_exp)
  register_eltwise("Sqrt",  .op_sqrt)
  register_eltwise("Round", .op_round)

  invisible(NULL)
}
```

- [ ] **Step 4: Run test — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/operations.R tests/testthat/test-operations-registry.R
git commit -m "feat(operations): default eltwise ops (Log, Abs, Exp, Sqrt, Round)"
```

---

## Phase Q — Query DSL

### Task Q0: Scaffold query source files + exports

**Files:**
- Create: `R/query_tokens.R` (empty shell with `@include` directive)
- Create: `R/query_ast.R`
- Create: `R/query_parse.R`
- Create: `R/query_eval.R`
- Create: `R/queries.R`
- Modify: `NAMESPACE` (auto-generated by roxygen; covered by Phase Z)

- [ ] **Step 1: Create `R/query_tokens.R`**

```r
#' @include utils.R
NULL

# Tokenizer — see dev/plans/2026-04-20-slice-3-queries-views.md task Q1.
# Reference: DataAxesFormats.jl tokens.jl + queries.jl:2780.
```

- [ ] **Step 2: Create `R/query_ast.R`**

```r
#' @include query_tokens.R utils.R
NULL

# AST node constructors + canonical-string serialiser.
# Reference: DataAxesFormats.jl queries.jl export list lines 5-53.
```

- [ ] **Step 3: Create `R/query_parse.R`**

```r
#' @include query_ast.R query_tokens.R
NULL

# Parser: tokens -> AST. Hand-rolled state machine.
# Reference: DataAxesFormats.jl queries.jl:2108 (parse_query).
```

- [ ] **Step 4: Create `R/query_eval.R`**

```r
#' @include query_ast.R operations.R format_api.R classes.R
NULL

# Evaluator: QueryState stack machine over DafReader.
# Reference: DataAxesFormats.jl queries.jl:1501 (QueryState), 1765
# (get_query_final_state).
```

- [ ] **Step 5: Create `R/queries.R`**

```r
#' @include query_eval.R query_parse.R
NULL

# Public entry points: parse_query, get_query, has_query,
# is_axis_query, query_axis_name, query_result_dimensions,
# get_frame, q().
```

- [ ] **Step 6: Verify `devtools::load_all()` still succeeds**

```
Rscript -e 'devtools::load_all(".")'
```

Expected: clean load (no method-registration errors).

- [ ] **Step 7: Commit**

```bash
git add R/query_tokens.R R/query_ast.R R/query_parse.R R/query_eval.R R/queries.R
git commit -m "scaffold(queries): empty source files with @include directives"
```

---

### Task Q1: Tokenizer — operator regex + value splitting

**Files:**
- Modify: `R/query_tokens.R`
- Create: `tests/testthat/test-query-tokens.R`

**Reference:** `tokens.jl:SPACE_REGEX / VALUE_REGEX`, `queries.jl:2780` (operator regex). Copy the regexes verbatim.

- [ ] **Step 1: Write failing test `tests/testthat/test-query-tokens.R`**

```r
test_that(".tokenize_query splits simple axis lookup", {
  toks <- .tokenize_query("@ cell")
  expect_equal(length(toks), 2L)
  expect_equal(toks[[1]]$type, "operator"); expect_equal(toks[[1]]$value, "@")
  expect_equal(toks[[2]]$type, "value");    expect_equal(toks[[2]]$value, "cell")
})

test_that(".tokenize_query splits vector lookup", {
  toks <- .tokenize_query("@ cell : UMIs")
  expect_equal(length(toks), 4L)
  expect_equal(vapply(toks, `[[`, "", "value"), c("@", "cell", ":", "UMIs"))
})

test_that(".tokenize_query handles double operators (::, >>, >-)", {
  toks <- .tokenize_query("@ cell @ gene :: UMIs >| Sum")
  vals <- vapply(toks, `[[`, "", "value")
  expect_equal(vals, c("@", "cell", "@", "gene", "::", "UMIs", ">|", "Sum"))
})

test_that(".tokenize_query preserves escaped values", {
  toks <- .tokenize_query("@ cell : \"weird name\"")
  vals <- vapply(toks, `[[`, "", "value")
  expect_equal(vals[[4L]], "weird name")
})

test_that(".tokenize_query records 1-based positions", {
  toks <- .tokenize_query("@ cell")
  expect_equal(toks[[1]]$pos, 1L)
  expect_equal(toks[[2]]$pos, 3L)
})

test_that(".tokenize_query rejects unknown operator characters", {
  expect_error(.tokenize_query("@ cell $ weird"), "unexpected character")
})
```

- [ ] **Step 2: Run test — expect failure** (`.tokenize_query` not defined)

- [ ] **Step 3: Implement `.tokenize_query` in `R/query_tokens.R`**

```r
.QUERY_OP_REGEX <- "^(?:[!<>]=|!~|[\\|-]/|[&^\\|]!|\\?\\?|@[-\\|]|=@|::|>[->\\|]|\\|\\||[!&*%./:<=>?@\\[\\]^\\|~])"

.tokenize_query <- function(s) {
  stopifnot(is.character(s), length(s) == 1L, !is.na(s))
  tokens <- list()
  i <- 1L
  n <- nchar(s)
  while (i <= n) {
    ch <- substr(s, i, i)
    if (grepl("\\s", ch, perl = TRUE)) { i <- i + 1L; next }
    op <- regmatches(substr(s, i, n),
                     regexpr(.QUERY_OP_REGEX, substr(s, i, n), perl = TRUE))
    if (length(op) == 1L && nzchar(op)) {
      tokens[[length(tokens) + 1L]] <- list(type = "operator",
                                            value = op, pos = i)
      i <- i + nchar(op)
      next
    }
    val_info <- .scan_value(s, i)
    if (is.null(val_info)) {
      stop(sprintf("unexpected character %s at position %d in query %s",
                   sQuote(ch), i, sQuote(s)), call. = FALSE)
    }
    tokens[[length(tokens) + 1L]] <- list(type = "value",
                                          value = val_info$value,
                                          pos = i)
    i <- val_info$next_pos
  }
  tokens
}

.scan_value <- function(s, start) {
  n <- nchar(s)
  if (substr(s, start, start) == "\"") {
    j <- start + 1L
    out <- character(0)
    while (j <= n) {
      ch <- substr(s, j, j)
      if (ch == "\\" && j < n) {
        out <- c(out, substr(s, j + 1L, j + 1L))
        j <- j + 2L
      } else if (ch == "\"") {
        return(list(value = paste0(out, collapse = ""), next_pos = j + 1L))
      } else {
        out <- c(out, ch); j <- j + 1L
      }
    }
    stop(sprintf("unterminated quoted value at position %d in query %s",
                 start, sQuote(s)), call. = FALSE)
  }
  m <- regmatches(substr(s, start, n),
                  regexpr("^[^\\s!&*%./:<=>?@\\[\\]^\\|~\"]+",
                          substr(s, start, n), perl = TRUE))
  if (length(m) == 1L && nzchar(m)) {
    list(value = m, next_pos = start + nchar(m))
  } else {
    NULL
  }
}
```

- [ ] **Step 4: Run test — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/query_tokens.R tests/testthat/test-query-tokens.R
git commit -m "feat(query-tokens): operator + value tokenizer with quoted-escape handling"
```

---

### Task Q2: AST node constructors (lookups)

**Files:**
- Modify: `R/query_ast.R`
- Create: `tests/testthat/test-query-ast.R`

**Reference:** Julia exports `Names`, `Axis`, `AsAxis`, `IfMissing`, `LookupScalar`, `LookupVector`, `LookupMatrix`, `IfNot`.

- [ ] **Step 1: Write failing test `tests/testthat/test-query-ast.R`**

```r
test_that(".qop_axis constructs an Axis node", {
  n <- .qop_axis("cell")
  expect_s3_class(n, "qop_Axis")
  expect_equal(n$op, "Axis")
  expect_equal(n$axis_name, "cell")
})

test_that(".qop_lookup_scalar / _vector / _matrix construct lookup nodes", {
  expect_equal(.qop_lookup_scalar("organism")$op,  "LookupScalar")
  expect_equal(.qop_lookup_vector("UMIs")$op,      "LookupVector")
  expect_equal(.qop_lookup_matrix("UMIs")$op,      "LookupMatrix")
})

test_that(".qop_names constructs a Names node (no args)", {
  expect_equal(.qop_names()$op, "Names")
})

test_that(".qop_if_missing captures default value", {
  n <- .qop_if_missing(0)
  expect_equal(n$op, "IfMissing")
  expect_identical(n$default, 0)
})

test_that("canonicalise_ast emits the canonical string", {
  ast <- list(.qop_axis("cell"), .qop_lookup_vector("UMIs"))
  expect_equal(.canonicalise_ast(ast), "@ cell : UMIs")
})
```

- [ ] **Step 2: Run test — expect failure** (constructors undefined).

- [ ] **Step 3: Implement in `R/query_ast.R`**

```r
.qop <- function(op, ...) {
  structure(list(op = op, ...),
            class = c(paste0("qop_", op), "qop"))
}

.qop_names        <- function() .qop("Names")
.qop_axis         <- function(axis_name) .qop("Axis", axis_name = axis_name)
.qop_as_axis      <- function(axis_name) .qop("AsAxis", axis_name = axis_name)
.qop_if_missing   <- function(default)   .qop("IfMissing", default = default)
.qop_if_not       <- function(value = NULL) .qop("IfNot", value = value)
.qop_lookup_scalar <- function(name = NULL) .qop("LookupScalar", name = name)
.qop_lookup_vector <- function(name = NULL) .qop("LookupVector", name = name)
.qop_lookup_matrix <- function(name = NULL) .qop("LookupMatrix", name = name)

.canonicalise_ast <- function(ast) {
  parts <- vapply(ast, .canonicalise_node, character(1))
  paste(parts, collapse = " ")
}

.canonicalise_node <- function(n) {
  switch(n$op,
    Names        = "?",
    Axis         = paste0("@ ", .escape_value(n$axis_name)),
    AsAxis       = paste0("=@ ", .escape_value(n$axis_name)),
    IfMissing    = paste0("|| ", .escape_value(format(n$default))),
    IfNot        = if (is.null(n$value)) "??" else paste0("?? ", .escape_value(format(n$value))),
    LookupScalar = if (is.null(n$name)) "." else paste0(". ", .escape_value(n$name)),
    LookupVector = if (is.null(n$name)) ":" else paste0(": ", .escape_value(n$name)),
    LookupMatrix = if (is.null(n$name)) "::" else paste0(":: ", .escape_value(n$name)),
    stop(sprintf("no canonicaliser for %s", n$op), call. = FALSE))
}

.escape_value <- function(s) {
  if (grepl("[\\s!&*%./:<=>?@\\[\\]^\\|~\"]", s, perl = TRUE)) {
    paste0("\"", gsub("[\\\\\"]", "\\\\\\0", s, perl = TRUE), "\"")
  } else {
    s
  }
}
```

- [ ] **Step 4: Run test — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/query_ast.R tests/testthat/test-query-ast.R
git commit -m "feat(query-ast): lookup node constructors + canonicalise_ast"
```

---

### Task Q3: AST nodes — masks, slicing, reductions, grouping, eltwise

**Files:**
- Modify: `R/query_ast.R`
- Modify: `tests/testthat/test-query-ast.R`

**Reference:** Julia queries.jl exports lines 7-53 (remaining nodes).

- [ ] **Step 1: Append failing tests**

```r
test_that("mask AST nodes construct cleanly", {
  expect_equal(.qop_begin_mask("age")$op, "BeginMask")
  expect_equal(.qop_begin_mask("age", negated = TRUE)$op, "BeginNegatedMask")
  expect_equal(.qop_end_mask()$op, "EndMask")
})

test_that("comparator nodes capture operator + value", {
  expect_equal(.qop_is_less(5)$op,           "IsLess")
  expect_equal(.qop_is_less_equal(5)$op,     "IsLessEqual")
  expect_equal(.qop_is_equal("x")$op,        "IsEqual")
  expect_equal(.qop_is_not_equal("x")$op,    "IsNotEqual")
  expect_equal(.qop_is_greater(5)$op,        "IsGreater")
  expect_equal(.qop_is_greater_equal(5)$op,  "IsGreaterEqual")
  expect_equal(.qop_is_match("^a")$op,       "IsMatch")
  expect_equal(.qop_is_not_match("^a")$op,   "IsNotMatch")
  expect_equal(.qop_is_less(5)$value,        5)
  expect_equal(.qop_is_match("^a")$pattern,  "^a")
})

test_that("logical mask nodes construct", {
  expect_equal(.qop_and_mask("x")$op, "AndMask")
  expect_equal(.qop_or_mask("x", negated = TRUE)$op, "OrNegatedMask")
  expect_equal(.qop_xor_mask("x")$op, "XorMask")
})

test_that("square slice nodes construct", {
  expect_equal(.qop_square_row_is("x")$op, "SquareRowIs")
  expect_equal(.qop_square_column_is("x")$op, "SquareColumnIs")
})

test_that("reduction / grouping nodes construct", {
  n <- .qop_reduce_to_column("Sum", params = list(type = "Int64"))
  expect_equal(n$op, "ReduceToColumn")
  expect_equal(n$reduction, "Sum")
  expect_equal(n$params, list(type = "Int64"))
  expect_equal(.qop_group_by("donor")$op, "GroupBy")
  expect_equal(.qop_count_by("age")$op, "CountBy")
})

test_that("eltwise node carries name + params", {
  n <- .qop_eltwise("Log", params = list(eps = 1, base = 2))
  expect_equal(n$op, "Eltwise")
  expect_equal(n$name, "Log")
  expect_equal(n$params, list(eps = 1, base = 2))
})
```

- [ ] **Step 2: Run test — expect failure**

- [ ] **Step 3: Append to `R/query_ast.R`**

```r
.qop_begin_mask <- function(property, negated = FALSE) {
  if (negated) .qop("BeginNegatedMask", property = property)
  else         .qop("BeginMask",        property = property)
}
.qop_end_mask <- function() .qop("EndMask")

.qop_is_less          <- function(value) .qop("IsLess",          value = value)
.qop_is_less_equal    <- function(value) .qop("IsLessEqual",    value = value)
.qop_is_equal         <- function(value) .qop("IsEqual",         value = value)
.qop_is_not_equal     <- function(value) .qop("IsNotEqual",      value = value)
.qop_is_greater       <- function(value) .qop("IsGreater",       value = value)
.qop_is_greater_equal <- function(value) .qop("IsGreaterEqual",  value = value)
.qop_is_match         <- function(pattern) .qop("IsMatch",      pattern = pattern)
.qop_is_not_match     <- function(pattern) .qop("IsNotMatch",   pattern = pattern)

.qop_and_mask <- function(property, negated = FALSE) {
  if (negated) .qop("AndNegatedMask", property = property)
  else         .qop("AndMask",        property = property)
}
.qop_or_mask <- function(property, negated = FALSE) {
  if (negated) .qop("OrNegatedMask", property = property)
  else         .qop("OrMask",        property = property)
}
.qop_xor_mask <- function(property, negated = FALSE) {
  if (negated) .qop("XorNegatedMask", property = property)
  else         .qop("XorMask",        property = property)
}

.qop_square_row_is    <- function(value) .qop("SquareRowIs",    value = value)
.qop_square_column_is <- function(value) .qop("SquareColumnIs", value = value)

.qop_group_by         <- function(property) .qop("GroupBy",         property = property)
.qop_group_rows_by    <- function(property) .qop("GroupRowsBy",    property = property)
.qop_group_columns_by <- function(property) .qop("GroupColumnsBy", property = property)
.qop_count_by         <- function(property) .qop("CountBy",         property = property)

.qop_reduce_to_column <- function(reduction, params = list()) {
  .qop("ReduceToColumn", reduction = reduction, params = params)
}
.qop_reduce_to_row <- function(reduction, params = list()) {
  .qop("ReduceToRow",    reduction = reduction, params = params)
}

.qop_eltwise <- function(name, params = list()) {
  .qop("Eltwise", name = name, params = params)
}
```

- [ ] **Step 4: Extend `.canonicalise_node` switch**

```r
# Append to the switch:
    BeginMask         = paste0("[ ",     .escape_value(n$property)),
    BeginNegatedMask  = paste0("[ ! ",   .escape_value(n$property)),
    EndMask           = "]",
    IsLess            = paste0("< ",     .escape_value(format(n$value))),
    IsLessEqual       = paste0("<= ",    .escape_value(format(n$value))),
    IsEqual           = paste0("= ",     .escape_value(format(n$value))),
    IsNotEqual        = paste0("!= ",    .escape_value(format(n$value))),
    IsGreater         = paste0("> ",     .escape_value(format(n$value))),
    IsGreaterEqual    = paste0(">= ",    .escape_value(format(n$value))),
    IsMatch           = paste0("~ ",     .escape_value(n$pattern)),
    IsNotMatch        = paste0("!~ ",    .escape_value(n$pattern)),
    AndMask           = paste0("& ",     .escape_value(n$property)),
    AndNegatedMask    = paste0("& ! ",   .escape_value(n$property)),
    OrMask            = paste0("| ",     .escape_value(n$property)),
    OrNegatedMask     = paste0("| ! ",   .escape_value(n$property)),
    XorMask           = paste0("^ ",     .escape_value(n$property)),
    XorNegatedMask    = paste0("^ ! ",   .escape_value(n$property)),
    SquareRowIs       = paste0("@- ",    .escape_value(format(n$value))),
    SquareColumnIs    = paste0("@| ",    .escape_value(format(n$value))),
    GroupBy           = paste0("/ ",     .escape_value(n$property)),
    GroupRowsBy       = paste0("-/ ",    .escape_value(n$property)),
    GroupColumnsBy    = paste0("|/ ",    .escape_value(n$property)),
    CountBy           = paste0("* ",     .escape_value(n$property)),
    ReduceToColumn    = .canonicalise_reduction(">|", n$reduction, n$params),
    ReduceToRow       = .canonicalise_reduction(">-", n$reduction, n$params),
    Eltwise           = .canonicalise_eltwise(n$name, n$params),
```

Helper:

```r
.canonicalise_reduction <- function(tok, reduction, params) {
  head <- paste0(tok, " ", .escape_value(reduction))
  if (length(params) == 0L) return(head)
  tail <- paste(vapply(names(params), function(k)
                        paste0(.escape_value(k), ": ",
                               .escape_value(format(params[[k]]))),
                      character(1)), collapse = " ")
  paste0(head, " ", tail)
}

.canonicalise_eltwise <- function(name, params) {
  head <- paste0("% ", .escape_value(name))
  if (length(params) == 0L) return(head)
  tail <- paste(vapply(names(params), function(k)
                        paste0(.escape_value(k), ": ",
                               .escape_value(format(params[[k]]))),
                      character(1)), collapse = " ")
  paste0(head, "(", tail, ")")
}
```

- [ ] **Step 5: Run test — expect pass**

- [ ] **Step 6: Commit**

```bash
git add R/query_ast.R tests/testthat/test-query-ast.R
git commit -m "feat(query-ast): mask, slice, reduction, grouping, eltwise nodes + canonicalisation"
```

---

### Task Q4: Parser — lookups (`@ axis`, `: name`, `:: name`, `. name`)

**Files:**
- Modify: `R/query_parse.R`
- Create: `tests/testthat/test-query-parse.R`

- [ ] **Step 1: Write failing test**

```r
test_that("parse_query handles empty string -> empty AST", {
  expect_equal(parse_query(""), list())
})

test_that("parse_query handles axis lookup", {
  ast <- parse_query("@ cell")
  expect_length(ast, 1L)
  expect_equal(ast[[1]]$op, "Axis")
  expect_equal(ast[[1]]$axis_name, "cell")
})

test_that("parse_query handles axis + vector lookup", {
  ast <- parse_query("@ cell : UMIs")
  expect_equal(vapply(ast, `[[`, "", "op"), c("Axis", "LookupVector"))
  expect_equal(ast[[2]]$name, "UMIs")
})

test_that("parse_query handles matrix lookup", {
  ast <- parse_query("@ cell @ gene :: UMIs")
  expect_equal(vapply(ast, `[[`, "", "op"),
               c("Axis", "Axis", "LookupMatrix"))
})

test_that("parse_query handles scalar lookup", {
  ast <- parse_query(". organism")
  expect_equal(ast[[1]]$op, "LookupScalar")
  expect_equal(ast[[1]]$name, "organism")
})

test_that("parse_query handles Names query (?)", {
  expect_equal(parse_query("?")[[1]]$op, "Names")
})

test_that("parse_query round-trips via canonicalise", {
  for (s in c("@ cell", "@ cell : UMIs", "@ cell @ gene :: UMIs",
              ". organism", "?")) {
    expect_equal(.canonicalise_ast(parse_query(s)), s, info = s)
  }
})

test_that("parse_query reports token position on error", {
  expect_error(parse_query("@ @"), "position 3")
})
```

- [ ] **Step 2: Run test — expect failure** (`parse_query` undefined).

- [ ] **Step 3: Implement in `R/query_parse.R`**

```r
#' Parse a query string into an AST (list of `qop` nodes).
#'
#' @param query_string A character scalar.
#' @return A list of AST node records.
#' @export
parse_query <- function(query_string) {
  stopifnot(is.character(query_string), length(query_string) == 1L,
            !is.na(query_string))
  tokens <- .tokenize_query(query_string)
  .parse_tokens(tokens, query_string)
}

.parse_tokens <- function(tokens, src) {
  ast <- list()
  i <- 1L
  n <- length(tokens)
  while (i <= n) {
    step <- .parse_next(tokens, i, src)
    ast[[length(ast) + 1L]] <- step$node
    i <- step$next_index
  }
  ast
}

.parse_next <- function(tokens, i, src) {
  tok <- tokens[[i]]
  if (tok$type == "operator") {
    switch(tok$value,
      "?"  = list(node = .qop_names(), next_index = i + 1L),
      "@"  = .parse_axis(tokens, i, src),
      "."  = .parse_lookup(tokens, i, src, ".", .qop_lookup_scalar),
      ":"  = .parse_lookup(tokens, i, src, ":", .qop_lookup_vector),
      "::" = .parse_lookup(tokens, i, src, "::", .qop_lookup_matrix),
      stop(sprintf("unexpected operator %s at position %d in query %s",
                   sQuote(tok$value), tok$pos, sQuote(src)), call. = FALSE)
    )
  } else {
    stop(sprintf("expected operator, got value %s at position %d in query %s",
                 sQuote(tok$value), tok$pos, sQuote(src)), call. = FALSE)
  }
}

.parse_axis <- function(tokens, i, src) {
  if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
    stop(sprintf("expected axis name after '@' at position %d in query %s",
                 tokens[[i]]$pos, sQuote(src)), call. = FALSE)
  }
  list(node = .qop_axis(tokens[[i + 1L]]$value), next_index = i + 2L)
}

.parse_lookup <- function(tokens, i, src, tok_val, ctor) {
  if (i + 1L > length(tokens) || tokens[[i + 1L]]$type != "value") {
    # bare lookup (no name) -- allowed by Julia grammar
    list(node = ctor(NULL), next_index = i + 1L)
  } else {
    list(node = ctor(tokens[[i + 1L]]$value), next_index = i + 2L)
  }
}
```

- [ ] **Step 4: Run test — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/query_parse.R tests/testthat/test-query-parse.R
git commit -m "feat(query-parse): lookups (@ . : :: ?) with error-position reporting"
```

---

### Task Q5: Parser — masks + comparators + logical combinators

**Files:**
- Modify: `R/query_parse.R`
- Modify: `tests/testthat/test-query-parse.R`

- [ ] **Step 1: Append failing tests**

```r
test_that("parse_query handles bracketed mask with comparator", {
  ast <- parse_query("@ donor [ age > 60 ]")
  ops <- vapply(ast, `[[`, "", "op")
  expect_equal(ops, c("Axis", "BeginMask", "IsGreater", "EndMask"))
  expect_equal(ast[[2]]$property, "age")
  expect_equal(ast[[3]]$value,    "60")
})

test_that("parse_query handles negated mask", {
  ast <- parse_query("@ gene [ ! is_lateral ]")
  ops <- vapply(ast, `[[`, "", "op")
  expect_equal(ops, c("Axis", "BeginNegatedMask", "EndMask"))
})

test_that("parse_query handles AND / OR / XOR mask combinators", {
  ast <- parse_query("@ donor [ age > 60 & sex = male ]")
  ops <- vapply(ast, `[[`, "", "op")
  expect_equal(ops, c("Axis", "BeginMask", "IsGreater",
                       "AndMask", "IsEqual", "EndMask"))
})

test_that("parse_query handles IsMatch operator", {
  ast <- parse_query("@ donor [ name ~ ^a ]")
  expect_equal(ast[[3]]$op, "IsMatch")
  expect_equal(ast[[3]]$pattern, "^a")
})

test_that("parse_query canonical-string round-trips for masks", {
  for (s in c("@ donor [ age > 60 ]",
              "@ gene [ ! is_lateral ]",
              "@ donor [ age > 60 & sex = male ]",
              "@ donor [ name ~ ^a ]")) {
    expect_equal(.canonicalise_ast(parse_query(s)), s, info = s)
  }
})
```

- [ ] **Step 2: Extend `.parse_next` dispatch for `[`, `]`, `&`, `|`, `^`**

```r
# In .parse_next switch:
      "["  = .parse_begin_mask(tokens, i, src, negated = FALSE),
      "]"  = list(node = .qop_end_mask(), next_index = i + 1L),
      "&"  = .parse_logical(tokens, i, src, .qop_and_mask),
      "|"  = .parse_logical(tokens, i, src, .qop_or_mask),
      "^"  = .parse_logical(tokens, i, src, .qop_xor_mask),
      "<"  = .parse_cmp(tokens, i, src, .qop_is_less),
      "<=" = .parse_cmp(tokens, i, src, .qop_is_less_equal),
      "="  = .parse_cmp(tokens, i, src, .qop_is_equal),
      "!=" = .parse_cmp(tokens, i, src, .qop_is_not_equal),
      ">"  = .parse_cmp(tokens, i, src, .qop_is_greater),
      ">=" = .parse_cmp(tokens, i, src, .qop_is_greater_equal),
      "~"  = .parse_cmp(tokens, i, src, .qop_is_match),
      "!~" = .parse_cmp(tokens, i, src, .qop_is_not_match),
```

Helpers:

```r
.parse_begin_mask <- function(tokens, i, src, negated) {
  # '[' (value) | '[' '!' (value)
  if (i + 1L > length(tokens)) {
    stop(sprintf("expected property after '[' at position %d in query %s",
                 tokens[[i]]$pos, sQuote(src)), call. = FALSE)
  }
  nxt <- tokens[[i + 1L]]
  if (nxt$type == "operator" && nxt$value == "!") {
    if (i + 2L > length(tokens) || tokens[[i + 2L]]$type != "value") {
      stop(sprintf("expected property name after '[ !' at position %d in query %s",
                   nxt$pos, sQuote(src)), call. = FALSE)
    }
    list(node = .qop_begin_mask(tokens[[i + 2L]]$value, negated = TRUE),
         next_index = i + 3L)
  } else if (nxt$type == "value") {
    list(node = .qop_begin_mask(nxt$value, negated = FALSE),
         next_index = i + 2L)
  } else {
    stop(sprintf("expected property after '[' at position %d in query %s",
                 tokens[[i]]$pos, sQuote(src)), call. = FALSE)
  }
}

.parse_logical <- function(tokens, i, src, ctor) {
  # '&' property | '&' '!' property
  nxt <- tokens[[i + 1L]]
  if (nxt$type == "operator" && nxt$value == "!") {
    list(node = ctor(tokens[[i + 2L]]$value, negated = TRUE),
         next_index = i + 3L)
  } else {
    list(node = ctor(nxt$value, negated = FALSE),
         next_index = i + 2L)
  }
}

.parse_cmp <- function(tokens, i, src, ctor) {
  nxt <- tokens[[i + 1L]]
  if (nxt$type != "value") {
    stop(sprintf("expected value after comparator at position %d in query %s",
                 tokens[[i]]$pos, sQuote(src)), call. = FALSE)
  }
  list(node = ctor(nxt$value), next_index = i + 2L)
}
```

- [ ] **Step 3: Run test — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/query_parse.R tests/testthat/test-query-parse.R
git commit -m "feat(query-parse): bracketed masks, comparators, logical combinators"
```

---

### Task Q6: Parser — slicing + grouping + reductions + eltwise + modifiers

**Files:**
- Modify: `R/query_parse.R`
- Modify: `tests/testthat/test-query-parse.R`

- [ ] **Step 1: Append failing tests**

```r
test_that("parse_query handles square slicing", {
  ast <- parse_query("@ cell @ gene :: UMIs @- cell1")
  ops <- vapply(ast, `[[`, "", "op")
  expect_equal(tail(ops, 1L), "SquareRowIs")
})

test_that("parse_query handles ReduceToColumn / ReduceToRow", {
  ast <- parse_query("@ cell @ gene :: UMIs >| Sum")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "ReduceToColumn")
  ast <- parse_query("@ cell @ gene :: UMIs >- Sum")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "ReduceToRow")
})

test_that("parse_query handles reduction with named params", {
  ast <- parse_query("@ cell @ gene :: UMIs >| Sum type: Int64")
  red <- tail(ast, 1L)[[1]]
  expect_equal(red$reduction, "Sum")
  expect_equal(red$params, list(type = "Int64"))
})

test_that("parse_query handles GroupBy / CountBy / GroupRowsBy / GroupColumnsBy", {
  ast <- parse_query("@ cell : UMIs / donor")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "GroupBy")
  ast <- parse_query("@ donor : age * sex")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "CountBy")
  ast <- parse_query("@ cell @ gene :: UMIs -/ donor")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "GroupRowsBy")
  ast <- parse_query("@ cell @ gene :: UMIs |/ type")
  expect_equal(tail(vapply(ast, `[[`, "", "op"), 1L), "GroupColumnsBy")
})

test_that("parse_query handles eltwise with params", {
  ast <- parse_query("@ cell : UMIs % Log eps: 1 base: 2")
  last <- tail(ast, 1L)[[1]]
  expect_equal(last$op, "Eltwise")
  expect_equal(last$name, "Log")
  expect_equal(last$params, list(eps = "1", base = "2"))
})

test_that("parse_query handles IfMissing / IfNot / AsAxis modifiers", {
  expect_equal(parse_query(". foo || 0")[[2]]$op, "IfMissing")
  expect_equal(parse_query("@ cell : bar ??")[[3]]$op, "IfNot")
  expect_equal(parse_query("=@ cell")[[1]]$op, "AsAxis")
})
```

- [ ] **Step 2: Extend `.parse_next` dispatch table**

```r
# In .parse_next switch:
      "@-"  = .parse_cmp(tokens, i, src, .qop_square_row_is),
      "@|"  = .parse_cmp(tokens, i, src, .qop_square_column_is),
      ">|"  = .parse_reduction(tokens, i, src, .qop_reduce_to_column),
      ">-"  = .parse_reduction(tokens, i, src, .qop_reduce_to_row),
      "/"   = .parse_lookup_like(tokens, i, src, .qop_group_by),
      "-/"  = .parse_lookup_like(tokens, i, src, .qop_group_rows_by),
      "|/"  = .parse_lookup_like(tokens, i, src, .qop_group_columns_by),
      "*"   = .parse_lookup_like(tokens, i, src, .qop_count_by),
      "%"   = .parse_eltwise(tokens, i, src),
      "||"  = .parse_if_missing(tokens, i, src),
      "??"  = .parse_if_not(tokens, i, src),
      "=@"  = .parse_lookup_like(tokens, i, src, .qop_as_axis),
```

Helpers:

```r
.parse_lookup_like <- function(tokens, i, src, ctor) {
  nxt <- tokens[[i + 1L]]
  if (nxt$type != "value") {
    stop(sprintf("expected name after %s at position %d in query %s",
                 sQuote(tokens[[i]]$value), tokens[[i]]$pos,
                 sQuote(src)), call. = FALSE)
  }
  list(node = ctor(nxt$value), next_index = i + 2L)
}

.parse_reduction <- function(tokens, i, src, ctor) {
  nxt <- tokens[[i + 1L]]
  if (nxt$type != "value") {
    stop(sprintf("expected reduction name after %s at position %d in query %s",
                 sQuote(tokens[[i]]$value), tokens[[i]]$pos,
                 sQuote(src)), call. = FALSE)
  }
  params <- list()
  j <- i + 2L
  while (j <= length(tokens) &&
         tokens[[j]]$type == "value" &&
         j + 1L <= length(tokens) &&
         tokens[[j + 1L]]$type == "operator" &&
         tokens[[j + 1L]]$value == ":") {
    k <- tokens[[j]]$value
    if (j + 2L > length(tokens) || tokens[[j + 2L]]$type != "value") break
    v <- tokens[[j + 2L]]$value
    params[[k]] <- v
    j <- j + 3L
  }
  list(node = ctor(nxt$value, params = params), next_index = j)
}

.parse_eltwise <- function(tokens, i, src) {
  nxt <- tokens[[i + 1L]]
  if (nxt$type != "value") {
    stop(sprintf("expected eltwise op name after '%%' at position %d in query %s",
                 tokens[[i]]$pos, sQuote(src)), call. = FALSE)
  }
  params <- list()
  j <- i + 2L
  # accept same `key: value` pairs as reductions
  while (j + 2L <= length(tokens) &&
         tokens[[j]]$type == "value" &&
         tokens[[j + 1L]]$type == "operator" &&
         tokens[[j + 1L]]$value == ":") {
    params[[tokens[[j]]$value]] <- tokens[[j + 2L]]$value
    j <- j + 3L
  }
  list(node = .qop_eltwise(nxt$value, params = params), next_index = j)
}

.parse_if_missing <- function(tokens, i, src) {
  nxt <- tokens[[i + 1L]]
  if (nxt$type == "value") {
    list(node = .qop_if_missing(nxt$value), next_index = i + 2L)
  } else {
    list(node = .qop_if_missing(NULL), next_index = i + 1L)
  }
}

.parse_if_not <- function(tokens, i, src) {
  if (i + 1L <= length(tokens) && tokens[[i + 1L]]$type == "value") {
    list(node = .qop_if_not(tokens[[i + 1L]]$value), next_index = i + 2L)
  } else {
    list(node = .qop_if_not(NULL), next_index = i + 1L)
  }
}
```

- [ ] **Step 3: Run test — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/query_parse.R tests/testthat/test-query-parse.R
git commit -m "feat(query-parse): slicing, grouping, reductions, eltwise, if-missing, if-not, as-axis"
```

---

### Task Q7: Evaluator — scalar + axis lookups

**Files:**
- Modify: `R/query_eval.R`
- Create: `tests/testthat/test-query-eval-lookups.R`

**Reference:** Julia `get_query_final_state` at queries.jl:1765. The R evaluator uses a simple state-passing pattern: each AST node receives `(state, daf)` and returns a new state.

- [ ] **Step 1: Write failing test**

```r
test_that("get_query returns scalar values", {
  d <- memory_daf(name = "t"); set_scalar(d, "organism", "human")
  expect_equal(get_query(d, ". organism"), "human")
})

test_that("get_query returns axis entry vector", {
  d <- memory_daf(name = "t"); add_axis(d, "cell", c("c1", "c2", "c3"))
  expect_equal(get_query(d, "@ cell"), c("c1", "c2", "c3"))
})

test_that("get_query with '?' returns scalar names", {
  d <- memory_daf(name = "t")
  set_scalar(d, "organism", "human"); set_scalar(d, "reference", "test")
  expect_setequal(get_query(d, ". ?"), c("organism", "reference"))
})

test_that("get_query with '? @' returns axis names", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", "c1"); add_axis(d, "gene", "g1")
  expect_setequal(get_query(d, "@ ?"), c("cell", "gene"))
})

test_that("get_query errors on missing scalar unless IfMissing", {
  d <- memory_daf(name = "t")
  expect_error(get_query(d, ". missing"), "no scalar")
  expect_equal(get_query(d, ". missing || 0"), "0")
})
```

- [ ] **Step 2: Run test — expect failure**

- [ ] **Step 3: Implement evaluator skeleton in `R/query_eval.R`**

```r
#' Evaluate a parsed AST against a DafReader.
#' @keywords internal
#' @noRd
.eval_query <- function(daf, ast) {
  state <- list(kind = "init", value = NULL, if_missing = NULL)
  for (node in ast) {
    state <- .apply_node(node, state, daf)
  }
  state$value
}

.apply_node <- function(node, state, daf) {
  dispatch <- switch(node$op,
    Names        = .apply_names,
    Axis         = .apply_axis,
    LookupScalar = .apply_lookup_scalar,
    LookupVector = .apply_lookup_vector,
    LookupMatrix = .apply_lookup_matrix,
    IfMissing    = .apply_if_missing,
    IfNot        = .apply_if_not,
    AsAxis       = .apply_as_axis,
    BeginMask    = .apply_begin_mask,
    BeginNegatedMask = .apply_begin_mask,
    EndMask      = .apply_end_mask,
    AndMask = , AndNegatedMask = , OrMask = , OrNegatedMask = ,
    XorMask = , XorNegatedMask = .apply_logical_mask,
    IsLess = , IsLessEqual = , IsEqual = , IsNotEqual = ,
    IsGreater = , IsGreaterEqual = , IsMatch = , IsNotMatch = .apply_comparator,
    SquareRowIs = , SquareColumnIs = .apply_square_slice,
    ReduceToColumn = , ReduceToRow = .apply_reduction,
    Eltwise = .apply_eltwise,
    GroupBy = , GroupRowsBy = , GroupColumnsBy = .apply_groupby,
    CountBy = .apply_countby,
    stop(sprintf("eval: no handler for %s", node$op), call. = FALSE))
  dispatch(node, state, daf)
}

# --- lookups -------------------------------------------------------------

.apply_axis <- function(node, state, daf) {
  if (!format_has_axis(daf, node$axis_name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "vector", value = state$if_missing,
                  axis = node$axis_name))
    }
    stop(sprintf("no axis %s in daf %s",
                 sQuote(node$axis_name),
                 sQuote(S7::prop(daf, "name"))), call. = FALSE)
  }
  state$value <- format_axis_array(daf, node$axis_name)
  state$axis  <- node$axis_name
  state$kind  <- "axis"
  state
}

.apply_lookup_scalar <- function(node, state, daf) {
  if (is.null(node$name)) {
    # bare '.' -> list scalar names (used with '?' follow-up)
    state$kind <- "scalar_names_ready"
    return(state)
  }
  if (!format_has_scalar(daf, node$name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "scalar", value = state$if_missing))
    }
    stop(sprintf("no scalar %s in daf %s",
                 sQuote(node$name),
                 sQuote(S7::prop(daf, "name"))), call. = FALSE)
  }
  state$value <- format_get_scalar(daf, node$name)
  state$kind  <- "scalar"
  state
}

.apply_names <- function(node, state, daf) {
  if (identical(state$kind, "scalar_names_ready")) {
    return(list(kind = "names", value = format_scalars_set(daf)))
  }
  if (identical(state$kind, "init")) {
    # '@ ?' unreachable here; handled by .apply_axis + Names follow-up below
  }
  if (identical(state$kind, "axis")) {
    # handled in .apply_lookup_vector when name is NULL
  }
  # axes-set: bare `@ ?` -> axes_set
  list(kind = "names", value = format_axes_set(daf))
}

.apply_if_missing <- function(node, state, daf) {
  state$if_missing <- node$default
  state
}

# Implementation for remaining nodes is completed in later tasks;
# stub them for now so the dispatcher doesn't explode on unrelated tests:
.apply_lookup_vector     <- function(node, state, daf) { stop("not yet implemented: LookupVector") }
.apply_lookup_matrix     <- function(node, state, daf) { stop("not yet implemented: LookupMatrix") }
.apply_if_not            <- function(node, state, daf) { stop("not yet implemented: IfNot") }
.apply_as_axis           <- function(node, state, daf) { stop("not yet implemented: AsAxis") }
.apply_begin_mask        <- function(node, state, daf) { stop("not yet implemented: mask") }
.apply_end_mask          <- function(node, state, daf) { stop("not yet implemented: mask") }
.apply_logical_mask      <- function(node, state, daf) { stop("not yet implemented: logical mask") }
.apply_comparator        <- function(node, state, daf) { stop("not yet implemented: comparator") }
.apply_square_slice      <- function(node, state, daf) { stop("not yet implemented: square slice") }
.apply_reduction         <- function(node, state, daf) { stop("not yet implemented: reduction") }
.apply_eltwise           <- function(node, state, daf) { stop("not yet implemented: eltwise") }
.apply_groupby           <- function(node, state, daf) { stop("not yet implemented: groupby") }
.apply_countby           <- function(node, state, daf) { stop("not yet implemented: countby") }
```

- [ ] **Step 4: Wire `get_query()` public entry in `R/queries.R`**

```r
#' Evaluate a query string against a daf reader.
#' @param daf A `DafReader`.
#' @param query_string A query string.
#' @return A scalar, vector, matrix, names set, or NULL if missing.
#' @export
get_query <- function(daf, query_string) {
  ast <- parse_query(query_string)
  .eval_query(daf, ast)
}

#' Parse a query string into a canonical form.
#' @inheritParams get_query
#' @return The canonical query string.
#' @export
canonical_query <- function(query_string) {
  .canonicalise_ast(parse_query(query_string))
}
```

- [ ] **Step 5: Run test — expect pass**

- [ ] **Step 6: Commit**

```bash
git add R/query_eval.R R/queries.R tests/testthat/test-query-eval-lookups.R
git commit -m "feat(query-eval): scalar + axis lookups + Names + IfMissing fallback"
```

---

### Task Q8: Evaluator — vector + matrix lookups

**Files:**
- Modify: `R/query_eval.R`
- Modify: `tests/testthat/test-query-eval-lookups.R`

- [ ] **Step 1: Append failing tests**

```r
test_that("get_query returns a vector", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_vector(d, "cell", "age", c(1, 2, 3))
  expect_equal(get_query(d, "@ cell : age"), c(1, 2, 3))
})

test_that("get_query with '@ axis : ?' returns vector names", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", "c1")
  set_vector(d, "cell", "age",  1)
  set_vector(d, "cell", "name", "x")
  expect_setequal(get_query(d, "@ cell : ?"), c("age", "name"))
})

test_that("get_query returns a matrix", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2"))
  add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2, dimnames = list(c("c1","c2"), c("g1","g2"))))
  m <- get_query(d, "@ cell @ gene :: UMIs")
  expect_equal(dim(m), c(2L, 2L))
  expect_equal(m[1, 1], 1)
})

test_that("get_query errors on missing vector/matrix with no IfMissing", {
  d <- memory_daf(name = "t"); add_axis(d, "cell", "c1")
  expect_error(get_query(d, "@ cell : nope"), "no vector")
})
```

- [ ] **Step 2: Implement vector + matrix lookups**

```r
.apply_lookup_vector <- function(node, state, daf) {
  if (!identical(state$kind, "axis")) {
    stop(sprintf("':' requires an axis in scope (got %s)", state$kind),
         call. = FALSE)
  }
  axis <- state$axis
  if (is.null(node$name)) {
    return(list(kind = "names", value = format_vectors_set(daf, axis)))
  }
  if (!format_has_vector(daf, axis, node$name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "vector", value = rep(state$if_missing,
                                               format_axis_length(daf, axis)),
                  axis = axis))
    }
    stop(sprintf("no vector %s on axis %s",
                 sQuote(node$name), sQuote(axis)), call. = FALSE)
  }
  list(kind = "vector",
       value = format_get_vector(daf, axis, node$name),
       axis  = axis)
}

.apply_lookup_matrix <- function(node, state, daf) {
  if (!identical(state$kind, "two_axes")) {
    stop(sprintf("'::' requires two axes in scope (got %s)", state$kind),
         call. = FALSE)
  }
  rows <- state$rows_axis; cols <- state$cols_axis
  if (is.null(node$name)) {
    return(list(kind = "names", value = format_matrices_set(daf, rows, cols)))
  }
  if (!format_has_matrix(daf, rows, cols, node$name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "matrix",
                  value = matrix(state$if_missing,
                                 format_axis_length(daf, rows),
                                 format_axis_length(daf, cols)),
                  rows_axis = rows, cols_axis = cols))
    }
    stop(sprintf("no matrix %s [%s, %s]",
                 sQuote(node$name), sQuote(rows), sQuote(cols)),
         call. = FALSE)
  }
  list(kind = "matrix",
       value = format_get_matrix(daf, rows, cols, node$name),
       rows_axis = rows, cols_axis = cols)
}
```

Replace `.apply_axis` (written in Q7) with this version that also handles the second `@ axis` token producing a `two_axes` scope:

```r
.apply_axis <- function(node, state, daf) {
  if (!format_has_axis(daf, node$axis_name)) {
    if (!is.null(state$if_missing)) {
      return(list(kind = "vector", value = state$if_missing,
                  axis = node$axis_name))
    }
    stop(sprintf("no axis %s in daf %s",
                 sQuote(node$axis_name),
                 sQuote(S7::prop(daf, "name"))), call. = FALSE)
  }
  if (identical(state$kind, "axis")) {
    # second axis -> matrix dimension in scope
    state$kind <- "two_axes"
    state$rows_axis <- state$axis
    state$cols_axis <- node$axis_name
    state$value <- NULL
    state$axis  <- NULL
    return(state)
  }
  state$value <- format_axis_array(daf, node$axis_name)
  state$axis  <- node$axis_name
  state$kind  <- "axis"
  state
}
```

Also extend `.apply_lookup_matrix` to accept `two_axes` kind (it already dispatches on it above — already correct).

- [ ] **Step 3: Run test — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-lookups.R
git commit -m "feat(query-eval): vector + matrix lookups + Names on axis/matrix scope"
```

---

### Task Q9: Evaluator — bracketed mask chains (comparators)

**Files:**
- Modify: `R/query_eval.R`
- Create: `tests/testthat/test-query-eval-masks.R`

**Reference:** Julia applies masks to an axis-in-scope. The result is the axis filtered to entries where the mask expression evaluates true.

- [ ] **Step 1: Write failing tests**

```r
test_that("mask with '>' comparator filters axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
  set_vector(d, "donor", "age", c(10, 50, 70, 90))
  expect_equal(get_query(d, "@ donor [ age > 60 ]"), c("d3", "d4"))
})

test_that("mask with '=' comparator filters axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3"))
  set_vector(d, "donor", "sex", c("M", "F", "M"))
  expect_equal(get_query(d, "@ donor [ sex = M ]"), c("d1", "d3"))
})

test_that("negated mask filters axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "gene", c("g1", "g2", "g3"))
  set_vector(d, "gene", "is_lateral", c(TRUE, FALSE, TRUE))
  expect_equal(get_query(d, "@ gene [ ! is_lateral ]"), "g2")
})

test_that("mask with '~' regex match filters axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "gene", c("HOX1", "MYC", "HOX2"))
  set_vector(d, "gene", "symbol", c("HOX1", "MYC", "HOX2"))
  expect_equal(get_query(d, "@ gene [ symbol ~ ^HOX ]"), c("HOX1", "HOX2"))
})
```

- [ ] **Step 2: Implement mask evaluator**

```r
.apply_begin_mask <- function(node, state, daf) {
  if (!identical(state$kind, "axis")) {
    stop("'[' mask requires an axis in scope", call. = FALSE)
  }
  vec <- format_get_vector(daf, state$axis, node$property)
  mask <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
  if (identical(node$op, "BeginNegatedMask")) mask <- !mask
  state$pending_mask <- mask
  state$pending_property <- node$property
  state$pending_vec <- vec
  state$kind <- "mask"
  state
}

.apply_comparator <- function(node, state, daf) {
  if (!identical(state$kind, "mask")) {
    stop("comparator outside of mask", call. = FALSE)
  }
  vec <- state$pending_vec
  test <- switch(node$op,
    IsLess         = vec <  .coerce_cmp(node$value, vec),
    IsLessEqual    = vec <= .coerce_cmp(node$value, vec),
    IsEqual        = vec == .coerce_cmp(node$value, vec),
    IsNotEqual     = vec != .coerce_cmp(node$value, vec),
    IsGreater      = vec >  .coerce_cmp(node$value, vec),
    IsGreaterEqual = vec >= .coerce_cmp(node$value, vec),
    IsMatch        = grepl(node$pattern, as.character(vec), perl = TRUE),
    IsNotMatch     = !grepl(node$pattern, as.character(vec), perl = TRUE))
  state$pending_mask <- test
  state
}

.apply_end_mask <- function(node, state, daf) {
  axis <- state$axis
  entries <- format_axis_array(daf, axis)
  list(kind = "axis", axis = axis, value = entries[state$pending_mask])
}

.coerce_cmp <- function(value_string, ref_vec) {
  if (is.numeric(ref_vec)) as.numeric(value_string)
  else if (is.logical(ref_vec)) as.logical(value_string)
  else as.character(value_string)
}
```

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-masks.R
git commit -m "feat(query-eval): mask chains with comparators (<, =, >, ~, and negated)"
```

---

### Task Q10: Evaluator — logical mask combinators (&, |, ^)

**Files:**
- Modify: `R/query_eval.R`
- Modify: `tests/testthat/test-query-eval-masks.R`

- [ ] **Step 1: Append failing tests**

```r
test_that("mask AND combines two properties", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
  set_vector(d, "donor", "age", c(10, 70, 70, 10))
  set_vector(d, "donor", "sex", c("M", "M", "F", "F"))
  expect_equal(get_query(d, "@ donor [ age > 60 & sex = M ]"), "d2")
})

test_that("mask OR combines two properties", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
  set_vector(d, "donor", "age", c(10, 70, 70, 10))
  set_vector(d, "donor", "sex", c("M", "M", "F", "F"))
  expect_setequal(get_query(d, "@ donor [ age > 60 | sex = F ]"),
                   c("d2", "d3", "d4"))
})

test_that("mask XOR and negated variants work", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3", "d4"))
  set_vector(d, "donor", "a", c(TRUE, TRUE, FALSE, FALSE))
  set_vector(d, "donor", "b", c(TRUE, FALSE, TRUE, FALSE))
  expect_setequal(get_query(d, "@ donor [ a ^ b ]"), c("d2", "d3"))
  expect_setequal(get_query(d, "@ donor [ a & ! b ]"), "d2")
})
```

- [ ] **Step 2: Implement logical combinator**

```r
.apply_logical_mask <- function(node, state, daf) {
  if (!identical(state$kind, "mask")) {
    stop("logical mask combinator outside of mask", call. = FALSE)
  }
  vec <- format_get_vector(daf, state$axis, node$property)
  m   <- if (is.logical(vec)) vec else !is.na(vec) & vec != 0
  if (grepl("NegatedMask$", node$op)) m <- !m
  combined <- switch(substr(node$op, 1, 3),
    And = state$pending_mask & m,
    Or  = state$pending_mask | m,
    Xor = xor(state$pending_mask, m))
  state$pending_mask <- combined
  state$pending_vec  <- m   # seed for any trailing comparator on this property
  state
}
```

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-masks.R
git commit -m "feat(query-eval): logical mask combinators (AND, OR, XOR + negated)"
```

---

### Task Q11: Evaluator — square slicing (`@-`, `@|`)

**Files:**
- Modify: `R/query_eval.R`
- Create: `tests/testthat/test-query-eval-slicing.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("SquareRowIs slices a matrix to one row", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2,
                    dimnames = list(c("c1","c2"), c("g1","g2"))))
  v <- get_query(d, "@ cell @ gene :: UMIs @- c1")
  expect_equal(v, c(g1 = 1, g2 = 3))
})

test_that("SquareColumnIs slices a matrix to one column", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2,
                    dimnames = list(c("c1","c2"), c("g1","g2"))))
  v <- get_query(d, "@ cell @ gene :: UMIs @| g2")
  expect_equal(v, c(c1 = 3, c2 = 4))
})
```

- [ ] **Step 2: Implement slicing**

```r
.apply_square_slice <- function(node, state, daf) {
  if (!identical(state$kind, "matrix")) {
    stop("square slice requires a matrix in scope", call. = FALSE)
  }
  m <- state$value
  if (identical(node$op, "SquareRowIs")) {
    entries <- rownames(m)
    idx <- match(node$value, entries)
    if (is.na(idx)) stop(sprintf("no row %s", sQuote(node$value)), call. = FALSE)
    return(list(kind = "vector",
                axis  = state$cols_axis,
                value = setNames(as.numeric(m[idx, ]), colnames(m))))
  }
  # SquareColumnIs
  entries <- colnames(m)
  idx <- match(node$value, entries)
  if (is.na(idx)) stop(sprintf("no column %s", sQuote(node$value)), call. = FALSE)
  list(kind = "vector",
       axis  = state$rows_axis,
       value = setNames(as.numeric(m[, idx]), rownames(m)))
}
```

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-slicing.R
git commit -m "feat(query-eval): SquareRowIs / SquareColumnIs matrix slicing"
```

---

### Task Q12: Evaluator — eltwise + reductions

**Files:**
- Modify: `R/query_eval.R`
- Create: `tests/testthat/test-query-eval-eltwise.R`
- Create: `tests/testthat/test-query-eval-reductions.R`

- [ ] **Step 1: Write failing tests (eltwise)**

```r
# test-query-eval-eltwise.R
test_that("% Log applies logarithm to vector", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2", "c3"))
  set_vector(d, "cell", "UMIs", c(1, 10, 100))
  v <- get_query(d, "@ cell : UMIs % Log base: 10")
  expect_equal(v, c(0, 1, 2))
})

test_that("% Log respects eps", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", "c1"); set_vector(d, "cell", "UMIs", 0)
  expect_equal(get_query(d, "@ cell : UMIs % Log eps: 1"), 0)
})

test_that("% Abs + Sqrt + Exp + Round", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); set_vector(d, "cell", "x", c(-4, 9))
  expect_equal(get_query(d, "@ cell : x % Abs"),   c(4, 9))
  expect_equal(get_query(d, "@ cell : x % Abs % Sqrt"), c(2, 3))
})
```

- [ ] **Step 2: Write failing tests (reductions)**

```r
# test-query-eval-reductions.R
test_that(">| Sum reduces matrix columns to vector", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2,
                    dimnames = list(c("c1","c2"), c("g1","g2"))))
  v <- get_query(d, "@ cell @ gene :: UMIs >| Sum")
  # ReduceToColumn = sum per column (over rows) -> length = ncol
  expect_equal(v, c(g1 = 3, g2 = 7))
})

test_that(">- Mean reduces matrix rows to vector", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2")); add_axis(d, "gene", c("g1", "g2"))
  set_matrix(d, "cell", "gene", "UMIs",
             matrix(c(1, 2, 3, 4), 2, 2,
                    dimnames = list(c("c1","c2"), c("g1","g2"))))
  v <- get_query(d, "@ cell @ gene :: UMIs >- Mean")
  expect_equal(v, c(c1 = 2, c2 = 3))
})
```

- [ ] **Step 3: Implement**

```r
.apply_eltwise <- function(node, state, daf) {
  if (!state$kind %in% c("vector", "matrix")) {
    stop("'%' eltwise requires vector or matrix in scope", call. = FALSE)
  }
  fn <- get_eltwise(node$name)
  params <- .coerce_params(node$params)
  state$value <- do.call(fn, c(list(state$value), params))
  state
}

.apply_reduction <- function(node, state, daf) {
  if (!identical(state$kind, "matrix")) {
    stop(sprintf("%s requires a matrix in scope", node$op), call. = FALSE)
  }
  fn <- get_reduction(node$reduction)
  params <- .coerce_params(node$params)
  m <- state$value
  if (identical(node$op, "ReduceToColumn")) {
    # Sum per column: fn applied to each column
    vals <- apply(m, 2L, function(col) do.call(fn, c(list(col), params)))
    return(list(kind = "vector", axis = state$cols_axis,
                value = setNames(vals, colnames(m))))
  }
  vals <- apply(m, 1L, function(row) do.call(fn, c(list(row), params)))
  list(kind = "vector", axis = state$rows_axis,
       value = setNames(vals, rownames(m)))
}

.coerce_params <- function(params) {
  # try numeric coercion for each value; fall back to string
  lapply(params, function(v) {
    n <- suppressWarnings(as.numeric(v))
    if (!is.na(n)) n else v
  })
}
```

- [ ] **Step 4: Run tests — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-eltwise.R tests/testthat/test-query-eval-reductions.R
git commit -m "feat(query-eval): eltwise (%) + reductions (>|, >-) via operations registry"
```

---

### Task Q13: Evaluator — GroupBy / CountBy / GroupRowsBy / GroupColumnsBy

**Files:**
- Modify: `R/query_eval.R`
- Create: `tests/testthat/test-query-eval-groupby.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("/ GroupBy groups vector entries", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
  set_vector(d, "cell", "donor", c("d1", "d1", "d2", "d2"))
  set_vector(d, "cell", "UMIs",  c(1, 2, 10, 20))
  v <- get_query(d, "@ cell : UMIs / donor >| Sum")
  expect_equal(v, c(d1 = 3, d2 = 30))
})

test_that("* CountBy builds co-occurrence matrix", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
  set_vector(d, "cell", "sex",  c("M", "M", "F", "F"))
  set_vector(d, "cell", "type", c("A", "B", "A", "B"))
  m <- get_query(d, "@ cell : sex * type")
  expect_equal(dim(m), c(2L, 2L))
  expect_equal(sort(rownames(m)), c("F", "M"))
  expect_equal(sort(colnames(m)), c("A", "B"))
})
```

- [ ] **Step 2: Implement**

```r
.apply_groupby <- function(node, state, daf) {
  # GroupBy on a vector: produce a "grouped_vector" kind that subsequent
  # reductions collapse per group.
  if (!identical(state$kind, "vector")) {
    stop(sprintf("%s requires a vector in scope", node$op), call. = FALSE)
  }
  grp <- format_get_vector(daf, state$axis, node$property)
  state$pending_groups <- grp
  state$kind <- "grouped_vector"
  state
}

# Extend reductions to handle grouped_vector:
.apply_reduction_grouped <- function(node, state, daf) {
  fn <- get_reduction(node$reduction)
  params <- .coerce_params(node$params)
  splitted <- split(state$value, state$pending_groups)
  vals <- vapply(splitted, function(x) do.call(fn, c(list(x), params)),
                 numeric(1))
  list(kind = "vector", axis = NULL, value = vals)
}
```

Replace `.apply_reduction` (written in Q12) with a version that dispatches to the grouped variant when state has pending groups:

```r
.apply_reduction <- function(node, state, daf) {
  if (identical(state$kind, "grouped_vector")) {
    return(.apply_reduction_grouped(node, state, daf))
  }
  if (identical(state$kind, "grouped_matrix_rows")) {
    return(.apply_reduction_grouped_matrix(node, state, daf, by = "rows"))
  }
  if (identical(state$kind, "grouped_matrix_cols")) {
    return(.apply_reduction_grouped_matrix(node, state, daf, by = "cols"))
  }
  if (!identical(state$kind, "matrix")) {
    stop(sprintf("%s requires a matrix or grouped scope", node$op),
         call. = FALSE)
  }
  fn <- get_reduction(node$reduction)
  params <- .coerce_params(node$params)
  m <- state$value
  if (identical(node$op, "ReduceToColumn")) {
    vals <- apply(m, 2L, function(col) do.call(fn, c(list(col), params)))
    return(list(kind = "vector", axis = state$cols_axis,
                value = setNames(vals, colnames(m))))
  }
  vals <- apply(m, 1L, function(row) do.call(fn, c(list(row), params)))
  list(kind = "vector", axis = state$rows_axis,
       value = setNames(vals, rownames(m)))
}

.apply_reduction_grouped_matrix <- function(node, state, daf, by) {
  fn <- get_reduction(node$reduction)
  params <- .coerce_params(node$params)
  m <- state$value
  grp <- if (identical(by, "rows")) state$pending_row_groups
         else state$pending_col_groups
  if (identical(by, "rows")) {
    idx <- split(seq_len(nrow(m)), grp)
    out <- vapply(idx, function(i) {
      sub <- m[i, , drop = FALSE]
      if (identical(node$op, "ReduceToColumn")) {
        apply(sub, 2L, function(col) do.call(fn, c(list(col), params)))
      } else {
        apply(sub, 1L, function(row) do.call(fn, c(list(row), params)))
      }
    }, numeric(if (identical(node$op, "ReduceToColumn")) ncol(m) else 1L))
    return(list(kind = "matrix", value = out,
                rows_axis = NULL, cols_axis = state$cols_axis))
  }
  idx <- split(seq_len(ncol(m)), grp)
  out <- vapply(idx, function(j) {
    sub <- m[, j, drop = FALSE]
    apply(sub, 1L, function(row) do.call(fn, c(list(row), params)))
  }, numeric(nrow(m)))
  list(kind = "matrix", value = out,
       rows_axis = state$rows_axis, cols_axis = NULL)
}
```

Implement CountBy:

```r
.apply_countby <- function(node, state, daf) {
  if (!identical(state$kind, "vector")) {
    stop("* CountBy requires a vector in scope", call. = FALSE)
  }
  a <- state$value
  b <- format_get_vector(daf, state$axis, node$property)
  t <- table(a, b)
  m <- as.matrix(t)
  dimnames(m) <- list(rownames(t), colnames(t))
  list(kind = "matrix", value = m,
       rows_axis = NULL, cols_axis = NULL)
}
```

- [ ] **Step 3: Implement GroupRowsBy / GroupColumnsBy (matrix variants)**

```r
# Stub-raise: matrix-grouped operations are more involved.
.apply_groupby <- function(node, state, daf) {
  switch(node$op,
    GroupBy        = .apply_groupby_vector(node, state, daf),
    GroupRowsBy    = .apply_groupby_rows(node, state, daf),
    GroupColumnsBy = .apply_groupby_columns(node, state, daf),
    stop(sprintf("unknown grouping op: %s", node$op), call. = FALSE))
}

.apply_groupby_vector <- function(node, state, daf) {
  if (!identical(state$kind, "vector")) {
    stop("GroupBy requires a vector in scope", call. = FALSE)
  }
  grp <- format_get_vector(daf, state$axis, node$property)
  state$pending_groups <- grp
  state$kind <- "grouped_vector"
  state
}

.apply_groupby_rows <- function(node, state, daf) {
  if (!identical(state$kind, "matrix")) {
    stop("GroupRowsBy requires a matrix in scope", call. = FALSE)
  }
  grp <- format_get_vector(daf, state$rows_axis, node$property)
  state$pending_row_groups <- grp
  state$kind <- "grouped_matrix_rows"
  state
}

.apply_groupby_columns <- function(node, state, daf) {
  if (!identical(state$kind, "matrix")) {
    stop("GroupColumnsBy requires a matrix in scope", call. = FALSE)
  }
  grp <- format_get_vector(daf, state$cols_axis, node$property)
  state$pending_col_groups <- grp
  state$kind <- "grouped_matrix_cols"
  state
}
```

Extend `.apply_reduction` for the grouped_matrix cases as well.

- [ ] **Step 4: Run tests — expect pass**

- [ ] **Step 5: Commit**

```bash
git add R/query_eval.R tests/testthat/test-query-eval-groupby.R
git commit -m "feat(query-eval): GroupBy, CountBy, GroupRowsBy, GroupColumnsBy"
```

---

### Task Q14: Frame extraction — `get_frame()`

**Files:**
- Modify: `R/queries.R`
- Create: `tests/testthat/test-query-frames.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("get_frame with default columns returns all vectors for axis", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2"))
  set_vector(d, "donor", "age", c(30, 60))
  set_vector(d, "donor", "sex", c("M", "F"))
  frame <- get_frame(d, "@ donor")
  expect_s3_class(frame, "data.frame")
  expect_setequal(names(frame), c("age", "sex"))
  expect_equal(nrow(frame), 2L)
})

test_that("get_frame with filtered axis respects mask", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", c("d1", "d2", "d3"))
  set_vector(d, "donor", "age", c(10, 70, 90))
  frame <- get_frame(d, "@ donor [ age > 50 ]", columns = "age")
  expect_equal(frame$age, c(70, 90))
  expect_equal(rownames(frame), c("d2", "d3"))
})

test_that("get_frame with explicit columns list preserves order", {
  d <- memory_daf(name = "t")
  add_axis(d, "donor", "d1")
  set_vector(d, "donor", "x", 1); set_vector(d, "donor", "y", 2)
  frame <- get_frame(d, "@ donor", columns = c("y", "x"))
  expect_equal(names(frame), c("y", "x"))
})
```

- [ ] **Step 2: Implement**

```r
#' Extract a data.frame of vectors along one axis.
#' @param daf A DafReader.
#' @param axis_query A query string that evaluates to an axis entry vector.
#' @param columns Optional character vector of vector names. Default: all
#'   vectors for the axis.
#' @return A data.frame with one column per vector, rows named by axis entries.
#' @export
get_frame <- function(daf, axis_query, columns = NULL) {
  axis_ast <- parse_query(axis_query)
  state <- list(kind = "init")
  for (node in axis_ast) state <- .apply_node(node, state, daf)
  if (!identical(state$kind, "axis")) {
    stop("axis_query did not resolve to an axis", call. = FALSE)
  }
  entries <- state$value
  axis_name <- state$axis
  if (is.null(columns)) columns <- format_vectors_set(daf, axis_name)
  cols <- lapply(columns, function(nm) {
    v <- format_get_vector(daf, axis_name, nm)
    idx <- match(entries, format_axis_array(daf, axis_name))
    v[idx]
  })
  names(cols) <- columns
  as.data.frame(cols, row.names = entries,
                stringsAsFactors = FALSE, optional = TRUE)
}
```

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/queries.R tests/testthat/test-query-frames.R
git commit -m "feat(queries): get_frame() — axis-query + columns -> data.frame"
```

---

### Task Q15: Query cache tier integration

**Files:**
- Modify: `R/queries.R`
- Modify: `R/query_eval.R`
- Create: `tests/testthat/test-query-cache.R`

**Reference:** `cache_key_query(canon)` is already scaffolded in `R/cache.R:26`. The `query` tier supports `cache_lookup` / `cache_store`. Invalidation bump: each `set_vector` / `delete_vector` already bumps `vector_version_counter`. For query-cache invalidation, record the counter tuple at store time; on lookup, compare and evict on mismatch.

- [ ] **Step 1: Write failing tests**

```r
test_that("get_query hits cache on second call", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1","c2"))
  set_vector(d, "cell", "age", c(1,2))
  v1 <- get_query(d, "@ cell : age")
  v2 <- get_query(d, "@ cell : age")
  expect_identical(v1, v2)
  # Check the cache tier has the entry
  expect_true(any(grepl("^query:", ls(d@cache$query))))
})

test_that("cache is invalidated when a vector is overwritten", {
  d <- memory_daf(name = "t")
  add_axis(d, "cell", c("c1","c2"))
  set_vector(d, "cell", "age", c(1,2))
  v1 <- get_query(d, "@ cell : age")
  set_vector(d, "cell", "age", c(10,20), overwrite = TRUE)
  v2 <- get_query(d, "@ cell : age")
  expect_equal(v1, c(1,2))
  expect_equal(v2, c(10,20))
})
```

- [ ] **Step 2: Implement caching in `get_query()`**

```r
get_query <- function(daf, query_string) {
  ast  <- parse_query(query_string)
  canon <- .canonicalise_ast(ast)
  key   <- cache_key_query(canon)
  cached <- .try_query_cache_get(daf, key)
  if (!is.null(cached)) return(cached$value)
  value <- .eval_query(daf, ast)
  .query_cache_put(daf, key, value, .collect_versions(daf, ast))
  value
}

.try_query_cache_get <- function(daf, key) {
  entry <- cache_lookup(daf, key)
  if (is.null(entry)) return(NULL)
  # version gate
  if (!identical(entry$versions, .current_versions(daf, entry$touched))) {
    cache_evict(daf, key)
    return(NULL)
  }
  entry
}

.query_cache_put <- function(daf, key, value, touched) {
  cache_store(daf, key,
              list(value = value,
                   versions = .current_versions(daf, touched),
                   touched = touched))
}

.collect_versions <- function(daf, ast) {
  # walk AST, collect the (axis, vector, matrix) touched.
  # Returns list(axes=c(), vectors=list(axis->names), matrices=list())
  axes <- character(0); vecs <- list(); mats <- list()
  scope_axis <- NULL; scope_two <- NULL
  for (n in ast) {
    switch(n$op,
      Axis = { axes <- c(axes, n$axis_name); scope_axis <- n$axis_name },
      LookupVector = if (!is.null(n$name)) {
        vecs[[scope_axis]] <- c(vecs[[scope_axis]], n$name)
      },
      LookupMatrix = if (!is.null(n$name)) {
        key <- paste(scope_two, collapse = "|")
        mats[[key]] <- c(mats[[key]], n$name)
      },
      NULL)
  }
  list(axes = unique(axes), vectors = vecs, matrices = mats)
}

.current_versions <- function(daf, touched) {
  list(
    axes = vapply(touched$axes,
                  function(a) S7::prop(daf, "axis_version_counter")[[a]] %||% 0L,
                  integer(1)),
    vectors = lapply(names(touched$vectors), function(a) {
      vapply(touched$vectors[[a]],
             function(v) S7::prop(daf, "vector_version_counter")[[paste(a, v, sep="|")]] %||% 0L,
             integer(1))
    })
  )
}
```

(`%||%` is a local helper — declare in `R/utils.R` if not present.)

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/queries.R R/query_eval.R tests/testthat/test-query-cache.R
git commit -m "feat(queries): query-tier cache with version-counter invalidation"
```

---

### Task Q16: `q()` convenience + query introspection helpers

**Files:**
- Modify: `R/queries.R`

- [ ] **Step 1: Append failing tests in `tests/testthat/test-query-parse.R`**

```r
test_that("q() is an alias for parse_query", {
  expect_identical(q("@ cell"), parse_query("@ cell"))
})

test_that("is_axis_query returns TRUE for axis-only query", {
  expect_true(is_axis_query("@ cell"))
  expect_true(is_axis_query("@ cell [ age > 60 ]"))
  expect_false(is_axis_query("@ cell : UMIs"))
})

test_that("query_axis_name returns the last axis in scope", {
  expect_equal(query_axis_name("@ cell"), "cell")
  expect_equal(query_axis_name("@ cell @ gene :: UMIs"), NA_character_)
})

test_that("query_result_dimensions returns 0/1/2 for scalar/vector/matrix", {
  expect_equal(query_result_dimensions(". organism"), 0L)
  expect_equal(query_result_dimensions("@ cell : UMIs"), 1L)
  expect_equal(query_result_dimensions("@ cell @ gene :: UMIs"), 2L)
})

test_that("has_query returns FALSE for missing data", {
  d <- memory_daf(name = "t")
  expect_false(has_query(d, ". organism"))
  set_scalar(d, "organism", "human")
  expect_true(has_query(d, ". organism"))
})
```

- [ ] **Step 2: Implement in `R/queries.R`**

```r
#' @export
q <- function(query_string) parse_query(query_string)

#' @export
is_axis_query <- function(query_string) {
  ast <- parse_query(query_string)
  if (length(ast) == 0L) return(FALSE)
  last <- ast[[length(ast)]]
  last$op %in% c("Axis", "EndMask")
}

#' @export
query_axis_name <- function(query_string) {
  ast <- parse_query(query_string)
  axes <- vapply(ast, function(n) {
    if (identical(n$op, "Axis")) n$axis_name else NA_character_
  }, character(1))
  axes <- axes[!is.na(axes)]
  if (length(axes) == 1L) axes else NA_character_
}

#' @export
query_result_dimensions <- function(query_string) {
  ast <- parse_query(query_string)
  for (n in rev(ast)) {
    switch(n$op,
      LookupScalar   = return(0L),
      LookupVector   = return(1L),
      LookupMatrix   = return(2L),
      ReduceToColumn = , ReduceToRow = return(1L),
      CountBy        = return(2L),
      Axis           = return(1L),
      NULL)
  }
  NA_integer_
}

#' @export
has_query <- function(daf, query_string) {
  result <- tryCatch(get_query(daf, query_string), error = function(e) NULL)
  !is.null(result) && (!is.vector(result) || length(result) > 0L)
}
```

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/queries.R tests/testthat/test-query-parse.R
git commit -m "feat(queries): q() alias + is_axis_query + query_axis_name + query_result_dimensions + has_query"
```

---

### Task Q17: Julia fixture — regeneration script + JSON fixture

**Files:**
- Create: `dev/scripts/regen-julia-queries-fixture.jl`
- Create: `tests/testthat/fixtures/julia-queries/fixture.json`
- Create: `tests/testthat/fixtures/julia-queries/README.md`

**Strategy:** Generate one JSON file containing `[{query, canonical, kind, value}]` records, where `kind ∈ {scalar, vector, matrix, names}` and `value` is the Julia-evaluated result. The R test loads the fixture and compares byte-for-byte (canonical) + value-for-value (result).

- [ ] **Step 1: Write `dev/scripts/regen-julia-queries-fixture.jl`**

```julia
using DataAxesFormats
using DataAxesFormats.ExampleData
using DataAxesFormats.Queries
using JSON
using SparseArrays

daf = example_cells_daf()

QUERIES = [
    ". ?",
    ". organism",
    "@ ?",
    "@ cell",
    "@ cell : ?",
    "@ cell : donor",
    "@ donor : age",
    "@ donor [ age > 60 ]",
    "@ gene [ ! is_lateral ]",
    "@ donor [ age > 60 & sex = male ]",
    "@ cell @ gene :: UMIs",
    "@ cell @ gene :: UMIs >| Sum",
    "@ cell @ gene :: UMIs >- Sum",
    "@ cell : donor / donor >| Count",
    "@ cell : UMIs % Log eps: 1 base: 2",
]

records = []
for q_str in QUERIES
    result = get_query(daf, q_str)
    canon = canonical_query(parse_query(q_str))
    kind, value = serialize_result(result)
    push!(records, Dict(
        "query" => q_str,
        "canonical" => canon,
        "kind" => kind,
        "value" => value,
    ))
end

function serialize_result(x::AbstractString)
    return ("scalar", x)
end

function serialize_result(x::Number)
    return ("scalar", x)
end

function serialize_result(x::NamedArrays.NamedVector)
    return ("vector", Dict("names"  => names(x, 1),
                            "values" => collect(x)))
end

function serialize_result(x::NamedArrays.NamedMatrix)
    return ("matrix", Dict("rownames" => names(x, 1),
                            "colnames" => names(x, 2),
                            "values"   => vec(Array(x))))
end

function serialize_result(x::AbstractVector)
    return ("vector", Dict("names" => string.(1:length(x)),
                            "values" => collect(x)))
end

function serialize_result(x::AbstractSet)
    return ("names", sort(collect(x)))
end

function serialize_result(x::Base.KeySet)
    return ("names", sort(collect(x)))
end

open("tests/testthat/fixtures/julia-queries/fixture.json", "w") do f
    JSON.print(f, records, 2)
end

# Also dump example_cells_daf to FilesDaf for the R side to read
julia_fixture_path = "tests/testthat/fixtures/julia-queries/example-daf"
fdaf = FilesDaf(julia_fixture_path, "w")
DataAxesFormats.Copies.copy_all!(daf, fdaf)
close(fdaf)
```

- [ ] **Step 2: Run the script under conda**

```bash
cd ~/src/dafr-native
conda run -n dafr-mcview julia --project=~/src/DataAxesFormats.jl dev/scripts/regen-julia-queries-fixture.jl
```

Expected: creates `tests/testthat/fixtures/julia-queries/fixture.json` and the example daf directory.

- [ ] **Step 3: Inspect fixture.json — smoke check**

```
Rscript -e 'str(jsonlite::fromJSON("tests/testthat/fixtures/julia-queries/fixture.json", simplifyVector = FALSE), max.level = 2)'
```

Expected: list of ~15 records with `query`, `canonical`, `kind`, `value` fields.

- [ ] **Step 4: Write `tests/testthat/fixtures/julia-queries/README.md`**

```markdown
# Julia queries fixture

Regenerate with:

```
conda run -n dafr-mcview julia --project=~/src/DataAxesFormats.jl \
  dev/scripts/regen-julia-queries-fixture.jl
```

`fixture.json` — query strings + canonical form + evaluated result.
`example-daf/` — serialised MemoryDaf at FilesDaf format (== Julia's example_cells_daf()).
```

- [ ] **Step 5: Commit**

```bash
git add dev/scripts/regen-julia-queries-fixture.jl \
        tests/testthat/fixtures/julia-queries/
git commit -m "fixture(queries): Julia-generated query fixture + regen script"
```

(Note: `dev/scripts/*` lives in the dev repo; commit from there.)

---

### Task Q18: Julia-compat end-to-end test

**Files:**
- Create: `tests/testthat/test-query-julia-compat.R`

- [ ] **Step 1: Write the test file**

```r
test_that("every fixture query produces identical canonical-string in R", {
  fixture <- jsonlite::fromJSON(
    test_path("fixtures", "julia-queries", "fixture.json"),
    simplifyVector = FALSE)
  for (rec in fixture) {
    canon <- canonical_query(rec$query)
    expect_identical(canon, rec$canonical, info = rec$query)
  }
})

test_that("every fixture query evaluates to identical result in R", {
  skip_if_not(file.exists(test_path("fixtures", "julia-queries",
                                     "example-daf", "daf.json")),
              "Julia example daf fixture missing")
  fixture <- jsonlite::fromJSON(
    test_path("fixtures", "julia-queries", "fixture.json"),
    simplifyVector = FALSE)
  daf <- files_daf(test_path("fixtures", "julia-queries", "example-daf"),
                    mode = "r")
  for (rec in fixture) {
    r_val <- get_query(daf, rec$query)
    expected <- .julia_value_to_r(rec$kind, rec$value)
    expect_equal(r_val, expected, info = rec$query)
  }
})

.julia_value_to_r <- function(kind, value) {
  switch(kind,
    scalar = value,
    vector = setNames(unlist(value$values), unlist(value$names)),
    matrix = {
      m <- matrix(unlist(value$values), nrow = length(value$rownames))
      dimnames(m) <- list(unlist(value$rownames), unlist(value$colnames))
      m
    },
    names = unlist(value),
    stop("unknown fixture kind: ", kind))
}
```

- [ ] **Step 2: Run**

```
Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-query-julia-compat.R")'
```

Expected: canonical test passes; eval test passes for all fixture queries.

- [ ] **Step 3: Commit**

```bash
git add tests/testthat/test-query-julia-compat.R
git commit -m "test(queries): end-to-end Julia fixture round-trip"
```

---

## Phase V — Views

### Task V1: `ViewDaf` S7 class + slot schema

**Files:**
- Create: `R/view_daf.R`
- Create: `tests/testthat/test-view-daf.R`

- [ ] **Step 1: Write failing test**

```r
test_that("ViewDaf is a subclass of DafReadOnly", {
  expect_true(S7::class_inherits(ViewDaf, DafReadOnly))
})

test_that("ViewDaf can be constructed with just a base daf", {
  d <- memory_daf(name = "base")
  v <- viewer(d, name = "v")
  expect_s7_class(v, ViewDaf)
  expect_equal(S7::prop(v, "name"), "v")
})

test_that("ViewDaf with no overrides mirrors base daf", {
  d <- memory_daf(name = "base")
  set_scalar(d, "organism", "human")
  add_axis(d, "cell", c("c1", "c2"))
  set_vector(d, "cell", "age", c(10, 20))

  v <- viewer(d, name = "v", data = list(VIEW_ALL_DATA))
  expect_equal(get_scalar(v, "organism"), "human")
  expect_equal(axis_vector(v, "cell"), c("c1", "c2"))
  expect_equal(get_vector(v, "cell", "age"), c(10, 20))
})
```

- [ ] **Step 2: Implement `R/view_daf.R`**

```r
#' @include classes.R queries.R format_api.R
NULL

#' Sentinel constants for wildcard view specifications.
#' @export
ALL_AXES     <- "*"
#' @export
ALL_SCALARS  <- "*"
#' @export
ALL_VECTORS  <- c("*", "*")
#' @export
ALL_MATRICES <- c("*", "*", "*")
#' @export
VIEW_ALL_AXES     <- list(ALL_AXES     = "=")
#' @export
VIEW_ALL_SCALARS  <- list(ALL_SCALARS  = "=")
#' @export
VIEW_ALL_VECTORS  <- list(ALL_VECTORS  = "=")
#' @export
VIEW_ALL_MATRICES <- list(ALL_MATRICES = "=")
#' @export
VIEW_ALL_DATA     <- list(VIEW_ALL_SCALARS, VIEW_ALL_VECTORS, VIEW_ALL_MATRICES)

#' A read-only view over a base daf with renamed / filtered axes and data.
#' @export
ViewDaf <- S7::new_class(
  name = "ViewDaf",
  package = "dafr",
  parent = DafReadOnly,
  properties = list(
    base          = DafReader,
    view_axes     = S7::class_list,    # name-in-view -> query-string or NULL or "="
    view_scalars  = S7::class_list,
    view_vectors  = S7::class_list,    # (axis, name) pair -> query
    view_matrices = S7::class_list     # (rows, cols, name) triple -> query
  )
)

#' Construct a ViewDaf over a base daf.
#' @param daf Base `DafReader`.
#' @param name Name for the view (defaults to `"<daf-name>.view"`).
#' @param axes Optional list of axis overrides; see `ALL_AXES`.
#' @param data Optional list of data overrides; see `VIEW_ALL_DATA`.
#' @return A `ViewDaf`.
#' @export
viewer <- function(daf, name = NULL, axes = NULL, data = NULL) {
  if (is.null(name)) name <- paste0(S7::prop(daf, "name"), ".view")
  ViewDaf(
    name                    = name,
    internal                = new_internal_env(),
    cache                   = new_cache_env(),
    axis_version_counter    = new_counter_env(),
    vector_version_counter  = new_counter_env(),
    matrix_version_counter  = new_counter_env(),
    base                    = daf,
    view_axes               = .resolve_view_axes(daf, axes),
    view_scalars            = .resolve_view_scalars(daf, data),
    view_vectors            = .resolve_view_vectors(daf, data),
    view_matrices           = .resolve_view_matrices(daf, data)
  )
}

.resolve_view_axes <- function(daf, axes) {
  # V1 path: no-override => identity mapping (one entry per base axis,
  # value "=" meaning "expose as-is"). Overrides land in Task V3.
  if (is.null(axes)) {
    return(setNames(rep(list("="), length(format_axes_set(daf))),
                     format_axes_set(daf)))
  }
  stop("view-axes override not yet implemented (see Task V3)",
       call. = FALSE)
}

.resolve_view_scalars  <- function(daf, data) setNames(as.list(format_scalars_set(daf)),
                                                        format_scalars_set(daf))
.resolve_view_vectors  <- function(daf, data) {
  out <- list()
  for (a in format_axes_set(daf)) {
    for (v in format_vectors_set(daf, a)) {
      out[[paste(a, v, sep = "|")]] <- list(axis = a, name = v, query = "=")
    }
  }
  out
}
.resolve_view_matrices <- function(daf, data) {
  out <- list()
  for (r in format_axes_set(daf)) {
    for (c in format_axes_set(daf)) {
      for (m in format_matrices_set(daf, r, c)) {
        out[[paste(r, c, m, sep = "|")]] <- list(rows = r, cols = c, name = m, query = "=")
      }
    }
  }
  out
}
```

- [ ] **Step 3: Run test — expect pass for slot + no-override cases**

- [ ] **Step 4: Commit**

```bash
git add R/view_daf.R tests/testthat/test-view-daf.R
git commit -m "feat(view_daf): ViewDaf class + viewer() constructor (no-override path)"
```

---

### Task V2: Format-api methods on ViewDaf

**Files:**
- Modify: `R/view_daf.R`
- Modify: `tests/testthat/test-view-daf.R`

- [ ] **Step 1: Append failing tests**

```r
test_that("ViewDaf format_has_scalar / format_get_scalar delegate to base", {
  d <- memory_daf(name = "base"); set_scalar(d, "organism", "human")
  v <- viewer(d, name = "v")
  expect_true(format_has_scalar(v, "organism"))
  expect_equal(format_get_scalar(v, "organism"), "human")
  expect_false(format_has_scalar(v, "nope"))
})

test_that("ViewDaf format_has_axis / format_axis_array delegate to base", {
  d <- memory_daf(name = "base"); add_axis(d, "cell", c("c1","c2"))
  v <- viewer(d, name = "v")
  expect_true(format_has_axis(v, "cell"))
  expect_equal(format_axis_array(v, "cell"), c("c1","c2"))
})

test_that("ViewDaf format_get_vector delegates via query", {
  d <- memory_daf(name = "base")
  add_axis(d, "cell", c("c1","c2"))
  set_vector(d, "cell", "age", c(10, 20))
  v <- viewer(d, name = "v")
  expect_equal(format_get_vector(v, "cell", "age"), c(10, 20))
})
```

- [ ] **Step 2: Register S7 methods on ViewDaf for every format_* generic**

Append to `R/view_daf.R`:

```r
.view_query_for_scalar <- function(view, name) {
  override <- view@view_scalars[[name]]
  if (is.null(override) || identical(override, "=") || identical(override, name)) {
    return(paste0(". ", name))
  }
  override
}

.view_query_for_axis <- function(view, axis) {
  override <- view@view_axes[[axis]]
  if (is.null(override) || identical(override, "=") || identical(override, axis)) {
    return(paste0("@ ", axis))
  }
  override
}

.view_query_for_vector <- function(view, axis, name) {
  key <- paste(axis, name, sep = "|")
  override <- view@view_vectors[[key]]
  if (is.null(override) || identical(override$query, "=")) {
    return(sprintf("@ %s : %s", axis, name))
  }
  override$query
}

S7::method(format_has_scalar, ViewDaf) <- function(daf, name) {
  q_str <- tryCatch(.view_query_for_scalar(daf, name), error = function(e) NULL)
  if (is.null(q_str)) return(FALSE)
  has_query(daf@base, q_str)
}

S7::method(format_get_scalar, ViewDaf) <- function(daf, name) {
  get_query(daf@base, .view_query_for_scalar(daf, name))
}

S7::method(format_scalars_set, ViewDaf) <- function(daf) {
  sort(names(daf@view_scalars), method = "radix")
}

S7::method(format_has_axis, ViewDaf) <- function(daf, axis) {
  !is.null(daf@view_axes[[axis]])
}

S7::method(format_axes_set, ViewDaf) <- function(daf) {
  sort(names(daf@view_axes), method = "radix")
}

S7::method(format_axis_length, ViewDaf) <- function(daf, axis) {
  length(format_axis_array(daf, axis))
}

S7::method(format_axis_array, ViewDaf) <- function(daf, axis) {
  get_query(daf@base, .view_query_for_axis(daf, axis))
}

S7::method(format_has_vector, ViewDaf) <- function(daf, axis, name) {
  key <- paste(axis, name, sep = "|")
  !is.null(daf@view_vectors[[key]])
}

S7::method(format_vectors_set, ViewDaf) <- function(daf, axis) {
  keys <- names(daf@view_vectors)
  prefix <- paste0(axis, "|")
  sub(prefix, "", keys[startsWith(keys, prefix)], fixed = TRUE)
}

S7::method(format_get_vector, ViewDaf) <- function(daf, axis, name) {
  get_query(daf@base, .view_query_for_vector(daf, axis, name))
}

S7::method(format_has_matrix, ViewDaf) <- function(daf, rows, cols, name) {
  key <- paste(rows, cols, name, sep = "|")
  !is.null(daf@view_matrices[[key]])
}

S7::method(format_matrices_set, ViewDaf) <- function(daf, rows, cols) {
  keys <- names(daf@view_matrices)
  prefix <- paste(rows, cols, "", sep = "|")
  sub(prefix, "", keys[startsWith(keys, prefix)], fixed = TRUE)
}

S7::method(format_get_matrix, ViewDaf) <- function(daf, rows, cols, name) {
  key <- paste(rows, cols, name, sep = "|")
  override <- daf@view_matrices[[key]]
  if (is.null(override) || identical(override$query, "=")) {
    get_query(daf@base, sprintf("@ %s @ %s :: %s", rows, cols, name))
  } else {
    get_query(daf@base, override$query)
  }
}

# delete / set generics: ViewDaf inherits from DafReadOnly. S7 falls through
# to the DafReadOnly guards (error "store is read-only"), so no explicit
# method is needed here. Confirm by running a write against a ViewDaf in
# tests.
```

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/view_daf.R tests/testthat/test-view-daf.R
git commit -m "feat(view_daf): format_* methods dispatching via get_query on base daf"
```

---

### Task V3: Axis / scalar / vector / matrix overrides

**Files:**
- Modify: `R/view_daf.R`
- Modify: `tests/testthat/test-view-daf.R`

- [ ] **Step 1: Append failing tests**

```r
test_that("viewer with axis override renames axis", {
  d <- memory_daf(name = "base"); add_axis(d, "cell", c("c1","c2"))
  v <- viewer(d, axes = list(obs = "cell"))
  expect_equal(axes_set(v), "obs")
  expect_equal(axis_vector(v, "obs"), c("c1","c2"))
})

test_that("viewer with filtered axis query", {
  d <- memory_daf(name = "base")
  add_axis(d, "cell", c("c1","c2","c3"))
  set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE))
  v <- viewer(d, axes = list(cell = "@ cell [ keep ]"))
  expect_equal(axis_vector(v, "cell"), c("c1","c3"))
})

test_that("viewer with vector override", {
  d <- memory_daf(name = "base")
  add_axis(d, "cell", c("c1","c2"))
  set_vector(d, "cell", "UMIs", c(1, 10))
  v <- viewer(d, data = list(
    list(c("cell", "log_UMIs"), "@ cell : UMIs % Log eps: 1")))
  expect_equal(get_vector(v, "cell", "log_UMIs"), c(log(2), log(11)))
})

test_that("viewer with scalar override", {
  d <- memory_daf(name = "base")
  set_scalar(d, "organism", "human")
  v <- viewer(d, data = list(list("species", ". organism")))
  expect_equal(get_scalar(v, "species"), "human")
})
```

- [ ] **Step 2: Extend `.resolve_view_*` helpers**

```r
.resolve_view_axes <- function(daf, axes) {
  if (is.null(axes)) {
    return(setNames(rep(list("="), length(format_axes_set(daf))),
                     format_axes_set(daf)))
  }
  out <- list()
  for (item in axes) {
    if (is.list(item) && length(item) == 1L) {
      name <- names(item); query <- item[[1]]
    } else {
      stop("axes item must be list(name = query)", call. = FALSE)
    }
    if (identical(name, ALL_AXES)) {
      # expand wildcard
      for (a in format_axes_set(daf)) out[[a]] <- query
    } else {
      out[[name]] <- query
    }
  }
  out
}

.resolve_view_scalars <- function(daf, data) {
  out <- setNames(as.list(format_scalars_set(daf)), format_scalars_set(daf))
  if (is.null(data)) return(out)
  for (item in data) {
    if (is.character(item[[1]]) && length(item[[1]]) == 1L) {
      out[[item[[1]]]] <- item[[2]]
    }
  }
  out
}

.resolve_view_vectors <- function(daf, data) {
  out <- list()
  for (a in format_axes_set(daf)) {
    for (v in format_vectors_set(daf, a)) {
      out[[paste(a, v, sep = "|")]] <- list(axis = a, name = v, query = "=")
    }
  }
  if (is.null(data)) return(out)
  for (item in data) {
    if (is.character(item[[1]]) && length(item[[1]]) == 2L) {
      key <- paste(item[[1]], collapse = "|")
      out[[key]] <- list(axis = item[[1]][[1]],
                         name = item[[1]][[2]],
                         query = item[[2]])
    }
  }
  out
}
```

Matrix override resolver:

```r
.resolve_view_matrices <- function(daf, data) {
  out <- list()
  for (r in format_axes_set(daf)) {
    for (c in format_axes_set(daf)) {
      for (m in format_matrices_set(daf, r, c)) {
        out[[paste(r, c, m, sep = "|")]] <- list(rows = r, cols = c,
                                                   name = m, query = "=")
      }
    }
  }
  if (is.null(data)) return(out)
  for (item in data) {
    if (is.character(item[[1]]) && length(item[[1]]) == 3L) {
      key <- paste(item[[1]], collapse = "|")
      out[[key]] <- list(rows = item[[1]][[1]],
                         cols = item[[1]][[2]],
                         name = item[[1]][[3]],
                         query = item[[2]])
    }
  }
  out
}
```

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/view_daf.R tests/testthat/test-view-daf.R
git commit -m "feat(view_daf): axis / scalar / vector overrides via query strings"
```

---

### Task V4: Wildcard sentinels (ALL_AXES / ALL_VECTORS / ALL_MATRICES)

**Files:**
- Modify: `R/view_daf.R`
- Create: `tests/testthat/test-view-wildcards.R`

- [ ] **Step 1: Write failing tests**

```r
test_that("VIEW_ALL_DATA exposes every scalar/vector/matrix in base", {
  d <- memory_daf(name = "base")
  set_scalar(d, "organism", "human")
  add_axis(d, "cell", "c1")
  set_vector(d, "cell", "age", 10)
  v <- viewer(d, data = list(VIEW_ALL_DATA))
  expect_setequal(scalars_set(v), "organism")
  expect_setequal(vectors_set(v, "cell"), "age")
})

test_that("ALL_AXES = NULL hides all axes", {
  d <- memory_daf(name = "base"); add_axis(d, "cell", "c1")
  v <- viewer(d, axes = list(list(ALL_AXES, NULL)))
  expect_length(axes_set(v), 0L)
})

test_that("wildcard + specific override: last wins", {
  d <- memory_daf(name = "base")
  add_axis(d, "cell", c("c1","c2","c3"))
  set_vector(d, "cell", "keep", c(TRUE, FALSE, TRUE))
  v <- viewer(d, axes = list(list(ALL_AXES, "="),
                               list("cell", "@ cell [ keep ]")))
  expect_equal(axis_vector(v, "cell"), c("c1", "c3"))
})
```

- [ ] **Step 2: Extend resolvers to handle `ALL_*` wildcards + NULL hide + last-wins ordering**

Rewrite `.resolve_view_axes` (from Task V3) to walk the spec in order and let later items override earlier ones:

```r
.resolve_view_axes <- function(daf, axes) {
  all_axes <- format_axes_set(daf)
  if (is.null(axes)) {
    return(setNames(rep(list("="), length(all_axes)), all_axes))
  }
  out <- list()
  for (item in axes) {
    if (is.list(item) && !is.null(names(item)) && length(item) == 1L) {
      name  <- names(item); query <- item[[1]]
    } else if (is.list(item) && length(item) == 2L && is.character(item[[1]])) {
      name  <- item[[1]]; query <- item[[2]]
    } else {
      stop("axes item must be list(name = query) or list(name, query)",
           call. = FALSE)
    }
    if (identical(name, ALL_AXES)) {
      if (is.null(query)) {
        out <- list()   # NULL hides everything
      } else {
        for (a in all_axes) out[[a]] <- query
      }
    } else {
      if (is.null(query)) {
        out[[name]] <- NULL
      } else {
        out[[name]] <- query
      }
    }
  }
  out
}
```

Rewrite `.resolve_view_scalars`:

```r
.resolve_view_scalars <- function(daf, data) {
  all_scalars <- format_scalars_set(daf)
  out <- list()
  if (is.null(data)) {
    return(setNames(as.list(all_scalars), all_scalars))
  }
  for (item in .flatten_view_data(data)) {
    if (is.character(item[[1]]) && length(item[[1]]) == 1L) {
      name  <- item[[1]]; query <- item[[2]]
      if (identical(name, ALL_SCALARS)) {
        if (is.null(query)) {
          out <- list()
        } else if (identical(query, "=")) {
          for (s in all_scalars) out[[s]] <- s
        } else {
          for (s in all_scalars) out[[s]] <- query
        }
      } else {
        if (is.null(query)) out[[name]] <- NULL
        else                out[[name]] <- query
      }
    }
  }
  out
}

.flatten_view_data <- function(data) {
  # `data` may be a nested list (VIEW_ALL_DATA is itself a list of specs).
  flat <- list()
  for (item in data) {
    if (is.list(item) && length(item) > 0L && is.list(item[[1]])) {
      flat <- c(flat, .flatten_view_data(item))
    } else {
      flat <- c(flat, list(item))
    }
  }
  flat
}
```

Analogous rewrites for `.resolve_view_vectors` (key is `c(axis, name)`, wildcard `ALL_VECTORS = c("*","*")`) and `.resolve_view_matrices` (key is `c(rows, cols, name)`, wildcard `ALL_MATRICES = c("*","*","*")`):

```r
.resolve_view_vectors <- function(daf, data) {
  out <- list()
  for (a in format_axes_set(daf)) {
    for (v in format_vectors_set(daf, a)) {
      out[[paste(a, v, sep = "|")]] <- list(axis = a, name = v, query = "=")
    }
  }
  if (is.null(data)) return(out)
  for (item in .flatten_view_data(data)) {
    if (is.character(item[[1]]) && length(item[[1]]) == 2L) {
      a <- item[[1]][[1]]; v <- item[[1]][[2]]; q <- item[[2]]
      if (identical(a, "*") && identical(v, "*")) {
        if (is.null(q)) {
          out <- list()
        } else if (identical(q, "=")) {
          # already identity by default; no-op
        } else {
          # Wildcard-with-query rewrites all vectors; rare in practice.
          for (k in names(out)) out[[k]]$query <- q
        }
      } else {
        key <- paste(a, v, sep = "|")
        if (is.null(q)) out[[key]] <- NULL
        else            out[[key]] <- list(axis = a, name = v, query = q)
      }
    }
  }
  out
}

.resolve_view_matrices <- function(daf, data) {
  out <- list()
  for (r in format_axes_set(daf)) {
    for (c in format_axes_set(daf)) {
      for (m in format_matrices_set(daf, r, c)) {
        out[[paste(r, c, m, sep = "|")]] <- list(rows = r, cols = c,
                                                   name = m, query = "=")
      }
    }
  }
  if (is.null(data)) return(out)
  for (item in .flatten_view_data(data)) {
    if (is.character(item[[1]]) && length(item[[1]]) == 3L) {
      rr <- item[[1]][[1]]; cc <- item[[1]][[2]]; nn <- item[[1]][[3]]
      q  <- item[[2]]
      if (rr == "*" && cc == "*" && nn == "*") {
        if (is.null(q)) out <- list()
        # identity "=" is default; no-op for the broad case
      } else {
        key <- paste(rr, cc, nn, sep = "|")
        if (is.null(q)) out[[key]] <- NULL
        else            out[[key]] <- list(rows = rr, cols = cc,
                                            name = nn, query = q)
      }
    }
  }
  out
}
```

- [ ] **Step 3: Run tests — expect pass**

- [ ] **Step 4: Commit**

```bash
git add R/view_daf.R tests/testthat/test-view-wildcards.R
git commit -m "feat(view_daf): ALL_* wildcards + NULL hide + last-wins resolution"
```

---

### Task V5: Julia-compat view tests

**Files:**
- Modify: `dev/scripts/regen-julia-queries-fixture.jl` (append view specs)
- Create: `tests/testthat/test-view-julia-compat.R`

- [ ] **Step 1: Extend the Julia regen script to emit a `views.json`**

```julia
# Append after query-fixture dump:
views_records = [
    Dict("name" => "filtered_donors",
         "axes" => [("donor", "@ donor [ age > 60 ]")]),
    Dict("name" => "renamed_cells",
         "axes" => [("obs", "@ cell")]),
]

open("tests/testthat/fixtures/julia-queries/views.json", "w") do f
    JSON.print(f, views_records, 2)
end
```

- [ ] **Step 2: Regenerate fixture**

```bash
conda run -n dafr-mcview julia --project=~/src/DataAxesFormats.jl \
  dev/scripts/regen-julia-queries-fixture.jl
```

- [ ] **Step 3: Write `tests/testthat/test-view-julia-compat.R`**

```r
test_that("Julia-specified view axis queries produce identical results in R", {
  skip_if_not(file.exists(test_path("fixtures", "julia-queries",
                                     "example-daf", "daf.json")),
              "Julia example daf fixture missing")
  daf <- files_daf(test_path("fixtures", "julia-queries", "example-daf"), "r")
  specs <- jsonlite::fromJSON(
    test_path("fixtures", "julia-queries", "views.json"),
    simplifyVector = FALSE)
  for (spec in specs) {
    axes_override <- lapply(spec$axes, function(pair)
                             setNames(list(pair[[2]]), pair[[1]]))
    v <- viewer(daf, name = spec$name, axes = axes_override)
    # Smoke: all axes expected in the view resolve
    for (axis in names(axes_override[[1]])) {
      expect_true(format_has_axis(v, axis), info = axis)
      expect_gt(format_axis_length(v, axis), 0L)
    }
  }
})
```

- [ ] **Step 4: Run test — expect pass**

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test-view-julia-compat.R
git commit -m "test(view_daf): Julia-generated view specs round-trip to R"
```

(Also commit the regen-script changes in the dev repo.)

---

## Phase Z — Polish + exit gate

### Task Z1: NAMESPACE generation + NEWS entry

**Files:**
- Modify: `NAMESPACE` (auto via roxygen)
- Modify: `NEWS.md`

- [ ] **Step 1: Regenerate documentation**

```
Rscript -e 'devtools::document()'
```

Expected: NAMESPACE updated with all Slice 3 exports (parse_query, get_query, has_query, is_axis_query, query_axis_name, query_result_dimensions, get_frame, q, canonical_query, ViewDaf, viewer, ALL_*, VIEW_ALL_*, register_reduction, register_eltwise, get_reduction, get_eltwise, registered_reductions, registered_eltwise).

- [ ] **Step 2: Append to `NEWS.md`**

```markdown
# dafr 0.3.0 (Slice 3)

## New features

- **Query DSL** (`parse_query()`, `get_query()`, `q()`): text-based query
  language over any `DafReader`. Supports axis lookups, vector/matrix
  lookups, bracketed masks with comparators, logical combinators, square
  slicing, `GroupBy` / `CountBy`, reductions (`>-` / `>|`), and eltwise
  operations (`%`). See `?parse_query` for the grammar.
- **Frames** (`get_frame()`): extract a `data.frame` of vectors along an
  axis query.
- **Views** (`ViewDaf`, `viewer()`): lazy read-only wrapper that exposes
  a renamed / filtered view of a base daf via query rewrites. No copies.
- **Operations registry** (`register_reduction()`, `register_eltwise()`):
  pluggable op table. Defaults shipped: `Sum`, `Mean`, `Max`, `Min`,
  `Count`, `Log`, `Abs`, `Exp`, `Sqrt`, `Round`.
- **Query cache tier** now populated (previously reserved). Entries keyed
  by canonical query string; invalidated on version-counter bumps.

## Compatibility

- Byte-compatible canonical query strings with Julia `DataAxesFormats`.
- Bidirectional view-spec round-trip tested against a Julia fixture.
```

- [ ] **Step 3: Commit**

```bash
git add NAMESPACE NEWS.md man/
git commit -m "docs(slice-3): regenerate NAMESPACE + man/ + NEWS entry"
```

---

### Task Z2: Deferred — roxygen `@examples` for all new exports

**Status:** DEFERRED to late-Slice-3 or early-Slice-4 per user instruction (2026-04-20). Do NOT block Slice 3 exit on this.

**When unblocked:** Add `@examples` blocks to every exported function in `R/queries.R`, `R/view_daf.R`, `R/operations.R`. Follow the pattern in `R/readers.R` (no existing examples; establish convention). Each example should:

1. Create a tiny `memory_daf(name = "example")` with a minimum dataset.
2. Show the function call.
3. Show the returned value (via `#> ` comments after the expected output).

Regenerate docs (`devtools::document()`) and run `devtools::check()` to confirm examples execute cleanly.

---

### Task Z3: Deferred — `alutil::sad()` style + document pass

**Status:** DEFERRED to late-Slice-3 cleanup per user instruction (2026-04-20).

**When unblocked:**

**⚠ Warning to run ahead of the commit:** `alutil::sad()` runs `styler::style_pkg(indent_by = 4); devtools::document()`. The existing dafr codebase is 2-space indent. Running this will REWRITE every R file under `R/` to 4-space indent — a huge mechanical diff that must land in a single dedicated commit (not mixed with substantive Slice 3 work).

Before running:

```
git diff --stat main..HEAD   # show what's already in the slice
git status                    # should be clean
```

Then:

```
Rscript -e 'alutil::sad()'
```

Review the diff:

```
git diff --stat
git diff -- R/memory_daf.R | head -100   # spot-check one file
```

Commit as:

```
git add R/ NAMESPACE man/
git commit -m "style: apply alutil::sad() (styler 4-space indent + roxygen regen)"
```

Then re-run the full test suite and `devtools::check(error_on = 'note')` — styler should never introduce semantic changes, but confirm.

---

### Task Z4: Slice 3 exit note

**Files:**
- Create: `dev/notes/slice-3-exit.md`

- [ ] **Step 1: Run the full verification suite**

```
Rscript -e '
  pkgbuild::compile_dll(debug = FALSE)
  devtools::load_all(".")
  out <- testthat::test_dir("tests/testthat")
  cat("\nSUMMARY:\n"); print(out)
'
```

Expected: all Slice 2 tests green + new Slice 3 tests green. Report exact pass/fail/skip counts in the exit note.

```
_R_CHECK_SYSTEM_CLOCK_=0 Rscript -e 'devtools::check(error_on = "note")'
```

Expected: 0 errors / 0 warnings / 0 notes.

- [ ] **Step 2: Draft `dev/notes/slice-3-exit.md`**

Structure (follow Slice 2 exit as template):

```markdown
# Slice 3 exit gate — <date>

## Deliverables
- [x] Query DSL: tokenizer, AST, parser, canonicaliser, evaluator
      (scalar/axis/vector/matrix lookups, masks, logical combinators,
      slicing, reductions, eltwise, GroupBy/CountBy, frames, cache).
- [x] ViewDaf class + viewer() with axis/scalar/vector/matrix override
      and wildcard sentinels.
- [x] Operations registry with default reductions + eltwise ops.
- [x] Julia-compat tests for both queries and views.

## Test + build status
<from the Step 1 output>

## Scope closed vs deferred
- Closed: <list from Phase Q/V>
- Deferred to Slice 4: chains, contracts, roxygen examples (Z2),
  alutil::sad() pass (Z3), L2 upstream PR (as confirmed 2026-04-20).

## Known mines laid in Slice 3 for Slice 4
<any subtle behaviour that a chains/contracts port needs to be aware
of — e.g., query cache invalidation semantics when a ViewDaf is chained
with another base>

## Ready-to-paste prompt for Slice 4
<kickoff breadcrumb pointing at chains + contracts>
```

- [ ] **Step 3: Commit (in dev repo)**

```bash
cd ~/src/dafr-native/dev
git add notes/slice-3-exit.md
git commit -m "notes(slice-3): exit gate with deliverables + deferred items"
```

- [ ] **Step 4: Tag and push**

```bash
cd ~/src/dafr-native
git tag slice-3
git push origin main --tags
```

Monitor CI (`gh run watch` or the Actions UI). CI should go green across linux / mac / windows R-CMD-check + altrep-sanity workflows.

If CI fails, diagnose + fix in a follow-up commit and re-tag.

---

## Self-review checklist (for plan author, before handing off)

- [ ] Every export in Julia `queries.jl` (lines 7-53) has an R equivalent *or* is explicitly deferred. Currently deferred (acknowledged, tracked for follow-up): `AsAxis` evaluator (parsed but no runtime behaviour beyond axis resolution), `full_vector_query`, `query_requires_relayout` (these are Julia-internal helpers; add if a consumer needs them).
- [ ] Every export in Julia `views.jl` (lines 10-24) has an R equivalent. `ViewData` / `ViewAxis` / `ViewAxes` / `ViewDatum` are Julia type aliases, not needed as R exports. Tensor views (`reversed_view_tensors`) are not in scope for Slice 3 (Julia uses them for 3-D data which the R backend does not yet support).
- [ ] Every task specifies exact file paths, shows test code in full, and shows at least the scaffolding of the implementation. Implementation bodies that reference Julia source cite the exact line number.
- [ ] Type consistency across tasks: `ast` is always a `list` of `qop_*` records; `state` is always a `list` with `kind ∈ {init, scalar, vector, matrix, axis, two_axes, mask, grouped_vector, grouped_matrix_rows, grouped_matrix_cols, names, scalar_names_ready}`.
- [ ] No placeholders (`TBD`, "implement later", "similar to Task N") — every step shows the code.
- [ ] Deferred items (roxygen examples, `alutil::sad()`, L2 upstream PR) are explicitly tracked, not silently dropped.

---

## Execution choice

**Plan complete and saved to `dev/plans/2026-04-20-slice-3-queries-views.md`. Two execution options:**

**1. Subagent-Driven (recommended)** — Dispatch a fresh subagent per task, review between tasks, fast iteration. Recommended for a slice this size (25+ tasks) — keeps each task in isolated context.

**2. Inline Execution** — Execute tasks in this session with batched checkpoints. Works, but task Q7–Q13 (evaluator) carry enough mutual dependency that a long session context will get crowded.

**Which approach?**
