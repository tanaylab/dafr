# Slice 10a — Query Builders Implementation Plan

> **For agentic workers:** Use superpowers:subagent-driven-development. Steps use checkbox (`- [ ]`) syntax.

**Goal:** Ship `DafrQuery` S7 class + 5 factory helpers + 53 builder exports + dispatch extension for `get_query` / `has_query` / `[.DafReader`. Native AST-backed port of the wrapper's pipe-chain DSL.

**Architecture:** 3 new R files (`R/query_class.R`, `R/query_builders.R`, `R/query_builders_exports.R`). 1 existing file extended (`R/queries.R` for dispatch). 7 new testthat files. Zero C++ touches.

**Tech Stack:** R 4.4+, S7 0.2.1 (existing). No new dependencies.

**Spec:** `dev/notes/2026-04-23-slice-10a-design.md`.

**Repo layout (unchanged from 10c):** Package repo at `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/`. Execute on feature branch `slice-10a`. Final merge at Phase Z.

**Dev loop per task:**
```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
```

**Known mines:**
- `.qop_<op>` internal constructors live in `R/query_ast.R` (existing); builders call them directly.
- `.canonicalise_ast` (existing) converts an AST list into a canonical string.
- `parse_query(canonical_query(q@canonical))@ast == q@ast` must hold. This is the round-trip identity — tested per builder and under composition.
- `escape_value` (now exported, 10c) handles all literal escaping. Builders call `escape_value` via `.escape_value` alias.
- Wrapper factories use `force()` — port verbatim to prevent late-binding over the loop variable in exports list.

---

## Phase 0: Branch setup

- [ ] **Step 0.1:** `git status --short` clean; `git checkout -b slice-10a` from `main`.
- [ ] **Step 0.2:** Baseline-green: `R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R`. Expected: 2075 PASS (slice-10c exit state), 0 FAIL.

---

## Phase A: DafrQuery class + factories + dispatch

**Files:**
- Create: `R/query_class.R`, `R/query_builders.R`
- Modify: `R/queries.R` (extend `get_query`, `has_query`; add `[.DafReader`)
- Create: `tests/testthat/test-dafrquery-class.R`, `tests/testthat/test-query-dispatch.R`

### Step A.1: Write failing tests for DafrQuery class

Create `tests/testthat/test-dafrquery-class.R`:

```r
test_that("DafrQuery constructs with ast and canonical", {
    ast <- list(list(op = "Axis", axis_name = "cell"))
    q <- DafrQuery(ast = ast, canonical = "@ cell")
    expect_identical(q@ast, ast)
    expect_identical(q@canonical, "@ cell")
})

test_that("DafrQuery validator rejects non-scalar canonical", {
    expect_error(DafrQuery(ast = list(), canonical = c("a", "b")))
})

test_that("DafrQuery format/as.character return canonical", {
    q <- DafrQuery(ast = list(list(op = "Axis", axis_name = "cell")), canonical = "@ cell")
    expect_identical(format(q), "@ cell")
    expect_identical(as.character(q), "@ cell")
})

test_that("DafrQuery print emits canonical and returns invisibly", {
    q <- DafrQuery(ast = list(list(op = "Axis", axis_name = "cell")), canonical = "@ cell")
    out <- capture.output(ret <- print(q))
    expect_true(any(grepl("@ cell", out)))
    expect_identical(ret, q)
})

test_that("DafrQuery length returns AST length", {
    q <- DafrQuery(ast = list(list(op = "Axis", axis_name = "cell")), canonical = "@ cell")
    expect_identical(length(q), 1L)
    q2 <- DafrQuery(
        ast = list(list(op = "Axis", axis_name = "cell"),
                   list(op = "LookupVector", name = "donor")),
        canonical = "@ cell : donor"
    )
    expect_identical(length(q2), 2L)
})
```

### Step A.2: Create R/query_class.R

```r
#' @include classes.R
NULL

#' Pipe-composable query object.
#'
#' Produced by the query builders (`Axis()`, `LookupVector()`, …) and
#' their composition via `|>`. Carries both the parsed AST (a list in
#' the shape `parse_query()` returns) and its canonical string form.
#'
#' @param ast List of AST nodes.
#' @param canonical Character scalar; the canonical query string for
#'   `ast`.
#' @return A `DafrQuery` instance.
#' @examples
#' DafrQuery(
#'     ast = list(list(op = "Axis", axis_name = "cell")),
#'     canonical = "@ cell"
#' )
#' @seealso [Axis()], [LookupVector()], [get_query()]
#' @export
DafrQuery <- S7::new_class(
    name = "DafrQuery",
    package = "dafr",
    properties = list(
        ast       = S7::class_list,
        canonical = S7::class_character
    ),
    validator = function(self) {
        if (length(self@canonical) != 1L || is.na(self@canonical)) {
            return("`canonical` must be a non-NA character scalar")
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
    cat("<DafrQuery>", x@canonical, "\n")
    invisible(x)
}

#' @export
length.DafrQuery <- function(x) length(x@ast)
```

### Step A.3: Write failing tests for dispatch

Create `tests/testthat/test-query-dispatch.R`:

```r
test_that("get_query accepts DafrQuery", {
    d <- example_cells_daf()
    q <- DafrQuery(
        ast = parse_query("@ cell : donor"),
        canonical = "@ cell : donor"
    )
    expect_identical(get_query(d, q), get_query(d, "@ cell : donor"))
})

test_that("get_query rejects non-character non-DafrQuery", {
    d <- memory_daf()
    expect_error(get_query(d, 42L), "character scalar or DafrQuery")
    expect_error(get_query(d, TRUE), "character scalar or DafrQuery")
    expect_error(get_query(d, NULL), "character scalar or DafrQuery")
})

test_that("has_query accepts DafrQuery", {
    d <- example_cells_daf()
    q <- DafrQuery(
        ast = parse_query(". organism"),
        canonical = ". organism"
    )
    expect_true(has_query(d, q))
})

test_that("[.DafReader accepts both character and DafrQuery", {
    d <- example_cells_daf()
    q <- DafrQuery(
        ast = parse_query("@ cell : donor"),
        canonical = "@ cell : donor"
    )
    expect_identical(d[q], d["@ cell : donor"])
})

test_that("[.DafReader errors on bad input", {
    d <- memory_daf()
    expect_error(d[42L])
})
```

### Step A.4: Create R/query_builders.R

```r
#' @include classes.R query_class.R query_ast.R query_parse.R
NULL

# Detect a DafrQuery as the pipe target in a builder's arguments.
# Mirrors wrapper R/utils.R:65.
.extract_query_and_value <- function(arg_val, arg_missing, dots,
                                     required = FALSE, default = NULL) {
    if (!arg_missing && inherits_dafr_query(arg_val)) {
        query <- arg_val
        value <- if (length(dots) && !inherits_dafr_query(dots[[1L]])) {
            dots[[1L]]
        } else {
            default
        }
    } else if (length(dots) && inherits_dafr_query(dots[[1L]])) {
        query <- dots[[1L]]
        value <- if (arg_missing) default else arg_val
    } else {
        query <- NULL
        value <- if (arg_missing) default else arg_val
    }
    provided <- !is.null(value) ||
        (!arg_missing && !inherits_dafr_query(arg_val))
    list(query = query, value = value, provided = provided)
}

inherits_dafr_query <- function(x) S7::S7_inherits(x, DafrQuery)

# Compose a DafrQuery from a prior pipe-target and a newly-built node.
.compose_query <- function(prior, new_ast_frag, new_canonical_frag) {
    if (is.null(prior)) {
        DafrQuery(ast = new_ast_frag, canonical = new_canonical_frag)
    } else {
        DafrQuery(
            ast = c(prior@ast, new_ast_frag),
            canonical = paste(prior@canonical, new_canonical_frag)
        )
    }
}

# Build a one-node AST fragment + its canonical string.
# `qop_builder` must be an existing `.qop_*` function in R/query_ast.R.
# Returns list(ast = list(node), canonical = character_scalar).
.build_fragment <- function(qop_builder, ...) {
    node <- qop_builder(...)
    frag_canonical <- .canonicalise_ast(list(node))
    list(ast = list(node), canonical = frag_canonical)
}

# ---- Factory helpers ------------------------------------------------------

.make_nullary <- function(op_name, qop_builder) {
    force(op_name)
    force(qop_builder)
    function(...) {
        dots <- list(...)
        non_query <- Filter(function(x) !inherits_dafr_query(x), dots)
        if (length(non_query) > 0L) {
            cli::cli_abort(
                "{.code {op_name}} expects zero arguments or one query object"
            )
        }
        res <- .extract_query_and_value(NULL, TRUE, dots, required = FALSE)
        frag <- .build_fragment(qop_builder)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}

.make_string_op <- function(op_name, qop_builder, param_name = "property") {
    force(op_name); force(qop_builder); force(param_name)
    function(value, ...) {
        res <- .extract_query_and_value(value, missing(value), list(...), required = TRUE)
        if (!res$provided) {
            cli::cli_abort("`{param_name}` is missing with no default")
        }
        if (!is.character(res$value) || length(res$value) != 1L) {
            cli::cli_abort("`{param_name}` must be a character scalar")
        }
        frag <- .build_fragment(qop_builder, res$value)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}

.make_value_op <- function(op_name, qop_builder, param_name = "value") {
    force(op_name); force(qop_builder); force(param_name)
    function(value, ...) {
        res <- .extract_query_and_value(value, missing(value), list(...), required = TRUE)
        if (!res$provided) {
            cli::cli_abort("`{param_name}` is missing with no default")
        }
        frag <- .build_fragment(qop_builder, res$value)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}

.make_optional_string_op <- function(op_name, qop_builder, param_name = "value") {
    force(op_name); force(qop_builder); force(param_name)
    function(value = NULL, ...) {
        res <- .extract_query_and_value(value, missing(value), list(...), required = FALSE, default = NULL)
        if (!is.null(res$value) && (!is.character(res$value) || length(res$value) != 1L)) {
            cli::cli_abort("`{param_name}` must be a character scalar or NULL")
        }
        frag <- .build_fragment(qop_builder, res$value)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}

.make_typed_reduction <- function(op_name, qop_builder) {
    force(op_name); force(qop_builder)
    function(type = NULL, ...) {
        dots <- list(...)
        res <- .extract_query_and_value(type, missing(type), dots, required = FALSE)
        if (!is.null(res$value) && (!is.character(res$value) || length(res$value) != 1L)) {
            cli::cli_abort("`type` must be a character scalar or NULL")
        }
        # Separate the type from other dots (params like eps, p, na_rm).
        params <- dots[vapply(dots, function(x) !inherits_dafr_query(x), logical(1L))]
        # If the type was in dots (first non-query arg), drop it.
        if (length(params) && identical(params[[1L]], res$value)) {
            params <- params[-1L]
        }
        frag <- .build_fragment(qop_builder, res$value, params)
        .compose_query(res$query, frag$ast, frag$canonical)
    }
}
```

### Step A.5: Create R/query_builders_exports.R

Placeholder for this phase; populate in B–F. Create an empty skeleton:

```r
#' @include query_builders.R
NULL

# Exports populated in phases B–F.
```

### Step A.6: Extend R/queries.R dispatch

Edit `get_query` (around line 17). Replace body with:

```r
get_query <- function(daf, query_string) {
    parts <- .get_query_dispatch(query_string)
    ast <- parts$ast
    canon <- parts$canonical
    key <- cache_key_query(canon)
    touched <- .collect_query_versions(daf, ast)
    stamp <- .snapshot_versions(daf, touched)
    cache_env <- S7::prop(daf, "cache")
    cached <- cache_lookup(cache_env, "query", key, stamp)
    if (!is.null(cached)) {
        return(cached)
    }
    value <- .eval_query(daf, ast)
    cache_store(cache_env, "query", key, value, stamp,
        size_bytes = as.numeric(object.size(value))
    )
    value
}

# Private: resolve either a character scalar or DafrQuery into (ast, canonical).
.get_query_dispatch <- function(q) {
    if (S7::S7_inherits(q, DafrQuery)) {
        list(ast = q@ast, canonical = q@canonical)
    } else if (is.character(q) && length(q) == 1L && !is.na(q)) {
        ast <- parse_query(q)
        list(ast = ast, canonical = .canonicalise_ast(ast))
    } else {
        stop("`query_string` must be a character scalar or DafrQuery", call. = FALSE)
    }
}
```

Similarly modify `has_query` to call `.get_query_dispatch` at its entry.

### Step A.7: Add [.DafReader method

At the end of `R/queries.R`, add:

```r
#' Index a DafReader with a query.
#'
#' `daf[q]` is shorthand for [get_query()]. Accepts either a
#' character-scalar query string or a [DafrQuery].
#'
#' @param x A [DafReader].
#' @param i A character scalar or [DafrQuery].
#' @return The query result (scalar, vector, matrix, or axis-entries).
#' @examples
#' d <- example_cells_daf()
#' d["@ cell : donor"] |> head()
#' d[Axis("cell") |> LookupVector("donor")] |> head()
#' @export
S7::method(`[`, DafReader) <- function(x, i) {
    get_query(x, i)
}
```

### Step A.8: Regen docs, test, commit

```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Commit:
```
git add R/query_class.R R/query_builders.R R/query_builders_exports.R R/queries.R tests/testthat/test-dafrquery-class.R tests/testthat/test-query-dispatch.R NAMESPACE man/
git commit -m "feat(10a): add DafrQuery class + 5 factory helpers + dispatch"
```

---

## Phase B: Element-wise builders (7)

**Builders:** `Abs`, `Clamp`, `Convert`, `Fraction`, `Log`, `Round`, `Significant`.

Mapping to factories + qop builders (in `R/query_ast.R` — grep `.qop_` to confirm exact names):
- `Abs` → `.make_nullary("Abs", .qop_abs)`
- `Round` → `.make_nullary("Round", .qop_round)`
- `Significant` → `.make_nullary("Significant", .qop_significant)` (takes digits as typed param)
- `Clamp`, `Convert`, `Fraction`, `Log` → `.make_typed_reduction(...)` (they all take params like min/max, type, eps, base).

### Step B.1: Write failing tests

Create `tests/testthat/test-builders-eltwise.R`:

```r
test_that("Abs builds a nullary eltwise query", {
    q <- Abs()
    expect_s3_class(q, "dafr::DafrQuery")
    expect_identical(q@canonical, "% Abs")
})

test_that("Abs composes after an axis+vector", {
    q <- Axis("cell") |> LookupVector("age") |> Abs()
    expect_identical(q@canonical, "@ cell : age % Abs")
})

test_that("Abs AST equals parse_query identity", {
    q <- Axis("cell") |> LookupVector("age") |> Abs()
    expect_identical(q@ast, parse_query("@ cell : age % Abs"))
})

# Similar block × 6 for Round, Clamp(min=0, max=1), Convert(type="double"),
# Fraction, Log(base=2, eps=1e-6), Significant(digits=2)
# ...
```

(Plan enumerates all 7 as explicit blocks; see wrapper `tests/testthat/test-query-ops.R` for the expected canonical strings.)

### Step B.2: Populate exports

Append to `R/query_builders_exports.R`:

```r
#' @export
Abs <- .make_nullary("Abs", .qop_abs)

#' @export
Round <- .make_nullary("Round", .qop_round)

#' @export
Significant <- .make_typed_reduction("Significant", .qop_significant)

#' @export
Clamp <- .make_typed_reduction("Clamp", .qop_clamp)

#' @export
Convert <- .make_typed_reduction("Convert", .qop_convert)

#' @export
Fraction <- .make_typed_reduction("Fraction", .qop_fraction)

#' @export
Log <- .make_typed_reduction("Log", .qop_log)
```

**Note:** `.qop_clamp`, `.qop_convert`, etc. may not yet exist in `R/query_ast.R`. Grep to confirm — if missing, add a thin `.qop_<name>` wrapper that calls `.qop_eltwise("<Name>", params)`. (The existing `.qop_eltwise` in `R/query_ast.R` handles the generic case via parameterised canonicalisation.)

### Step B.3: Run + commit

```
Rscript -e 'devtools::document()' && R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
git add R/query_builders_exports.R R/query_ast.R tests/testthat/test-builders-eltwise.R NAMESPACE man/
git commit -m "feat(10a): add 7 element-wise query builders"
```

---

## Phase C: Reduction builders (19)

**Builders (all via `.make_typed_reduction` or `.make_nullary`):**

Nullary: `Count`, `Max`, `Mean`, `Median`, `Min`, `Sum`, `ReduceToColumn`, `ReduceToRow`.
Typed-reduction (take `type` and/or other params): `CountBy`, `GeoMean`, `GroupBy`, `GroupColumnsBy`, `GroupRowsBy`, `Mode`, `Quantile`, `Std`, `StdN`, `Var`, `VarN`.

Actually `GroupBy`/`GroupColumnsBy`/`GroupRowsBy`/`CountBy` are string-op-style (take property name). Let me re-check:

- `GroupBy(property)` / `GroupRowsBy(property)` / `GroupColumnsBy(property)` / `CountBy(property)` → `.make_string_op` not `.make_typed_reduction`.
- Reductions with optional type + other params: `GeoMean(type, eps)`, `Quantile(type, p)`, `Std(type, na_rm)`, etc. → `.make_typed_reduction`.

Final partition:
- `.make_nullary` (8): `Count`, `Max`, `Mean`, `Median`, `Min`, `Sum`, `ReduceToColumn`, `ReduceToRow`.
- `.make_string_op` (4): `CountBy`, `GroupBy`, `GroupColumnsBy`, `GroupRowsBy`.
- `.make_typed_reduction` (7): `GeoMean`, `Mode`, `Quantile`, `Std`, `StdN`, `Var`, `VarN`.

### Step C.1: Write tests

Create `tests/testthat/test-builders-reductions.R` — 19 builders × 4 assertions = 76. Cover type-dispatch: `Sum(type = "integer")`, `Quantile(p = 0.5)`, `GroupBy("donor")`, `Mode()`.

### Step C.2: Append exports (19 one-liners)

```r
#' @export
Count <- .make_nullary("Count", .qop_count)
#' @export
Max <- .make_nullary("Max", .qop_max)
#' @export
Mean <- .make_nullary("Mean", .qop_mean)
#' @export
Median <- .make_nullary("Median", .qop_median)
#' @export
Min <- .make_nullary("Min", .qop_min)
#' @export
Sum <- .make_nullary("Sum", .qop_sum)
#' @export
ReduceToColumn <- .make_nullary("ReduceToColumn", .qop_reduce_to_column)
#' @export
ReduceToRow <- .make_nullary("ReduceToRow", .qop_reduce_to_row)

#' @export
CountBy <- .make_string_op("CountBy", .qop_count_by)
#' @export
GroupBy <- .make_string_op("GroupBy", .qop_group_by)
#' @export
GroupColumnsBy <- .make_string_op("GroupColumnsBy", .qop_group_columns_by)
#' @export
GroupRowsBy <- .make_string_op("GroupRowsBy", .qop_group_rows_by)

#' @export
GeoMean <- .make_typed_reduction("GeoMean", .qop_geomean)
#' @export
Mode <- .make_typed_reduction("Mode", .qop_mode)
#' @export
Quantile <- .make_typed_reduction("Quantile", .qop_quantile)
#' @export
Std <- .make_typed_reduction("Std", .qop_std)
#' @export
StdN <- .make_typed_reduction("StdN", .qop_std_n)
#' @export
Var <- .make_typed_reduction("Var", .qop_var)
#' @export
VarN <- .make_typed_reduction("VarN", .qop_var_n)
```

### Step C.3: Commit

```
git add … && git commit -m "feat(10a): add 19 reduction query builders"
```

---

## Phase D: Selection/axis builders (13)

**Builders:**

- `.make_string_op` (2): `Axis`, `AsAxis`, `Names` (wait — `Names` is actually nullary; let me re-check).
- Actually: `Axis(name)` → string; `AsAxis(name = NULL)` → optional string; `BeginMask(property)` / `BeginNegatedMask(property)` → string; `EndMask()` → nullary; `IfMissing(default)` → value; `IfNot(value = NULL)` → optional; `LookupScalar(name = NULL)` / `LookupVector(name = NULL)` / `LookupMatrix(name = NULL)` → optional string; `Names()` → nullary; `SquareColumnIs(value)` / `SquareRowIs(value)` → value.

Final partition:
- `.make_string_op` (3): `Axis`, `BeginMask`, `BeginNegatedMask`.
- `.make_nullary` (2): `EndMask`, `Names`.
- `.make_value_op` (3): `IfMissing`, `SquareColumnIs`, `SquareRowIs`.
- `.make_optional_string_op` (5): `AsAxis`, `IfNot`, `LookupScalar`, `LookupVector`, `LookupMatrix`.

Total: 3 + 2 + 3 + 5 = 13. ✓

### Step D.1: Tests

Create `tests/testthat/test-builders-selection.R`. Round-trip `Axis("name with spaces")` through escape/unescape and canonical-string equality.

### Step D.2: Exports (13 one-liners)

```r
#' @export
Axis <- .make_string_op("Axis", .qop_axis, param_name = "axis_name")
#' @export
BeginMask <- .make_string_op("BeginMask", function(prop) .qop_begin_mask(prop, negated = FALSE))
#' @export
BeginNegatedMask <- .make_string_op("BeginNegatedMask", function(prop) .qop_begin_mask(prop, negated = TRUE))
#' @export
EndMask <- .make_nullary("EndMask", .qop_end_mask)
#' @export
Names <- .make_nullary("Names", .qop_names)

#' @export
IfMissing <- .make_value_op("IfMissing", .qop_if_missing, param_name = "default")
#' @export
SquareColumnIs <- .make_value_op("SquareColumnIs", .qop_square_column_is)
#' @export
SquareRowIs <- .make_value_op("SquareRowIs", .qop_square_row_is)

#' @export
AsAxis <- .make_optional_string_op("AsAxis", .qop_as_axis, param_name = "axis_name")
#' @export
IfNot <- .make_optional_string_op("IfNot", .qop_if_not)
#' @export
LookupScalar <- .make_optional_string_op("LookupScalar", .qop_lookup_scalar, param_name = "name")
#' @export
LookupVector <- .make_optional_string_op("LookupVector", .qop_lookup_vector, param_name = "name")
#' @export
LookupMatrix <- .make_optional_string_op("LookupMatrix", .qop_lookup_matrix, param_name = "name")
```

**Note:** Several `.qop_*` builders may not exist with exact names — grep `R/query_ast.R` and add thin wrappers where missing. For `BeginMask`/`BeginNegatedMask`, the existing `.qop_begin_mask(property, negated)` signature means we need two lambdas.

### Step D.3: Commit

```
git commit -m "feat(10a): add 13 selection/axis query builders"
```

---

## Phase E: Logical mask builders (6)

**Builders (all `.make_string_op`):** `AndMask`, `AndNegatedMask`, `OrMask`, `OrNegatedMask`, `XorMask`, `XorNegatedMask`.

### Step E.1: Tests

Create `tests/testthat/test-builders-masks.R`. 6 × 4 = 24 assertions. Plus 4 cross-cutting for mask composition ordering.

### Step E.2: Exports (6 one-liners)

```r
#' @export
AndMask <- .make_string_op("AndMask",
    function(prop) .qop_mask("AndMask", prop, negated = FALSE))
#' @export
AndNegatedMask <- .make_string_op("AndNegatedMask",
    function(prop) .qop_mask("AndMask", prop, negated = TRUE))
#' @export
OrMask <- .make_string_op("OrMask",
    function(prop) .qop_mask("OrMask", prop, negated = FALSE))
#' @export
OrNegatedMask <- .make_string_op("OrNegatedMask",
    function(prop) .qop_mask("OrMask", prop, negated = TRUE))
#' @export
XorMask <- .make_string_op("XorMask",
    function(prop) .qop_mask("XorMask", prop, negated = FALSE))
#' @export
XorNegatedMask <- .make_string_op("XorNegatedMask",
    function(prop) .qop_mask("XorMask", prop, negated = TRUE))
```

**Note:** Exact `.qop_mask` helper may not exist; add it in `R/query_ast.R` if absent, dispatching on the And/Or/Xor kind. Check existing pattern first.

### Step E.3: Commit

```
git commit -m "feat(10a): add 6 logical mask query builders"
```

---

## Phase F: Comparison builders (8)

**Builders (all `.make_value_op`):** `IsEqual`, `IsGreater`, `IsGreaterEqual`, `IsLess`, `IsLessEqual`, `IsNotEqual`, `IsMatch`, `IsNotMatch`.

### Step F.1: Tests

Create `tests/testthat/test-builders-comparison.R`. 8 × 4 = 32. `IsMatch("regex.*")` regex composition round-trip.

### Step F.2: Exports (8 one-liners)

```r
#' @export
IsEqual <- .make_value_op("IsEqual", .qop_is_equal)
#' @export
IsNotEqual <- .make_value_op("IsNotEqual", .qop_is_not_equal)
#' @export
IsGreater <- .make_value_op("IsGreater", .qop_is_greater)
#' @export
IsGreaterEqual <- .make_value_op("IsGreaterEqual", .qop_is_greater_equal)
#' @export
IsLess <- .make_value_op("IsLess", .qop_is_less)
#' @export
IsLessEqual <- .make_value_op("IsLessEqual", .qop_is_less_equal)
#' @export
IsMatch <- .make_value_op("IsMatch", .qop_is_match)
#' @export
IsNotMatch <- .make_value_op("IsNotMatch", .qop_is_not_match)
```

`.qop_is_match` / `.qop_is_not_match` may need to be added in `R/query_ast.R` (existing `.qop_is_less`/etc. confirmed present at `R/query_ast.R:84+`).

### Step F.3: Commit

```
git commit -m "feat(10a): add 8 comparison query builders"
```

---

## Phase Z: Polish + tag

### Step Z.1: Write NEWS entry

Append a `### Slice 10a` section under the `# dafr (development version)` heading. Document: 53 new builder exports; `DafrQuery` class; dispatch extension; `daf[Axis(...) |> LookupVector(...)]` syntax.

### Step Z.2: devtools::check

```
Rscript -e 'devtools::check(error_on = "never", vignettes = FALSE)' 2>&1 | tail -30
```

Expected: 0 ERROR, 0 WARNING, ≤ 4 NOTE (the same pre-existing carry-over as 10c).

### Step Z.3: Final test run

```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: +260 assertions over 2075, so ≥ 2335 PASS.

### Step Z.4: Commit NEWS + merge + tag

```
git add NEWS.md
git commit -m "docs(10a): add NEWS entry for slice 10a — 53 new builder exports"
git checkout main
git merge --no-ff slice-10a -m "merge(10a): query builders — 53 new exports"
git tag slice-10a
```

### Step Z.5: Exit note

Write `dev/notes/slice-10a-exit.md` following 10c's template.

```
cd dev
git add notes/slice-10a-exit.md
git commit -m "notes(10a): add exit note for slice 10a"
cd ..
```

---

## Self-review

- Spec §4.1 DafrQuery class → Phase A Step A.2. ✓
- Spec §4.2 extract_query_and_value → Phase A Step A.4. ✓
- Spec §4.3 Factory helpers → Phase A Step A.4. ✓
- Spec §4.4 Dispatch → Phase A Steps A.6–A.7. ✓
- Spec §5 Builder table (53 total) → Phases B–F. ✓
- Spec §6 Error handling → tests per phase. ✓
- Spec §7 Test plan → 7 new test files. ✓
- Spec §8 Execution order → Phases 0, A, B, C, D, E, F, Z. ✓
- Spec §9 Exit criterion → Phase Z. ✓
