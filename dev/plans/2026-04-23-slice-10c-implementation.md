# Slice 10c — Small Ports Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship the 25-export wrapper-parity surface for native `dafr`: handler constants, query introspection, version counters, group helpers, class-surface sugar, DataFrame helpers, and contract UX. Pure R; no C++; no new hard deps.

**Architecture:** Surface layer over existing native internals. 3 new R files (`R/groups.R`, `R/contract_ux.R`, `R/dataframes.R`). 8 existing files extended. Zero S7 schema changes: the `Contract` class already has a flat `data` slot with `$kind`-dispatched records, so `tensor_contract` adds a `$kind = "tensor"` record type. Safest-first execution order — contract UX lands last under a green suite.

**Tech Stack:** R 4.4+, S7 0.2.1 (existing). `rlang` promoted to Imports (for `check_installed`). `tidyr` + `tibble` added to Suggests (gated by `check_installed` in `get_tidy`). No C++ changes. No `LinkingTo` changes. `Matrix`, `cli`, `bit64`, `jsonlite`, `matrixStats`, `methods` remain as-is.

**Repo layout:**
- Package repo: `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/` — tracks `git@github.com:tanaylab/dafr.git`. Source, tests, DESCRIPTION, NEWS commits → package repo. Execute on feature branch `slice-10c` (created in Phase 0; final merge at Phase Z).
- Dev repo (nested, gitignored): `.../dafr-native/dev/` — plans/notes/scripts only. Spec at `dev/notes/2026-04-23-slice-10c-design.md`. No code commits land here.

**Dev loop per task:**
1. From `/net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native/`:
   ```
   R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
   ```
   (`NOT_CRAN=true` is needed to fire stress tests. `test-helpers.R:26` is a pre-existing known FAIL under Rscript — not a regression.)
2. Inspect; iterate to green.
3. Stage + commit with the message given in the task. Package repo only (`dev/` is gitignored). **Never `--amend`, `--no-verify`, or force-push.** Use `/bin/rm` / `/bin/cp` (aliased with `-i`).

**Known mines from earlier slices (honor throughout):**
- `.escape_value(s)` at `R/query_ast.R:67` uses the regex `[\\s!&*%./:<=>?@\\[\\]^\\|~\"]` to decide when to quote; quoting wraps in double quotes and escapes `\\` and `"`. `unescape_value` must invert this exactly. Test the round-trip on every literal in the regex alphabet.
- `Contract` S7 class (`R/contracts.R:57`) has properties `name`, `is_relaxed`, `axes`, `data`. `data` is a flat list of records with `$kind` ∈ `{"scalar","vector","matrix"}` today; this slice adds `"tensor"`.
- `chain_reader(list(daf), name = ...)` returns a `ReadOnlyChainDaf` — the target of `read_only()`.
- Version counters in `DafReader` S7 class are environments (not lists). Access via `S7::prop(daf, "axis_version_counter")[[key]]`.
- `get_frame` exists in `R/queries.R:250`. Phase F renames it to `get_dataframe_query`.
- testthat edition 3 is on (see DESCRIPTION). Use `expect_equal`, `expect_error`, `expect_true`, `expect_warning` — not `expect_that`.

---

## Pre-planning decisions (settled before tasks)

### 1. Phase order

0 → A → B → C → D → E → F → G → Z, in that sequence:

- **Phase 0 (branch setup):** create `slice-10c` branch from `slice-9d-n`; no code changes.
- **A (Handler constants + shim):** smallest; unblocks slice 10b `unsupported_handler = WARN_HANDLER` usage.
- **B (Query introspection):** `escape_value` / `unescape_value` / `query_requires_relayout`. Round-trip identity locked early.
- **C (Version counters):** 3 thin accessors; mechanical.
- **D (Group helpers):** new `R/groups.R`; FNV-32 hash in pure R.
- **E (Class-surface sugar):** `is_daf`, `daf_name`, `complete_path`, `read_only`.
- **F (DataFrame helpers):** new `R/dataframes.R` + `get_frame` → `get_dataframe_query` rename. DESCRIPTION dependency bump.
- **G (Contract UX):** new `R/contract_ux.R` + `R/contracts.R` tensor-record extension. Highest-risk; lands last.
- **Z (Polish):** NEWS 10c entry, `devtools::document()`, `devtools::check()`, tag `slice-10c`, merge to `main`.

### 2. Test file layout

One testthat file per export group:

| File | Exports tested | Assertion budget |
|---|---|---|
| `tests/testthat/test-handlers-constants.R` | 4 | ~15 |
| `tests/testthat/test-query-introspection.R` | 3 | ~25 |
| `tests/testthat/test-version-counters.R` | 3 | ~15 |
| `tests/testthat/test-groups.R` | 3 | ~15 |
| `tests/testthat/test-class-sugar.R` | 4 | ~15 |
| `tests/testthat/test-dataframes.R` | 3 | ~15 |
| `tests/testthat/test-contract-ux.R` | 5 | ~30 |

Total ~130 assertions.

### 3. Commit convention

Per `git log --oneline -5` on `main`: `<type>(<scope>): <one-line summary>`, e.g. `feat(10c): add handler constants`, `feat(10c): export version counters`, `refactor(10c): rename get_frame → get_dataframe_query`. Body optional; use HEREDOC for multi-line messages.

### 4. No documentation stubs

Every new export carries a runnable `@examples` block at `feat` commit time (not deferred to 10d). This is cheaper than the 10d backfill would be, and `devtools::check()` will flag missing examples anyway.

### 5. Roxygen regeneration

At the end of each phase with new exports, run `Rscript -e 'devtools::document()'` and stage the resulting `NAMESPACE` + `man/*.Rd` edits in the same commit as the R source change. Never hand-edit `NAMESPACE`.

---

## Phase 0: Branch setup

**Files:** none.

**Preconditions:** on `main`, `git status` clean. Predecessor tag `slice-9d-n` points to current `main` HEAD (verify with `git describe --tags --exact-match`).

- [ ] **Step 0.1: Verify predecessor state**

Run:
```
cd /net/mraid20/ifs/wisdom/tanay_lab/tgdata/users/aviezerl/src/dafr-native
git status --short
git describe --tags --exact-match HEAD 2>/dev/null || git log -1 --oneline
```

Expected: working tree clean; HEAD at or descended from `slice-9d-n`.

- [ ] **Step 0.2: Create branch**

Run:
```
git checkout -b slice-10c
```

Expected: switched to a new branch `slice-10c`.

- [ ] **Step 0.3: Baseline-green test run**

Run:
```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: all tests green (modulo the known pre-existing `test-helpers.R:26` skip). This establishes the pre-slice baseline.

---

## Phase A: Handler constants + shim

**Files:**
- Modify: `R/handlers.R` (append ~30 lines after existing `emit_action`)
- Create: `tests/testthat/test-handlers-constants.R`

**Preconditions:** Phase 0 complete.

- [ ] **Step A.1: Write the failing tests**

Create `tests/testthat/test-handlers-constants.R`:

```r
test_that("handler constants equal lowercase action tokens", {
    expect_equal(ERROR_HANDLER, "error")
    expect_equal(WARN_HANDLER, "warn")
    expect_equal(IGNORE_HANDLER, "ignore")
})

test_that("inefficient_action_handler registers a string handler", {
    old <- tryCatch(
        get("inefficient", envir = dafr:::.dafr_handlers),
        error = function(e) NULL
    )
    on.exit(
        {
            if (is.null(old)) {
                if (exists("inefficient", envir = dafr:::.dafr_handlers, inherits = FALSE)) {
                    rm("inefficient", envir = dafr:::.dafr_handlers)
                }
            } else {
                assign("inefficient", old, envir = dafr:::.dafr_handlers)
            }
        },
        add = TRUE
    )
    inefficient_action_handler(IGNORE_HANDLER)
    expect_identical(get("inefficient", envir = dafr:::.dafr_handlers), "ignore")
    inefficient_action_handler(WARN_HANDLER)
    expect_identical(get("inefficient", envir = dafr:::.dafr_handlers), "warn")
    inefficient_action_handler(ERROR_HANDLER)
    expect_identical(get("inefficient", envir = dafr:::.dafr_handlers), "error")
})

test_that("inefficient_action_handler accepts functions", {
    old <- tryCatch(
        get("inefficient", envir = dafr:::.dafr_handlers),
        error = function(e) NULL
    )
    on.exit(
        {
            if (is.null(old)) {
                if (exists("inefficient", envir = dafr:::.dafr_handlers, inherits = FALSE)) {
                    rm("inefficient", envir = dafr:::.dafr_handlers)
                }
            } else {
                assign("inefficient", old, envir = dafr:::.dafr_handlers)
            }
        },
        add = TRUE
    )
    captured <- NULL
    inefficient_action_handler(function(msg) captured <<- msg)
    dafr:::emit_action("inefficient", "hello")
    expect_identical(captured, "hello")
})

test_that("inefficient_action_handler rejects bad input", {
    expect_error(inefficient_action_handler(42L),
        "string or a function"
    )
})

test_that("emit_action round-trip via ERROR_HANDLER", {
    old <- tryCatch(
        get("inefficient", envir = dafr:::.dafr_handlers),
        error = function(e) NULL
    )
    on.exit(
        {
            if (is.null(old)) {
                if (exists("inefficient", envir = dafr:::.dafr_handlers, inherits = FALSE)) {
                    rm("inefficient", envir = dafr:::.dafr_handlers)
                }
            } else {
                assign("inefficient", old, envir = dafr:::.dafr_handlers)
            }
        },
        add = TRUE
    )
    inefficient_action_handler(ERROR_HANDLER)
    expect_error(dafr:::emit_action("inefficient", "bad"), "bad")
})
```

- [ ] **Step A.2: Run test to verify it fails**

Run:
```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R 2>&1 | grep -E "^(FAIL|Error|test-handlers)" | head
```

Expected: FAIL — `ERROR_HANDLER` / `WARN_HANDLER` / `IGNORE_HANDLER` / `inefficient_action_handler` not found.

- [ ] **Step A.3: Write minimal implementation**

Append to `R/handlers.R`:

```r
#' Inefficient-action handler constants.
#'
#' String constants matching the lowercase tokens accepted by
#' [register_dafr_handler()]. Pass these to
#' [inefficient_action_handler()] or any registry entry that takes
#' an action token.
#'
#' @return Character scalar (one of `"error"`, `"warn"`, `"ignore"`).
#' @examples
#' inefficient_action_handler(WARN_HANDLER)
#' @name handler-constants
NULL

#' @rdname handler-constants
#' @export
ERROR_HANDLER <- "error"

#' @rdname handler-constants
#' @export
WARN_HANDLER <- "warn"

#' @rdname handler-constants
#' @export
IGNORE_HANDLER <- "ignore"

#' Register a handler for the `"inefficient"` action category.
#'
#' Thin wrapper around `register_dafr_handler("inefficient", handler)`.
#' Exists to match the Julia-facade wrapper's API.
#'
#' @param handler One of [`ERROR_HANDLER`][handler-constants],
#'   [`WARN_HANDLER`][handler-constants],
#'   [`IGNORE_HANDLER`][handler-constants], or a function
#'   `function(message, ...)`.
#' @return Invisibly `NULL`.
#' @examples
#' inefficient_action_handler(IGNORE_HANDLER)
#' inefficient_action_handler(function(msg) message("inefficient: ", msg))
#' @export
inefficient_action_handler <- function(handler) {
    register_dafr_handler("inefficient", handler)
    invisible()
}
```

- [ ] **Step A.4: Regen NAMESPACE and run tests**

Run:
```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: `NAMESPACE` gains 4 new `export()` entries; all tests green.

- [ ] **Step A.5: Commit**

From the package repo root:

```
git add R/handlers.R tests/testthat/test-handlers-constants.R NAMESPACE man/
git commit -m "feat(10c): add handler constants + inefficient_action_handler shim"
```

---

## Phase B: Query introspection

**Files:**
- Modify: `R/query_ast.R` (add `@export` to `.escape_value` → `escape_value`; add new `unescape_value`)
- Modify: `R/queries.R` (add `query_requires_relayout`)
- Create: `tests/testthat/test-query-introspection.R`

**Preconditions:** Phase A complete.

- [ ] **Step B.1: Write the failing tests**

Create `tests/testthat/test-query-introspection.R`:

```r
test_that("escape_value is a public alias of .escape_value", {
    expect_identical(escape_value("plain"), "plain")
    expect_identical(escape_value("has space"), "\"has space\"")
    expect_identical(escape_value("quo\"ted"), "\"quo\\\"ted\"")
    expect_identical(escape_value("back\\slash"), "\"back\\\\slash\"")
})

test_that("unescape_value inverts escape_value", {
    cases <- c(
        "plain",
        "has space",
        "has\ttab",
        "quo\"ted",
        "back\\slash",
        "colons:are:special",
        "and!bangs",
        "and*stars",
        "and%percent",
        "and.dots",
        "and/slashes",
        "and<less",
        "and=equal",
        "and>greater",
        "and?q",
        "and@at",
        "and[bracket",
        "and]bracket",
        "and^caret",
        "and|pipe",
        "and~tilde",
        "and\"quote",
        "and&amp",
        "embedded\nnewline",
        "embedded\rreturn",
        "", # empty
        " ", # single space
        "unicode é",
        "combining ́",
        "a\\b\"c d"
    )
    for (s in cases) {
        expect_identical(unescape_value(escape_value(s)), s,
            info = sprintf("case: %s", paste(charToRaw(s), collapse = " "))
        )
    }
})

test_that("unescape_value leaves unquoted strings unchanged", {
    expect_identical(unescape_value("plain"), "plain")
    expect_identical(unescape_value(""), "")
})

test_that("query_requires_relayout TRUE when matrix axes swap", {
    d <- example_cells_daf()
    # "@ gene @ cell :: UMIs" when UMIs is stored as (cell, gene) → relayout.
    # example_cells_daf stores UMIs as (cell, gene); the natural
    # query is "@ cell @ gene :: UMIs".
    expect_false(query_requires_relayout(d, "@ cell @ gene :: UMIs"))
    expect_true(query_requires_relayout(d, "@ gene @ cell :: UMIs"))
})

test_that("query_requires_relayout FALSE for non-matrix queries", {
    d <- example_cells_daf()
    expect_false(query_requires_relayout(d, "@ cell : donor"))
    expect_false(query_requires_relayout(d, ". organism"))
    expect_false(query_requires_relayout(d, "@ cell"))
})

test_that("query_requires_relayout errors on parse failure", {
    d <- example_cells_daf()
    expect_error(query_requires_relayout(d, "@ @ @"))
})
```

- [ ] **Step B.2: Run tests to verify they fail**

Run:
```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R 2>&1 | grep -E "test-query-introspection" | head
```

Expected: fail — `escape_value` / `unescape_value` / `query_requires_relayout` not exported.

- [ ] **Step B.3: Export escape_value and add unescape_value**

Edit `R/query_ast.R` at line 67. Replace the existing `.escape_value` block:

```r
.escape_value <- function(s) {
    if (grepl("[\\s!&*%./:<=>?@\\[\\]^\\|~\"]", s, perl = TRUE)) {
        paste0("\"", gsub("[\\\\\"]", "\\\\\\0", s, perl = TRUE), "\"")
    } else {
        s
    }
}
```

…with:

```r
#' Escape a value for use as a query literal.
#'
#' If `s` contains any of the query metacharacters (whitespace, `!`,
#' `&`, `*`, `%`, `.`, `/`, `:`, `<`, `=`, `>`, `?`, `@`, `[`, `]`,
#' `^`, `|`, `~`, `"`), the result is double-quoted and any backslash
#' or double-quote inside is backslash-escaped. Otherwise `s` is
#' returned unchanged.
#'
#' @param s Character scalar.
#' @return Character scalar suitable for concatenation into a query
#'   string.
#' @examples
#' escape_value("plain")
#' escape_value("has space")
#' unescape_value(escape_value("has \"quotes\""))
#' @seealso [unescape_value()], [canonical_query()]
#' @export
escape_value <- function(s) {
    if (grepl("[\\s!&*%./:<=>?@\\[\\]^\\|~\"]", s, perl = TRUE)) {
        paste0("\"", gsub("[\\\\\"]", "\\\\\\0", s, perl = TRUE), "\"")
    } else {
        s
    }
}

# Kept as a private alias because many internals and tests call
# `.escape_value` directly; do not remove without a separate sweep.
.escape_value <- escape_value

#' Inverse of [escape_value()].
#'
#' Strips an outer pair of double quotes (if present) and unescapes
#' `\\` and `\"` sequences. Leaves already-bare strings unchanged.
#'
#' @param s Character scalar (an escaped query literal).
#' @return Character scalar (the original value).
#' @examples
#' unescape_value("\"a b\"")
#' unescape_value("plain")
#' stopifnot(identical(unescape_value(escape_value("a b")), "a b"))
#' @seealso [escape_value()]
#' @export
unescape_value <- function(s) {
    stopifnot(is.character(s), length(s) == 1L)
    if (!startsWith(s, "\"") || !endsWith(s, "\"") || nchar(s) < 2L) {
        return(s)
    }
    inner <- substr(s, 2L, nchar(s) - 1L)
    # Replace \" and \\ sequences left-to-right in a single pass so
    # that "\\\"" decodes to "\"" rather than the escape of a literal
    # quote following a backslash. Use a regex with capture group.
    gsub("\\\\([\\\\\"])", "\\1", inner, perl = TRUE)
}
```

- [ ] **Step B.4: Add query_requires_relayout to R/queries.R**

Append to `R/queries.R` (after `query_result_dimensions`, before `has_query`):

```r
#' Does evaluating this query require a matrix relayout (transpose)?
#'
#' Walks the parsed AST and returns `TRUE` if any `LookupMatrix` node
#' would read a matrix stored with axis order different from the order
#' implied by the surrounding `@ rows @ cols` scopes, or if a
#' `ReduceToColumn`/`ReduceToRow` would force a relayout.
#'
#' @inheritParams get_query
#' @return Logical scalar.
#' @examples
#' d <- example_cells_daf()
#' query_requires_relayout(d, "@ cell @ gene :: UMIs") # stored order → FALSE
#' query_requires_relayout(d, "@ gene @ cell :: UMIs") # swapped → TRUE
#' @export
query_requires_relayout <- function(daf, query_string) {
    ast <- parse_query(query_string)
    rows_axis <- NULL
    cols_axis <- NULL
    two_axes <- FALSE
    scope_axis <- NULL
    for (n in ast) {
        switch(n$op,
            Axis = {
                if (isTRUE(two_axes)) {
                    # ignore further Axis nodes inside two-axis scope
                } else if (!is.null(scope_axis)) {
                    rows_axis <- scope_axis
                    cols_axis <- n$axis_name
                    two_axes <- TRUE
                } else {
                    scope_axis <- n$axis_name
                }
            },
            LookupMatrix = {
                if (isTRUE(two_axes) && !is.null(n$name)) {
                    if (!format_has_matrix(daf, rows_axis, cols_axis, n$name) &&
                        format_has_matrix(daf, cols_axis, rows_axis, n$name)) {
                        return(TRUE)
                    }
                }
            },
            ReduceToColumn = ,
            ReduceToRow = {
                if (isTRUE(two_axes)) {
                    # A column-reduction of a (rows, cols) matrix stored
                    # as (cols, rows) requires relayout to iterate by column.
                    return(
                        !format_has_matrix(daf, rows_axis, cols_axis,
                            last_matrix_name(ast)
                        ) &&
                            format_has_matrix(daf, cols_axis, rows_axis,
                                last_matrix_name(ast)
                            )
                    )
                }
            },
            NULL
        )
    }
    FALSE
}

# Find the most recent LookupMatrix $name in an AST, for reduction
# dispatch in query_requires_relayout. Returns NA_character_ if absent.
last_matrix_name <- function(ast) {
    for (n in rev(ast)) {
        if (identical(n$op, "LookupMatrix") && !is.null(n$name)) {
            return(n$name)
        }
    }
    NA_character_
}
```

- [ ] **Step B.5: Run tests, regen docs**

Run:
```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: all tests green. `NAMESPACE` gains `export(escape_value)`, `export(unescape_value)`, `export(query_requires_relayout)`.

- [ ] **Step B.6: Commit**

```
git add R/query_ast.R R/queries.R tests/testthat/test-query-introspection.R NAMESPACE man/
git commit -m "feat(10c): add escape_value/unescape_value/query_requires_relayout"
```

---

## Phase C: Version counters

**Files:**
- Modify: `R/cache.R` (append 3 exports near the existing internal `axis_stamp`/`vector_stamp`/`matrix_stamp` block)
- Create: `tests/testthat/test-version-counters.R`

**Preconditions:** Phase B complete.

- [ ] **Step C.1: Write the failing tests**

Create `tests/testthat/test-version-counters.R`:

```r
test_that("axis_version_counter starts at 0L and increments on mutation", {
    d <- memory_daf(name = "vc")
    expect_identical(axis_version_counter(d, "cell"), 0L)
    add_axis(d, "cell", c("c1", "c2"))
    expect_identical(axis_version_counter(d, "cell"), 1L)
    add_axis(d, "gene", c("g1", "g2", "g3"))
    # Unrelated axis mutation does not bump "cell".
    expect_identical(axis_version_counter(d, "cell"), 1L)
    expect_identical(axis_version_counter(d, "gene"), 1L)
})

test_that("vector_version_counter tracks per-vector mutation", {
    d <- memory_daf(name = "vc")
    add_axis(d, "cell", c("c1", "c2", "c3"))
    expect_identical(vector_version_counter(d, "cell", "donor"), 0L)
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    expect_identical(vector_version_counter(d, "cell", "donor"), 1L)
    # Reads do not bump.
    get_vector(d, "cell", "donor")
    expect_identical(vector_version_counter(d, "cell", "donor"), 1L)
    # Overwrite bumps.
    set_vector(d, "cell", "donor", c("B", "A", "B"), overwrite = TRUE)
    expect_identical(vector_version_counter(d, "cell", "donor"), 2L)
    # Unrelated vector is 0.
    expect_identical(vector_version_counter(d, "cell", "age"), 0L)
})

test_that("matrix_version_counter tracks per-matrix mutation", {
    d <- memory_daf(name = "vc")
    add_axis(d, "cell", c("c1", "c2"))
    add_axis(d, "gene", c("g1", "g2", "g3"))
    expect_identical(matrix_version_counter(d, "cell", "gene", "UMIs"), 0L)
    set_matrix(d, "cell", "gene", "UMIs",
        matrix(1:6, nrow = 2, ncol = 3))
    expect_identical(matrix_version_counter(d, "cell", "gene", "UMIs"), 1L)
})

test_that("version_counter return type is integer(1)", {
    d <- memory_daf(name = "vc")
    expect_type(axis_version_counter(d, "cell"), "integer")
    expect_length(axis_version_counter(d, "cell"), 1L)
    expect_type(vector_version_counter(d, "cell", "x"), "integer")
    expect_length(vector_version_counter(d, "cell", "x"), 1L)
    expect_type(matrix_version_counter(d, "cell", "gene", "x"), "integer")
    expect_length(matrix_version_counter(d, "cell", "gene", "x"), 1L)
})
```

- [ ] **Step C.2: Run tests to verify they fail**

```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R 2>&1 | grep "test-version-counters" | head
```

Expected: FAIL — functions not exported.

- [ ] **Step C.3: Implement the three accessors**

Append to `R/cache.R` (after the existing `matrix_stamp` block, ~line 140):

```r
# ---- Public version-counter accessors --------------------------------------

#' Per-axis version counter.
#'
#' Returns the monotonic counter for `axis` on `daf`. Incremented on
#' `add_axis` / `delete_axis`. Returns `0L` if the axis has never been
#' mutated (including non-existent axes, to match wrapper semantics).
#'
#' @param daf A [DafReader].
#' @param axis Axis name (character scalar).
#' @return `integer(1)`.
#' @examples
#' d <- memory_daf()
#' axis_version_counter(d, "cell") # 0L
#' add_axis(d, "cell", c("c1", "c2"))
#' axis_version_counter(d, "cell") # 1L
#' @seealso [vector_version_counter()], [matrix_version_counter()]
#' @export
axis_version_counter <- function(daf, axis) {
    S7::prop(daf, "axis_version_counter")[[axis]] %||% 0L
}

#' Per-vector version counter.
#'
#' Returns the monotonic counter for the `name` vector on `axis`.
#' Incremented on `set_vector` / `delete_vector`. Returns `0L` if the
#' vector has never been mutated.
#'
#' @inheritParams axis_version_counter
#' @param name Vector name (character scalar).
#' @return `integer(1)`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' vector_version_counter(d, "cell", "donor") # 0L
#' set_vector(d, "cell", "donor", c("A", "B"))
#' vector_version_counter(d, "cell", "donor") # 1L
#' @export
vector_version_counter <- function(daf, axis, name) {
    key <- paste0(axis, ":", name)
    S7::prop(daf, "vector_version_counter")[[key]] %||% 0L
}

#' Per-matrix version counter.
#'
#' Returns the monotonic counter for the `name` matrix on
#' `(rows_axis, columns_axis)`. Incremented on `set_matrix` /
#' `delete_matrix` / `relayout_matrix`. Returns `0L` if never mutated.
#'
#' @inheritParams axis_version_counter
#' @param rows_axis,columns_axis Axis names.
#' @param name Matrix name.
#' @return `integer(1)`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' add_axis(d, "gene", c("g1", "g2", "g3"))
#' matrix_version_counter(d, "cell", "gene", "UMIs") # 0L
#' @export
matrix_version_counter <- function(daf, rows_axis, columns_axis, name) {
    key <- paste0(rows_axis, ":", columns_axis, ":", name)
    S7::prop(daf, "matrix_version_counter")[[key]] %||% 0L
}
```

- [ ] **Step C.4: Regen docs + run tests**

```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: all tests green.

- [ ] **Step C.5: Commit**

```
git add R/cache.R tests/testthat/test-version-counters.R NAMESPACE man/
git commit -m "feat(10c): export axis/vector/matrix version counters"
```

---

## Phase D: Group helpers

**Files:**
- Create: `R/groups.R`
- Create: `tests/testthat/test-groups.R`

**Preconditions:** Phase C complete.

- [ ] **Step D.1: Write the failing tests**

Create `tests/testthat/test-groups.R`:

```r
test_that("compact_groups renumbers non-zero indices in first-seen order", {
    res <- compact_groups(c(5L, 10L, 5L, 0L, 10L))
    expect_identical(res$n_groups, 2L)
    expect_identical(res$group_indices, c(1L, 2L, 1L, 0L, 2L))
})

test_that("compact_groups is identity on already-compact input", {
    res <- compact_groups(c(1L, 2L, 1L, 0L, 2L))
    expect_identical(res$n_groups, 2L)
    expect_identical(res$group_indices, c(1L, 2L, 1L, 0L, 2L))
})

test_that("compact_groups handles all-zero input", {
    res <- compact_groups(c(0L, 0L, 0L))
    expect_identical(res$n_groups, 0L)
    expect_identical(res$group_indices, c(0L, 0L, 0L))
})

test_that("compact_groups handles empty input", {
    res <- compact_groups(integer(0L))
    expect_identical(res$n_groups, 0L)
    expect_identical(res$group_indices, integer(0L))
})

test_that("collect_group_members is the inverse of group-index assignment", {
    members <- collect_group_members(c(1L, 2L, 1L, 0L, 2L))
    expect_length(members, 2L)
    expect_identical(members[[1L]], c(1L, 3L))
    expect_identical(members[[2L]], c(2L, 5L))
})

test_that("collect_group_members ignores 0-index entries", {
    members <- collect_group_members(c(0L, 0L, 1L))
    expect_length(members, 1L)
    expect_identical(members[[1L]], 3L)
})

test_that("group_names produces deterministic prefix+hash names", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    g1 <- group_names(d, "cell", list(c(1L, 2L), c(3L, 4L)), prefix = "grp_")
    g2 <- group_names(d, "cell", list(c(1L, 2L), c(3L, 4L)), prefix = "grp_")
    expect_identical(g1, g2)
    expect_true(all(startsWith(g1, "grp_")))
    expect_length(g1, 2L)
})

test_that("group_names differs for different members", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    g1 <- group_names(d, "cell", list(c(1L, 2L)), prefix = "grp_")
    g2 <- group_names(d, "cell", list(c(1L, 3L)), prefix = "grp_")
    expect_false(identical(g1, g2))
})

test_that("group_names is member-order invariant", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    g1 <- group_names(d, "cell", list(c(1L, 2L, 3L)), prefix = "")
    g2 <- group_names(d, "cell", list(c(3L, 1L, 2L)), prefix = "")
    expect_identical(g1, g2)
})

test_that("group_names errors on out-of-range indices", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    expect_error(group_names(d, "cell", list(c(1L, 5L)), prefix = ""))
})
```

- [ ] **Step D.2: Run tests to verify they fail**

```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R 2>&1 | grep "test-groups" | head
```

Expected: FAIL — functions not defined.

- [ ] **Step D.3: Create R/groups.R**

```r
#' @include classes.R format_api.R utils.R
NULL

# ---- FNV-32 hash (stable across sessions; pure R; no deps) ----------------

.FNV32_OFFSET <- 2166136261  # stored as double to dodge int32 overflow
.FNV32_PRIME  <- 16777619

.fnv32_hex <- function(bytes) {
    h <- .FNV32_OFFSET
    for (b in as.integer(bytes)) {
        h <- bitwXor(h, b)
        # Multiply mod 2^32 via double arithmetic; FNV prime fits comfortably.
        h <- (h * .FNV32_PRIME) %% 4294967296
    }
    sprintf("%08x", as.integer(h - (h >= 2147483648) * 4294967296))
}

.stable_hash <- function(strings) {
    # Sort + NUL-join so that group_names is member-order invariant
    # but sensitive to membership and to entry names.
    payload <- paste(sort(unique(strings)), collapse = "\x00")
    .fnv32_hex(charToRaw(payload))
}

# ---- compact_groups --------------------------------------------------------

#' Renumber group indices to be dense in 1..N.
#'
#' Given a vector where element `i` is the group index for entry `i`
#' (0 denotes "no group"), return a list with `n_groups` (the number
#' of unique non-zero groups) and `group_indices` (the renumbered
#' indices, using first-seen order). Zeros are preserved.
#'
#' @param group_indices Integer vector (or coercible).
#' @return `list(n_groups = integer(1), group_indices = integer(N))`.
#' @examples
#' compact_groups(c(5L, 10L, 5L, 0L, 10L))
#' # $n_groups = 2; $group_indices = c(1, 2, 1, 0, 2)
#' @seealso [collect_group_members()], [group_names()]
#' @export
compact_groups <- function(group_indices) {
    group_indices <- as.integer(group_indices)
    if (any(!is.na(group_indices) & group_indices < 0L)) {
        cli::cli_abort(
            "`group_indices` must be >= 0 (0 denotes no group)",
            call = NULL
        )
    }
    n_groups <- 0L
    out <- group_indices
    seen <- new.env(parent = emptyenv(), hash = TRUE)
    for (i in seq_along(group_indices)) {
        gi <- group_indices[[i]]
        if (is.na(gi) || gi == 0L) {
            next
        }
        key <- as.character(gi)
        compact <- seen[[key]]
        if (is.null(compact)) {
            n_groups <- n_groups + 1L
            compact <- n_groups
            seen[[key]] <- compact
        }
        out[[i]] <- compact
    }
    list(n_groups = n_groups, group_indices = out)
}

# ---- collect_group_members -------------------------------------------------

#' Invert group-index assignment into per-group entry lists.
#'
#' For each non-zero group in `group_indices`, returns the integer
#' positions that belong to it. Entries with index 0 are omitted.
#'
#' @inheritParams compact_groups
#' @return A list of integer vectors. Length equals `max(group_indices)`.
#' @examples
#' collect_group_members(c(1L, 2L, 1L, 0L, 2L))
#' # list(c(1, 3), c(2, 5))
#' @seealso [compact_groups()], [group_names()]
#' @export
collect_group_members <- function(group_indices) {
    group_indices <- as.integer(group_indices)
    if (length(group_indices) == 0L) {
        return(list())
    }
    if (any(!is.na(group_indices) & group_indices < 0L)) {
        cli::cli_abort(
            "`group_indices` must be >= 0 (0 denotes no group)",
            call = NULL
        )
    }
    n_groups <- max(0L, max(group_indices, na.rm = TRUE))
    if (n_groups == 0L) {
        return(list())
    }
    out <- vector("list", n_groups)
    for (i in seq_along(group_indices)) {
        gi <- group_indices[[i]]
        if (!is.na(gi) && gi > 0L) {
            out[[gi]] <- c(out[[gi]], i)
        }
    }
    out <- lapply(out, function(v) if (is.null(v)) integer(0L) else v)
    out
}

# ---- group_names -----------------------------------------------------------

#' Deterministic names for groups of axis entries.
#'
#' For each group in `entries_of_groups`, build a name from a stable
#' hash of the member entry names. Same-members → same name across
#' sessions and dafs.
#'
#' @param daf A [DafReader].
#' @param axis Axis name whose entries the group indices reference.
#' @param entries_of_groups A list of integer vectors; each vector is
#'   the 1-based axis positions belonging to the group.
#' @param prefix Character scalar prepended to each name.
#' @return Character vector of length `length(entries_of_groups)`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
#' group_names(d, "cell", list(c(1L, 2L), c(3L, 4L)), prefix = "grp_")
#' @seealso [compact_groups()], [collect_group_members()]
#' @export
group_names <- function(daf, axis, entries_of_groups, prefix) {
    stopifnot("`daf` must be a DafReader" = S7::S7_inherits(daf, DafReader))
    stopifnot(
        "`entries_of_groups` must be a list" = is.list(entries_of_groups),
        "`prefix` must be a character scalar" =
            is.character(prefix) && length(prefix) == 1L
    )
    entry_names <- format_axis_array(daf, axis)
    n <- length(entry_names)
    vapply(entries_of_groups, function(members) {
        members <- as.integer(members)
        if (length(members) && (any(members < 1L) || any(members > n))) {
            cli::cli_abort(
                "group member index out of range for axis {.val {axis}} (1..{n})",
                call = NULL
            )
        }
        names <- entry_names[members]
        paste0(prefix, .stable_hash(names))
    }, character(1L))
}
```

- [ ] **Step D.4: Regen docs + run tests**

```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: all tests green.

- [ ] **Step D.5: Commit**

```
git add R/groups.R tests/testthat/test-groups.R NAMESPACE man/
git commit -m "feat(10c): add compact_groups/collect_group_members/group_names"
```

---

## Phase E: Class-surface sugar

**Files:**
- Modify: `R/classes.R` (append `is_daf`, `daf_name`)
- Modify: `R/complete.R` (append `complete_path` public export)
- Modify: `R/chain_daf.R` (append `read_only` factory)
- Create: `tests/testthat/test-class-sugar.R`

**Preconditions:** Phase D complete.

- [ ] **Step E.1: Write the failing tests**

Create `tests/testthat/test-class-sugar.R`:

```r
test_that("is_daf recognises every daf subclass and rejects non-dafs", {
    d_mem <- memory_daf(name = "m")
    expect_true(is_daf(d_mem))
    overlay <- memory_daf(name = "o")
    d_chain <- chain_reader(list(d_mem, overlay))
    expect_true(is_daf(d_chain))
    expect_false(is_daf(NULL))
    expect_false(is_daf(list()))
    expect_false(is_daf(42L))
    expect_false(is_daf("memory_daf"))
})

test_that("daf_name returns the name property", {
    d <- memory_daf(name = "hello")
    expect_identical(daf_name(d), "hello")
})

test_that("daf_name errors on non-daf input", {
    expect_error(daf_name(NULL), "DafReader")
    expect_error(daf_name(42L), "DafReader")
})

test_that("complete_path matches the internal .complete_path", {
    tmp <- tempfile("dafr-10c-")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    fd <- files_daf(tmp, mode = "w+", name = "fd")
    expect_identical(complete_path(fd), dafr:::.complete_path(fd))
})

test_that("read_only wraps a writer into a read-only chain", {
    d <- memory_daf(name = "inner")
    add_axis(d, "cell", c("c1", "c2"))
    set_scalar(d, "x", "y")
    ro <- read_only(d)
    expect_true(is_daf(ro))
    expect_s3_class(ro, "dafr::DafReadOnly")
    expect_identical(daf_name(ro), "inner")
    expect_identical(get_scalar(ro, "x"), "y")
    expect_error(set_scalar(ro, "z", "w"))
})

test_that("read_only accepts an explicit name override", {
    d <- memory_daf(name = "inner")
    ro <- read_only(d, name = "outer")
    expect_identical(daf_name(ro), "outer")
})

test_that("read_only errors on non-daf input", {
    expect_error(read_only(NULL), "DafReader")
})
```

- [ ] **Step E.2: Run tests to verify they fail**

```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R 2>&1 | grep "test-class-sugar" | head
```

Expected: FAIL — functions not exported.

- [ ] **Step E.3: Add is_daf + daf_name to R/classes.R**

Append to `R/classes.R`:

```r
# ---- Class-surface sugar ---------------------------------------------------

#' Test whether an object is a `DafReader`.
#'
#' Non-throwing predicate for any of the S7 class descendants
#' (`MemoryDaf`, `FilesDaf`, `ReadOnlyChainDaf`, `WriteChainDaf`,
#' `ViewDaf`, `ContractDaf`, ...).
#'
#' @param x Any R object.
#' @return `TRUE` if `x` inherits from [DafReader], else `FALSE`.
#' @examples
#' is_daf(memory_daf())
#' is_daf(NULL)
#' @export
is_daf <- function(x) S7::S7_inherits(x, DafReader)

#' Return the name of a `DafReader`.
#'
#' Asserts that `x` is a [DafReader] and returns its `name` property
#' (the string passed to the constructor).
#'
#' @param x A [DafReader].
#' @return Character scalar.
#' @examples
#' daf_name(memory_daf(name = "hello"))
#' @seealso [is_daf()], [read_only()]
#' @export
daf_name <- function(x) {
    if (!is_daf(x)) {
        stop("`x` must be a DafReader", call. = FALSE)
    }
    S7::prop(x, "name")
}
```

- [ ] **Step E.4: Add complete_path to R/complete.R**

Append to `R/complete.R`:

```r
#' Canonical disk path of a (possibly chained) daf.
#'
#' Public alias of the internal `.complete_path`. For a `FilesDaf`,
#' returns the root directory on disk. For a chain whose last writer
#' is a `FilesDaf`, returns that directory. Errors on dafs with no
#' on-disk location.
#'
#' @param daf A [DafReader].
#' @return Character scalar (absolute path).
#' @examples
#' tmp <- tempfile("dafr-")
#' dir.create(tmp)
#' fd <- files_daf(tmp, mode = "w+", name = "fd")
#' complete_path(fd)
#' unlink(tmp, recursive = TRUE)
#' @export
complete_path <- function(daf) .complete_path(daf)
```

- [ ] **Step E.5: Add read_only to R/chain_daf.R**

Append to `R/chain_daf.R` (after `chain_reader`):

```r
#' Wrap a writer into a read-only view via a 1-element chain.
#'
#' Returns a `ReadOnlyChainDaf` that reads from `daf` but rejects
#' writes. Implementation delegates to [chain_reader()] with a single
#' entry; there is no separate read-only class.
#'
#' @param daf A [DafReader] (typically a [DafWriter]).
#' @param name Optional chain name; defaults to `daf_name(daf)`.
#' @return A [ReadOnlyChainDaf].
#' @examples
#' d <- memory_daf(name = "inner")
#' add_axis(d, "cell", c("c1", "c2"))
#' ro <- read_only(d)
#' daf_name(ro)
#' @seealso [chain_reader()], [is_daf()]
#' @export
read_only <- function(daf, name = NULL) {
    if (!is_daf(daf)) {
        stop("`daf` must be a DafReader", call. = FALSE)
    }
    chain_reader(list(daf), name = name %||% S7::prop(daf, "name"))
}
```

- [ ] **Step E.6: Regen docs + run tests**

```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: all tests green.

- [ ] **Step E.7: Commit**

```
git add R/classes.R R/complete.R R/chain_daf.R tests/testthat/test-class-sugar.R NAMESPACE man/
git commit -m "feat(10c): add is_daf/daf_name/complete_path/read_only"
```

---

## Phase F: DataFrame helpers

**Files:**
- Create: `R/dataframes.R`
- Modify: `R/queries.R` (remove `get_frame` export; keep logic or move)
- Modify: `DESCRIPTION` (add `rlang` Imports; add `tidyr`, `tibble` Suggests)
- Create: `tests/testthat/test-dataframes.R`
- Modify: any existing test file referencing `get_frame` (inline rename)

**Preconditions:** Phase E complete.

- [ ] **Step F.1: Find existing get_frame references in tests**

Run:
```
grep -rn "get_frame" tests/testthat/ R/ | grep -v "\.Rd"
```

Note the files + line numbers. Typical: `R/queries.R`, `tests/testthat/test-queries.R`, possibly examples in other man pages.

- [ ] **Step F.2: Write the failing tests**

Create `tests/testthat/test-dataframes.R`:

```r
test_that("get_dataframe returns a data.frame with axis-entry rownames", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2", "c3"))
    set_vector(d, "cell", "donor", c("A", "B", "A"))
    set_vector(d, "cell", "age", c(1L, 2L, 3L))
    df <- get_dataframe(d, "cell")
    expect_s3_class(df, "data.frame")
    expect_identical(rownames(df), c("c1", "c2", "c3"))
    expect_setequal(colnames(df), c("donor", "age"))
})

test_that("get_dataframe respects columns arg", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    set_vector(d, "cell", "age", c(1L, 2L))
    df <- get_dataframe(d, "cell", columns = "donor")
    expect_identical(colnames(df), "donor")
    expect_identical(df$donor, c("A", "B"))
})

test_that("get_dataframe_query is the query-string form", {
    d <- memory_daf()
    add_axis(d, "donor", c("d1", "d2", "d3"))
    set_vector(d, "donor", "age", c(20L, 30L, 40L))
    df <- get_dataframe_query(d, "@ donor")
    expect_identical(rownames(df), c("d1", "d2", "d3"))
    expect_true("age" %in% colnames(df))
})

test_that("get_dataframe cache=TRUE serves from the query cache", {
    d <- memory_daf()
    add_axis(d, "cell", c("c1", "c2"))
    set_vector(d, "cell", "donor", c("A", "B"))
    df1 <- get_dataframe(d, "cell", cache = TRUE)
    df2 <- get_dataframe(d, "cell", cache = TRUE)
    expect_identical(df1, df2)
})

test_that("get_tidy errors informatively without tidyr/tibble", {
    # tidyr and tibble are typically installed in a dev env; use a
    # fake libpath to simulate absence.
    if (requireNamespace("tidyr", quietly = TRUE) &&
        requireNamespace("tibble", quietly = TRUE)) {
        d <- memory_daf()
        add_axis(d, "cell", c("c1", "c2"))
        set_vector(d, "cell", "donor", c("A", "B"))
        tidy <- get_tidy(d, "cell")
        expect_s3_class(tidy, "tbl_df")
        expect_setequal(colnames(tidy), c("name", "key", "value"))
    } else {
        d <- memory_daf()
        expect_error(get_tidy(d, "cell"), "tidyr|tibble")
    }
})

test_that("get_frame is no longer exported", {
    expect_false("get_frame" %in% getNamespaceExports("dafr"))
})
```

- [ ] **Step F.3: Run tests to verify they fail**

```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R 2>&1 | grep "test-dataframes" | head
```

Expected: FAIL — `get_dataframe`, `get_dataframe_query`, `get_tidy` not found; `get_frame` still exported.

- [ ] **Step F.4: Update DESCRIPTION**

Edit `DESCRIPTION`:

```
Imports:
    S7,
    Matrix,
    cli,
    bit64,
    jsonlite,
    matrixStats,
    methods,
    rlang
```

And update `Suggests`:

```
Suggests:
    bench,
    knitr,
    pkgload,
    rmarkdown,
    rprojroot,
    scran,
    Seurat,
    SingleCellExperiment,
    testthat (>= 3.0.0),
    tibble,
    tidyr,
    withr
```

- [ ] **Step F.5: Create R/dataframes.R**

```r
#' @include classes.R format_api.R queries.R
NULL

# ---- Internals -------------------------------------------------------------

# Extracted body of the old get_frame, now driving both
# get_dataframe_query and get_dataframe. Kept private.
.get_dataframe_from_query <- function(daf, query_string, cache = TRUE) {
    # Cache key: route through the existing query cache if requested.
    if (isTRUE(cache)) {
        ast <- parse_query(query_string)
        canon <- .canonicalise_ast(ast)
        key <- cache_key_query(canon)
        touched <- .collect_query_versions(daf, ast)
        stamp <- .snapshot_versions(daf, touched)
        cache_env <- S7::prop(daf, "cache")
        cached <- cache_lookup(cache_env, "query", key, stamp)
        if (!is.null(cached) && is.data.frame(cached)) {
            return(cached)
        }
    }
    axis_ast <- parse_query(query_string)
    state <- list(kind = "init", value = NULL, if_missing = NULL)
    for (node in axis_ast) state <- .apply_node(node, state, daf)
    if (!identical(state$kind, "axis")) {
        stop("query did not resolve to an axis", call. = FALSE)
    }
    entries <- state$value
    axis_name <- state$axis
    columns <- format_vectors_set(daf, axis_name)
    cols <- lapply(columns, function(nm) {
        v <- format_get_vector(daf, axis_name, nm)
        full_entries <- format_axis_array(daf, axis_name)
        idx <- match(entries, full_entries)
        v[idx]
    })
    names(cols) <- columns
    df <- as.data.frame(cols,
        row.names = entries,
        stringsAsFactors = FALSE, optional = TRUE
    )
    if (isTRUE(cache)) {
        cache_store(cache_env, "query", key, df, stamp,
            size_bytes = as.numeric(object.size(df))
        )
    }
    df
}

# ---- Public exports --------------------------------------------------------

#' Extract vectors on an axis as a `data.frame`.
#'
#' Returns a `data.frame` with one column per vector on `axis`, rows
#' named by the axis entries. When `columns` is `NULL`, all vectors
#' defined on `axis` are included.
#'
#' @param daf A [DafReader].
#' @param axis Axis name.
#' @param columns Optional character vector of vector names to
#'   include. Defaults to all vectors on `axis`.
#' @param cache Logical; if `TRUE` (default), the result is memoised
#'   in the query cache and served on repeat calls.
#' @return A `data.frame`.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "cell", c("c1", "c2"))
#' set_vector(d, "cell", "donor", c("A", "B"))
#' get_dataframe(d, "cell")
#' @seealso [get_dataframe_query()], [get_tidy()]
#' @export
get_dataframe <- function(daf, axis, columns = NULL, cache = TRUE) {
    stopifnot("`daf` must be a DafReader" = is_daf(daf))
    df <- .get_dataframe_from_query(daf, sprintf("@ %s", axis), cache = cache)
    if (!is.null(columns)) {
        missing_cols <- setdiff(columns, colnames(df))
        if (length(missing_cols)) {
            stop(sprintf("columns not on axis %s: %s",
                sQuote(axis),
                paste(sQuote(missing_cols), collapse = ", ")
            ), call. = FALSE)
        }
        df <- df[, columns, drop = FALSE]
    }
    df
}

#' Extract an axis-resolving query's result as a `data.frame`.
#'
#' The query-string counterpart of [get_dataframe()]. The query must
#' resolve to an axis (possibly mask-filtered).
#'
#' @param daf A [DafReader].
#' @param query A query string resolving to an axis.
#' @param cache Logical; if `TRUE` (default), serve from the query cache.
#' @return A `data.frame` with axis-entry rownames.
#' @examples
#' d <- memory_daf()
#' add_axis(d, "donor", c("d1", "d2"))
#' set_vector(d, "donor", "age", c(20L, 30L))
#' get_dataframe_query(d, "@ donor")
#' @seealso [get_dataframe()], [get_tidy()]
#' @export
get_dataframe_query <- function(daf, query, cache = TRUE) {
    stopifnot("`daf` must be a DafReader" = is_daf(daf))
    .get_dataframe_from_query(daf, query, cache = cache)
}

#' Pivot axis vectors into a tidy long-format tibble.
#'
#' Requires `tidyr` and `tibble`; errors with an install hint if
#' either is missing.
#'
#' @inheritParams get_dataframe
#' @param ... Passed to [tidyr::pivot_longer()].
#' @return A `tibble` with columns `name`, `key`, `value`.
#' @examples
#' if (requireNamespace("tidyr", quietly = TRUE) &&
#'     requireNamespace("tibble", quietly = TRUE)) {
#'     d <- memory_daf()
#'     add_axis(d, "cell", c("c1", "c2"))
#'     set_vector(d, "cell", "donor", c("A", "B"))
#'     get_tidy(d, "cell")
#' }
#' @seealso [get_dataframe()], [get_dataframe_query()]
#' @export
get_tidy <- function(daf, axis, columns = NULL, cache = TRUE, ...) {
    rlang::check_installed(
        c("tidyr", "tibble"),
        reason = "for `get_tidy()`"
    )
    df <- get_dataframe(daf, axis, columns = columns, cache = cache)
    df$name <- rownames(df)
    rownames(df) <- NULL
    tib <- tibble::as_tibble(df)
    tidyr::pivot_longer(tib, -"name",
        names_to = "key", values_to = "value", ...
    )
}
```

- [ ] **Step F.6: Remove get_frame from R/queries.R**

Edit `R/queries.R`. Remove the block at lines 238–271 (the `get_frame` docstring + function) **entirely**. Also update the top-of-file comment on lines 4–6 to drop `get_frame`:

```r
# Public entry points: parse_query, get_query, has_query,
# is_axis_query, query_axis_name, query_result_dimensions,
# query_requires_relayout.
```

- [ ] **Step F.7: Rename any existing get_frame references in tests**

For each file found in Step F.1 under `tests/testthat/`, replace `get_frame(` with `get_dataframe_query(`. Use a safe shell command or edit each file.

Example:
```
grep -l "get_frame(" tests/testthat/*.R
# for each file listed:
#   sed -i 's/get_frame(/get_dataframe_query(/g' tests/testthat/<file>
```

(Prefer the `Edit` tool on each file for auditability.)

- [ ] **Step F.8: Regen docs + run tests**

```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: all tests green. `NAMESPACE` gains `export(get_dataframe)`, `export(get_dataframe_query)`, `export(get_tidy)`; loses `export(get_frame)`.

- [ ] **Step F.9: Commit**

```
git add R/dataframes.R R/queries.R DESCRIPTION NAMESPACE man/ tests/testthat/
git commit -m "$(cat <<'EOF'
refactor(10c): add get_dataframe{,_query}/get_tidy; rename get_frame

- New get_dataframe(daf, axis, columns = NULL, cache = TRUE) — axis-name form.
- New get_dataframe_query(daf, query, cache = TRUE) — query-string form
  (replaces get_frame; no compat shim, pre-1.0 package).
- New get_tidy(daf, axis, columns, cache, ...) — tidyr/tibble gated via
  rlang::check_installed.
- DESCRIPTION: rlang → Imports; tidyr, tibble → Suggests.
EOF
)"
```

---

## Phase G: Contract UX

**Files:**
- Modify: `R/contracts.R` (extend validator + `.data_key` + `.verify_contract` + `.verify_access` + add `.verify_tensor_data`)
- Create: `R/contract_ux.R` (5 new exports)
- Create: `tests/testthat/test-contract-ux.R`

**Preconditions:** Phase F complete.

- [ ] **Step G.1: Write the failing tests**

Create `tests/testthat/test-contract-ux.R`:

```r
test_that("axis_contract builds an axis record", {
    a <- axis_contract("cell", RequiredInput, "per-cell axis")
    expect_identical(a$kind, "axis")
    expect_identical(a$name, "cell")
    expect_identical(a$expectation, RequiredInput)
    expect_identical(a$description, "per-cell axis")
})

test_that("tensor_contract builds a tensor record with kind=tensor", {
    t <- tensor_contract("batch", "cell", "gene", "UMIs",
        RequiredInput, "integer", "per-batch umi matrix"
    )
    expect_identical(t$kind, "tensor")
    expect_identical(t$main_axis, "batch")
    expect_identical(t$rows_axis, "cell")
    expect_identical(t$columns_axis, "gene")
    expect_identical(t$name, "UMIs")
    expect_identical(t$expectation, RequiredInput)
    expect_identical(t$type, "integer")
})

test_that("create_contract returns a Contract with concatenated data", {
    c <- create_contract(
        scalars = list(contract_scalar("organism", RequiredInput, "character", "species")),
        vectors = list(contract_vector("cell", "donor", RequiredInput, "character", "donor")),
        matrices = list(contract_matrix("cell", "gene", "UMIs", RequiredInput, "integer", "UMIs")),
        axes = list(
            axis_contract("cell", RequiredInput, "per-cell axis"),
            axis_contract("gene", RequiredInput, "per-gene axis")
        )
    )
    expect_s4_class(c, "dafr::Contract")
    expect_length(c@data, 3L)
    expect_named(c@axes, c("cell", "gene"))
})

test_that("create_contract accepts tensors and stores them in data with kind=tensor", {
    c <- create_contract(
        tensors = list(tensor_contract("batch", "cell", "gene", "UMIs",
            RequiredInput, "integer", "per-batch"
        )),
        axes = list(axis_contract("batch", RequiredInput, ""),
                    axis_contract("cell", RequiredInput, ""),
                    axis_contract("gene", RequiredInput, ""))
    )
    expect_length(c@data, 1L)
    expect_identical(c@data[[1L]]$kind, "tensor")
})

test_that("create_contract rejects wrong-kind elements in typed lists", {
    v <- contract_vector("cell", "donor", RequiredInput, "character", "d")
    expect_error(
        create_contract(scalars = list(v)),
        "scalars"
    )
})

test_that("contract_docs returns a character scalar", {
    c <- create_contract(
        axes = list(axis_contract("cell", RequiredInput, "per-cell axis")),
        vectors = list(contract_vector("cell", "donor", RequiredInput, "character", "donor id"))
    )
    md <- contract_docs(c, format = "markdown")
    expect_type(md, "character")
    expect_length(md, 1L)
    expect_true(grepl("cell", md, fixed = TRUE))
    txt <- contract_docs(c, format = "text")
    expect_type(txt, "character")
})

test_that("verify_contract green path succeeds", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf()
        add_axis(d, "cell", c("c1", "c2"))
        set_vector(d, "cell", "donor", c("A", "B"))
        c <- create_contract(
            axes = list(axis_contract("cell", RequiredInput, "")),
            vectors = list(contract_vector("cell", "donor", RequiredInput,
                "character", "donor id"))
        )
        expect_silent(verify_contract(c, d))
    })
})

test_that("verify_contract errors on missing required axis", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf()
        c <- create_contract(
            axes = list(axis_contract("cell", RequiredInput, ""))
        )
        expect_error(verify_contract(c, d), "missing.*axis.*cell")
    })
})

test_that("verify_contract succeeds on tensor when main-axis matrices exist", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf()
        add_axis(d, "batch", c("b1", "b2"))
        add_axis(d, "cell", c("c1", "c2"))
        add_axis(d, "gene", c("g1", "g2", "g3"))
        set_matrix(d, "cell", "gene", "b1_UMIs",
            matrix(1:6, 2, 3))
        set_matrix(d, "cell", "gene", "b2_UMIs",
            matrix(7:12, 2, 3))
        c <- create_contract(
            axes = list(
                axis_contract("batch", RequiredInput, ""),
                axis_contract("cell", RequiredInput, ""),
                axis_contract("gene", RequiredInput, "")
            ),
            tensors = list(tensor_contract("batch", "cell", "gene",
                "UMIs", RequiredInput, "integer", ""))
        )
        expect_silent(verify_contract(c, d))
    })
})

test_that("verify_contract errors on tensor with missing per-entry matrix", {
    withr::with_options(list(dafr.enforce_contracts = TRUE), {
        d <- memory_daf()
        add_axis(d, "batch", c("b1", "b2"))
        add_axis(d, "cell", c("c1", "c2"))
        add_axis(d, "gene", c("g1", "g2"))
        set_matrix(d, "cell", "gene", "b1_UMIs",
            matrix(1:4, 2, 2))
        # b2_UMIs is deliberately missing.
        c <- create_contract(
            axes = list(
                axis_contract("batch", RequiredInput, ""),
                axis_contract("cell", RequiredInput, ""),
                axis_contract("gene", RequiredInput, "")
            ),
            tensors = list(tensor_contract("batch", "cell", "gene",
                "UMIs", RequiredInput, "integer", ""))
        )
        expect_error(verify_contract(c, d), "b2_UMIs")
    })
})

test_that("existing slice-7 Contract() construction path still works", {
    # Regression guard: flat data slot still accepts mixed records.
    c <- Contract(
        axes = list(cell = list(RequiredInput, "per-cell axis")),
        data = list(contract_scalar("organism", RequiredInput, "character", "s"))
    )
    expect_length(c@data, 1L)
    expect_identical(c@data[[1L]]$kind, "scalar")
})

test_that("Contract validator accepts tensor records in data", {
    rec <- tensor_contract("batch", "cell", "gene", "UMIs",
        RequiredInput, "integer", "")
    expect_silent(Contract(data = list(rec)))
})
```

- [ ] **Step G.2: Run tests to verify they fail**

```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R 2>&1 | grep "test-contract-ux" | head
```

Expected: FAIL — none of the new exports exist; `Contract` validator rejects tensor records.

- [ ] **Step G.3: Extend R/contracts.R for tensor records**

**Edit 1.** Find the `Contract` validator block at `R/contracts.R:66-87`. The current validator walks `self@data` expecting each record to have `$kind` ∈ `{"scalar","vector","matrix"}` and `$expectation` + `$type`. Extend it to accept `"tensor"` records. Replace the validator body (approximately the `for (i in seq_along(self@data))` loop) with:

```r
    for (i in seq_along(self@data)) {
        rec <- self@data[[i]]
        if (!is.list(rec) || !("kind" %in% names(rec))) {
            return(sprintf("data[[%d]] must be a record with $kind", i))
        }
        if (!(rec$kind %in% c("scalar", "vector", "matrix", "tensor"))) {
            return(sprintf(
                "data[[%d]] has unknown $kind: %s", i, sQuote(rec$kind)
            ))
        }
        .assert_expectation(rec$expectation, sprintf("data[[%d]] expectation", i))
        .assert_type(rec$type, sprintf("data[[%d]] type", i))
    }
```

**Edit 2.** Find `.data_key` (around `R/contracts.R:206`; grep to confirm). Extend it to handle `kind = "tensor"`:

```r
.data_key <- function(rec) {
    switch(rec$kind,
        scalar = .access_key_scalar(rec$name),
        vector = .access_key_vector(rec$axis, rec$name),
        matrix = .access_key_matrix(rec$rows_axis, rec$columns_axis, rec$name),
        tensor = sprintf("tensor:%s:%s:%s:%s",
            rec$main_axis, rec$rows_axis, rec$columns_axis, rec$name),
        stop(sprintf("unknown kind: %s", sQuote(rec$kind)), call. = FALSE)
    )
}
```

**Edit 3.** Add a new `.verify_tensor_data` helper near the existing `.verify_matrix_data` (around `R/contracts.R:674+`):

```r
.verify_tensor_data <- function(cd, rec, is_for_output) {
    base <- S7::prop(cd, "base")
    comp <- S7::prop(cd, "computation")
    dname <- S7::prop(base, "name")
    main <- rec$main_axis
    ra <- rec$rows_axis
    ca <- rec$columns_axis
    if (!format_has_axis(base, main)) {
        if (.is_mandatory(rec$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s tensor: %s main axis: %s not present for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), rec$name, main, comp, dname
            ), call. = FALSE)
        }
        return(invisible())
    }
    entries <- format_axis_array(base, main)
    for (entry in entries) {
        mat_name <- sprintf("%s_%s", entry, rec$name)
        has_it <- format_has_matrix(base, ra, ca, mat_name)
        if (!has_it && .is_mandatory(rec$expectation, is_for_output)) {
            stop(sprintf(
                "missing %s tensor matrix: %s of the rows axis: %s and the columns axis: %s for the computation: %s on the daf data: %s",
                .direction_name(is_for_output), mat_name, ra, ca, comp, dname
            ), call. = FALSE)
        }
        if (has_it && .is_forbidden(rec$expectation, is_for_output,
            S7::prop(cd, "overwrite"))) {
            stop(sprintf(
                "pre-existing %s tensor matrix: %s for the computation: %s on the daf data: %s",
                rec$expectation, mat_name, comp, dname
            ), call. = FALSE)
        }
    }
    invisible()
}
```

**Edit 4.** Extend `.verify_contract` at `R/contracts.R:780+`. In the `for (key in ls(...))` loop, add a `"tensor"` branch to the switch:

```r
        rec <- switch(parts[[1L]],
            scalar = list(kind = "scalar", name = parts[[2L]],
                          expectation = tracker$expectation, type = tracker$type),
            vector = list(kind = "vector", axis = parts[[2L]], name = parts[[3L]],
                          expectation = tracker$expectation, type = tracker$type),
            matrix = list(kind = "matrix", rows_axis = parts[[2L]],
                          columns_axis = parts[[3L]], name = parts[[4L]],
                          expectation = tracker$expectation, type = tracker$type),
            tensor = list(kind = "tensor", main_axis = parts[[2L]],
                          rows_axis = parts[[3L]], columns_axis = parts[[4L]],
                          name = parts[[5L]],
                          expectation = tracker$expectation, type = tracker$type)
        )
        switch(rec$kind,
            scalar = .verify_scalar_data(cd, rec, is_for_output),
            vector = .verify_vector_data(cd, rec, is_for_output),
            matrix = .verify_matrix_data(cd, rec, is_for_output),
            tensor = .verify_tensor_data(cd, rec, is_for_output)
        )
```

**Edit 5.** Extend `.verify_access` at `R/contracts.R:732+` to recognise `"tensor"` key kind. Inside the `for (key in ...)` block, add an `else if (kind == "tensor")` branch that iterates the main-axis entries and skips the "unused RequiredInput" diagnostic if all per-entry matrices exist. Minimal safe implementation: skip the check for tensors (tensors are always considered "accessed" at verify time to avoid false positives):

```r
        } else if (kind == "matrix") {
            if (format_has_axis(base, parts[[2L]]) &&
                format_has_axis(base, parts[[3L]]) &&
                format_has_matrix(base, parts[[2L]], parts[[3L]], parts[[4L]])) {
                stop(sprintf(
                    "unused RequiredInput matrix: %s of the rows axis: %s and the columns axis: %s of the computation: %s on the daf data: %s",
                    parts[[4L]], parts[[2L]], parts[[3L]], comp, dname
                ), call. = FALSE)
            }
        } else if (kind == "tensor") {
            # Skip unused-check for tensors; access tracking for
            # per-entry matrices would require a separate tracker per
            # entry. Future work.
            next
        }
```

- [ ] **Step G.4: Create R/contract_ux.R**

```r
#' @include classes.R contracts.R
NULL

# ---- axis_contract, tensor_contract record constructors --------------------

#' Axis-contract record.
#'
#' Builds an axis specification record for use in
#' [create_contract()]'s `axes` argument.
#'
#' @param name Axis name.
#' @param expectation One of the [expectation-constants] (e.g.
#'   [RequiredInput], [CreatedOutput]).
#' @param description Free-text description (character scalar).
#' @return A list record with class `"dafr_axis_contract"`.
#' @examples
#' axis_contract("cell", RequiredInput, "per-cell axis")
#' @seealso [create_contract()], [tensor_contract()], [expectation-constants]
#' @export
axis_contract <- function(name, expectation, description) {
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    if (!is.character(description) || length(description) != 1L) {
        stop("`description` must be a character scalar", call. = FALSE)
    }
    structure(
        list(
            kind = "axis",
            name = name,
            expectation = expectation,
            description = description
        ),
        class = "dafr_axis_contract"
    )
}

#' Tensor-contract record.
#'
#' Builds a tensor specification record for use in
#' [create_contract()]'s `tensors` argument. A tensor is a 3-D
#' structure stored as per-main-axis-entry matrices named
#' `<entry>_<name>` on `(rows_axis, columns_axis)`.
#'
#' @param main_axis Axis whose entries index the tensor slices.
#' @param rows_axis,columns_axis Axis names for each per-entry matrix.
#' @param name Tensor name; individual matrices will be
#'   `<main_axis_entry>_<name>`.
#' @param expectation One of the [expectation-constants].
#' @param type R class name of the matrix values
#'   (e.g. `"integer"`, `"numeric"`).
#' @param description Free-text description.
#' @return A list record with `$kind = "tensor"`.
#' @examples
#' tensor_contract("batch", "cell", "gene", "UMIs",
#'     RequiredInput, "integer", "per-batch UMI matrices")
#' @seealso [create_contract()], [axis_contract()]
#' @export
tensor_contract <- function(main_axis, rows_axis, columns_axis, name,
                            expectation, type, description) {
    .assert_name(main_axis, "main_axis")
    .assert_name(rows_axis, "rows_axis")
    .assert_name(columns_axis, "columns_axis")
    .assert_name(name, "name")
    .assert_expectation(expectation, "expectation")
    .assert_type(type, "type")
    if (!is.character(description) || length(description) != 1L) {
        stop("`description` must be a character scalar", call. = FALSE)
    }
    list(
        kind = "tensor",
        main_axis = main_axis,
        rows_axis = rows_axis,
        columns_axis = columns_axis,
        name = name,
        expectation = expectation,
        type = type,
        description = description
    )
}

# ---- create_contract -------------------------------------------------------

.assert_kind_list <- function(x, kind, arg) {
    if (!is.list(x)) {
        stop(sprintf("`%s` must be a list", arg), call. = FALSE)
    }
    for (i in seq_along(x)) {
        k <- x[[i]]$kind
        if (is.null(k) || !identical(k, kind)) {
            stop(sprintf(
                "`%s[[%d]]` must have kind %s (got %s)",
                arg, i, sQuote(kind),
                if (is.null(k)) "NULL" else sQuote(k)
            ), call. = FALSE)
        }
    }
    invisible()
}

#' Construct a [Contract()] from typed per-category argument lists.
#'
#' User-friendly constructor that concatenates `scalars`, `vectors`,
#' `matrices`, and `tensors` into the flat `data` slot of [Contract()],
#' and converts `axes` (a list of [axis_contract()] records) into the
#' named-list form the underlying class expects.
#'
#' @param scalars List of [contract_scalar()] records.
#' @param vectors List of [contract_vector()] records.
#' @param matrices List of [contract_matrix()] records.
#' @param tensors List of [tensor_contract()] records.
#' @param axes List of [axis_contract()] records.
#' @param is_relaxed Logical; if `TRUE`, accesses to properties
#'   outside the contract are allowed at enforcement time.
#' @return A [Contract()] object.
#' @examples
#' create_contract(
#'     axes = list(
#'         axis_contract("cell", RequiredInput, "per-cell axis"),
#'         axis_contract("gene", RequiredInput, "per-gene axis")
#'     ),
#'     scalars = list(contract_scalar("organism", RequiredInput, "character", "species")),
#'     vectors = list(contract_vector("cell", "donor", RequiredInput, "character", "donor id")),
#'     matrices = list(contract_matrix("cell", "gene", "UMIs", RequiredInput, "integer", "UMIs"))
#' )
#' @seealso [Contract()], [axis_contract()], [tensor_contract()],
#'   [verify_contract()], [contract_docs()]
#' @export
create_contract <- function(scalars = list(),
                            vectors = list(),
                            matrices = list(),
                            tensors = list(),
                            axes = list(),
                            is_relaxed = FALSE) {
    .assert_kind_list(scalars, "scalar", "scalars")
    .assert_kind_list(vectors, "vector", "vectors")
    .assert_kind_list(matrices, "matrix", "matrices")
    .assert_kind_list(tensors, "tensor", "tensors")
    .assert_kind_list(axes, "axis", "axes")
    if (!is.logical(is_relaxed) || length(is_relaxed) != 1L || is.na(is_relaxed)) {
        stop("`is_relaxed` must be TRUE or FALSE", call. = FALSE)
    }
    axes_named <- stats::setNames(
        lapply(axes, function(a) list(a$expectation, a$description)),
        vapply(axes, `[[`, character(1L), "name")
    )
    Contract(
        name       = "",
        is_relaxed = is_relaxed,
        axes       = axes_named,
        data       = c(scalars, vectors, matrices, tensors)
    )
}

# ---- contract_docs ---------------------------------------------------------

.format_expectation <- function(e) e

.render_axis_row <- function(a_name, spec, format) {
    sprintf("%s | %s | %s",
        a_name, .format_expectation(spec[[1L]]), spec[[2L]]
    )
}

.render_data_row <- function(rec, format) {
    key <- switch(rec$kind,
        scalar = rec$name,
        vector = sprintf("%s / %s", rec$axis, rec$name),
        matrix = sprintf("%s x %s / %s",
            rec$rows_axis, rec$columns_axis, rec$name),
        tensor = sprintf("%s: %s x %s / %s",
            rec$main_axis, rec$rows_axis, rec$columns_axis, rec$name)
    )
    sprintf("%s | %s | %s | %s",
        key, rec$kind, .format_expectation(rec$expectation),
        if (is.null(rec$description)) "" else rec$description
    )
}

#' Render a [Contract()] as human-readable documentation.
#'
#' Returns a single character string describing the axes and data
#' entries. `format = "markdown"` renders as pipe-delimited tables;
#' `format = "text"` uses indented lines.
#'
#' @param contract A [Contract()].
#' @param format One of `"markdown"` or `"text"`.
#' @return Character scalar.
#' @examples
#' c <- create_contract(
#'     axes = list(axis_contract("cell", RequiredInput, "per-cell axis")),
#'     vectors = list(contract_vector("cell", "donor", RequiredInput,
#'         "character", "donor id"))
#' )
#' cat(contract_docs(c), "\n")
#' @seealso [create_contract()], [verify_contract()]
#' @export
contract_docs <- function(contract, format = c("markdown", "text")) {
    format <- match.arg(format)
    if (!S7::S7_inherits(contract, Contract)) {
        stop("`contract` must be a Contract (see create_contract())", call. = FALSE)
    }
    axes_lines <- vapply(names(contract@axes),
        function(n) .render_axis_row(n, contract@axes[[n]], format),
        character(1L)
    )
    data_lines <- vapply(contract@data,
        function(r) .render_data_row(r, format),
        character(1L)
    )
    if (format == "markdown") {
        parts <- c(
            "## Axes",
            "",
            "name | expectation | description",
            "---- | ----------- | -----------",
            axes_lines,
            "",
            "## Data",
            "",
            "key | kind | expectation | description",
            "--- | ---- | ----------- | -----------",
            data_lines
        )
    } else {
        parts <- c(
            "Axes:",
            paste0("  ", axes_lines),
            "",
            "Data:",
            paste0("  ", data_lines)
        )
    }
    paste(parts, collapse = "\n")
}

# ---- verify_contract -------------------------------------------------------

#' Single-pass contract verification (input + output).
#'
#' Wraps `daf` in a fresh [contractor()] call using `contract`, then
#' runs [verify_input()] and [verify_output()]. Errors early with a
#' diagnostic on contract violation; returns `invisible(daf)` on
#' success.
#'
#' @param contract A [Contract()].
#' @param daf A [DafReader].
#' @return Invisibly `daf`.
#' @examples
#' withr::with_options(list(dafr.enforce_contracts = TRUE), {
#'     d <- memory_daf()
#'     add_axis(d, "cell", c("c1", "c2"))
#'     set_vector(d, "cell", "donor", c("A", "B"))
#'     c <- create_contract(
#'         axes = list(axis_contract("cell", RequiredInput, "per-cell axis")),
#'         vectors = list(contract_vector("cell", "donor", RequiredInput,
#'             "character", "donor id"))
#'     )
#'     verify_contract(c, d)
#' })
#' @seealso [create_contract()], [verify_input()], [verify_output()],
#'   [contractor()]
#' @export
verify_contract <- function(contract, daf) {
    if (!S7::S7_inherits(contract, Contract)) {
        stop("`contract` must be a Contract (see create_contract())", call. = FALSE)
    }
    if (!is_daf(daf)) {
        stop("`daf` must be a DafReader", call. = FALSE)
    }
    cd <- contractor("verify_contract", contract, daf, overwrite = FALSE)
    if (!S7::S7_inherits(cd, ContractDaf)) {
        # Enforcement disabled (options(dafr.enforce_contracts = FALSE));
        # contractor returns daf unchanged. Nothing to verify.
        return(invisible(daf))
    }
    verify_input(cd)
    verify_output(cd)
    invisible(daf)
}
```

- [ ] **Step G.5: Regen docs + run tests**

```
Rscript -e 'devtools::document()' &&
R CMD INSTALL . &&
cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: all tests green. `NAMESPACE` gains 5 new exports; existing slice-7 contract tests remain green.

- [ ] **Step G.6: Commit**

```
git add R/contracts.R R/contract_ux.R tests/testthat/test-contract-ux.R NAMESPACE man/
git commit -m "$(cat <<'EOF'
feat(10c): add create_contract/axis_contract/tensor_contract/contract_docs/verify_contract

Contract UX layer over the existing record-constructor family.
create_contract presents a typed per-category API
(scalars/vectors/matrices/tensors/axes) and concatenates into the
flat Contract@data slot. tensor_contract is new; verify walks extended
via a $kind="tensor" branch. No Contract S7 schema change.
EOF
)"
```

---

## Phase Z: Polish + NEWS + tag

**Files:**
- Modify: `NEWS.md` (add `# dafr 0.1.0` top entry with 10c deltas; fold existing per-slice bullets under `## Development history` / `### Slice 10c`).
- Modify: `DESCRIPTION` (no change here unless `devtools::check()` surfaces missing entries).

**Preconditions:** Phases A–G complete and green.

- [ ] **Step Z.1: Write the NEWS scratch entry**

Read the current `NEWS.md`:

```
cat NEWS.md | head -40
```

Prepend a new `### Slice 10c — Wrapper-parity surface` section (inside the existing top-level header if one exists, otherwise at the top). Content:

```markdown
### Slice 10c — Wrapper-parity surface (2026-04-23)

**New exports (25):**

- Handler constants: `ERROR_HANDLER`, `WARN_HANDLER`, `IGNORE_HANDLER`, `inefficient_action_handler()`.
- Query introspection: `escape_value()`, `unescape_value()`, `query_requires_relayout()`.
- Version counters: `axis_version_counter()`, `vector_version_counter()`, `matrix_version_counter()`.
- Group helpers: `compact_groups()`, `collect_group_members()`, `group_names()`.
- Class-surface sugar: `is_daf()`, `daf_name()`, `complete_path()`, `read_only()`.
- DataFrame helpers: `get_dataframe()`, `get_dataframe_query()`, `get_tidy()`.
- Contract UX: `create_contract()`, `axis_contract()`, `tensor_contract()`, `contract_docs()`, `verify_contract()`.

**Breaking changes vs. `dafJuliaWrapper` (Julia-facade):**

- `get_frame` was renamed to `get_dataframe_query`. No compatibility
  shim (native package is pre-1.0). Users of the wrapper's `get_frame`
  should migrate to `get_dataframe_query` (query-string form) or
  `get_dataframe` (axis-name form).
- `create_contract` takes typed per-category args
  (`scalars`/`vectors`/`matrices`/`tensors`/`axes`) rather than the
  wrapper's flat `data = list(...)`. There is no `name` field on a
  contract; the computation name lives on `contractor()`.
- `tensor_contract` parameter is now `type` (aligning with native's
  existing `contract_scalar`/`contract_vector`/`contract_matrix`)
  rather than the wrapper's `dtype`.
- `axis_version_counter` / `vector_version_counter` /
  `matrix_version_counter` return `integer(1)`, not character. Native
  counters are per-process and fit comfortably in R's signed integer
  range.

**New Imports / Suggests:**

- `rlang` → `Imports` (used for `check_installed` gating in `get_tidy`).
- `tidyr`, `tibble` → `Suggests` (required only for `get_tidy`).

**Deliberately deferred:**

- `h5df` HDF5-backed Daf store (post-0.1.0).
- `set_seed` Julia RNG hook (not applicable to native).
- AnnData facade + h5ad round-trip (slice 10b).
- Query builder functions (`Axis()`, `LookupVector()`, …) (slice 10a).
- Tensor `.verify_access` tracking — tensors are currently never
  flagged as "unused RequiredInput".
```

- [ ] **Step Z.2: Run full devtools::check**

```
Rscript -e 'devtools::check(error_on = "never", vignettes = FALSE)' 2>&1 | tee /tmp/slice-10c-check.log | tail -80
```

Expected: 0 ERROR, 0 WARNING. NOTEs pre-existing and documented elsewhere are acceptable. New NOTEs on the 10c surface must be resolved (likely: missing `@return` / undocumented args; fix inline).

- [ ] **Step Z.3: Final full test run**

```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: full suite green (modulo the pre-existing `test-helpers.R:26` skip).

- [ ] **Step Z.4: Commit NEWS**

```
git add NEWS.md
git commit -m "docs(10c): add NEWS entry for slice 10c — 25 new exports"
```

- [ ] **Step Z.5: Merge to main**

```
git checkout main
git merge --no-ff slice-10c -m "merge(10c): wrapper-parity small ports — 25 new exports"
```

- [ ] **Step Z.6: Tag and verify**

```
git tag slice-10c
git log --oneline -5
git describe --tags --exact-match HEAD
```

Expected: tag points to merge commit; `git describe` echoes `slice-10c`.

- [ ] **Step Z.7: Write exit note**

Create `dev/notes/slice-10c-exit.md` following the house style (see
`dev/notes/slice-9d-n-exit.md` for template). Contents:
- Final export count (25) vs. budget (25).
- Final assertion count vs. budget (130).
- Known issues / deferred items.
- Carry-over to 10a.
- Commit hash of merge.

Commit in the `dev/` nested repo:

```
cd dev
git add notes/slice-10c-exit.md
git commit -m "notes(10c): add exit note for slice 10c"
cd ..
```

---

## Self-review checklist

| Spec section | Plan task | Status |
|---|---|---|
| §4.1 Handler constants | Phase A | ✓ |
| §4.2 Query introspection | Phase B | ✓ |
| §4.3 Version counters | Phase C | ✓ |
| §4.4 Group helpers | Phase D | ✓ |
| §4.5 Class-surface sugar | Phase E | ✓ |
| §4.6 DataFrame helpers | Phase F | ✓ |
| §4.7 Contract UX | Phase G | ✓ |
| §5 Error handling | Distributed across A–G (tests + assertions) | ✓ |
| §6 Test plan | Per-phase tests (7 new test files) | ✓ |
| §7 Dependency changes | Phase F step F.4 (DESCRIPTION) | ✓ |
| §8 Slice execution order | Phase A–G ordering | ✓ |
| §9 Exit criterion | Phase Z | ✓ |
| Locked decision #8 (tensors, revised) | Phase G steps G.3 | ✓ |
| Locked decision #9 (FNV-32) | Phase D step D.3 | ✓ |
| Locked decision #11 (`check_installed` gate) | Phase F step F.5 | ✓ |
| Locked decision #12 (escape round-trip) | Phase B step B.1 (30-case table) | ✓ |
| Locked decision #15 (`complete_path` alias) | Phase E step E.4 | ✓ |
