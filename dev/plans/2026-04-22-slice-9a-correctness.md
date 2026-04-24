# Slice 9a Implementation Plan — Julia-parity correctness

> **For agentic workers:** REQUIRED SUB-SKILL: Use `superpowers:subagent-driven-development` to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Align `dafr` with DAF.jl on grouped-matrix operator semantics, G1 vector reduction syntax (`>>`), and Convert type-name vocabulary. After this slice, Julia-emitted queries parse and evaluate to byte-equivalent output in `dafr`.

**Architecture:** Three small, well-isolated changes to the query-evaluation pipeline — (1) a dispatch-condition inversion at `R/query_eval.R:1107-1110`, (2) a one-line parser alias at `R/query_parse.R:63`, (3) a normalization layer atop `.op_convert` at `R/operations.R:166`. Test inversion follows mechanically from (1). No changes to C++ kernels, no new dependencies, no public-export churn.

**Tech Stack:** R 4.4+, S7 multi-dispatch, cpp11 kernels (untouched here), `bit64::integer64` for Int64 (already wired). `devtools` for test + check. Julia 1.12.5 in conda env `dafr-mcview` for fixture regen.

---

## Preconditions

- Clean working tree at `main` = `f7978cc` (tag `slice-8`).
- `~/src/DataAxesFormats.jl` at `49fbba140437387a378217c2fa658d4231d0c8c1` — must match before T6 regenerates fixtures.
- Working branch: create `slice-9a-correctness` off `main`.

```bash
cd /home/aviezerl/src/dafr-native
git checkout -b slice-9a-correctness
cd ~/src/DataAxesFormats.jl && git rev-parse HEAD  # expect 49fbba14...
```

---

## File Structure

| Path | Change | Owner task |
|------|--------|------------|
| `R/query_eval.R` | Modify lines 1107–1110 + verify cascade in G4a/G4b (lines 1157–1207), fallback (lines 1066, 1084) | T1 |
| `tests/testthat/test-query-grouped-slice8.R` | Rewrite reduction tokens per swap table | T2 |
| `tests/testthat/test-query-eval-groupby.R` | Audit and invert any affected assertions | T3 |
| `R/query_parse.R` | Add one dispatch line at line 63 | T4 |
| `tests/testthat/test-query-parse.R` | New TDD tests for `>>` alias | T4 |
| `tests/testthat/test-query-eval-groupby.R` | New end-to-end `>>` test (co-located with existing G1 test) | T4 |
| `R/operations.R` | Normalize Julia type names in `.op_convert` (lines 166–197), add `integer64` branch | T5 |
| `tests/testthat/test-ops-convert.R` (or nearest `test-ops-*.R`; **discover in T5 step 1**) | TDD tests for Julia aliases + Int64 round-trip | T5 |
| `dev/scripts/regen-julia-queries-fixture.jl` | New fixture records for G1/G2/G3/Mode-char/Convert-{Int32,Int64,Bool} | T6 |
| `tests/testthat/fixtures/julia-queries/fixture.json` | Regenerated output | T6 |
| `R/view.R`, `tests/testthat/test-*complete*.R`, possibly `dev/notes/axis-rename-findings.md` | Rename re-apply investigation | T7 |
| `NEWS.md` | Slice 9a entry with **Breaking changes** section | T8 |
| `dev/notes/slice-9a-exit.md` | Exit note following Slice-8 template | T8 |

---

## Operator semantics reference (post-swap)

This table is load-bearing for T1, T2, T3, T6. Pin it mentally.

| Pattern | Before swap (R) | After swap (Julia convention) | Output shape |
|---------|-----------------|-------------------------------|--------------|
| G2 | `-/ g >\|` (rows + ReduceToColumn) | `-/ g >-` (rows + ReduceToRow) | `ngroups × ncol` |
| G3 | `\|/ g >-` (cols + ReduceToRow) | `\|/ g >\|` (cols + ReduceToColumn) | `nrow × ngroups` |
| G4a | `-/ g >-` (rows + ReduceToRow) | `-/ g >\|` (rows + ReduceToColumn) | vector, length ngroups |
| G4b | `\|/ g >\|` (cols + ReduceToColumn) | `\|/ g >-` (cols + ReduceToRow) | vector, length ngroups |
| G1 (unchanged) | `/ g >\|` | `/ g >\|` or `/ g >>` | vector, length ngroups |

**Rule of thumb:** after the swap, "group on axis X, reduce to axis X" produces the matrix (G2/G3). "Group on axis X, reduce to the other axis" produces the vector (G4a/G4b).

---

## Task 1: Semantic swap G2/G3 dispatch

**Files:**
- Modify: `R/query_eval.R:1066, 1084, 1107-1110, 1157-1207`

This is the structural change. Tests will break after this task — T2 and T3 repair them.

- [ ] **Step 1: Create branch and capture baseline test count**

```bash
cd /home/aviezerl/src/dafr-native
git checkout -b slice-9a-correctness
Rscript -e 'devtools::test()' 2>&1 | tail -5 > /tmp/slice-9a-baseline.txt
cat /tmp/slice-9a-baseline.txt
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1744 ]` (approximate; numbers documented in `dev/notes/slice-8-exit.md`).

- [ ] **Step 2: Read and confirm current G2/G3 dispatch conditions**

Read `R/query_eval.R:1107-1110` — expect exactly these four lines:

```r
    is_g2 <- identical(by, "rows") && identical(node$op, "ReduceToColumn")
    is_g3 <- identical(by, "cols") && identical(node$op, "ReduceToRow")
    is_g4a <- identical(by, "rows") && identical(node$op, "ReduceToRow")
    is_g4b <- identical(by, "cols") && identical(node$op, "ReduceToColumn")
```

If different, stop and re-read the plan's "Operator semantics reference" table — something upstream has changed.

- [ ] **Step 3: Swap the four dispatch conditions**

Edit `R/query_eval.R:1107-1110` to:

```r
    is_g2 <- identical(by, "rows") && identical(node$op, "ReduceToRow")
    is_g3 <- identical(by, "cols") && identical(node$op, "ReduceToColumn")
    is_g4a <- identical(by, "rows") && identical(node$op, "ReduceToColumn")
    is_g4b <- identical(by, "cols") && identical(node$op, "ReduceToRow")
```

- [ ] **Step 4: Fix the G4a inner-op derivation**

Read `R/query_eval.R:1157-1166`. The current code sets `inner_op <- "ReduceToColumn"`. Under the new convention, G4a is `by=rows, op=ReduceToColumn` and its inner G2 step should produce a `ngroups × ncol` matrix via `by=rows, op=ReduceToRow`. Swap the inner op:

```r
        if (is_g4a) {
            # Row-grouped + ReduceToColumn: G2 gives ngroups x ncol, then reduce
            # each row (across cols) to a scalar per group.
            inner_by <- "rows"
            inner_op <- "ReduceToRow"
            inner_state <- state
            inner_node <- list(op = inner_op,
                reduction = node$reduction, params = node$params)
            inner <- .apply_reduction_grouped_matrix(inner_node, inner_state,
                daf, by = inner_by)
```

Also update the comment on line 1158 from "Row-grouped + ReduceToRow" to "Row-grouped + ReduceToColumn".

- [ ] **Step 5: Fix the G4b inner-op derivation**

Read `R/query_eval.R:1185-1190`. Current inner op is `"ReduceToRow"`. Under the new convention, G4b is `by=cols, op=ReduceToRow` and its inner G3 step should produce `nrow × ngroups` via `by=cols, op=ReduceToColumn`:

```r
        # G4b: col-grouped + ReduceToRow: G3 gives nrow x ngroups, then
        # reduce each column (across rows) to a scalar per group.
        inner_node <- list(op = "ReduceToColumn",
            reduction = node$reduction, params = node$params)
        inner <- .apply_reduction_grouped_matrix(inner_node, state, daf,
            by = "cols")
```

Also update the comment on line 1185.

- [ ] **Step 6: Fix the fallback-path dispatch conditions**

Read `R/query_eval.R:1066, 1084`. These are inside `.apply_reduction_grouped_matrix_fallback` and must match the top-level dispatch. Current:

```r
    if (identical(by, "rows") && identical(node$op, "ReduceToColumn")) {
        # G2 fallback: by="rows", op="ReduceToColumn" -> ngroups x ncol.
```

Swap to `ReduceToRow` for G2 fallback (~line 1066). Swap to `ReduceToColumn` for G3 fallback (~line 1084):

```r
    if (identical(by, "rows") && identical(node$op, "ReduceToRow")) {
        # G2 fallback: by="rows", op="ReduceToRow" -> ngroups x ncol.
```

```r
    # G3 fallback: by="cols", op="ReduceToColumn" -> nrow x ngroups.
    if (identical(by, "cols") && identical(node$op, "ReduceToColumn")) {
```

Update comments at lines 1065 and 1083-1084 to match.

- [ ] **Step 7: Verify axis=2/axis=3 kernel mapping is still correct**

Read `R/query_eval.R:1126`. Current code: `axis <- if (is_g2) 2L else 3L`. After the swap, G2 still produces `ngroups × ncol` (groups along rows), which still maps to kernel `axis=2`. G3 still produces `nrow × ngroups` (groups along cols), which still maps to kernel `axis=3`. **No change needed** — but write this verification in a commit message comment and confirm by reading the line.

- [ ] **Step 8: Dry-run a single G2 query manually in the REPL**

```bash
Rscript -e '
  devtools::load_all()
  d <- memory_daf(name = "t")
  add_axis(d, "r", paste0("r", 1:4))
  add_axis(d, "c", paste0("c", 1:3))
  set_vector(d, "r", "rg", c("a", "a", "b", "b"))
  m <- matrix(seq_len(12), 4, 3,
              dimnames = list(paste0("r", 1:4), paste0("c", 1:3)))
  set_matrix(d, "r", "c", "x", m)
  # Under new convention: G2 = rows + ReduceToRow
  cat("G2 result (-/ rg >- Sum):\n")
  print(get_query(d, "@ r @ c :: x -/ rg >- Sum"))
  # Expected: 2x3 matrix with rows "a","b", cols c1..c3
  #   a = (1+2, 5+6, 9+10)   = (3, 11, 19)
  #   b = (3+4, 7+8, 11+12)  = (7, 15, 23)
'
```

Expected output:

```
G2 result (-/ rg >- Sum):
  c1 c2 c3
a  3 11 19
b  7 15 23
```

- [ ] **Step 9: Run the Slice-8 kernel benchmarks as a smoke check**

The swap is dispatch-only; kernel behavior must be unchanged.

```bash
Rscript dev/benchmarks/run-slice-4-perf-wedge.R 2>&1 | tail -15
```

Expected: no crashes, no numerical regressions vs. the committed `dev/benchmarks/slice-4-perf-wedge-2026-04-21.csv`. (The harness runs kernels, not queries — unaffected by the swap.)

- [ ] **Step 10: Commit the dispatch swap**

Tests will be broken after this commit. That's expected — T2 repairs them.

```bash
git add R/query_eval.R
git commit -m "$(cat <<'EOF'
refactor(query_eval): swap G2/G3 dispatch to match Julia convention

BREAKING: grouped-matrix operator pairings now match DAF.jl.
- G2 is now `by=rows + ReduceToRow` (was: ReduceToColumn).
- G3 is now `by=cols + ReduceToColumn` (was: ReduceToRow).
- G4a inner op inverted: G4a = `by=rows + ReduceToColumn`, inner G2 = ReduceToRow.
- G4b inner op inverted: G4b = `by=cols + ReduceToRow`, inner G3 = ReduceToColumn.
- Fallback conditions at .apply_reduction_grouped_matrix_fallback updated.
Kernel axis=2/3 mapping unchanged (output shapes unchanged). Tests
repaired in the next commit (token-swap in test-query-grouped-slice8.R).
EOF
)"
```

---

## Task 2: Invert grouped-matrix test tokens

**Files:**
- Modify: `tests/testthat/test-query-grouped-slice8.R` (~379 lines, ~25 matrix-grouping tests)

**Token swap rule (after T1 lands):**

| Before (old R) | After (Julia convention) | Why |
|----------------|--------------------------|-----|
| `-/ g >\|` (old G2) | `-/ g >-` (new G2) | Keep matrix-output intent |
| `-/ g >-` (old G4a) | `-/ g >\|` (new G4a) | Keep vector-output intent |
| `\|/ g >-` (old G3) | `\|/ g >\|` (new G3) | Keep matrix-output intent |
| `\|/ g >\|` (old G4b) | `\|/ g >-` (new G4b) | Keep vector-output intent |

`/ g >|` (G1) is unaffected — do not touch.

Expected values and shapes stay the same.

- [ ] **Step 1: Capture current failure set**

```bash
Rscript -e 'devtools::test(filter = "query-grouped-slice8")' 2>&1 | tail -30
```

Record the failing-test count. Expect many failures.

- [ ] **Step 2: Read the test file end-to-end**

Read all 379 lines of `tests/testthat/test-query-grouped-slice8.R`. Classify each test by which pattern it targets (G1, G2, G3, G4a, G4b, fallback, Mode-char). For each non-G1 test, identify every `get_query(..., "... -/ ... >... ..." )` or `"... |/ ... >... ..."` string and apply the swap rule.

- [ ] **Step 3: Apply token swaps**

For each matrix-grouping test, edit the query strings per the swap table. **Do not** edit expected values, assertion shapes, or any prose that names G2/G3/G4 (those stay — they correctly describe the *intended* pattern; the tokens just now match the new dispatch).

Be careful: some tests use `sprintf("@ r @ c :: x -/ rg >| %s", op)` — edit the template string, not per-op.

- [ ] **Step 4: Run the affected test file, expect all pass**

```bash
Rscript -e 'devtools::test(filter = "query-grouped-slice8")' 2>&1 | tail -10
```

Expected: all tests in this file pass. If any fail, read the failure — it's almost always a missed token swap or a test that used an unusual pattern (e.g., a template var that confused the swap rule).

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/test-query-grouped-slice8.R
git commit -m "$(cat <<'EOF'
test(slice-9a): invert grouped-matrix tokens for G2/G3/G4 swap

Mechanical token swap per new dispatch:
  old G2 `-/ >|`  -> new G2 `-/ >-`
  old G4a `-/ >-` -> new G4a `-/ >|`
  old G3 `|/ >-`  -> new G3 `|/ >|`
  old G4b `|/ >|` -> new G4b `|/ >-`
G1 tests (`/ >|`) unchanged — G1 is not affected by the swap.
Expected shapes and values unchanged — only operator tokens flip.
EOF
)"
```

---

## Task 3: Audit and fix pre-Slice-8 grouped tests

**Files:**
- Modify: `tests/testthat/test-query-eval-groupby.R`

- [ ] **Step 1: Read the file and classify assertions**

```bash
Rscript -e 'devtools::test(filter = "query-eval-groupby")' 2>&1 | tail -20
```

Run first; record failing tests. Then read `tests/testthat/test-query-eval-groupby.R` top-to-bottom. For each failing assertion, check whether it uses `-/` or `|/` with `>|` or `>-` — those need the same token swap as T2. G1 (`/` + `>|`) and non-grouping tests stay.

- [ ] **Step 2: Apply token swaps for any affected tests**

Use the same rule as T2 step 3.

- [ ] **Step 3: Run affected test file, expect all pass**

```bash
Rscript -e 'devtools::test(filter = "query-eval-groupby")' 2>&1 | tail -10
```

- [ ] **Step 4: Run the full suite to catch any other collateral**

```bash
Rscript -e 'devtools::test()' 2>&1 | tail -5
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1744+ ]`. If other files fail, repeat the token-swap analysis for those files; they're also T3 scope.

- [ ] **Step 5: Commit**

```bash
git add tests/testthat/
git commit -m "$(cat <<'EOF'
test(slice-9a): audit pre-Slice-8 grouped tests for G2/G3 swap

Mechanical inversion of grouped-matrix tokens in test-query-eval-groupby.R
(and any other files flagged by the full suite). Same swap rule as
test-query-grouped-slice8.R. G1 assertions unchanged.
EOF
)"
```

---

## Task 4: Parser alias for `>>`

**Files:**
- Modify: `R/query_parse.R:63`
- Modify: `tests/testthat/test-query-parse.R`
- Modify: `tests/testthat/test-query-eval-groupby.R` (add end-to-end `>>` test)

**Context:** Tokenizer regex `>[->\\|]` at `R/query_tokens.R:7` already accepts `>>` as an operator token. The parser dispatch table at `R/query_parse.R:40-76` is missing the `>>` case. This task adds one dispatch line aliasing `>>` to `.qop_reduce_to_column` (same AST node as `>|`).

- [ ] **Step 1: TDD — write failing parse-identity test**

Read `tests/testthat/test-query-parse.R` briefly to see the test style, then append a new `test_that` block at the end:

```r
test_that(">> parses identically to >| (G1 alias)", {
    a <- parse_query("@ ax : x / g >> Sum")
    b <- parse_query("@ ax : x / g >| Sum")
    expect_identical(a, b)
})

test_that(">> alias works for Mode on character (Julia-parity G1)", {
    a <- parse_query("@ ax : color / g >> Mode")
    b <- parse_query("@ ax : color / g >| Mode")
    expect_identical(a, b)
})
```

- [ ] **Step 2: Run the test, confirm it fails**

```bash
Rscript -e 'devtools::test(filter = "query-parse")' 2>&1 | tail -15
```

Expected failure mode: `"unexpected operator '>>' at position ..."` — that's the dispatch-table miss.

- [ ] **Step 3: Add the parser dispatch entry**

Edit `R/query_parse.R:63` (or immediately after it) to add the `>>` alias. The dispatch table currently has:

```r
            ">|" = .parse_reduction(tokens, i, src, .qop_reduce_to_column),
            ">-" = .parse_reduction(tokens, i, src, .qop_reduce_to_row),
```

Insert a new line after the `>|` entry:

```r
            ">|" = .parse_reduction(tokens, i, src, .qop_reduce_to_column),
            ">>" = .parse_reduction(tokens, i, src, .qop_reduce_to_column),
            ">-" = .parse_reduction(tokens, i, src, .qop_reduce_to_row),
```

- [ ] **Step 4: Re-run the parse test, expect pass**

```bash
Rscript -e 'devtools::test(filter = "query-parse")' 2>&1 | tail -10
```

- [ ] **Step 5: Add an end-to-end `>>` test in groupby file**

Append to `tests/testthat/test-query-eval-groupby.R`:

```r
test_that(">> G1 alias evaluates identically to >|", {
    d <- memory_daf(name = "t")
    add_axis(d, "cell", c("c1", "c2", "c3", "c4"))
    set_vector(d, "cell", "donor", c("d1", "d1", "d2", "d2"))
    set_vector(d, "cell", "UMIs", c(1, 2, 10, 20))
    a <- get_query(d, "@ cell : UMIs / donor >> Sum")
    b <- get_query(d, "@ cell : UMIs / donor >| Sum")
    expect_equal(a, b)
    expect_equal(a, c(d1 = 3, d2 = 30))
})
```

- [ ] **Step 6: Run full suite to confirm no regressions**

```bash
Rscript -e 'devtools::test()' 2>&1 | tail -5
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1747+ ]` (3 new tests).

- [ ] **Step 7: Commit**

```bash
git add R/query_parse.R tests/testthat/test-query-parse.R tests/testthat/test-query-eval-groupby.R
git commit -m "$(cat <<'EOF'
feat(parse): accept `>>` as G1 alias for `>|` (Julia parity)

Julia's grouped-vector reduction syntax `/ group >> Op` now parses in
dafr, producing the same AST as R's existing `/ group >| Op`. Both
tokens remain accepted; canonical round-trip form is unchanged.
Tokenizer regex already matched `>>`; this adds the parser dispatch
entry at R/query_parse.R:63.
EOF
)"
```

---

## Task 5: Convert type-name aliases + `bit64::integer64`

**Files:**
- Modify: `R/operations.R:166-197` (`.op_convert`)
- Create or modify: `tests/testthat/test-ops-convert.R` (discover exact file in Step 1)

**Context:** `bit64` is already in `DESCRIPTION` Imports; `bit64::as.integer64` is already imported (`R/dafr-package.R:6`). This task normalizes Julia type names at the top of `.op_convert` and adds a new `integer64` branch for `Int64`.

- [ ] **Step 1: Discover the Convert test file**

```bash
cd /home/aviezerl/src/dafr-native
ls tests/testthat/ | grep -iE 'convert|ops' | head -5
```

Use the file that contains existing `.op_convert` / `Convert` tests. If none exists, create `tests/testthat/test-ops-convert.R`.

- [ ] **Step 2: TDD — write failing alias tests**

Append to the discovered file (adjust the test file name below if different):

```r
test_that(".op_convert accepts Julia aliases for Float32/Float64", {
    x <- c(1L, 2L, 3L)
    expect_identical(.op_convert(x, type = "Float32"),
                     .op_convert(x, type = "double"))
    expect_identical(.op_convert(x, type = "Float64"),
                     .op_convert(x, type = "double"))
})

test_that(".op_convert accepts Int32 as integer alias", {
    x <- c(1.0, 2.0, 3.0)
    expect_identical(.op_convert(x, type = "Int32"),
                     .op_convert(x, type = "integer"))
})

test_that(".op_convert accepts Bool as logical alias", {
    x <- c(0, 1, 2)
    expect_identical(.op_convert(x, type = "Bool"),
                     .op_convert(x, type = "logical"))
})

test_that(".op_convert Int64 returns bit64::integer64", {
    x <- c(1L, 2L, 3L)
    out <- .op_convert(x, type = "Int64")
    expect_s3_class(out, "integer64")
    expect_identical(as.integer(out), c(1L, 2L, 3L))
})

test_that(".op_convert Int64 on dense numeric", {
    x <- c(10, 20, 30)
    out <- .op_convert(x, type = "Int64")
    expect_s3_class(out, "integer64")
    expect_identical(as.numeric(out), c(10, 20, 30))
})

test_that(".op_convert Int64 on dgCMatrix densifies (documented)", {
    m <- Matrix::sparseMatrix(i = c(1, 2), j = c(1, 2),
                              x = c(1, 2), dims = c(2, 2))
    out <- .op_convert(m, type = "Int64")
    expect_s3_class(out, "integer64")
    expect_false(methods::is(out, "dgCMatrix"))
})

test_that(".op_convert still errors on unknown type", {
    expect_error(.op_convert(c(1, 2), type = "Float16"),
                 "'type' must be one of")
})
```

- [ ] **Step 3: Run tests, confirm they fail**

```bash
Rscript -e 'devtools::test(filter = "ops-convert")' 2>&1 | tail -15
```

Expected: all new tests fail with `'type' must be one of 'double', 'integer', 'logical'` errors for the alias cases.

- [ ] **Step 4: Implement the alias normalization + Int64 branch**

Replace `.op_convert` (`R/operations.R:166-197`) with:

```r
.op_convert <- function(x, ..., type) {
    if (missing(type)) {
        stop("Convert: 'type' parameter is required (one of 'double', 'integer', 'logical', 'integer64'; Julia aliases 'Float32'/'Float64'/'Int32'/'Int64'/'Bool' also accepted)",
            call. = FALSE
        )
    }
    # Normalize Julia type names to R-native canonical form.
    julia_aliases <- c(
        Float32 = "double",
        Float64 = "double",
        Int32 = "integer",
        Int64 = "integer64",
        Bool = "logical"
    )
    if (is.character(type) && length(type) == 1L && type %in% names(julia_aliases)) {
        type <- unname(julia_aliases[type])
    }
    valid_types <- c("double", "integer", "logical", "integer64")
    if (!is.character(type) || length(type) != 1L || !type %in% valid_types) {
        stop(sprintf(
            "Convert: 'type' must be one of 'double', 'integer', 'logical', 'integer64' (or Julia aliases 'Float32'/'Float64'/'Int32'/'Int64'/'Bool'); got %s",
            sQuote(as.character(type)[1L])
        ), call. = FALSE)
    }
    # integer64 path: densify sparse input (no sparse integer64 class exists).
    if (type == "integer64") {
        if (methods::is(x, "dgCMatrix")) {
            x <- as.matrix(x)
        }
        return(bit64::as.integer64(x))
    }
    # Sparse preservation for dgCMatrix
    if (methods::is(x, "dgCMatrix")) {
        if (type == "double") return(x)
        if (type == "integer") {
            if (length(x@x) > 0L && any(x@x != floor(x@x))) {
                stop("Convert: non-integer value in integer coercion",
                    call. = FALSE)
            }
            x@x <- as.double(as.integer(x@x))
            return(x)
        }
        if (type == "logical") {
            x@x <- as.double(x@x != 0)
            return(Matrix::drop0(x))
        }
    }
    # Dense or vector: existing behaviour via storage.mode
    storage.mode(x) <- type
    x
}
```

- [ ] **Step 5: Run Convert tests, expect pass**

```bash
Rscript -e 'devtools::test(filter = "ops-convert")' 2>&1 | tail -15
```

- [ ] **Step 6: Run full suite for regressions**

```bash
Rscript -e 'devtools::test()' 2>&1 | tail -5
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1754+ ]` (7 new tests).

- [ ] **Step 7: Commit**

```bash
git add R/operations.R tests/testthat/
git commit -m "$(cat <<'EOF'
feat(ops): accept Julia type-name aliases in Convert, support Int64

.op_convert now normalizes Julia type names before dispatch:
  Float32/Float64 -> double
  Int32           -> integer
  Int64           -> integer64 (new branch, via bit64::as.integer64)
  Bool            -> logical
R-native canonical names continue to work. Int64 on dgCMatrix
densifies (no sparse integer64 class in R) — documented.
bit64 is already in Imports; no DESCRIPTION change.
EOF
)"
```

---

## Task 6: Extend Julia-queries fixture

**Files:**
- Modify: `dev/scripts/regen-julia-queries-fixture.jl`
- Regenerate: `tests/testthat/fixtures/julia-queries/fixture.json`

**Precondition:** T1, T2, T3, T4, T5 all landed on the branch.

- [ ] **Step 1: Verify DAF.jl commit hash matches kickoff**

```bash
cd ~/src/DataAxesFormats.jl && git rev-parse HEAD
```

Expected: `49fbba140437387a378217c2fa658d4231d0c8c1`. If different, STOP — investigate before regenerating. Mixed DAF.jl versions in the fixture will poison the byte-parity claim.

- [ ] **Step 2: Read the existing regen script**

Read `dev/scripts/regen-julia-queries-fixture.jl` end-to-end. Note the existing record structure (query string → canonical form → kind → value) and the record-registration pattern.

- [ ] **Step 3: Add G1 builtin-op records with `>>` syntax**

Append new records for every builtin op on `example_cells_daf`. Use `age` (UInt32 donor vector) grouped by `sex`:

```julia
# G1 records with Julia `>>` syntax (parity with dafr after T4).
push_record("@ donor : age / sex >> Sum")
push_record("@ donor : age / sex >> Mean")
push_record("@ donor : age / sex >> Min")
push_record("@ donor : age / sex >> Max")
push_record("@ donor : age / sex >> Median")
push_record("@ donor : age / sex >> Quantile p 0.25")
push_record("@ donor : age / sex >> Var")
push_record("@ donor : age / sex >> Std")
push_record("@ donor : age / sex >> VarN eps 0.1")
push_record("@ donor : age / sex >> StdN eps 0.1")
push_record("@ donor : age / sex >> GeoMean")
# Mode on character — G1 with the `>>` syntax, Mode-on-string
push_record("@ cell : experiment / donor >> Mode")
```

Adapt `push_record` to match the actual helper name in the existing script (Step 2 establishes it).

- [ ] **Step 4: Add G2 and G3 matrix-grouping records**

```julia
# G2 (rows + ReduceToRow in Julia convention)
push_record("@ cell @ gene :: UMIs -/ experiment >- Sum")
push_record("@ cell @ gene :: UMIs -/ experiment >- Mean")
push_record("@ cell @ gene :: UMIs -/ experiment >- Max")
push_record("@ cell @ gene :: UMIs -/ experiment >- Var")
# G3 (cols + ReduceToColumn in Julia convention)
push_record("@ gene @ cell :: UMIs |/ experiment >| Sum")
push_record("@ gene @ cell :: UMIs |/ experiment >| Mean")
push_record("@ gene @ cell :: UMIs |/ experiment >| Max")
```

- [ ] **Step 5: Add Convert records with Julia type names**

```julia
push_record("@ gene @ cell :: UMIs % Convert type Int32")
push_record("@ gene @ cell :: UMIs % Convert type Int64")
push_record("@ gene @ cell :: UMIs % Convert type Bool")
push_record("@ gene : is_lateral % Convert type Int32")
```

- [ ] **Step 6: Regenerate the fixture**

```bash
cd /home/aviezerl/src/dafr-native
conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
    dev/scripts/regen-julia-queries-fixture.jl
```

Expected: no Julia errors; `tests/testthat/fixtures/julia-queries/fixture.json` updated.

- [ ] **Step 7: Run Julia-parity tests**

```bash
Rscript -e 'devtools::test(filter = "query-julia-compat")' 2>&1 | tail -30
```

Expected: all byte-parity assertions pass. Diagnose any failure by:
1. Printing R's value: `get_query(daf, "<query>")`.
2. Comparing to the fixture's JSON value for that query.
3. If mismatch: trace through `.op_*` formula authority in `R/operations.R` to find the deviation.

- [ ] **Step 8: Commit**

```bash
git add dev/scripts/regen-julia-queries-fixture.jl tests/testthat/fixtures/julia-queries/fixture.json
git commit -m "$(cat <<'EOF'
test(fixtures): extend julia-queries for Slice 9a surface

New records cover:
- G1 with Julia `>>` syntax (every builtin op + Quantile + Mode-on-char)
- G2 with Julia convention `-/ ... >-`
- G3 with Julia convention `|/ ... >|`
- Convert with Julia type names (Int32, Int64, Bool)
Regenerated against DataAxesFormats.jl at
49fbba140437387a378217c2fa658d4231d0c8c1 (unchanged since Slice 3).
Byte-parity with dafr confirmed on every new record.
EOF
)"
```

---

## Task 7: Axis-rename view re-apply investigation (TIMEBOXED 2–3h)

**Files:**
- Read only: `R/view.R`, `R/complete_daf.R` (or wherever `complete_daf` re-applies views)
- Modify conditionally: add round-trip test to `tests/testthat/test-*complete*.R`
- Create conditionally: `dev/notes/axis-rename-findings.md`

**Timebox rule:** Start a wall-clock timer. At 2h, write the findings note and stop, even mid-investigation. Do not partially implement.

- [ ] **Step 1: Locate the code paths (30 min cap)**

```bash
cd /home/aviezerl/src/dafr-native
grep -nE 'rename|renamed_|base_daf_view' R/view.R R/complete_daf.R R/*.R 2>/dev/null | head -40
grep -rnE '"=\s' tests/testthat/*view* tests/testthat/*complete* 2>/dev/null | head -20
```

Identify:
- Where `viewer()` constructs a view.
- Where `complete_daf` serializes/re-applies `base_daf_view` JSON (Slice 8 T13 landed this).
- Whether `viewer()` accepts an arg shape that performs axis rename.

Keep notes in `/tmp/slice-9a-t7-scratch.md` while investigating.

- [ ] **Step 2: Try constructing a rename view (60 min cap)**

Experiment:

```r
d <- memory_daf(name = "t")
add_axis(d, "cell", c("c1", "c2", "c3"))
set_vector(d, "cell", "umi_count", c(10, 20, 30))
# Attempt: rename "cell" to "renamed_cell"
v1 <- viewer(d, axes = list("renamed_cell" = "cell"))
v2 <- viewer(d, axes = list(renamed_cell = "=cell"))
v3 <- viewer(d, axes = c(renamed_cell = "cell"))
# ... whichever shapes the code accepts or rejects
```

If one shape works: proceed to Step 3. If all fail with clear rejection: proceed to Step 4.

- [ ] **Step 3: Happy path — add round-trip test (30 min cap)**

If a rename shape works, construct a test that:
1. Creates a view with a renamed axis.
2. Writes via `complete_daf` to a tmpdir.
3. Reopens, confirms the renamed axis is queryable.

Append to `tests/testthat/test-complete-daf.R` (or the file that tests `complete_daf`). Exact code depends on which shape works in Step 2.

- [ ] **Step 4: Rabbit-hole path — write findings and stop (30 min cap)**

If no shape works within the timebox, write `dev/notes/axis-rename-findings.md`:

```markdown
# Axis-rename view re-apply — findings (Slice 9a T7)

**Date:** 2026-04-22
**Timebox:** 2-3 hours (used: XXm)

## What we tried
[list exact arg shapes attempted]

## What we learned
[summary of viewer()'s current arg handling re: rename]

## What a fix would need
[concrete pointer: which file / function / API gap]

## Recommended next slice
[Slice 10+ task sketch]
```

- [ ] **Step 5: Commit what actually exists**

Either (Happy path):

```bash
git add tests/testthat/test-complete-daf.R
git commit -m "test(complete-daf): round-trip renamed-axis view through complete_daf"
```

Or (Rabbit-hole path — commit to dev/ repo, not package repo):

```bash
cd /home/aviezerl/src/dafr-native/dev
git add notes/axis-rename-findings.md
git commit -m "notes(slice-9a): axis-rename view re-apply findings"
cd /home/aviezerl/src/dafr-native
# No package-repo commit for T7 in this case.
```

---

## Task 8: NEWS, check, exit note, merge

**Files:**
- Modify: `NEWS.md`
- Create: `dev/notes/slice-9a-exit.md`

- [ ] **Step 1: Add NEWS entry**

Read `NEWS.md` to see the existing format (Slice 8 entry is the most recent template). Add a new top entry:

```markdown
# dafr (development version)

## Slice 9a — Julia-parity correctness (2026-04-22)

### Breaking changes

- **Grouped-matrix operator semantics inverted to match DAF.jl.**
  The pairing between `GroupRowsBy` / `GroupColumnsBy` and
  `ReduceToRow` / `ReduceToColumn` has been swapped. Queries written
  under the previous R convention will now dispatch to a different
  pattern and produce a different output shape.

  | Pattern | Before | After |
  |---------|--------|-------|
  | G2 (matrix output, groups × ncol) | `-/ g >\|` | `-/ g >-` |
  | G3 (matrix output, nrow × groups) | `\|/ g >-` | `\|/ g >\|` |
  | G4a (vector output, length groups) | `-/ g >-` | `-/ g >\|` |
  | G4b (vector output, length groups) | `\|/ g >\|` | `\|/ g >-` |

  G1 vector reduction (`/ g >\|`) is unchanged.

### New features

- **Julia G1 reduction syntax (`>>`)** accepted as an alias for
  `>|`. `@ ax : x / g >> Sum` now parses identically to
  `@ ax : x / g >| Sum`.
- **Convert op accepts Julia type names.** `Float32`, `Float64`
  (→ R `double`); `Int32` (→ `integer`); `Int64` (→
  `bit64::integer64`); `Bool` (→ `logical`). R-native names continue
  to work. `Int64` on `dgCMatrix` densifies (no sparse `integer64`
  class exists in R).
- **Julia-queries fixture extended** with new records for G1 (with
  `>>`), G2, G3, Mode-on-character, and Convert with Julia type
  names. All new records byte-parity with DAF.jl.
```

If T7 produced a test, add a bullet under "New features" for renamed-axis view round-trip. If T7 produced a findings note, no NEWS bullet.

- [ ] **Step 2: Run full test suite**

```bash
Rscript -e 'devtools::test()' 2>&1 | tail -5
```

Expected: `[ FAIL 0 | WARN 1 | SKIP 1 | PASS 1755+ ]`.

- [ ] **Step 3: Run R CMD check**

```bash
Rscript -e 'devtools::check(error_on = "note")' 2>&1 | tail -50
```

Expected: exits with code 1 due to the **two** structural notes from Slice 8 (`benchmarks/` top-level dir, 6.1 MB installed size). No new notes, no warnings, no errors.

If any new note/warning/error appears: diagnose and fix before proceeding.

- [ ] **Step 4: Write exit note**

Use `dev/notes/slice-8-exit.md` as the template. Create `dev/notes/slice-9a-exit.md`:

```markdown
# Slice 9a — Exit gate

**Date:** 2026-04-22 (or actual)
**Branch:** `slice-9a-correctness`
**Predecessor:** Slice 8 (`f7978cc`, tag `slice-8`)

## Delivered

- Semantic swap of G2/G3 grouped-matrix dispatch to Julia convention.
- Parser alias `>>` for `>|` (G1 Julia syntax).
- Convert op accepts Julia type names + Int64 via bit64::integer64.
- Julia-queries fixture extended with N new records, byte-parity confirmed.
- [T7 outcome: either round-trip test or findings note]

## Test / check status

- `devtools::test()`: [counts].
- `devtools::check(error_on = "note")`: 2 structural notes (same as Slice 8 baseline), no new notes/warnings/errors.

## TDD regression note

Per memory `feedback_slice4_p3_tdd.md`: T1/T2/T3 are regression-guard
commits, not failing-first TDD. The test inversion is mechanically
derived from the dispatch swap, not an independent specification.

## Breaking change announcement

NEWS.md entry under "Slice 9a" documents the G2/G3 swap with a
before/after table.

## Public surface

110 exports, unchanged.

## Deferred / still open

- G3 kernel thread-bucket memory fix (Slice 9b).
- DAF.jl bake-off harness (Slice 9b).
- Perf parity target shape (decision #5, Slice 9b kickoff).
- Axis-rename re-apply [if T7 took rabbit-hole path].
- Slice 8 carry-forward list (bestify, reconstruct_axis, H5/AnnData/Zarr, etc.).
```

- [ ] **Step 5: Commit NEWS + exit note**

```bash
git add NEWS.md
git commit -m "docs(slice-9a): NEWS entry for Julia-parity correctness"

cd /home/aviezerl/src/dafr-native/dev
git add notes/slice-9a-exit.md
git commit -m "notes(slice-9a): exit gate"
cd /home/aviezerl/src/dafr-native
```

- [ ] **Step 6: Merge to main**

Return to `main` and merge with a merge commit (not fast-forward, per Slice 8 convention):

```bash
git checkout main
git merge --no-ff slice-9a-correctness -m "$(cat <<'EOF'
merge: slice 9a — Julia-parity correctness

Correctness workstream from Slice 9. Breaking change: G2/G3
grouped-matrix dispatch inverted to match DAF.jl. Parser accepts
Julia's `>>` G1 syntax. Convert accepts Julia type names incl. Int64.
Julia-queries fixture extended; byte-parity confirmed. Perf parity
deferred to Slice 9b.
EOF
)"
git tag slice-9a
```

- [ ] **Step 7: Final verification**

```bash
git log --oneline -5
git tag | grep slice-9
Rscript -e 'devtools::test()' 2>&1 | tail -3
```

Confirm: merge commit present, tag `slice-9a` present, full suite still passes on `main`.

---

## Parallelization hints for subagent-driven execution

After T1+T2+T3 land in a single session (tight coupling), T4, T5, and T7 are independent and can run in parallel subagents. T6 must wait for T1+T4+T5. T8 is the final serial gate.

Recommended dispatch order:
1. Session A: T1 → T2 → T3 (sequential, one session).
2. Sessions B/C/D (parallel): T4, T5, T7.
3. Session E: T6 (after A/B/C merge).
4. Session F: T8 (after E).

---

## Exit criteria (repeated from spec for convenience)

1. `devtools::test()` — 1744+ PASS, 0 FAIL. Pre-existing 1 SKIP + 1 WARN baseline preserved.
2. `devtools::check(error_on = "note")` — same 2 structural notes as Slice 8. No new notes/warnings/errors.
3. Extended Julia-queries fixture — every new record byte-parity.
4. `>>` and `>|` parse G1 identically.
5. Convert aliases accepted; `Int64` round-trips via `bit64::integer64`.
6. Axis-rename: test landed OR findings note committed.
7. `NEWS.md` has Slice 9a entry with Breaking changes subhead.
8. `dev/notes/slice-9a-exit.md` written.
9. Public exports: 110, unchanged.
10. Single merge commit into `main`, tagged `slice-9a`.
