# Kickoff: audit dafr for R-only quirks not surfaced by ported Julia tests

## Why this audit exists

dafr is a port of `DataAxesFormats.jl`. Test parity is excellent — most
of the queries.jl test suite has been mirrored under
`tests/testthat/test-query-*.R`. **That is not enough.** A test parity
suite proves the R port matches Julia *on the cases Julia chose to
test*; it does not exercise R-specific footguns that the Julia source
cannot have.

Concrete precedent (`R/query_eval.R`, fixed in commit ba9baa7):

- Julia DAF.jl uses a typed dispatched helper
  `as_booleans(::AbstractVector{<:T})` to reduce mask values to bool —
  for strings it returns `vec .!= ""`, for reals `vec .!= 0`.
  Type-correct by construction.
- The R port inlined the same idea five times as
  `!is.na(vec) & vec != 0`. For a **character** vector, R coerces `0` to
  `"0"` and returns `TRUE` for every entry — including `""`. So outlier
  cells (`metacell == ""`) silently passed every `[ metacell ]` mask
  while a chained `: metacell ?? : type` dropped them, producing
  inconsistent results from queries that ought to agree.
- No Julia mask test ever uses a string-valued property (only
  `is_lateral`, `is_low`, `is_doublet`, …). The dafr ports inherited the
  gap. The bug shipped for months and surfaced through a real user
  query.

We need to find every other place this pattern could be hiding.

## What you are auditing for

R has a handful of quiet behaviors that Julia does not, and that the
ported tests will not catch. The audit target is `R/` and the C++ in
`src/` (kernels and ALTREP). Concentrate on `R/query_eval.R`,
`R/dataframes.R`, `R/reconstruction.R`, `R/queries.R`, `R/format_api.R`,
and the reduction kernels in `src/`.

### 1. Coercion footguns

- `vec != 0`, `vec == 0`, `vec > 0` against a character vector silently
  coerces 0 → `"0"` and returns nonsense. Same for `<`, `<=`, `>=`.
- `length(x) == 0`, `nrow(x)` on `NULL`, `dim(x)` on a vector — easy to
  reason about, easy to misuse.
- `as.numeric(factor_vec)` returns the *integer codes*, not the labels.
  If a factor ever flows in where `character` was expected, the values
  silently change.
- `c(int_vec, double_scalar)` promotes everything to double;
  `c(int_vec, "x")` promotes everything to character. Rarely a bug but
  worth flagging if any reduction relies on type stability.
- `seq_len(n)` when `n == 0` returns `integer(0)` (correct), but `1:n`
  when `n == 0` returns `c(1, 0)`. If any code uses `1:n` instead of
  `seq_len`, it’ll iterate twice on an empty axis.

### 2. NA propagation

- Julia uses sentinel values (`""` for missing strings, NaN for missing
  reals); R uses typed `NA`. They are not equivalent.
  - `NA & FALSE` is `FALSE`, `NA & TRUE` is `NA`. Code that expects
    “false-ish” may break on `NA`.
  - `sum(c(1, NA))` is `NA` unless `na.rm = TRUE`. Reductions in dafr
    that don’t pass `na.rm` may silently produce NA from clean Julia
    inputs.
  - `match(NA, x)` returns the position of `NA` in x if x has any NA, or
    `NA` otherwise. Easy to misread.
- Logical mask with `NA` entries: `vec[mask]` keeps positions where
  `mask == NA` as `NA` rows in the result. Code paths that expect a
  clean subset can corrupt downstream length assumptions.

### 3. Empty-vector edge cases

- `which(logical(0))` is `integer(0)` (fine), but `range(numeric(0))` is
  `c(Inf, -Inf)` with a warning. Reductions on empty axes can surprise.
- `sapply(empty_list, fn)` returns
  [`list()`](https://rdrr.io/r/base/list.html), not
  [`vector()`](https://rdrr.io/r/base/vector.html). If the caller
  assumes a typed atomic vector, the result is a list.
- `do.call(rbind, list())` is `NULL`. Frame-construction code that joins
  per-axis results may yield `NULL` instead of an empty data frame.

### 4. Identity vs equality, NULL vs NA

- `identical(x, NULL)` vs `is.null(x)` vs `length(x) == 0` — three ways
  to ask “is this empty”, and they disagree on `list(NULL)`,
  `numeric(0)`, etc. Cross-check that the R version uses the same
  predicate the Julia version implies.
- `==` on `NULL` returns `logical(0)`, not an error and not `FALSE`. An
  `if (x == "foo")` where x is `NULL` errors only if `x` becomes
  `logical(0)` (zero-length condition).

### 5. Factor surprises

- A factor coming through
  [`data.frame()`](https://rdrr.io/r/base/data.frame.html) defaults;
  `stringsAsFactors` is `FALSE` since R 4.0 but legacy code may still
  convert. If a property is read back as a factor,
  `as.character(factor)` is needed before any string comparison.
- The `.as_booleans` helper just added in `R/query_eval.R:957` already
  handles factors; verify other “treat as character” sites do too.

### 6. Numeric precision and integer overflow

- Julia distinguishes `Int64` from `Float64`; R has `integer` (32-bit)
  and `numeric` (double). Counts that fit in Int64 but not Int32 (e.g.,
  a large axis_length × axis_length) may overflow silently in R.
- `sum(integer)` overflows to `NA_integer_` with a warning;
  `sum(double)` is fine. Reduction kernels that assume integer
  accumulation need to promote.
- `1L * 1L * 1L * ...` chains in indexing math.

### 7. Sparse-matrix oddities (Matrix package)

- [`Matrix::sparseMatrix`](https://rdrr.io/pkg/Matrix/man/sparseMatrix.html)
  returns dgCMatrix; arithmetic between a sparse and a dense often
  densifies. If a kernel relies on sparsity for performance, an
  unintended densification is silent.
- Subscripting a sparse matrix by a logical or integer vector can flip
  representation (dgC ↔︎ dgT ↔︎ dense).

### 8. R-specific path / IO normalization

- Already hit once: `normalizePath` resolves macOS `/var` →
  `/private/var`. Anywhere else dafr stores a path, check that the
  storage form is consistent with downstream comparisons.
- [`tools::file_path_sans_ext`](https://rdrr.io/r/tools/fileutils.html)
  doesn’t strip `.tar.gz` (only `.gz`).
- Windows path separators (`\\` vs `/`); `file.path` produces `/` on
  Windows but readers may receive `\\`-quoted strings from user code.

### 9. UTF-8 / encoding

- R’s default encoding on Windows is sometimes `latin1`; on Linux/macOS,
  `UTF-8`. `nchar(x, type = "chars")` vs `"bytes"` differ for non-ASCII.
  If any axis entries or property values contain non-ASCII, equality
  comparisons can fail across platforms.

## How to do this audit

1.  **Start with the precedent.** Read commit `ba9baa7` (the
    `.as_booleans` fix) and `tests/testthat/test-query-mask-string.R` to
    see what “missing-test class” looks like.
2.  **Grep for the buggy pattern’s siblings.** Examples:
    - `vec\s*!=\s*0` and `vec\s*==\s*0` — every match needs a string-vec
      test or a coercion guard.
    - `as.numeric\b` immediately preceded by something that *could* be a
      factor.
    - `1:n` (suspect; prefer `seq_len(n)`).
    - `length\(.*\)\s*>\s*0` followed by a use-as-truthy.
    - `unlist\(.*recursive` — flatten that drops names you might rely
      on.
    - `match\(` with no `nomatch =` argument — silent NA on non-matches.
    - `sum\(.*int\b` or `prod\(.*int\b` without `na.rm`.
3.  **Cross-reference with Julia.** For every match, find the Julia code
    it ports. If Julia uses a typed helper (`as_booleans`, `cast_value`,
    `densify`, `as_named`, `as_strings`), the R port should mirror that
    helper instead of inlining the rule. If the helper doesn’t exist in
    R yet, write it.
4.  **Write at least one targeted test per finding.** Use the same
    pattern as `test-query-mask-string.R`: a tiny in-memory daf with
    string-valued properties, then assert equivalence between the two
    query phrasings that ought to agree (mask vs chained `??`, etc.).
5.  **For each finding, file as a new commit:**
    `fix(parity): <one-line>` matching style of `ba9baa7`. Reference the
    Julia source location in the commit body.

## Constraints

- **Do not break Julia parity.** When R semantics differ from Julia,
  default to aligning R with Julia. (See user feedback memory: “Julia
  DAF is the parity reference”.)
- **Tests must use `testthat` and run under
  `cd tests && NOT_CRAN=true Rscript testthat.R`.**
- **Do not introduce abstractions for hypothetical findings.** Add a
  helper only when ≥2 sites need it. Otherwise inline the fix where the
  bug is.
- **Stay within `R/`, `src/`, `tests/testthat/`.** Don’t touch the Julia
  upstream or `_pkgdown.yml`.
- **Don’t refactor unrelated code while you’re in there.** One finding,
  one commit, one test.

## Deliverable

A punch list of findings — each with:

1.  The R-only quirk (one sentence).
2.  The site(s) (`file:line`).
3.  The Julia reference (`queries.jl:NNNN` or equivalent).
4.  The test that demonstrates the bug.
5.  The fix (commit SHA or proposed diff).

If a finding turns out to be a non-issue under closer reading, document
it (“checked X, R behavior matches Julia because Y”) so the audit
doesn’t have to re-walk the same paths next quarter.

## Out of scope

- Performance auditing (separate pass, see `inst/benchmarks/`).
- Cross-version Julia compatibility (the parity target is the current
  `~/src/DataAxesFormats.jl`, no others).
- New features. This audit is strictly about correctness gaps in the
  existing port.
