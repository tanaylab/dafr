# Slice 10d — Release Polish Implementation Plan

> **For agentic workers:** Use superpowers:subagent-driven-development.

**Goal:** Ship `dafr 0.1.0` — first public release. No new exports. Vignettes + pkgdown config + README rewrite + `@examples` backfill + NEWS 0.1.0 + version bump + `v0.1.0` tag.

**Architecture:** Pure docs + release plumbing. No `R/` source changes beyond `@examples` backfills picked up by roxygen. 4 new `vignettes/*.Rmd` files; new `_pkgdown.yml`; new `cran-comments.md`; rewritten `NEWS.md`; bumped `DESCRIPTION`.

**Tech Stack:** `knitr` + `rmarkdown` (existing Suggests). No new dependencies.

**Spec:** `dev/notes/2026-04-23-slice-10d-design.md`.

**Dev loop per task:**
```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
```

For vignette-build verification:
```
Rscript -e 'devtools::build_vignettes(quiet = FALSE)'
```

For full check (Phase Z):
```
Rscript -e 'devtools::check(error_on = "never", vignettes = TRUE)' 2>&1 | tail -40
```

---

## Phase 0: Branch setup

- [ ] `git checkout main && git checkout -b slice-10d`.
- [ ] Baseline-green: `R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R` — 2616 PASS.

---

## Phase A: `@examples` backfill + README rewrite

### Step A.1: Enumerate missing `@examples`

Run:
```
Rscript -e 'devtools::check(error_on = "never", vignettes = FALSE, document = FALSE)' 2>&1 | grep -E "missing.*example|undocumented|without example" | head -80
```

**If the output is empty** (nothing flagged), skip to Step A.3 — slices 10c/10a/10b already added examples inline.

**If a list appears:** for each flagged function, add a runnable `\examples{}` block to its roxygen. Use `\dontrun{}` for anything taking >1 second or requiring external fixtures.

Helpful patterns:
- For Daf-specific functions: use `example_cells_daf()` or build a minimal `memory_daf()` inline.
- For contract functions: use `withr::with_options(list(dafr.enforce_contracts = TRUE), { ... })`.
- For h5ad functions: wrap in `\dontrun{}` (file I/O).

### Step A.2: Commit `@examples` additions

If any edits were made:
```
Rscript -e 'devtools::document()'
git add R/ man/
git commit -m "docs(10d): backfill @examples on previously-skipped exports"
```

### Step A.3: Rewrite README

Check if `README.Rmd` exists. If it does, edit it; if not, copy wrapper's `README.Rmd` as the starting point:

```
cp ~/src/dafr/README.Rmd README.Rmd
```

Then adapt:
- **Installation** section: replace "install Julia + DAF.jl + JuliaCall" with native-only R install (`remotes::install_github("tanaylab/dafr")`).
- **Note on Data Transfer** section: delete (no JuliaCall copy tax).
- **Future plans: dplyr-like API** paragraph: delete or rewrite.
- **Query syntax** mini-tutorial: keep; check that examples work on native (they should, since the query string parser is semantically equivalent).
- Add a new **Native advantages** section with bullets:
  - No Julia install required — pure R + C++.
  - mmap-backed reads via `mmap_dgCMatrix` / `mmap_int` / `mmap_lgl` / `mmap_real`.
  - OpenMP-parallel kernels (Sum / Mean / Var / Mode / Quantile / GeoMean).
  - `register_eltwise` / `register_reduction` for user-defined query ops.
  - Pipe-chain builders: `daf[Axis("cell") |> LookupVector("age") |> IsGreater(2)]`.
  - AnnData interop via `DafAnnData` + `h5ad_as_daf` / `daf_as_h5ad`.

Render the .Rmd to `README.md`:
```
Rscript -e 'rmarkdown::render("README.Rmd", output_format = "github_document")'
```

Commit:
```
git add README.Rmd README.md
git commit -m "docs(10d): rewrite README for native package — drop Julia, add Native advantages"
```

---

## Phase B: Vignettes + pkgdown

### Step B.1: Create `vignettes/dafr.Rmd` — Getting Started

```markdown
---
title: "Getting Started with dafr"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Getting Started with dafr}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(dafr)
```

## What is a Daf?

A Daf (Data Axes Format) is a multi-axis container for biological /
scientific data. Think of it as a typed dictionary of scalars,
per-axis vectors, and per-axis-pair matrices, with cache invalidation
and lazy mmap-backed reads.

## Create a Daf

```{r}
d <- memory_daf(name = "demo")
add_axis(d, "cell",  c("c1", "c2", "c3"))
add_axis(d, "gene",  c("g1", "g2"))
set_scalar(d, "organism", "human")
set_vector(d, "cell", "donor", c("A", "B", "A"))
set_matrix(d, "cell", "gene", "UMIs",
           matrix(1:6, nrow = 3, ncol = 2))
print(d)
```

## Reading data

```{r}
get_scalar(d, "organism")
get_vector(d, "cell", "donor")
get_matrix(d, "cell", "gene", "UMIs")
```

## Queries

Queries let you compose reads:

```{r}
get_query(d, "@ cell : donor")
get_query(d, ". organism")
```

Or via pipe-chain builders:

```{r}
d[Axis("cell") |> LookupVector("donor")]
```

## Data frames

```{r}
get_dataframe(d, "cell")
```

## Persistence

```{r, eval = FALSE}
# Write to a directory:
fd <- files_daf(tempfile("dafr-"), mode = "w+", name = "persisted")
copy_all(d, fd)
```

## Example data

```{r}
d2 <- example_cells_daf()
axes_set(d2)
```
```

### Step B.2: Create `vignettes/queries.Rmd` — Query DSL

```markdown
---
title: "Query DSL"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Query DSL}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(dafr)
d <- example_cells_daf()
```

# Two equivalent forms

Queries can be written as strings or as pipe-chain builder objects.
The following two expressions are equivalent:

```{r}
d["@ cell : donor"]
d[Axis("cell") |> LookupVector("donor")]
```

# String form

```{r}
# Scalar
d[". organism"]
# Axis entries
d["@ cell"] |> head()
# Vector on axis
d["@ cell : donor"] |> head()
# Matrix
d["@ cell @ gene :: UMIs"] |> dim()
```

# Builder form

```{r}
# Masks
q <- Axis("cell") |> BeginMask("donor") |> IsEqual("D1") |> EndMask()
d[q] |> length()
```

# Reductions

```{r}
# Per-gene mean UMI
d[Axis("gene") |> Axis("cell") |> LookupMatrix("UMIs") |> Mean()] |> head()
```

# Comparison + mask composition

```{r}
q <- Axis("cell") |> BeginMask("age") |> IsGreater(30) |> EndMask()
# (Assumes age exists on cell)
```

# See `?Axis`, `?LookupVector`, `?IsGreater` for the full builder list.
```

### Step B.3: Create `vignettes/native-performance.Rmd` — Native advantages

```markdown
---
title: "Native performance and mmap readers"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Native performance and mmap readers}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(dafr)
```

# Why native?

dafr is a pure R + C++ port of the Julia `DataAxesFormats.jl` package.
Compared to the Julia-facade wrapper, native has:

- No JuliaCall copy tax on cross-language boundaries.
- mmap-backed reads for vectors and sparse matrices (no double-buffer).
- OpenMP-parallel query kernels (Sum, Mean, Var, Mode, Quantile, ...).
- User-extensible op registry (`register_eltwise`, `register_reduction`).

# Mmap readers

When a FilesDaf is opened read-only, vectors and sparse matrices are
mmap'd — the OS maps the on-disk file into process memory without
copying.

```{r, eval = FALSE}
fd <- files_daf("/path/to/daf", mode = "r")
x <- get_vector(fd, "cell", "donor")   # mmap'd — no allocation
```

Low-level mmap constructors are also exported for advanced uses:
`mmap_dgCMatrix`, `mmap_int`, `mmap_lgl`, `mmap_real`.

# Parallel kernels

Reductions above a size threshold dispatch to OpenMP. The threshold
is controlled via `options(dafr.kernel_threshold = N)` where N is the
minimum element count for parallel execution.

```{r, eval = FALSE}
options(dafr.kernel_threshold = 1e5)   # default 1e6
```

# Bake-off headline (2026-04-22 post-9c)

On a metacell-scale fixture (100k × 5k × 0.02 density):

| Op    | Wall  |
|-------|------:|
| Sum   | 28 ms |
| Var   | 26 ms |
| Mode  | 108 ms |
| Quantile | 44 ms |

See `dev/benchmarks/2026-04-22-post-slice-9c/` for full methodology.
```

### Step B.4: Create `vignettes/anndata.Rmd` — h5ad round-trip

```markdown
---
title: "AnnData interop"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{AnnData interop}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
library(dafr)
```

# DafAnnData facade

If you've been using `anndata` / `scanpy` / `Seurat`, the facade
exposes a Daf with familiar property names:

```{r}
d <- example_cells_daf()
ann <- as_anndata(d)
ann$n_obs
ann$n_vars
ann$obs_names |> head()
dim(ann$X)
```

The facade is read-only: `ann$X <- ...` errors. To modify data, write
to the underlying Daf: `set_matrix(d, ...)`.

# Loading h5ad

```{r, eval = FALSE}
d <- h5ad_as_daf("path/to/file.h5ad")
```

Requires `hdf5r` (a Suggests dep): `install.packages("hdf5r")`.

# Writing h5ad

```{r, eval = FALSE}
daf_as_h5ad(d, "out.h5ad", overwrite = TRUE)
```

# Fixture round-trip

```{r}
fixture <- system.file("extdata", "small_test.h5ad", package = "dafr")
if (file.exists(fixture) && requireNamespace("hdf5r", quietly = TRUE)) {
    d <- h5ad_as_daf(fixture)
    cat("loaded:", daf_name(d), "\n")
    cat("n_obs:", length(format_axis_array(d, "obs")), "\n")
    cat("n_vars:", length(format_axis_array(d, "var")), "\n")
}
```

# Limitations (0.1.0)

- Sparse matrix h5ad encoding (`csr_matrix` / `csc_matrix`) not yet
  translated; read path warns and skips.
- Categorical (factor) columns skipped on read.
- Nested `uns` groups skipped.
- `varm` / `obsm` / `obsp` / `varp` / `raw` not translated.
```

### Step B.5: Commit vignettes

```
git add vignettes/
git commit -m "docs(10d): add 4 vignettes (getting started, queries, performance, anndata)"
```

### Step B.6: Update DESCRIPTION for VignetteBuilder

Check current DESCRIPTION for `VignetteBuilder:`. If absent, add:
```
VignetteBuilder: knitr
```
Ensure `knitr` and `rmarkdown` are in Suggests (already are).

Verify the vignette builds:
```
Rscript -e 'devtools::build_vignettes()'
```

Expected: 4 HTML files written under `doc/`.

Commit DESCRIPTION:
```
git add DESCRIPTION
git commit -m "docs(10d): add VignetteBuilder: knitr"
```
(If DESCRIPTION was unchanged, skip.)

### Step B.7: Create `_pkgdown.yml`

Port from wrapper's `_pkgdown.yml` as a starting point, then adapt:

```yaml
url: https://tanaylab.github.io/dafr/

template:
  bootstrap: 5

reference:
  - title: Core data model
    contents:
      - DafReader
      - DafReadOnly
      - DafWriter
      - memory_daf
      - files_daf
      - is_daf
      - daf_name
      - complete_path
      - read_only
      - empty_cache

  - title: Readers
    contents:
      - get_scalar
      - get_vector
      - get_matrix
      - has_scalar
      - has_vector
      - has_matrix
      - has_axis
      - scalars_set
      - axes_set
      - axis_entries
      - vectors_set
      - matrices_set

  - title: Writers
    contents:
      - add_axis
      - delete_axis
      - set_scalar
      - delete_scalar
      - set_vector
      - delete_vector
      - set_matrix
      - delete_matrix
      - relayout_matrix

  - title: Query DSL (string form)
    contents:
      - get_query
      - has_query
      - parse_query
      - canonical_query
      - is_axis_query
      - query_axis_name
      - query_result_dimensions
      - query_requires_relayout
      - escape_value
      - unescape_value

  - title: Query builders (DafrQuery)
    contents:
      - DafrQuery
      - Abs
      - Clamp
      - Convert
      - Fraction
      - Log
      - Round
      - Significant
      - Count
      - CountBy
      - GeoMean
      - GroupBy
      - GroupColumnsBy
      - GroupRowsBy
      - Max
      - Mean
      - Median
      - Min
      - Mode
      - Quantile
      - ReduceToColumn
      - ReduceToRow
      - Std
      - StdN
      - Sum
      - Var
      - VarN
      - Axis
      - AsAxis
      - BeginMask
      - BeginNegatedMask
      - EndMask
      - IfMissing
      - IfNot
      - LookupMatrix
      - LookupScalar
      - LookupVector
      - Names
      - SquareColumnIs
      - SquareRowIs
      - AndMask
      - AndNegatedMask
      - OrMask
      - OrNegatedMask
      - XorMask
      - XorNegatedMask
      - IsEqual
      - IsGreater
      - IsGreaterEqual
      - IsLess
      - IsLessEqual
      - IsMatch
      - IsNotEqual
      - IsNotMatch

  - title: DataFrames
    contents:
      - get_dataframe
      - get_dataframe_query
      - get_tidy

  - title: AnnData interop
    contents:
      - DafAnnData
      - as_anndata
      - h5ad_as_daf
      - daf_as_h5ad

  - title: Chains & views
    contents:
      - chain_reader
      - chain_writer
      - view_daf
      - ViewDaf
      - ReadOnlyChainDaf
      - WriteChainDaf

  - title: Contracts
    contents:
      - Contract
      - ContractDaf
      - contract_scalar
      - contract_vector
      - contract_matrix
      - create_contract
      - axis_contract
      - tensor_contract
      - contract_docs
      - contractor
      - verify_input
      - verify_output
      - verify_contract
      - merge_contracts
      - expectation-constants

  - title: Mmap readers
    contents:
      - mmap_dgCMatrix
      - mmap_int
      - mmap_lgl
      - mmap_real

  - title: Op registry
    contents:
      - register_eltwise
      - register_reduction
      - registered_eltwise
      - registered_reductions
      - get_eltwise
      - get_reduction

  - title: Handlers
    contents:
      - register_dafr_handler
      - inefficient_action_handler
      - handler-constants

  - title: Version counters
    contents:
      - axis_version_counter
      - vector_version_counter
      - matrix_version_counter

  - title: Group helpers
    contents:
      - compact_groups
      - collect_group_members
      - group_names

  - title: Example data
    contents:
      - example_cells_daf
      - example_metacells_daf

  - title: Copy & concat
    contents:
      - copy_all
      - copy_axis
      - copy_vector
      - copy_matrix
      - copy_scalar
      - concatenate

  - title: Complete
    contents:
      - open_daf
      - complete_daf
      - complete_chain
```

(List may need adjustment based on actual NAMESPACE; check `Rscript -e 'getNamespaceExports("dafr")'` output to confirm coverage.)

### Step B.8: Spot-check pkgdown coverage

Add `dev/scripts/check-pkgdown-coverage.R`:

```r
#!/usr/bin/env Rscript
# Verifies every exported symbol appears in _pkgdown.yml reference sections.

pkg <- "dafr"
yml <- yaml::read_yaml("_pkgdown.yml")

exports <- sort(getNamespaceExports(pkg))
listed <- unlist(lapply(yml$reference, function(r) r$contents), use.names = FALSE)
listed <- sort(unique(listed))

missing <- setdiff(exports, listed)
extra <- setdiff(listed, exports)

if (length(missing) > 0L) {
    cat("Missing from _pkgdown.yml:\n")
    cat(paste0("  ", missing, "\n"), sep = "")
}
if (length(extra) > 0L) {
    cat("Listed in _pkgdown.yml but not exported:\n")
    cat(paste0("  ", extra, "\n"), sep = "")
}
if (length(missing) == 0L && length(extra) == 0L) {
    cat("_pkgdown.yml reference sections cover all exports.\n")
}
```

Run:
```
Rscript dev/scripts/check-pkgdown-coverage.R
```

Fix any missing / extra entries. Iterate until clean.

### Step B.9: Commit pkgdown config

```
git add _pkgdown.yml
git commit -m "docs(10d): add _pkgdown.yml with every export categorised"
```

Commit the coverage script in `dev/`:
```
cd dev && git add scripts/check-pkgdown-coverage.R && git commit -m "scripts(10d): add pkgdown coverage check"
cd ..
```

---

## Phase C: NEWS 0.1.0 + version bump + cran-comments stub

### Step C.1: Rewrite NEWS.md

Current top heading is `# dafr (development version)`. Rewrite as:

```markdown
# dafr 0.1.0 (2026-04-23)

First public release.

## Headline features

- **Full Daf data model** — scalars, per-axis vectors, per-axis-pair
  matrices, axis entries, cache invalidation, disk persistence.
- **Query DSL** — string form (`daf["@ cell : donor"]`) and pipe-chain
  builders (`daf[Axis("cell") |> LookupVector("donor")]`). 53 exported
  builders covering 5 categories (element-wise, reductions, selection,
  logical masks, comparison).
- **mmap-backed reads** — vectors and sparse matrices from a read-only
  FilesDaf are mmap'd with zero-copy access.
- **OpenMP-parallel kernels** — Sum, Mean, Var, Mode, Quantile, GeoMean
  dispatch to parallel C++ for large inputs.
- **AnnData interop** — `DafAnnData` R6 facade + `h5ad_as_daf` /
  `daf_as_h5ad` for round-trip I/O.
- **Contracts** — `contract_scalar` / `contract_vector` /
  `contract_matrix` / `tensor_contract` / `axis_contract` +
  `create_contract` + `verify_contract` for computation validation.
- **Class-surface sugar** — `is_daf`, `daf_name`, `complete_path`,
  `read_only`, `axis_version_counter` / `vector_version_counter` /
  `matrix_version_counter`, group helpers (`compact_groups`,
  `collect_group_members`, `group_names`), and DataFrame helpers
  (`get_dataframe`, `get_dataframe_query`, `get_tidy`).

## Known gaps

- `h5df` HDF5-backed Daf store (post-0.1.0).
- Sparse-matrix h5ad encoding, categorical obs/var columns, nested
  `uns` groups — currently skipped via the unsupported-feature handler
  (warn by default).
- `verify_contract` tracker-marker workaround (static-check semantics).
- `get_dataframe_query` dropped the wrapper's `columns` kwarg.
- CRAN submission pending installed-size and benchmarks-dir NOTE burn-down.

## Breaking changes vs. `dafJuliaWrapper` (Julia-facade)

- `get_frame` renamed to `get_dataframe_query`.
- `create_contract` takes typed per-category args, not a flat `data` list.
- `tensor_contract` parameter is now `type`, not `dtype`.
- Version counters return `integer`, not stringified UInt32.
- `read_only()` via 1-element `chain_reader`, not a new S7 class.

---

# dafr 0.1.0 — Development history

(The remainder of the file is the existing slice-by-slice ledger from the
development period, preserved as historical record. See the commit log
for the source-level narrative.)

<!-- Existing slice sections (10b, 10a, 10c, 9d-N, 9d-M, 9c, 9b, 9a,
8, 7, ...) follow below unchanged. -->
```

**Preserve the existing content below** — just prepend the new 0.1.0 entry.

### Step C.2: Bump version

Edit `DESCRIPTION`:
```
Version: 0.0.0.9000
```
→
```
Version: 0.1.0
```

Also update `Date:` or add a `Date:` line if you want (optional).

### Step C.3: cran-comments.md stub

Create `cran-comments.md`:

```markdown
## Release summary

This is the first public release of `dafr` — a native R + C++ port of
`DataAxesFormats.jl` (Julia). It does not supersede an existing CRAN
package.

## Test environments

- Local R 4.4+ on Linux — PASS, 2616+ testthat assertions.

## R CMD check results

0 errors | 0 warnings | 4 notes.

Known NOTEs (non-blocking, will be addressed before CRAN submission):

- Non-standard top-level directory `benchmarks/` — developer
  benchmark runner; will be `.Rbuildignore`'d before submission.
- Hidden directory `.claude/` — developer-tool session state;
  already gitignored, pending `.Rbuildignore` entry.
- Installed package size (~6 MB) — compiled C++ kernels + fixtures;
  `strip` pass + fixture subset pending.
- "Unable to verify current time" — build-environment flake.

## Notes

This package exports 130+ user-facing functions, a pipe-composable
query DSL, and an AnnData facade. See `NEWS.md` for the full 0.1.0
feature list.
```

### Step C.4: Commit NEWS + version + cran-comments

```
git add NEWS.md DESCRIPTION cran-comments.md
git commit -m "release(10d): NEWS 0.1.0, version bump, cran-comments stub"
```

---

## Phase Z: Merge + tag v0.1.0

### Step Z.1: Full devtools::check

```
Rscript -e 'devtools::check(error_on = "never", vignettes = TRUE)' 2>&1 | tail -40
```

Expected: 0 ERROR, 0 WARNING, ≤ 4 NOTEs (pre-existing carry-over). Any new NOTE must be fixed before merging.

Allow 15-20 minutes (vignette build adds time).

### Step Z.2: Final test run

```
R CMD INSTALL . && cd tests && NOT_CRAN=true Rscript testthat.R
```

Expected: 2616 PASS or similar.

### Step Z.3: Merge to main

```
git checkout main
git merge --no-ff slice-10d -m "merge(10d): docs + release polish — dafr 0.1.0"
```

### Step Z.4: Tag

```
git tag v0.1.0
git tag slice-10d
git describe --tags --exact-match HEAD
```

Expected: echoes one of the tags (likely `v0.1.0` with `slice-10d` visible via `git tag --points-at HEAD`).

### Step Z.5: Exit note

Write `dev/notes/slice-10d-exit.md` per the house style. Cover:
- Summary: 0.1.0 shipped; 4 vignettes, pkgdown config, README, cran-comments stub, version bump.
- Per-phase commits.
- Post-release roadmap (from the spec §10 — `h5df`, post-slice-10 cleanup, CRAN submission, dplyr verbs).

Commit in nested `dev/`:
```
cd dev && git add notes/slice-10d-exit.md && git commit -m "notes(10d): add exit note — dafr 0.1.0 shipped"
cd ..
```

---

## Self-review

- Spec §3 every locked decision → corresponding Phase step. ✓
- Spec §6 test plan (vignette build, pkgdown coverage) → Phase B. ✓
- Spec §8 ordering → Phase 0 / A / B / C / Z. ✓
- Spec §9 exit criterion → Phase Z steps. ✓
