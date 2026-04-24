# Slice 9b — Perf Parity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Reach tiered perf parity with DAF.jl across the query surface: build a reproducible dafr↔DAF.jl bake-off harness, measure gaps against per-tier thresholds, close every breach classified as **Fix**, resolve the G3 kernel memory explosion.

**Architecture:** Two coordinated runners (R `bench::mark`, Julia `BenchmarkTools.jl`) read a shared YAML query set and execute against five SHA256-verified on-disk FilesDaf fixtures. A comparison script joins both CSVs, tags breaches per-tier, and emits a markdown triage report. Per-gap fixes each ship as one commit (or small chain) with the affected query re-measured and appended to a perf log.

**Tech Stack:** R (cpp11, bench, jsonlite, yaml, Matrix, S7), Julia 1.12 (DataAxesFormats.jl at `49fbba14`, BenchmarkTools.jl), bash.

**Design doc:** `dev/notes/2026-04-22-slice-9b-design.md` (dev repo commit `c8ccadd`).

**Kickoff:** `dev/notes/slice-9b-kickoff.md`.

**Pre-work already landed (not a task here):** CI fix (helper + Windows path) at package-repo commit `50fa293`, merged into `main`, CI green on ubuntu + macos + windows.

**Tier gates (design locked):**

| Tier | Threshold (`ratio = dafr/julia`) |
|------|----|
| kernel | ≤ 1.2× |
| blas | ≤ 1.1× |
| mmap | ≤ 1.5× |
| light | ≤ 2.0× |
| grouped | ≤ 1.2× (same as kernel; grouped kernels are C++-backed) |
| chain | ≤ 2.0× (dispatch-dominated; treat as light) |
| complete | ≤ 2.0× (dispatch-dominated; treat as light) |

---

## Task 0: Branch setup

**Files:** none (git operation).

- [ ] **Step 1: Create feature branch**

```bash
cd /home/aviezerl/src/dafr-native
git checkout -b slice-9b-perf-parity
git status
```

Expected: `On branch slice-9b-perf-parity`. Clean working tree.

- [ ] **Step 2: Verify CI-fix commit is present on the branch**

```bash
git log --oneline -1
```

Expected: HEAD is at `50fa293 fix(ci): restore R CMD check cleanliness across platforms` (or a later commit if any have landed).

---

## Task 1: Harness directory scaffold + gitignore

**Files:**
- Create: `benchmarks/README.md`
- Create: `benchmarks/.gitignore`
- Modify: `.Rbuildignore` (add `benchmarks/fixture/data` and `benchmarks/.*\.cache` so generated fixtures don't ship in the CRAN tarball)

- [ ] **Step 1: Create benchmarks/README.md (skeleton; fill in as tasks complete)**

```markdown
# dafr ↔ DAF.jl bake-off harness

Measures dafr (R) vs DataAxesFormats.jl (Julia) performance across a
shared query set and fixture corpus.

## Reproduce

1. Build fixtures (one-time, ~30s):

       Rscript benchmarks/fixture/build-fixture.R

2. Sync the light-tier queries from the julia-queries test fixture
   (re-run whenever that fixture changes):

       Rscript benchmarks/build-queries.R

3. Run R-side:

       Rscript benchmarks/R/run-bakeoff.R --out /tmp/r-times.csv

4. Run Julia-side (needs `conda activate dafr-mcview` first):

       julia --project=benchmarks/julia benchmarks/julia/run_bakeoff.jl --out /tmp/julia-times.csv

5. Compare:

       Rscript benchmarks/compare.R --r /tmp/r-times.csv \
           --julia /tmp/julia-times.csv --out /tmp/report.md

See `dev/notes/2026-04-22-slice-9b-design.md` for the design rationale.
```

- [ ] **Step 2: Create benchmarks/.gitignore**

```
fixture/data/
*.cache
```

- [ ] **Step 3: Update .Rbuildignore**

Append these lines to `/home/aviezerl/src/dafr-native/.Rbuildignore`:

```
^benchmarks/fixture/data$
^benchmarks/.*\.cache$
```

- [ ] **Step 4: Commit**

```bash
git add benchmarks/README.md benchmarks/.gitignore .Rbuildignore
git commit -m "bench(9b): scaffold benchmarks/ directory"
```

---

## Task 2: Fixture builder — fixtures 1, 3, 4, 5

**Files:**
- Create: `benchmarks/fixture/build-fixture.R`

This task covers the four fast fixtures (cells_daf, chain_triple, view_renamed, mmap_reopen). The `big_sparse` fixture is in its own task because it's the only one that needs a reproducible-seed sparse generator and nnz tuning.

- [ ] **Step 1: Create the fixture builder**

File `benchmarks/fixture/build-fixture.R`:

```r
#!/usr/bin/env Rscript
# Build the shared FilesDaf fixture corpus for the bake-off harness.
# Idempotent: existing fixtures are removed and rebuilt.

suppressPackageStartupMessages({
    library(dafr)
    library(Matrix)
})

fixture_root <- file.path(
    normalizePath(dirname(sys.frame(1)$ofile), winslash = "/"),
    "data"
)
if (dir.exists(fixture_root)) unlink(fixture_root, recursive = TRUE)
dir.create(fixture_root, recursive = TRUE)

.sha256_dir <- function(path) {
    files <- sort(list.files(path, recursive = TRUE, full.names = TRUE))
    hashes <- vapply(files, function(f) {
        digest::digest(file = f, algo = "sha256")
    }, character(1L))
    digest::digest(paste(basename(files), hashes, collapse = "\n"),
                   algo = "sha256")
}

.log <- function(name, path) {
    cat(sprintf("  built %-20s sha256=%s\n  at %s\n",
                name, substr(.sha256_dir(path), 1, 16), path))
}

# ---------- fixture 1: cells_daf ----------
cells_daf_dir <- file.path(fixture_root, "cells_daf")
src <- example_cells_daf()
dst <- files_daf(cells_daf_dir, name = "cells", mode = "w+")
copy_all(source = src, destination = dst)
rm(dst); gc()
.log("cells_daf", cells_daf_dir)

# ---------- fixture 3: chain_triple ----------
chain_root <- file.path(fixture_root, "chain_triple")
dir.create(chain_root)
base_dir <- file.path(chain_root, "base")
mid_dir  <- file.path(chain_root, "mid")
leaf_dir <- file.path(chain_root, "leaf")
base <- files_daf(base_dir, name = "base", mode = "w+")
copy_all(source = example_cells_daf(), destination = base)
mid  <- files_daf(mid_dir,  name = "mid",  mode = "w+")
complete_chain(base_daf = base, new_daf = mid, absolute = TRUE)
leaf <- files_daf(leaf_dir, name = "leaf", mode = "w+")
complete_chain(base_daf = mid,  new_daf = leaf, absolute = TRUE)
.log("chain_triple", chain_root)

# ---------- fixture 4: view_renamed ----------
view_root <- file.path(fixture_root, "view_renamed")
dir.create(view_root)
vr_base_dir <- file.path(view_root, "base")
vr_leaf_dir <- file.path(view_root, "leaf")
vr_base <- files_daf(vr_base_dir, name = "base", mode = "w+")
copy_all(source = example_cells_daf(), destination = vr_base)
vr_leaf <- files_daf(vr_leaf_dir, name = "leaf", mode = "w+")
complete_chain(
    base_daf = vr_base, new_daf = vr_leaf,
    axes = list(list("renamed_cell", "@ cell")),
    absolute = TRUE
)
.log("view_renamed", view_root)

# ---------- fixture 5: mmap_reopen ----------
# Same bytes as cells_daf, isolated directory so reopen benchmarks don't
# race against in-memory handles from the other fixtures.
mmap_dir <- file.path(fixture_root, "mmap_reopen")
dst2 <- files_daf(mmap_dir, name = "mmap", mode = "w+")
copy_all(source = example_cells_daf(), destination = dst2)
rm(dst2); gc()
.log("mmap_reopen", mmap_dir)

cat("\nNext: run benchmarks/fixture/build-big-sparse.R\n")
```

- [ ] **Step 2: Verify the four fast fixtures build end-to-end**

```bash
cd /home/aviezerl/src/dafr-native
Rscript benchmarks/fixture/build-fixture.R
```

Expected: 4 `built …` lines, no errors. `benchmarks/fixture/data/` contains
4 subdirectories.

- [ ] **Step 3: Commit**

```bash
git add benchmarks/fixture/build-fixture.R
git commit -m "bench(9b): fixture builder for cells_daf, chain_triple, view_renamed, mmap_reopen"
```

---

## Task 3: Fixture builder — big_sparse

**Files:**
- Create: `benchmarks/fixture/build-big-sparse.R`

Separate file because the 10 000 × 10 000 sparse matrix is the slow part of
fixture construction and users may want to regenerate just this one.

- [ ] **Step 1: Create the big-sparse builder**

File `benchmarks/fixture/build-big-sparse.R`:

```r
#!/usr/bin/env Rscript
# Build the big_sparse fixture: 10k × 10k dgCMatrix, ~5 % nnz,
# plus group_100 and group_1000 vectors on rows.

suppressPackageStartupMessages({
    library(dafr)
    library(Matrix)
})

fixture_root <- file.path(
    normalizePath(dirname(sys.frame(1)$ofile), winslash = "/"),
    "data"
)
big_dir <- file.path(fixture_root, "big_sparse")
if (dir.exists(big_dir)) unlink(big_dir, recursive = TRUE)

N <- 10000L
nnz_target <- as.integer(0.05 * N * N)
set.seed(9001L)

# Reproducible uniform-random sparse: pick nnz_target (i, j) pairs, dedupe.
idx <- unique(data.frame(
    i = sample.int(N, nnz_target, replace = TRUE),
    j = sample.int(N, nnz_target, replace = TRUE)
))
# top up to ~5 % nnz after dedupe
while (nrow(idx) < nnz_target) {
    extra <- data.frame(
        i = sample.int(N, nnz_target - nrow(idx), replace = TRUE),
        j = sample.int(N, nnz_target - nrow(idx), replace = TRUE)
    )
    idx <- unique(rbind(idx, extra))
}
idx <- idx[seq_len(nnz_target), ]
vals <- runif(nnz_target, min = 0.1, max = 10)

m <- Matrix::sparseMatrix(i = idx$i, j = idx$j, x = vals, dims = c(N, N))
stopifnot(inherits(m, "dgCMatrix"))
rownames(m) <- sprintf("row_%05d", seq_len(N))
colnames(m) <- sprintf("col_%05d", seq_len(N))

# Reproducible group labels
group_100  <- sprintf("g100_%03d",  sample.int(100L,  N, replace = TRUE))
group_1000 <- sprintf("g1000_%04d", sample.int(1000L, N, replace = TRUE))

d <- files_daf(big_dir, name = "big_sparse", mode = "w+")
add_axis(d, "row", rownames(m))
add_axis(d, "col", colnames(m))
set_matrix(d, "row", "col", "value", m)
set_vector(d, "row", "group_100",  group_100)
set_vector(d, "row", "group_1000", group_1000)
rm(d); gc()

cat(sprintf("  built big_sparse           nnz=%d (%.3f%%)\n",
            nnz_target, 100 * nnz_target / (N * N)))
```

- [ ] **Step 2: Verify big_sparse builds**

```bash
Rscript benchmarks/fixture/build-big-sparse.R
```

Expected: `built big_sparse nnz=5000000 (5.000%)`, no errors.

- [ ] **Step 3: Spot-check contents**

```bash
Rscript -e '
d <- dafr::files_daf("benchmarks/fixture/data/big_sparse", name="big", mode="r")
cat("axes:", paste(dafr::axes_set(d), collapse=", "), "\n")
cat("matrix names:", paste(dafr::matrix_names(d, "row", "col"), collapse=", "), "\n")
cat("vector names (row):", paste(dafr::vector_names(d, "row"), collapse=", "), "\n")
'
```

Expected: `axes: row, col`, `matrix names: value`, `vector names (row): group_100, group_1000`.

- [ ] **Step 4: Commit**

```bash
git add benchmarks/fixture/build-big-sparse.R
git commit -m "bench(9b): big_sparse fixture (10k × 10k, 5% nnz, grouped vectors)"
```

---

## Task 4: Query-set YAML with hand-authored perf tiers

**Files:**
- Create: `benchmarks/queries.yaml` (hand-authored tiers only; light tier comes from Task 5)
- Suggests addition: `yaml` package (already transitively present; confirm)

- [ ] **Step 1: Confirm yaml is installed**

```bash
Rscript -e 'cat("yaml available:", requireNamespace("yaml", quietly=TRUE))'
```

If `FALSE`, install it with `install.packages("yaml")` and add to `Suggests:` in `DESCRIPTION`.

- [ ] **Step 2: Create queries.yaml with kernel, blas, grouped, mmap, chain, complete tiers**

File `benchmarks/queries.yaml`:

```yaml
# Single source of truth for bake-off queries.
# The `light` tier at the top is machine-regenerated from the julia-queries
# fixture by benchmarks/build-queries.R — edit only the other tiers here.

# --- light ---
# (populated by build-queries.R; DO NOT hand-edit below this marker)
#<LIGHT-START>
#<LIGHT-END>

# --- kernel (big_sparse column/row reductions) ---
- id: kernel_sum_row
  text: "/ row : value @ col %> Sum"
  category: kernel
  fixture: big_sparse

- id: kernel_sum_col
  text: "/ col : value @ row %> Sum"
  category: kernel
  fixture: big_sparse

- id: kernel_mean_row
  text: "/ row : value @ col %> Mean"
  category: kernel
  fixture: big_sparse

- id: kernel_mean_col
  text: "/ col : value @ row %> Mean"
  category: kernel
  fixture: big_sparse

- id: kernel_var_row
  text: "/ row : value @ col %> Var"
  category: kernel
  fixture: big_sparse

- id: kernel_var_col
  text: "/ col : value @ row %> Var"
  category: kernel
  fixture: big_sparse

- id: kernel_max_row
  text: "/ row : value @ col %> Max"
  category: kernel
  fixture: big_sparse

- id: kernel_max_col
  text: "/ col : value @ row %> Max"
  category: kernel
  fixture: big_sparse

- id: kernel_median_row
  text: "/ row : value @ col %> Median"
  category: kernel
  fixture: big_sparse

- id: kernel_mode_row
  text: "/ row : value @ col %> Mode"
  category: kernel
  fixture: big_sparse

- id: kernel_geomean_row
  text: "/ row : value @ col %> GeoMean"
  category: kernel
  fixture: big_sparse

- id: kernel_geomean_col
  text: "/ col : value @ row %> GeoMean"
  category: kernel
  fixture: big_sparse

# --- grouped (G1/G2/G3) ---
- id: grouped_g1_sum_100
  text: "/ row : value @ col %> Sum / group_100 >| Sum"
  category: grouped
  fixture: big_sparse

- id: grouped_g1_sum_1000
  text: "/ row : value @ col %> Sum / group_1000 >| Sum"
  category: grouped
  fixture: big_sparse

- id: grouped_g2_mean_100
  text: "/ row / col :: value -/ group_100 >- Mean"
  category: grouped
  fixture: big_sparse

- id: grouped_g2_mean_1000
  text: "/ row / col :: value -/ group_1000 >- Mean"
  category: grouped
  fixture: big_sparse

- id: grouped_g3_mean_100
  text: "/ row / col :: value |/ group_100 >| Mean"
  category: grouped
  fixture: big_sparse

- id: grouped_g3_mean_1000
  text: "/ row / col :: value |/ group_1000 >| Mean"
  category: grouped
  fixture: big_sparse

- id: grouped_g4a_sum_100
  text: "/ row : value @ col %> Sum / group_100 >- Sum"
  category: grouped
  fixture: big_sparse

- id: grouped_g4b_sum_100
  text: "/ row : value @ col %> Sum / group_100 >| Mean"
  category: grouped
  fixture: big_sparse

- id: grouped_g3_max_100
  text: "/ row / col :: value |/ group_100 >| Max"
  category: grouped
  fixture: big_sparse

# --- blas (dense double matrix ops) ---
- id: blas_cells_umi_row_mean
  text: "/ cell : UMIs @ gene %> Mean"
  category: blas
  fixture: cells_daf

- id: blas_cells_umi_col_sum
  text: "/ gene : UMIs @ cell %> Sum"
  category: blas
  fixture: cells_daf

- id: blas_big_dense_row_sum
  text: "/ row / col :: value |> Float64 / col %> Sum"
  category: blas
  fixture: big_sparse

- id: blas_big_dense_col_sum
  text: "/ row / col :: value |> Float64 / row %> Sum"
  category: blas
  fixture: big_sparse

# --- mmap (reopen + read) ---
- id: mmap_open_read_matrix
  text: "/ cell / gene :: UMIs"
  category: mmap
  fixture: mmap_reopen
  reopen: true   # runner closes + reopens the FilesDaf each iteration

- id: mmap_open_read_vector
  text: "/ cell : donor"
  category: mmap
  fixture: mmap_reopen
  reopen: true

- id: mmap_open_read_scalar
  text: ". organism"
  category: mmap
  fixture: mmap_reopen
  reopen: true

- id: mmap_open_read_axis
  text: "@ cell"
  category: mmap
  fixture: mmap_reopen
  reopen: true

# --- chain (3-layer read-through) ---
- id: chain_read_scalar
  text: ". organism"
  category: chain
  fixture: chain_triple

- id: chain_read_vector
  text: "/ cell : donor"
  category: chain
  fixture: chain_triple

- id: chain_read_matrix
  text: "/ cell / gene :: UMIs"
  category: chain
  fixture: chain_triple

- id: chain_reduce
  text: "/ cell : UMIs @ gene %> Sum"
  category: chain
  fixture: chain_triple

# --- complete (reopen + query through reconstructed view) ---
- id: complete_reopen_axis
  text: "@ renamed_cell"
  category: complete
  fixture: view_renamed
  reopen: true

- id: complete_reopen_vector
  text: "/ renamed_cell : donor"
  category: complete
  fixture: view_renamed
  reopen: true
```

- [ ] **Step 3: Validate the YAML parses**

```bash
Rscript -e '
q <- yaml::read_yaml("benchmarks/queries.yaml")
cat("entries:", length(q), "\n")
cat("categories:", paste(sort(unique(vapply(q, function(x) x$category, character(1L)))), collapse=", "), "\n")
'
```

Expected: `entries: 35`, `categories: blas, chain, complete, grouped, kernel, mmap` (no light yet).

- [ ] **Step 4: Commit**

```bash
git add benchmarks/queries.yaml
git commit -m "bench(9b): hand-authored query tiers (kernel/blas/grouped/mmap/chain/complete)"
```

---

## Task 5: Light-tier sync from julia-queries fixture

**Files:**
- Create: `benchmarks/build-queries.R`

- [ ] **Step 1: Write the sync script**

File `benchmarks/build-queries.R`:

```r
#!/usr/bin/env Rscript
# Regenerate the <LIGHT-START>...<LIGHT-END> block in benchmarks/queries.yaml
# from tests/testthat/fixtures/julia-queries/fixture.json.
# Idempotent. Safe to re-run whenever the julia-queries fixture changes.

suppressPackageStartupMessages({
    library(jsonlite)
})

fixture_path <- "tests/testthat/fixtures/julia-queries/fixture.json"
yaml_path    <- "benchmarks/queries.yaml"
light_fixture_name <- "cells_daf"

records <- read_json(fixture_path, simplifyVector = FALSE)
cat(sprintf("loaded %d julia-queries records\n", length(records)))

.yaml_str <- function(s) {
    # Emit as a single-quoted YAML scalar, escaping embedded single quotes.
    sprintf("'%s'", gsub("'", "''", s, fixed = TRUE))
}

light_lines <- character(0)
for (i in seq_along(records)) {
    r <- records[[i]]
    light_lines <- c(light_lines,
        sprintf("- id: julia_queries_%03d", i),
        sprintf("  text: %s",         .yaml_str(r$query)),
        sprintf("  category: light"),
        sprintf("  fixture: %s",       light_fixture_name),
        ""
    )
}

txt <- readLines(yaml_path)
start <- grep("^#<LIGHT-START>$", txt)
end   <- grep("^#<LIGHT-END>$",   txt)
stopifnot(length(start) == 1L, length(end) == 1L, start < end)

new_txt <- c(
    txt[seq_len(start)],
    light_lines,
    txt[seq(end, length(txt))]
)
writeLines(new_txt, yaml_path)
cat(sprintf("wrote %d light entries into %s\n", length(records), yaml_path))
```

- [ ] **Step 2: Run the sync**

```bash
Rscript benchmarks/build-queries.R
```

Expected: `loaded 51 julia-queries records` then `wrote 51 light entries into benchmarks/queries.yaml`.

- [ ] **Step 3: Verify the full yaml parses and totals 86 entries**

```bash
Rscript -e '
q <- yaml::read_yaml("benchmarks/queries.yaml")
cat("entries:", length(q), "\n")
print(table(vapply(q, function(x) x$category, character(1L))))
'
```

Expected: `entries: 86`, with counts `blas 4 | chain 4 | complete 2 | grouped 9 | kernel 12 | light 51 | mmap 4`.

- [ ] **Step 4: Commit**

```bash
git add benchmarks/build-queries.R benchmarks/queries.yaml
git commit -m "bench(9b): sync light tier (51 queries) from julia-queries fixture"
```

---

## Task 6: Bake-off runner — R side

**Files:**
- Create: `benchmarks/R/run-bakeoff.R`

- [ ] **Step 1: Write the runner**

File `benchmarks/R/run-bakeoff.R`:

```r
#!/usr/bin/env Rscript
# R-side bake-off runner.
#
# Usage:
#   Rscript benchmarks/R/run-bakeoff.R --out OUT.csv [--only id1,id2,...] [--fixture NAME]
#
# Reads benchmarks/queries.yaml, opens each required fixture once (unless
# reopen=true, in which case the fixture is reopened inside the benched expression),
# executes each query via bench::mark(), emits a CSV aligned with the Julia runner.

suppressPackageStartupMessages({
    library(dafr)
    library(bench)
    library(yaml)
    library(digest)
})

args <- commandArgs(trailingOnly = TRUE)
.arg <- function(flag, default = NULL) {
    i <- match(flag, args)
    if (is.na(i) || i == length(args)) return(default)
    args[i + 1L]
}
out_path    <- .arg("--out", "/tmp/r-times.csv")
only_ids    <- .arg("--only", NULL)
only_fixt   <- .arg("--fixture", NULL)
min_iter    <- as.integer(.arg("--min-iter", "5"))

# ---- single-threaded baseline unless --par ----
if (is.null(.arg("--par", NULL))) {
    Sys.setenv(OMP_NUM_THREADS = "1")
    options(dafr.kernel_threshold = Inf)   # force scalar code paths
    if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
        RhpcBLASctl::blas_set_num_threads(1)
        RhpcBLASctl::omp_set_num_threads(1)
    }
}

fixture_root <- "benchmarks/fixture/data"
queries <- yaml::read_yaml("benchmarks/queries.yaml")

if (!is.null(only_ids)) {
    ids <- strsplit(only_ids, ",", fixed = TRUE)[[1L]]
    queries <- Filter(function(q) q$id %in% ids, queries)
}
if (!is.null(only_fixt)) {
    queries <- Filter(function(q) q$fixture == only_fixt, queries)
}
cat(sprintf("running %d queries\n", length(queries)))

fixtures_needed <- unique(vapply(queries, function(q) q$fixture, character(1L)))
opened <- list()
for (name in fixtures_needed) {
    path <- file.path(fixture_root, name)
    if (name == "chain_triple") {
        opened[[name]] <- dafr::complete_daf(file.path(path, "leaf"), "r")
    } else if (name == "view_renamed") {
        opened[[name]] <- dafr::complete_daf(file.path(path, "leaf"), "r")
    } else {
        opened[[name]] <- dafr::files_daf(path, name = name, mode = "r")
    }
    cat(sprintf("opened fixture %s\n", name))
}

# ---- fixture checksums (sha256 per top-level file) ----
.sha256_dir <- function(path) {
    files <- sort(list.files(path, recursive = TRUE, full.names = TRUE))
    hashes <- vapply(files, function(f) digest::digest(file = f, algo = "sha256"),
                     character(1L))
    digest::digest(paste(basename(files), hashes, collapse = "\n"), algo = "sha256")
}
checksums <- vapply(fixtures_needed,
                    function(n) substr(.sha256_dir(file.path(fixture_root, n)), 1, 16),
                    character(1L))

# ---- benchmark loop ----
rows <- vector("list", length(queries))
for (k in seq_along(queries)) {
    q <- queries[[k]]
    reopen <- isTRUE(q$reopen)
    text <- q$text
    fixture_name <- q$fixture
    fixture_path <- file.path(fixture_root, fixture_name)

    expr <- if (reopen) {
        bquote({
            d <- if (.(fixture_name) == "view_renamed") {
                dafr::complete_daf(file.path(.(fixture_path), "leaf"), "r")
            } else {
                dafr::files_daf(.(fixture_path), name = .(fixture_name), mode = "r")
            }
            dafr::get_query(d, .(text))
        })
    } else {
        bquote(dafr::get_query(.(opened[[fixture_name]]), .(text)))
    }

    t0 <- Sys.time()
    b <- bench::mark(
        eval(expr),
        min_iterations = min_iter,
        filter_gc = FALSE,
        check = FALSE,
        time_unit = "ns"
    )
    dt <- as.numeric(Sys.time() - t0, units = "secs")
    cat(sprintf("  [%3d/%3d] %-30s %-22s ok (%.1fs)\n",
                k, length(queries), q$id, q$category, dt))

    rows[[k]] <- data.frame(
        query_id       = q$id,
        query_text     = q$text,
        category       = q$category,
        fixture        = q$fixture,
        median_time_ns = as.numeric(b$median),
        min_time_ns    = as.numeric(b$min),
        gc_time_ns     = sum(b$total_time_gc, na.rm = TRUE),
        allocations    = sum(as.numeric(b$mem_alloc), na.rm = TRUE),
        n_iter         = nrow(b$time[[1L]]),
        stringsAsFactors = FALSE
    )
}

df <- do.call(rbind, rows)

# ---- header lines ----
commit <- tryCatch(
    system("git rev-parse HEAD", intern = TRUE)[1L],
    error = function(e) "unknown"
)
header_lines <- c(
    sprintf("# runner: R"),
    sprintf("# dafr_commit: %s", commit),
    sprintf("# R_version: %s", getRversion()),
    sprintf("# platform: %s", R.version$platform),
    sprintf("# OMP_NUM_THREADS: %s", Sys.getenv("OMP_NUM_THREADS", "default")),
    sprintf("# BLAS: %s", extSoftVersion()[["BLAS"]]),
    sprintf("# fixtures: %s", paste(sprintf("%s=%s", names(checksums), checksums), collapse = "; "))
)

dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
writeLines(header_lines, out_path)
write.table(df, out_path, sep = ",", row.names = FALSE, col.names = TRUE,
            append = TRUE, qmethod = "double")
cat(sprintf("\nwrote %s (%d rows)\n", out_path, nrow(df)))
```

- [ ] **Step 2: Smoke-test on a tiny subset**

```bash
Rscript benchmarks/R/run-bakeoff.R --out /tmp/r-smoke.csv --only julia_queries_001,kernel_sum_row
```

Expected: `running 2 queries`, `opened fixture cells_daf`, `opened fixture big_sparse`, 2 progress rows, `wrote /tmp/r-smoke.csv (2 rows)`. No errors.

- [ ] **Step 3: Sanity-check the CSV**

```bash
head -15 /tmp/r-smoke.csv
```

Expected: 7 header lines starting with `#`, then the column header, then 2 data rows with non-zero `median_time_ns`.

- [ ] **Step 4: Commit**

```bash
git add benchmarks/R/run-bakeoff.R
git commit -m "bench(9b): R-side bake-off runner (bench::mark, fixture-open cache)"
```

---

## Task 7: Bake-off runner — Julia side

**Files:**
- Create: `benchmarks/julia/Project.toml`
- Create: `benchmarks/julia/run_bakeoff.jl`

- [ ] **Step 1: Create Project.toml**

File `benchmarks/julia/Project.toml`:

```toml
name = "DafrBakeoff"
uuid = "00000000-0000-0000-0000-000000000009"
authors = ["dafr"]
version = "0.1.0"

[deps]
DataAxesFormats = "dcbd2528-b13e-4e19-a9a7-3d76c79b01de"
BenchmarkTools = "6e4b80f9-dd63-53aa-95a3-0cdb28fa8baf"
YAML = "ddb6d928-2868-570f-bddf-ab3f9cf99eb6"
CSV = "336ed68f-0bac-5ca0-87d4-7b16caf5d00b"
DataFrames = "a93c6f00-e57d-5684-b7b6-d8193f3e46c0"
SHA = "ea8e919c-243c-51af-8825-aaa63cd721ce"
ArgParse = "c7e460c6-2fb9-53a9-8c5b-16f535851c63"
```

- [ ] **Step 2: Verify the Julia project resolves (dev-dep on DataAxesFormats)**

```bash
cd /home/aviezerl/src/dafr-native/benchmarks/julia
conda run -n dafr-mcview julia --project=. -e '
using Pkg
Pkg.develop(path="/home/aviezerl/src/DataAxesFormats.jl")
Pkg.develop(path="/home/aviezerl/src/TanayLabUtilities.jl")
Pkg.add(["BenchmarkTools","YAML","CSV","DataFrames","ArgParse"])
Pkg.status()
'
```

Expected: `Pkg.status()` lists DataAxesFormats dev'd, plus BenchmarkTools/YAML/CSV/DataFrames/ArgParse resolved. Project.toml and Manifest.toml are updated. **Commit both** — fixing the manifest is the price of reproducibility.

- [ ] **Step 3: Write the Julia runner**

File `benchmarks/julia/run_bakeoff.jl`:

```julia
#!/usr/bin/env julia
# Julia-side bake-off runner.
#
# Usage:
#   julia --project=benchmarks/julia benchmarks/julia/run_bakeoff.jl --out OUT.csv
#       [--only id1,id2,...] [--fixture NAME]

using DataAxesFormats
using BenchmarkTools
using YAML
using CSV
using DataFrames
using SHA
using LinearAlgebra
using ArgParse
using Dates

function parse_args_()
    s = ArgParseSettings()
    @add_arg_table! s begin
        "--out";     arg_type = String; default = "/tmp/julia-times.csv"
        "--only";    arg_type = String; default = ""
        "--fixture"; arg_type = String; default = ""
        "--par";     action = :store_true
    end
    return parse_args(s)
end

const ARGS_ = parse_args_()

if !ARGS_["par"]
    BLAS.set_num_threads(1)
    ENV["JULIA_NUM_THREADS"] = "1"
end

const FIXTURE_ROOT = joinpath(@__DIR__, "..", "fixture", "data")

function sha256_dir(path::AbstractString)
    files = sort(collect(walkdir(path) |> x -> Iterators.flatten([[joinpath(r, f) for f in fs] for (r, _, fs) in x])))
    buf = IOBuffer()
    for f in files
        open(f) do io
            write(buf, basename(f), "=", bytes2hex(SHA.sha256(io)), "\n")
        end
    end
    return bytes2hex(SHA.sha256(String(take!(buf))))[1:16]
end

function open_fixture(name::AbstractString)
    path = joinpath(FIXTURE_ROOT, name)
    if name == "chain_triple" || name == "view_renamed"
        # FilesDaf leaf has base_daf_repository scalar; DAF.jl's
        # complete_daf reconstructs the chain.
        return complete_daf(joinpath(path, "leaf"); mode = "r")
    else
        return FilesDaf(path; mode = "r", name = name)
    end
end

function reopen_fixture(name::AbstractString)
    return open_fixture(name)
end

function main()
    queries_all = YAML.load_file(joinpath(@__DIR__, "..", "queries.yaml"))
    queries = queries_all
    if !isempty(ARGS_["only"])
        ids = Set(split(ARGS_["only"], ","))
        queries = filter(q -> q["id"] in ids, queries)
    end
    if !isempty(ARGS_["fixture"])
        queries = filter(q -> q["fixture"] == ARGS_["fixture"], queries)
    end
    println("running $(length(queries)) queries")

    fixtures_needed = unique(q["fixture"] for q in queries)
    opened = Dict{String,Any}()
    for name in fixtures_needed
        opened[name] = open_fixture(name)
        println("opened fixture $name")
    end
    checksums = Dict(name => sha256_dir(joinpath(FIXTURE_ROOT, name))
                     for name in fixtures_needed)

    rows = DataFrame(query_id = String[], query_text = String[],
                     category = String[], fixture = String[],
                     median_time_ns = Float64[], min_time_ns = Float64[],
                     gc_time_ns = Float64[], allocations = Float64[],
                     n_iter = Int[])

    for (k, q) in enumerate(queries)
        text      = q["text"]
        name      = q["fixture"]
        reopen    = get(q, "reopen", false)

        bench = if reopen
            @benchmark get_query(reopen_fixture($name), $text)  samples=50 seconds=5 evals=1
        else
            daf = opened[name]
            @benchmark get_query($daf, $text) samples=50 seconds=5 evals=1
        end

        push!(rows, (q["id"], text, q["category"], name,
                     Float64(median(bench).time),
                     Float64(minimum(bench).time),
                     Float64(median(bench).gctime),
                     Float64(median(bench).memory),
                     length(bench.times)))

        @printf "  [%3d/%3d] %-30s %-22s ok\n" k length(queries) q["id"] q["category"]
    end

    commit = try
        strip(read(`git rev-parse HEAD`, String))
    catch
        "unknown"
    end

    header = [
        "# runner: Julia",
        "# dafr_commit: $commit",
        "# julia_version: $(VERSION)",
        "# platform: $(Sys.MACHINE)",
        "# JULIA_NUM_THREADS: $(get(ENV, "JULIA_NUM_THREADS", "default"))",
        "# BLAS: $(BLAS.get_config())",
        "# fixtures: " * join(["$k=$v" for (k,v) in checksums], "; "),
    ]

    mkpath(dirname(ARGS_["out"]))
    open(ARGS_["out"], "w") do io
        for line in header
            println(io, line)
        end
        CSV.write(io, rows, append = true, writeheader = true)
    end
    println("\nwrote $(ARGS_["out"]) ($(nrow(rows)) rows)")
end

using Printf
main()
```

- [ ] **Step 4: Smoke-test the Julia runner on the same two queries**

```bash
cd /home/aviezerl/src/dafr-native
conda run -n dafr-mcview julia --project=benchmarks/julia \
    benchmarks/julia/run_bakeoff.jl --out /tmp/julia-smoke.csv \
    --only julia_queries_001,kernel_sum_row
```

Expected: `running 2 queries`, 2 progress rows, `wrote /tmp/julia-smoke.csv (2 rows)`. No errors.

- [ ] **Step 5: Verify CSV**

```bash
head -15 /tmp/julia-smoke.csv
```

Expected: 7 header lines starting with `#`, then CSV header, then 2 data rows.

- [ ] **Step 6: Commit**

```bash
git add benchmarks/julia/Project.toml benchmarks/julia/Manifest.toml benchmarks/julia/run_bakeoff.jl
git commit -m "bench(9b): Julia-side bake-off runner (BenchmarkTools, shared fixtures)"
```

---

## Task 8: Comparison script + markdown report

**Files:**
- Create: `benchmarks/compare.R`

- [ ] **Step 1: Write the comparison script**

File `benchmarks/compare.R`:

```r
#!/usr/bin/env Rscript
# Join R and Julia bake-off CSVs on query_id, compute ratios, flag breaches.
#
# Usage:
#   Rscript benchmarks/compare.R --r R.csv --julia J.csv --out report.md
#       [--csv comparison.csv]

suppressPackageStartupMessages({
    library(dplyr, warn.conflicts = FALSE)
})

args <- commandArgs(trailingOnly = TRUE)
.arg <- function(flag, default = NULL) {
    i <- match(flag, args); if (is.na(i) || i == length(args)) return(default)
    args[i + 1L]
}
r_path   <- .arg("--r",      stop("--r required"))
j_path   <- .arg("--julia",  stop("--julia required"))
out_path <- .arg("--out",    "/tmp/report.md")
csv_path <- .arg("--csv",    sub("\\.md$", ".csv", out_path))

THRESHOLDS <- c(
    kernel   = 1.2,
    blas     = 1.1,
    mmap     = 1.5,
    light    = 2.0,
    grouped  = 1.2,
    chain    = 2.0,
    complete = 2.0
)

.read_csv <- function(path) {
    lines   <- readLines(path)
    headers <- grep("^#", lines, value = TRUE)
    data    <- grep("^#", lines, invert = TRUE, value = TRUE)
    df      <- read.csv(text = data, stringsAsFactors = FALSE)
    list(df = df, headers = headers)
}

.extract_fixtures <- function(headers) {
    line <- grep("^# fixtures:", headers, value = TRUE)[1L]
    if (is.na(line)) return(character(0))
    sort(trimws(strsplit(sub("^# fixtures:\\s*", "", line), ";", fixed = TRUE)[[1L]]))
}

r <- .read_csv(r_path); j <- .read_csv(j_path)
r_fix <- .extract_fixtures(r$headers); j_fix <- .extract_fixtures(j$headers)
if (!identical(r_fix, j_fix)) {
    stop(sprintf("fixture checksum mismatch:\n  R:     %s\n  Julia: %s",
                 paste(r_fix, collapse = "; "), paste(j_fix, collapse = "; ")))
}

joined <- inner_join(r$df, j$df,
                     by = c("query_id", "query_text", "category", "fixture"),
                     suffix = c("_r", "_j"))
if (nrow(joined) == 0L) stop("no query_ids joined")
if (nrow(joined) != nrow(r$df) || nrow(joined) != nrow(j$df)) {
    warning(sprintf("R had %d, Julia had %d, joined %d",
                    nrow(r$df), nrow(j$df), nrow(joined)))
}

joined$ratio     <- joined$median_time_ns_r / joined$median_time_ns_j
joined$threshold <- THRESHOLDS[joined$category]
joined$breach    <- joined$ratio > joined$threshold

write.csv(joined, csv_path, row.names = FALSE)
cat(sprintf("wrote %s\n", csv_path))

# ---- markdown report ----
.fmt_ns <- function(x) {
    units <- c("ns","µs","ms","s")
    i <- 1L; while (x >= 1000 && i < 4L) { x <- x / 1000; i <- i + 1L }
    sprintf("%6.2f %s", x, units[i])
}

md <- c(
    sprintf("# Bake-off: R vs Julia (%s)", Sys.Date()),
    "",
    "## Headers",
    "",
    "```", r$headers, "```", "",
    "```", j$headers, "```", ""
)

.section <- function(df, heading) {
    if (nrow(df) == 0L) return(c(sprintf("## %s", heading),
                                 "", "_(none)_", ""))
    df <- df[order(-df$ratio), ]
    tbl <- c(
        sprintf("## %s", heading),
        "",
        "| query_id | category | fixture | R (median) | J (median) | ratio | threshold |",
        "|---|---|---|---|---|---|---|"
    )
    for (i in seq_len(nrow(df))) {
        tbl <- c(tbl,
            sprintf("| %s | %s | %s | %s | %s | %.2f× | %.2f× |",
                    df$query_id[i], df$category[i], df$fixture[i],
                    .fmt_ns(df$median_time_ns_r[i]),
                    .fmt_ns(df$median_time_ns_j[i]),
                    df$ratio[i], df$threshold[i]))
    }
    c(tbl, "")
}

md <- c(md, .section(joined[joined$breach, ], "BREACHED"),
             .section(joined[!joined$breach, ], "Within threshold"))

writeLines(md, out_path)
cat(sprintf("wrote %s (%d breached / %d total)\n",
            out_path, sum(joined$breach), nrow(joined)))
```

- [ ] **Step 2: Smoke-test comparison**

```bash
Rscript benchmarks/compare.R --r /tmp/r-smoke.csv --julia /tmp/julia-smoke.csv --out /tmp/smoke-report.md
head -40 /tmp/smoke-report.md
```

Expected: two sections ("BREACHED", "Within threshold"), one of which has 2 rows. No errors.

- [ ] **Step 3: Verify fixture mismatch protection**

Corrupt a header in the smoke CSV and re-run:

```bash
sed -i 's/cells_daf=[^;]*/cells_daf=deadbeefdeadbeef/' /tmp/r-smoke.csv
Rscript benchmarks/compare.R --r /tmp/r-smoke.csv --julia /tmp/julia-smoke.csv --out /tmp/smoke-report.md || echo "rejected (expected)"
```

Expected: error `fixture checksum mismatch:`, exit non-zero. Restore the original header after.

- [ ] **Step 4: Commit**

```bash
git add benchmarks/compare.R
git commit -m "bench(9b): comparison script with checksum guard + markdown report"
```

---

## Task 9: Baseline run

**Files:**
- Create (outside repo): `dev/benchmarks/2026-04-22-baseline/r-times.csv`
- Create (outside repo): `dev/benchmarks/2026-04-22-baseline/julia-times.csv`
- Create (outside repo): `dev/benchmarks/2026-04-22-baseline/comparison.csv`
- Create (outside repo): `dev/benchmarks/2026-04-22-baseline/report.md`
- Create: `dev/benchmarks/perf-log.md` (append-only ledger)

- [ ] **Step 1: Full R-side run**

```bash
cd /home/aviezerl/src/dafr-native
mkdir -p dev/benchmarks/2026-04-22-baseline
Rscript benchmarks/R/run-bakeoff.R --out dev/benchmarks/2026-04-22-baseline/r-times.csv
```

Expected: `running 86 queries`, 86 progress lines, `wrote dev/benchmarks/2026-04-22-baseline/r-times.csv (86 rows)`.

- [ ] **Step 2: Full Julia-side run**

```bash
conda run -n dafr-mcview julia --project=benchmarks/julia \
    benchmarks/julia/run_bakeoff.jl \
    --out dev/benchmarks/2026-04-22-baseline/julia-times.csv
```

Expected: `running 86 queries`, 86 progress lines, `wrote .../julia-times.csv (86 rows)`.

- [ ] **Step 3: Compare**

```bash
Rscript benchmarks/compare.R \
    --r     dev/benchmarks/2026-04-22-baseline/r-times.csv \
    --julia dev/benchmarks/2026-04-22-baseline/julia-times.csv \
    --out   dev/benchmarks/2026-04-22-baseline/report.md
```

Expected: `wrote .../comparison.csv`, `wrote .../report.md (N breached / 86 total)` for some N.

- [ ] **Step 4: Inspect breaches**

```bash
head -80 dev/benchmarks/2026-04-22-baseline/report.md
```

Note the BREACHED queries, grouped by category. **Save this count: it is the
T4 triage input.**

- [ ] **Step 5: Seed the perf log**

Create `dev/benchmarks/perf-log.md`:

```markdown
# Slice 9b perf log

Append a row per gap-close. Newest at top.

| date | commit | breach_id | before_ratio | after_ratio | notes |
|---|---|---|---|---|---|
| 2026-04-22 | (baseline) | — | — | — | Full 86-query baseline captured; see `dev/benchmarks/2026-04-22-baseline/report.md`. |
```

- [ ] **Step 6: Commit baseline to dev repo**

```bash
cd /home/aviezerl/src/dafr-native/dev
git add benchmarks/2026-04-22-baseline benchmarks/perf-log.md
git commit -m "bench(slice-9b): T3 baseline — full 86-query run"
```

---

## Task 10: Triage (human-in-the-loop)

**Files:** none in this task — this is a human decision step where I enumerate the breaches and confirm each with the user.

- [ ] **Step 1: Enumerate breaches by category**

For each row where `breach = TRUE` in the baseline comparison CSV, draft a proposed disposition (`Fix`, `Defer`, `Accept`, `Investigate`) with a one-line justification. Organize by category (kernel first, then grouped, blas, mmap, chain, complete, light).

- [ ] **Step 2: Present the disposition list to the user and lock**

Present the triage table (breach_id | tier | ratio | proposed disposition | justification) and wait for user confirmation before opening any sub-task. Do NOT skip this gate.

- [ ] **Step 3: Write triage note and instantiate per-gap tasks**

Save the locked triage table as `dev/notes/2026-04-22-slice-9b-triage.md`
(dev repo). For each **Fix** disposition, instantiate a concrete task from
the Task 11 template — number them `11.1`, `11.2`, ..., `11.N`. Append the
instantiated tasks to this plan document (not a separate file) before
starting implementation so execution has a single source of truth.

- [ ] **Step 4: Commit triage**

```bash
cd /home/aviezerl/src/dafr-native/dev
git add notes/2026-04-22-slice-9b-triage.md
git commit -m "notes(slice-9b): T4 triage — disposition per breach"
```

---

## Task 11: Per-gap close — TEMPLATE

**Each `Fix` disposition from Task 10 becomes a concrete sub-task following
this template.** The template is not itself executable — it is copy-pasted
into the plan once per Fix breach after triage lands. Replace `<breach_id>`,
`<root_cause>`, `<file_path>`, `<before_ratio>`, `<target_ratio>`.

### Task 11.X: Close breach `<breach_id>`

**Files:**
- Modify: `<file_path>`
- (optional) Test: `tests/testthat/test-perf-<breach_id>.R` (regression
  guard if the fix introduces a code path not otherwise covered)

- [ ] **Step 1: Establish the root cause**

Before touching code, record in the commit message what the root cause is
(e.g., "R-side dispatch walks the full AST twice; Julia caches the parse"
or "CSC iteration re-decodes the pointer on every access"). A `Fix` without
a stated root cause is an `Investigate` that hasn't finished.

- [ ] **Step 2: Write the regression guard test (only if introducing a new code path)**

```r
# Only when the fix adds or rewires a code path. Pure inline optimizations
# of an existing path rely on the 1813-test suite for correctness.
test_that("<breach_id>: <new path> produces correct result", {
    # concrete expectation derived from .op_* formula authority
    expect_equal(<expr>, <expected>)
})
```

- [ ] **Step 3: Apply the fix in `<file_path>`**

One focused change. No drive-by refactors. If the fix requires splitting
a helper or extracting a shared routine, that's in scope; anything more
is a separate commit.

- [ ] **Step 4: Re-measure on just this query**

```bash
Rscript benchmarks/R/run-bakeoff.R --out /tmp/r-after.csv --only <breach_id>
conda run -n dafr-mcview julia --project=benchmarks/julia \
    benchmarks/julia/run_bakeoff.jl --out /tmp/julia-after.csv --only <breach_id>
Rscript benchmarks/compare.R --r /tmp/r-after.csv --julia /tmp/julia-after.csv \
    --out /tmp/after-report.md
```

Expected: `0 breached / 1 total`. If still breached, revert and reclassify in
triage (maybe `Investigate` or `Accept`).

- [ ] **Step 5: Run the full dafr test suite**

```bash
Rscript -e 'devtools::test()'
```

Expected: `[ FAIL 0 | WARN ≤1 | SKIP 1 | PASS ≥1813 ]`. Any new FAIL is a
correctness regression — revert the fix.

- [ ] **Step 6: Commit**

```bash
git add <file_path> [tests/testthat/test-perf-<breach_id>.R]
git commit -m "perf(9b): close <breach_id> — <root_cause_one_liner>"
```

- [ ] **Step 7: Append to perf log**

Add a row at the top of `dev/benchmarks/perf-log.md`:

```markdown
| 2026-MM-DD | <commit-sha> | <breach_id> | <before_ratio> | <after_ratio> | <root cause + fix summary> |
```

Commit in dev repo.

- [ ] **Step 8: Every 3–5 closes, full-suite bake-off re-run**

```bash
Rscript benchmarks/R/run-bakeoff.R --out /tmp/r-full.csv
conda run -n dafr-mcview julia --project=benchmarks/julia \
    benchmarks/julia/run_bakeoff.jl --out /tmp/julia-full.csv
Rscript benchmarks/compare.R --r /tmp/r-full.csv --julia /tmp/julia-full.csv \
    --out /tmp/mid-report.md
```

Compare breach list against the previous full run. Any new breach =
regression introduced by a recent close. Investigate before continuing.

---

## Task 12: G3 kernel memory fix

**Files:**
- Modify: `src/kernel_grouped_reduce_csc.cpp`
- Modify: `src/kernel_grouped_quantile_csc.cpp`
- Modify: `src/kernel_grouped_mode_csc.cpp`
- (optional) Modify: `src/openmp_shim.h` if a new scheduling primitive is needed
- Test: `tests/testthat/test-kernel-grouped-memory.R`

**Decision input:** at Task 9 (baseline), a **parallel** run labeled
`YYYY-MM-DD-par-128` is executed on a 128-core lab machine. The run either
reports an OOM/swap at the G3 queries (`grouped_g3_*`) or succeeds with
observed peak RSS. That data chooses between the three kickoff options.

### Task 12.0: Lab-machine profile run (precondition for choosing strategy)

- [ ] **Step 1: Run parallel variant on lab machine**

```bash
# on the 128-core lab machine:
cd /home/aviezerl/src/dafr-native
Rscript benchmarks/R/run-bakeoff.R --par --out /tmp/r-par.csv \
    --only grouped_g3_mean_100,grouped_g3_mean_1000,grouped_g3_max_100
```

Record peak RSS (via `/usr/bin/time -v` or `ps -o rss` sampling) and elapsed
time per query. Save summary as
`dev/benchmarks/2026-MM-DD-g3-profile/profile.md`.

- [ ] **Step 2: Choose strategy — confirm with user**

Match observed behaviour against the three candidates:

| Observation | Strategy |
|---|---|
| OOM / swap on 128 × 10k × 1000 case | **Row-partition fallback** (maintains parallelism) |
| Fits in RAM but slower than baseline due to allocation | **Adaptive thread cap** (simpler; loses parallelism at scale) |
| Only G3 × group_1000 is problematic; G3 × group_100 is fine | **Sequential fallback** beyond a tunable size threshold |

Present the chosen strategy to the user with rationale. Lock before coding.

### Task 12.X (instantiated from chosen option)

Once locked, the implementation gets its own set of concrete TDD tasks
following the Task 11 template: failing memory test first (assert peak
allocation under threshold for the chosen input), minimal implementation,
verify, commit.

**Common steps regardless of option:**

- [ ] Write a failing test in `tests/testthat/test-kernel-grouped-memory.R`
  that asserts the kernel allocates less than an option-specific upper bound
  on a configurable `(nthreads, nrow, ngroups)` triple (use `bench::mark`'s
  `mem_alloc` field).
- [ ] Apply the fix across all three kernels in lockstep — they share the
  bucket structure and any divergence between them is a latent bug.
- [ ] Re-run the G3 queries via `run-bakeoff.R --par` and confirm: no OOM,
  peak allocation within option budget, correctness preserved by full
  test-suite run.
- [ ] Add NEWS bullet (breaking if the strategy changes user-visible
  behaviour under memory pressure; otherwise enhancement).
- [ ] Commit.

---

## Task 13: Exit

**Files:**
- Create: `dev/notes/slice-9b-exit.md`
- Modify: `NEWS.md`

- [ ] **Step 1: Confirm exit criteria**

All of these must be TRUE before proceeding:

1. All **Fix** breaches closed; remaining tracked as **Accept** (with
   measurement) or **Defer** (with named target slice) in the triage note.
2. G3 memory fix in place (any of the three strategies) with the lab-machine
   parallel run showing no OOM.
3. Full `devtools::test()` green.
4. Full `devtools::check(args = c("--no-manual","--as-cran"), error_on = "warning")`
   clean (pre-existing structural notes OK: benchmarks dir, installed size).
5. CI green on ubuntu + macos + windows.
6. `benchmarks/` harness reproducible from a clean checkout of the branch.

- [ ] **Step 2: Write exit note**

File `dev/notes/slice-9b-exit.md`: final perf table (baseline vs closing),
disposition-per-breach list, G3 fix summary, any follow-up work for 9c.

- [ ] **Step 3: Write NEWS entry**

Top of `NEWS.md`, under a new `## Slice 9b — Perf parity with DAF.jl (YYYY-MM-DD)`
heading: summary of breaches closed, G3 fix one-liner, breaking changes if
any (G3 user-visible behaviour under memory pressure potentially; confirm
from Task 12).

- [ ] **Step 4: Commit exit artefacts**

```bash
# dev repo
cd /home/aviezerl/src/dafr-native/dev
git add notes/slice-9b-exit.md
git commit -m "notes(slice-9b): exit gate"
git push

# package repo
cd /home/aviezerl/src/dafr-native
git add NEWS.md
git commit -m "docs(slice-9b): NEWS entry for perf parity with DAF.jl"
```

- [ ] **Step 5: Merge to main**

```bash
git checkout main
git merge --no-ff slice-9b-perf-parity -m "merge: slice 9b — perf parity with DAF.jl"
git tag slice-9b
git push --follow-tags origin main
```

- [ ] **Step 6: Wait for CI green on main**

```bash
gh run watch --exit-status $(gh run list --limit 1 --json databaseId --jq '.[0].databaseId')
```

Expected: all three R-CMD-check jobs + altrep-sanity pass.

---

## Notes and invariants

- **Formula authority:** `R/operations.R` `.op_*` is the source of truth for
  every op's formula; any perf rewrite MUST match bit-exactly. The 1813-test
  suite is the correctness safety net.
- **cpp11 only** for any C++ kernel changes (NOT Rcpp). OpenMP via
  `openmp_shim.h` helpers (`DAFR_PARALLEL_FOR`, `dafr_omp_get_*`), never
  raw pragmas.
- **4-space R indent**, no tabs.
- **S7 multi-dispatch uses `list(ClassA, ...)` signatures.**
- **`dafr.kernel_threshold` option** gates parallel kernel dispatch; the
  baseline forces `Inf` (scalar paths).
- **Julia DAF pinned** at `49fbba14...` since Slice 3 (7 slices stable).
  Verify before regenerating fixtures: `cd ~/src/DataAxesFormats.jl && git rev-parse HEAD`.
- **`normalizePath` on Windows** produces backslashes; use
  `normalizePath(..., winslash = "/")` when comparing to forward-slash strings.
- **No emojis.** No `--no-verify`, no `--amend`, no force-push. NEW commits only.

## Deferred (not in 9b)

Per slice-9a-kickoff / 9a exit:

- `bestify` heuristic for `copy_vector` / `copy_matrix`
- `reconstruct_axis` with pre-existing target axis
- H5df / AnnData / Zarr backends
- Long-vector (>2³¹) ALTREP scenarios
- UInt32 > 2³¹ read arm (Slice-2 inherited)
- Multi-writer FS locking on FilesDaf
- `computation()` dual-/triple-contract forms
- `@examples` for the ~25 skipped exports
