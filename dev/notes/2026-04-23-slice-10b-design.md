# Slice 10b — Design: AnnData Facade + h5ad Round-Trip

**Date:** 2026-04-23
**Predecessor:** Slice 10a (tag `slice-10a` on `main`).
**Parent kickoff:** `dev/notes/slice-10-kickoff.md` §"10b — AnnData".
**Scope:** 4 exports — `DafAnnData` R6 class, `as_anndata`, `h5ad_as_daf`, `daf_as_h5ad`. Pure R; `hdf5r` as Suggests; `R6` as new Imports.

## 1. Goal

Close the AnnData interop gap. Users migrating from `anndata` / `scanpy` / `Seurat` workflows need:
1. An in-memory facade (`DafAnnData`) that exposes a Daf as if it were an `AnnData`-shaped object.
2. Round-trippable h5ad I/O.

**Done signal.** 4 exports; ~100 assertions green; `h5ad_as_daf(p1)` then `daf_as_h5ad(d, p2)` yields a byte-identical round-trip on the fixture; merged to `main` with tag `slice-10b`.

## 2. Out of scope

- Write-through facade (the R6 wrapper is read-only; modifying `daf_ann$X[i,j] <- v` errors).
- `h5df` HDF5-backed Daf store (separate post-release slice).
- `SingleCellExperiment` / `Seurat` integration (not user-requested for 0.1.0).
- Non-h5ad formats (loom, zarr, etc.).

## 3. Locked decisions

| # | Topic | Decision |
|---|---|---|
| 1 | Facade class system | R6. Matches wrapper semantics. Read-only via active bindings that error on assignment. |
| 2 | `hdf5r` gating | `Suggests`; `rlang::check_installed("hdf5r")` at the entry of each of the 3 h5ad functions (`h5ad_as_daf`, `daf_as_h5ad` — not needed in `as_anndata` or `DafAnnData$new`). |
| 3 | `R6` Imports | `R6` moves to hard `Imports` (facade class is always usable). |
| 4 | Auto-axis detection | `obs_axis`: first of `"cell"`, `"metacell"` present on daf; `var_axis`: `"gene"` if present; else error with hint. |
| 5 | `x_name` default | `"UMIs"` (wrapper convention). Callers pass an alternate name for non-UMI datasets. |
| 6 | Facade guard message | `"DafAnnData facade is read-only. Use the underlying Daf object to modify data."` Match wrapper verbatim so downstream error-message matching keeps working. |
| 7 | Fixture | `inst/extdata/small_test.h5ad` (~10 KB, 50 obs × 20 var). Generated once by a repo script and checked in. |
| 8 | Write-path overwrite | `daf_as_h5ad(..., overwrite = FALSE)` checks file existence BEFORE opening hdf5r for write. Prevents silent clobber. |
| 9 | hdf5r cleanup | All `H5File` handles closed before return via `on.exit(h5$close_all())`. No live connections held. |
| 10 | String dtype | Python h5ad writes strings as `dtype = O` (object arrays). Decode explicitly: read as list → coerce via `vapply` + `rawToChar`. |
| 11 | Categorical (factor) encoding | `/obs/_index` strings with a `categories` group sibling. Factor levels round-trip as character vector (factor-ness not preserved to daf — which stores character anyway). |
| 12 | obs/var dtype preservation | Integer columns stay integer (not silently widened). Double columns stay double. Character columns stay character. |
| 13 | Layers / uns | `layers/<name>` maps to Daf matrices on `(obs_axis, var_axis)`. `uns/<name>` maps to Daf scalars or to nested groups (recurse). |
| 14 | Sparse encoding | Read CSR (`encoding-type = "csr_matrix"`) → transpose to CSC (Daf stores row-major in `obs × var` AnnData convention, but native Daf uses dgCMatrix in the orthogonal orientation). Handle both `csr_matrix` and `csc_matrix` encodings on read. |
| 15 | `unsupported_handler` | On encountering an h5ad feature that we don't translate (e.g. `raw`, `varm` with complex dtype, multidimensional uns), route through `register_dafr_handler("inefficient", handler)`. The `unsupported_handler` constant must be `ERROR_HANDLER` / `WARN_HANDLER` / `IGNORE_HANDLER` (10c exports). |
| 16 | Phase ordering | 0 → A (facade + as_anndata, no hdf5r) → B (h5ad I/O, hdf5r-gated) → Z (polish + tag). |

## 4. Surface specification

### 4.1 `DafAnnData` R6 class (R/anndata_facade.R)

Port the wrapper's `R/anndata_facade.R:25+` almost verbatim, adjusting Julia references to native R.

```r
DafAnnData <- R6::R6Class(
    "DafAnnData",
    public = list(
        daf = NULL,
        obs_axis = NULL,
        var_axis = NULL,
        x_name = NULL,
        initialize = function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
            if (!is_daf(daf)) {
                stop("`daf` must be a DafReader", call. = FALSE)
            }
            self$daf <- daf
            self$obs_axis <- obs_axis %||% .auto_obs_axis(daf)
            self$var_axis <- var_axis %||% .auto_var_axis(daf)
            self$x_name <- x_name
        }
    ),
    active = list(
        X = function(value) {
            if (!missing(value)) .read_only_error()
            get_matrix(self$daf, self$obs_axis, self$var_axis, self$x_name)
        },
        obs = function(value) {
            if (!missing(value)) .read_only_error()
            get_dataframe(self$daf, self$obs_axis)
        },
        var = function(value) {
            if (!missing(value)) .read_only_error()
            get_dataframe(self$daf, self$var_axis)
        },
        obs_names = function(value) {
            if (!missing(value)) .read_only_error()
            axis_entries(self$daf, self$obs_axis)
        },
        var_names = function(value) {
            if (!missing(value)) .read_only_error()
            axis_entries(self$daf, self$var_axis)
        },
        n_obs = function(value) {
            if (!missing(value)) .read_only_error()
            length(axis_entries(self$daf, self$obs_axis))
        },
        n_vars = function(value) {
            if (!missing(value)) .read_only_error()
            length(axis_entries(self$daf, self$var_axis))
        },
        shape = function(value) {
            if (!missing(value)) .read_only_error()
            c(self$n_obs, self$n_vars)
        },
        layers = function(value) {
            if (!missing(value)) .read_only_error()
            # Return a named list of matrices (excluding x_name).
            # Iterate format_matrices_set for (obs_axis, var_axis).
            ...
        },
        uns = function(value) {
            if (!missing(value)) .read_only_error()
            # Return a named list of all scalars on daf.
            ...
        }
    )
)
```

### 4.2 `as_anndata(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs")`

```r
#' @export
as_anndata <- function(daf, obs_axis = NULL, var_axis = NULL, x_name = "UMIs") {
    DafAnnData$new(daf, obs_axis = obs_axis, var_axis = var_axis, x_name = x_name)
}
```

### 4.3 `h5ad_as_daf(path, name = NULL, mode = "r", unsupported_handler = WARN_HANDLER)`

```r
#' @export
h5ad_as_daf <- function(path, name = NULL, mode = "r", unsupported_handler = WARN_HANDLER) {
    rlang::check_installed("hdf5r", reason = "for `h5ad_as_daf()`")
    old_handler <- ...  # save current inefficient handler
    inefficient_action_handler(unsupported_handler)
    on.exit(inefficient_action_handler(old_handler))

    h5 <- hdf5r::H5File$new(path, mode = mode)
    on.exit(h5$close_all(), add = TRUE)

    d <- memory_daf(name = name %||% tools::file_path_sans_ext(basename(path)))

    # 1. X → set_matrix(d, "obs", "var", "UMIs")
    # 2. obs → add_axis + set_vector per column
    # 3. var → add_axis + set_vector per column
    # 4. layers/ → set_matrix per name
    # 5. uns/ → set_scalar (flat) or nested walk
    ...

    d
}
```

### 4.4 `daf_as_h5ad(daf, path, obs_axis = NULL, var_axis = NULL, x_name = "UMIs", overwrite = FALSE, unsupported_handler = WARN_HANDLER)`

Inverse of `h5ad_as_daf`. Write:
- `X` dataset (dense or CSR) from `get_matrix(daf, obs_axis, var_axis, x_name)`.
- `obs/` group with one dataset per vector on obs_axis.
- `var/` group with one dataset per vector on var_axis.
- `layers/` group with one dataset per matrix (excluding `x_name`).
- `uns/` group with one dataset per scalar.

Overwrite check:
```r
if (!overwrite && file.exists(path)) {
    stop("file exists; pass overwrite = TRUE to replace", call. = FALSE)
}
```

## 5. Error handling

| Scenario | Response |
|---|---|
| `hdf5r` missing | `rlang::check_installed("hdf5r")` install-hint error |
| `h5ad_as_daf(nonexistent_path)` | hdf5r-level error surfaced |
| `daf_as_h5ad(..., overwrite = FALSE)` + existing file | "file exists; pass overwrite = TRUE" |
| `DafAnnData$new(not_a_daf)` | "`daf` must be a DafReader" |
| Missing obs/var axis on daf | "no axis named 'cell'/'metacell'/'gene'; pass obs_axis/var_axis explicitly" |
| Unsupported h5ad feature (raw, multidim uns, etc.) | dispatch via `inefficient_action_handler` — default `WARN_HANDLER` |
| Write to h5ad on daf with categorical column | cast to character; note via handler |
| Writing $X active binding | `"DafAnnData facade is read-only. Use the underlying Daf object to modify data."` |

## 6. Test plan

Budget: ~100 assertions across 3 new test files.

| File | Coverage | Assertions |
|---|---|---|
| `test-anndata-facade.R` | R6 class construction, all 10 active bindings, read-only guards, auto-axis detection, shape correctness | ~40 |
| `test-anndata-format.R` | h5ad round-trip on fixture: X, obs, var, layers, uns, obs_names, var_names; dtype preservation; overwrite guard; missing file error | ~45 |
| `test-anndata-handlers.R` | unsupported_handler dispatch (ignore/warn/error paths); WARN_HANDLER default | ~15 |

**Non-negotiable test mines:**
- `DafAnnData$X[i,j] <- v` errors with the exact wrapper message.
- `daf_as_h5ad(d, p, overwrite = FALSE)` errors BEFORE opening hdf5r.
- Fixture round-trip: `h5ad_as_daf(p1)` → `daf_as_h5ad(d, p2)` → `h5ad_as_daf(p2)` yields identical Daf structure (same axes, same vectors, same matrix).

## 7. Dependency changes

- `R6` → `Imports`.
- `hdf5r` → `Suggests`.

## 8. Slice execution order

- **Phase 0:** branch `slice-10b` off `main`.
- **Phase A:** `DafAnnData` R6 class + `as_anndata` factory. No hdf5r touch. `test-anndata-facade.R`.
- **Phase B:** `h5ad_as_daf` + `daf_as_h5ad` + `inst/extdata/small_test.h5ad` fixture. `test-anndata-format.R` + `test-anndata-handlers.R`.
- **Phase Z:** NEWS entry; merge; tag `slice-10b`; exit note.

## 9. Exit criterion

- 4 exports in NAMESPACE.
- ~100 new assertions green.
- `devtools::check` 0E / 0W / 4N (same pre-existing carry-over).
- h5ad fixture round-trip works.
- Merged + tagged.

## 10. Carry-over

Post-slice-10 cleanup remains the single catch-all for pre-existing issues.
