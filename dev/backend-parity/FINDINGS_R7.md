# Round 7 findings: cross-backend write+read parity

Audit kicked off 2026-05-17 per `dev/adversarial-parity/NEXT_AUDIT.md`.

**Final tally:**
- Day-3 single-backend round-trip: 11 / 246 reads diverged (4.5%).
- Day-4 cross-backend (6 pairs): 35 / 567 reads diverged (6.2%).
- All 35 diverged on exactly the same 5 bug classes (A-F below);
  cross-backend just multiplied the bug-class counts by the
  number of paths that touched the buggy backend.

After Day-5 fixes:
- Single-backend: 0 / 246. **Clean.**
- Cross-backend: 0 / 567. **Clean.**

Regression guards live in `tests/testthat/test-backend-parity-r7.R`
(one focused test per bug class).

---

## Bug class A — scalar value validators conflate NaN with NA

**Symptom:** `set_scalar(daf, "x", NaN)` raised
`Error: scalar 'x' value may not be NA`.

**Location:** `R/utils.R:49` (`.assert_scalar_value`) used
`is.na(value)`, which returns `TRUE` for `NaN`. The validator should
distinguish "missing" (true NA) from "not-a-number" (a valid Float64).

**Julia parity:** DataAxesFormats.jl accepts NaN scalars; only true
`missing` is rejected.

**Fix (`R/utils.R`):** `is.na(value) && !is.nan(value)`.

**Status: fixed.**

---

## Bug class B — UTF-8 string Encoding tag dropped (FilesDaf scalars)

**Symptom:** Memory-backed `unicode_str` scalar reads come back with
`Encoding() == "UTF-8"`; the same scalar via FilesDaf comes back with
`Encoding() == "unknown"`. Bytes are byte-identical and `identical()`
returns `TRUE`, but `serialize()` (and therefore content-addressable
hashing) differs.

**Severity:** practically negligible (Linux platform encoding is
UTF-8, byte identity is preserved). But the harness false-flagged it
on `unicode_str`, `unicode_axis` entries, `cell|unicode_label`, and
`unicode_axis|tag` dimnames - 6 false positives.

**Root cause:** `.read_scalar_json` fast-path reads with
`readChar(useBytes = TRUE)` and returns the regex-captured string
unchanged - so the resulting R string keeps the byte-tag.

**Fixes:**
- `R/files_io.R` `.read_scalar_json`: explicitly
  `Encoding(s) <- "UTF-8"` on the regex fast-path String branch.
- Harness `dev/backend-parity/serialize.R` (`.bp_canonicalise` /
  `.bp_names_hash`): `enc2utf8()` strings before hashing so
  bytes-equal-but-tag-different strings hash the same.

**Status: fixed (both content and harness).**

---

## Bug class C — Float64 scalar truncated to 5 sig figs (FilesDaf)

**Symptom:** `set_scalar(d, "pi_val", 3.141592653589793)` reads back
as `3.1416` from FilesDaf. Memory and Zarr return full precision.

**Root cause:** `.write_scalar_json` in `R/files_io.R` calls
`jsonlite::toJSON(obj, auto_unbox = FALSE)` without a `digits=`
argument; jsonlite defaults to `digits = 4`.

**Fix (`R/files_io.R`):** pass `digits = 17` (the minimum to round-
trip Float64 without precision loss).

**Status: fixed.**

---

## Bug class D — Int64 dense vectors truncated to low 32 bits (FilesDaf)

**Symptom:** A vector `[-2^62, 0, 1, 2^32, 2^62]` reads back as
`[0, 0, 1, 0, 0]` from FilesDaf. Pattern: every value whose
low 32 bits are zero becomes zero; values that fit in Int32 are
correct.

**Root cause:** `R/files_io.R` `.read_bin_dense` Int64 / UInt64
branches used `readBin(con, what = "integer", size = 8L, n = n)`.
Base R has no 8-byte integer type, so `readBin` silently truncated
each 8-byte word to its low 4 bytes.

**Fix:** read 8-byte doubles and bit-alias them into `integer64`
(same internal storage as `bit64::as.integer64`):
```r
raw_dbl <- readBin(con, what = "double", n = n, size = 8L,
                   endian = "little")
oldClass(raw_dbl) <- "integer64"
raw_dbl
```

**Status: fixed.**

---

## Bug class E — All-NaN Float64 vector reads back as all-zero (FilesDaf)

**Symptom:** `[NaN, NaN, NaN, NaN, NaN]` reads back as
`[0, 0, 0, 0, 0]` from FilesDaf. Memory and Zarr preserve NaN.
Mixed-NaN vectors (`[1, NaN, 3, NaN, 5]`) round-tripped correctly
because at least one non-NaN value forced the dense path.

**Root cause:** the auto-sparsifier's nnz count was
`sum(vec != 0, na.rm = TRUE)`. For NaN, `vec != 0` evaluates to `NA`
which `na.rm = TRUE` drops, so nnz becomes 0 and the vector is
written as a length-N empty sparse vector. The sparse reader
defaults missing positions to 0.

**Fixes (`R/files_io.R` + `R/files_daf_write.R`):**
- `.should_sparsify_numeric`: count NaN as nonzero:
  `sum(is.nan(vec) | (vec != 0), na.rm = TRUE)`.
- Sparse write path: `which(is.nan(vec) | vec != 0)` so NaN slots
  are written into the sparse representation.

**Status: fixed.**

---

## Bug class F — ZarrDaf did not reorder named-subset vectors

**Symptom:** `set_vector(d, "cell", "named_subset",
c(C = 2.5, A = 0.5, B = 1.5, E = 3.5, D = -1.0))` against ZarrDaf
read back as `[2.5, 0.5, 1.5, 3.5, -1]` (input order). Memory and
Files returned the axis-ordered `[0.5, 1.5, 2.5, -1.0, 3.5]`.

**Root cause:** `.validate_vector_value` (which validates AND
reorders) was called inside FilesDaf and MemoryDaf's
`format_set_vector` methods, but ZarrDaf had its own inline length
check and skipped the reorder.

**Fix (`R/writers.R`):** move the `.validate_vector_value` call into
the user-facing `set_vector` dispatcher so every backend receives
an axis-ordered, un-named vec. The per-backend calls inside
FilesDaf/MemoryDaf `format_set_vector` become idempotent no-ops on
the now-normalised input; they were retained as defense in depth.

**Status: fixed.**

---

## What this audit did NOT cover

Tracking these as "future round" candidates:

- **Float32 / Int8 / Int16 / UInt8 / UInt16 / UInt32 / UInt64
  fixtures.** R has no native storage for any of these (it has
  logical, integer, double, character, integer64). Round-tripping
  the missing dtypes requires a Julia-side fixture seed (the way
  Round 6 did) or a writeBin/readBin path that doesn't round-trip
  through R atomic types.
- **AltRep + mmap interactions** (deferred per NEXT_AUDIT.md).
- **H5adAsDaf / HttpDaf** backends - not in the Round-7 fixture
  matrix.
- **Sparse-matrix dropped-zero structural divergence.** Day-5
  follow-up extended the fixture with `sparse_with_explicit_zero`
  (built via `sparseMatrix(i, j, x)` with one `x == 0`) and
  `sparse_dropped_zero` (its `drop0()` twin). Both pairs round-
  trip with the original `(i, p, x)` triples preserved on all
  three backends and across all six `copy_all` pairs - no silent
  `drop0` normalisation. Regression guards in
  `tests/testthat/test-backend-parity-r7.R`.
- **Concat / chain_writer / view_daf round-trips** were in the
  audit's "in scope" list but only `copy_all` was exercised.
- **`set_*` immediately followed by `get_*` (cache correctness)**
  was implicitly verified for Memory (no persistence step) but
  not for Files / Zarr.

---

## Reproducibility

```sh
cd /home/aviezerl/src/dafr-native
Rscript dev/backend-parity/round_trip.R
python3 dev/backend-parity/diff.py dev/backend-parity/single_backend.jsonl

Rscript dev/backend-parity/cross_format.R
python3 dev/backend-parity/diff.py dev/backend-parity/cross_backend.jsonl
```

Both diff runs should report `total divergences: 0`.

Regression tests:
```sh
cd /home/aviezerl/src/dafr-native/tests
NOT_CRAN=true Rscript -e \
 'devtools::load_all(".."); testthat::test_file("testthat/test-backend-parity-r7.R")'
```

---

## Findings log

- Day 1 (2026-05-17): Bug A surfaced during fixture build. Workaround
  in place; fix deferred to Day 5.
- Day 3 (2026-05-17): Bugs B / C / D / E / F surfaced after the
  single-backend round-trip pass. 11 divergences / 243 reads.
- Day 4 (2026-05-17): cross-backend pass confirmed no new bug
  classes - just multipath multipliers. 35 divergences / 567 reads.
- Day 5 (2026-05-17): A / B / C / D / E / F fixed. Both harness
  runs report 0 divergences. Regression tests added.
