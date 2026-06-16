#!/usr/bin/env python
# Generate tests/testthat/fixtures/anndata_nullable_strings.h5ad
#
# Real anndata >= 0.12 file exercising the `nullable-string-array` encoding
# (a {values, mask} group, NOT a plain string dataset). In 0.12 this encoding
# is used for the obs/var `_index`, for categorical `categories`, and for plain
# pandas `string`-dtype columns. dafr must read all three; the older path only
# handled plain `string-array` datasets.
#
# Run in an env with anndata >= 0.12 (e.g. conda env `daf_env`):
#   conda run -n daf_env python dev/fixtures/generate_anndata_nullable_strings.py
#
# The committed fixture is deterministic; only re-run if the schema changes.
import anndata as ad
import numpy as np
import pandas as pd

# pandas `string` arrays write as `nullable-string-array`; opt in (default off
# in 0.12 because the encoding is newer than anndata < 0.11 can read).
ad.settings.allow_write_nullable_strings = True

obs = pd.DataFrame(
    {
        "cell_type": pd.Categorical(["B", "T", "B"]),       # categorical: nullable-string categories
        "batch": pd.array(["x", "y", "z"], dtype="string"),  # plain nullable-string-array column
        "n_umis": pd.array([10, None, 30], dtype="Int64"),   # nullable-integer column (masked NA)
        "is_doublet": pd.array([True, None, False], dtype="boolean"),  # nullable-boolean column (masked NA)
        "score": np.array([1.5, 2.5, 3.5]),                 # plain float (control)
    },
    index=["o1", "o2", "o3"],
)
# NB: a `string` column that actually CONTAINS a missing value is written by
# anndata 0.12 as `categorical` (codes with -1), not nullable-string-array, so
# `batch` is kept fully populated to exercise the nullable-string-array column
# path. Nullable `Int64`/`boolean` columns CAN carry an NA in-encoding (codes
# with a mask), so `n_umis`/`is_doublet` exercise the mask -> NA path directly.
# Encoding strings are asymmetric: `nullable-string-array` vs `nullable-integer`
# / `nullable-boolean` (no `-array` suffix).
var = pd.DataFrame(index=["v1", "v2"])
X = np.array([[11.0, 12.0], [21.0, 22.0], [31.0, 32.0]], dtype=np.float64)

a = ad.AnnData(X=X, obs=obs, var=var)
out = "tests/testthat/fixtures/anndata_nullable_strings.h5ad"
a.write_h5ad(out)
print("wrote", out, "with anndata", ad.__version__)
