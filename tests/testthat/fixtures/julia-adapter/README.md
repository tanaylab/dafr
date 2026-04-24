# Julia adapter fixture

Fixture for testing R/Julia compatibility of `adapter()` + `computation()`.

Regenerate with:

    conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
      dev/scripts/regen-julia-adapter-fixture.jl

## Contents

`fixture.json` fields:
- `daf_jl_head` — HEAD commit of DataAxesFormats.jl at regen time; the string `"HAND_WRITTEN"` flags that the fixture was produced by the R-side reference implementation rather than Julia regen.
- `result_returned` — value returned by the computation from `adapter()`.
- `total_umis_length` — length of the `total_umis` vector (856).
- `total_umis_values` — per-cell UMI totals as an integer vector; the R adapter+computation roundtrip must reproduce this bit-identically.
