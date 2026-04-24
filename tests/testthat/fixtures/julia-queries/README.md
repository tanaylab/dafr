# Julia queries fixture

Fixture for testing R/Julia compatibility of the query DSL.

Regenerate with:

    conda run -n dafr-mcview julia --project=/home/aviezerl/src/DataAxesFormats.jl \
      dev/scripts/regen-julia-queries-fixture.jl

## Contents

- `fixture.json` — list of `{query, canonical, kind, value}` records,
  where `kind` is one of `scalar`, `vector`, `matrix`, `names`.
- `example-daf/` — FilesDaf dump of `example_cells_daf()`; the R test
  reads this via `files_daf(..., mode = "r")`.
