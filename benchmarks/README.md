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
