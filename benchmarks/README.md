# dafr ↔ DAF.jl bake-off harness

Measures dafr (R) vs DataAxesFormats.jl (Julia) performance across a
shared query set and fixture corpus.

## Reproduce

**Prerequisite:** the bake-off loads `library(dafr)` from the *installed*
package, not the source tree. After any change to `R/*.R` or `src/*.cpp`,
run `R CMD INSTALL . --preclean` from the package root before step 3 or
the new code will not be exercised and results will be misleading.

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
