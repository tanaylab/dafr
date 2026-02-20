#!/bin/bash
set -euo pipefail

# Install the R package
${R} CMD INSTALL --build . -l "${PREFIX}/lib/R/library"

# Set up Julia packages needed by dafr
# This installs DataAxesFormats.jl and its dependencies into the default Julia environment
# so they are available when the user calls setup_daf()
if command -v julia &> /dev/null; then
    echo "Installing Julia dependencies for dafr..."
    julia -e '
    using Pkg
    Pkg.add("Suppressor")
    Pkg.add("RCall")
    Pkg.add("Logging")
    Pkg.add("DataFrames")
    Pkg.add("HDF5")
    Pkg.add("LinearAlgebra")
    Pkg.add("Muon")
    Pkg.add("NamedArrays")
    Pkg.add("SparseArrays")
    Pkg.add(url="https://github.com/tanaylab/TanayLabUtilities.jl")
    Pkg.add(url="https://github.com/tanaylab/DataAxesFormats.jl")
    Pkg.precompile()
    '
    echo "Julia dependencies installed successfully."
else
    echo "WARNING: Julia not found during build. Julia packages must be installed manually."
    echo "After installing Julia, run in R:"
    echo '  library(dafr)'
    echo '  setup_daf()'
fi
