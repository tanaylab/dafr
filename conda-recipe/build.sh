#!/bin/bash
set -ex

export DISABLE_AUTOBREW=1

# `configure` probes $CONDA_PREFIX for c-blosc / libzstd, which is how a conda
# build gets packed/sharded Zarr v3 read. During conda-build the host prefix is
# $PREFIX, so point the probe at it.
export BLOSC_HOME="${PREFIX}"
export ZSTD_HOME="${PREFIX}"

${R} CMD INSTALL --build . ${R_ARGS}
