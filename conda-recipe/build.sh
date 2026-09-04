#!/bin/bash
set -ex

export DISABLE_AUTOBREW=1

# `configure` probes $CONDA_PREFIX for c-blosc / libzstd, which is how a conda
# build gets packed/sharded Zarr v3 read. During conda-build the host prefix is
# $PREFIX, so point the probe at it.
export BLOSC_HOME="${PREFIX}"
export ZSTD_HOME="${PREFIX}"

# `source: path: ..` copies the working tree as it is, objects included. A
# stale src/*.o from a local devtools build would be linked instead of being
# recompiled here, so the codec paths this recipe enables would never actually
# be built. Start from source every time.
rm -f src/*.o src/*.so src/Makevars

${R} CMD INSTALL --build . ${R_ARGS}
