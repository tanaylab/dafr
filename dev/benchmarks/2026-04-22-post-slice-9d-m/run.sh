#!/bin/bash
# Wrap profile.R in /usr/bin/time -v per thread count.
# Captures peak RSS (Maximum resident set size) which is what we care
# about for the G3 thread-bucket memory pathology.
#
# IMPORTANT: libgomp reads OMP_NUM_THREADS at DSO-load time, so we must
# export it in the shell before Rscript starts — setting it inside R
# is too late.
#
# Run from this directory.
set -euo pipefail

for t in 1 8 32 128; do
    echo "=============================================================="
    echo "OMP_NUM_THREADS=${t}"
    echo "=============================================================="
    OMP_NUM_THREADS="${t}" /usr/bin/time -v Rscript profile.R "${t}" 2> "rusage-threads-${t}.txt"
    # Echo the peak-RSS line from the rusage log.
    grep -E "Maximum resident|Elapsed|User time|System time" "rusage-threads-${t}.txt" || true
done

echo
echo "done; per-thread CSV + rusage files written to $(pwd)"
