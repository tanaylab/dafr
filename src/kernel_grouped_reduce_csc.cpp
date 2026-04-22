// src/kernel_grouped_reduce_csc.cpp
// Grouped reduction engine over dgCMatrix for the op family:
//   Sum / Mean / Min / Max / Var / Std / VarN / StdN / GeoMean
// Two axes:
//   axis=2 (G2, row-group):  output is ngroups x ncol  (ReduceToColumn)
//   axis=3 (G3, col-group):  output is nrow    x ngroups (ReduceToRow)
// Separate kernels (quantile-grouped / mode-grouped) land in Task 9.
// Dense twin lives in src/kernel_grouped_reduce_dense.cpp.
//
// Acc + derive_op live in kernel_grouped_acc.h so the dense kernel uses
// the exact same accumulator and per-op derivation (for dense input,
// a.nnz == n_total so n_zeros = 0 and derive_op naturally reduces to
// the ungrouped closed-form).

#include <cpp11.hpp>
#include "openmp_shim.h"
#include "kernel_grouped_acc.h"
#include <string>
#include <vector>

[[cpp11::register]]
cpp11::writable::doubles_matrix<cpp11::by_column> kernel_grouped_reduce_csc_cpp(
    cpp11::doubles x, cpp11::integers i, cpp11::integers p,
    int nrow, int ncol,
    cpp11::integers group,       // 1-based; length nrow (G2) or ncol (G3)
    int ngroups,
    cpp11::integers n_in_group,  // length ngroups; caller pre-tabulates
    int axis,                    // 2 = G2 (row-group), 3 = G3 (col-group)
    std::string op,              // "Sum"|"Mean"|"Min"|"Max"|"Var"|"Std"|"VarN"|"StdN"|"GeoMean"
    double eps,
    int threshold
) {
    using dafr_grouped::Acc;
    using dafr_grouped::acc_merge;
    using dafr_grouped::derive_op;

    const double *px = REAL(x.data());
    const int *pi = INTEGER(i.data());
    const int *pp = INTEGER(p.data());
    const int *pg = INTEGER(group.data());
    const int *png = INTEGER(n_in_group.data());

    const bool need_log = (op == "GeoMean");

    if (axis == 2) {
        // G2 (row-group): output is ngroups x ncol. One pass per column,
        // each column gets its own local accumulator vector of length ngroups.
        cpp11::writable::doubles_matrix<cpp11::by_column> out(ngroups, ncol);
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            std::vector<Acc> accs(ngroups);
            for (int g = 0; g < ngroups; ++g) accs[g].need_log = need_log;
            for (int k = pp[j]; k < pp[j + 1]; ++k) {
                const int g = pg[pi[k]] - 1;     // 1-based -> 0-based
                accs[g].push(px[k], eps);
            }
            for (int g = 0; g < ngroups; ++g) {
                out(g, j) = derive_op(op, accs[g], png[g], eps);
            }
        }
        return out;
    }

    // axis == 3 (G3, col-group): output is nrow x ngroups. Thread-bucket
    // pattern: tacc[tid] is a flat Acc vector of length nrow * ngroups,
    // indexed as (r + g * nrow) for locality when post-processing by
    // column of the output matrix.
    //
    // FIXME: this is O(nthreads * nrow * ngroups) memory. For large nrow
    // and ngroups (e.g. 1e6 * 100 cells * 8 threads ~= 10 GB of Acc),
    // consider a row-partitioned fallback. Not implemented in this slice.
    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    const int nthreads = dafr_omp_get_max_threads_capped(ncol, threshold);
    std::vector<std::vector<Acc>> tacc(nthreads,
        std::vector<Acc>((size_t)nrow * (size_t)ngroups));
    // Set need_log on every accumulator in each thread bucket.
    if (need_log) {
        for (int t = 0; t < nthreads; ++t)
            for (auto &a : tacc[t]) a.need_log = true;
    }

    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int j = 0; j < ncol; ++j) {
        const int tid = dafr_omp_get_thread_num();
        const int g = pg[j] - 1;   // 1-based -> 0-based; constant per column
        auto &tbuf = tacc[tid];
        const size_t base = (size_t)g * (size_t)nrow;
        for (int k = pp[j]; k < pp[j + 1]; ++k) {
            tbuf[base + (size_t)pi[k]].push(px[k], eps);
        }
    }

    // Serial merge of thread buckets.
    std::vector<Acc> accs((size_t)nrow * (size_t)ngroups);
    if (need_log) for (auto &a : accs) a.need_log = true;
    for (int t = 0; t < nthreads; ++t) {
        const auto &tb = tacc[t];
        const size_t N = accs.size();
        for (size_t idx = 0; idx < N; ++idx) {
            acc_merge(accs[idx], tb[idx]);
        }
    }

    // Parallel post-process (rows independent).
    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        for (int g = 0; g < ngroups; ++g) {
            out(r, g) = derive_op(op,
                accs[(size_t)r + (size_t)g * (size_t)nrow],
                png[g], eps);
        }
    }
    return out;
}
