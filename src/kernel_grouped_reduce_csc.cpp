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

    // axis == 3 (G3, col-group): output is nrow x ngroups.
    //
    // Row-partition: each thread owns a disjoint row range [r0, r1).
    // All threads scan every column, but only push when pi[k] falls in
    // their row range. Writes to accs[base + r] are race-free because
    // slot ownership is fixed by r, and row ranges across threads are
    // disjoint. No thread buckets, no serial merge.
    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    std::vector<Acc> accs((size_t)nrow * (size_t)ngroups);
    if (need_log) {
        for (auto &a : accs) a.need_log = true;
    }

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: scan every column, filter by row-range.
        for (int j = 0; j < ncol; ++j) {
            const int g = pg[j] - 1;
            const size_t base = (size_t)g * (size_t)nrow;
            const int k_end = pp[j + 1];
            for (int k = pp[j]; k < k_end; ++k) {
                const int r = pi[k];
                if (r < r0 || r >= r1) continue;
                accs[base + (size_t)r].push(px[k], eps);
            }
        }

        // Pass 2: post-process rows in [r0, r1). Rows are independent;
        // the same thread owns both the writes and reads in this range.
        for (int r = r0; r < r1; ++r) {
            for (int g = 0; g < ngroups; ++g) {
                out(r, g) = derive_op(op,
                    accs[(size_t)r + (size_t)g * (size_t)nrow],
                    png[g], eps);
            }
        }
    }
    return out;
}
