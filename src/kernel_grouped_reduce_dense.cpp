// src/kernel_grouped_reduce_dense.cpp
// Grouped reduction engine over a dense doubles_matrix<> for the op family:
//   Sum / Mean / Min / Max / Var / Std / VarN / StdN / GeoMean.
// Mirror of kernel_grouped_reduce_csc.cpp:
//   axis=2 (G2, row-group):  output is ngroups x ncol  (ReduceToColumn)
//   axis=3 (G3, col-group):  output is nrow    x ngroups (ReduceToRow)
// Uses the shared Acc + derive_op from kernel_grouped_acc.h. For dense
// input every cell is explicit, so a.nnz == n_total and derive_op's
// n_zeros = 0 branch is taken for every op (Min/Max do not fold in an
// extra 0, GeoMean's implicit-zero term vanishes).

#include <cpp11.hpp>
#include "openmp_shim.h"
#include "kernel_grouped_acc.h"
#include <string>
#include <vector>

[[cpp11::register]]
cpp11::writable::doubles_matrix<cpp11::by_column> kernel_grouped_reduce_dense_cpp(
    cpp11::doubles_matrix<> m,
    cpp11::integers group,
    int ngroups,
    cpp11::integers n_in_group,
    int axis,                    // 2 = G2 (row-group), 3 = G3 (col-group)
    std::string op,
    double eps,
    int threshold
) {
    using dafr_grouped::Acc;
    using dafr_grouped::acc_merge;
    using dafr_grouped::derive_op;

    const int nrow = m.nrow();
    const int ncol = m.ncol();
    const int *pg = INTEGER(group.data());
    const int *png = INTEGER(n_in_group.data());

    const bool need_log = (op == "GeoMean");

    if (axis == 2) {
        // G2 (row-group): output is ngroups x ncol. Per-column iteration,
        // per-column local accs of length ngroups.
        cpp11::writable::doubles_matrix<cpp11::by_column> out(ngroups, ncol);
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            std::vector<Acc> accs(ngroups);
            for (int g = 0; g < ngroups; ++g) accs[g].need_log = need_log;
            for (int r = 0; r < nrow; ++r) {
                const int g = pg[r] - 1;       // 1-based -> 0-based
                accs[g].push(m(r, j), eps);
            }
            for (int g = 0; g < ngroups; ++g) {
                out(g, j) = derive_op(op, accs[g], png[g], eps);
            }
        }
        return out;
    }

    // axis == 3 (G3, col-group): output is nrow x ngroups. Every cell is
    // explicit so we can iterate rows in parallel and maintain per-row
    // local accs of length ngroups — no thread-bucket needed because
    // rows are independent output cells. This avoids the large
    // O(nthreads * nrow * ngroups) footprint of the sparse kernel.
    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);
    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        std::vector<Acc> accs(ngroups);
        for (int g = 0; g < ngroups; ++g) accs[g].need_log = need_log;
        for (int j = 0; j < ncol; ++j) {
            const int g = pg[j] - 1;
            accs[g].push(m(r, j), eps);
        }
        for (int g = 0; g < ngroups; ++g) {
            out(r, g) = derive_op(op, accs[g], png[g], eps);
        }
    }
    return out;
}
