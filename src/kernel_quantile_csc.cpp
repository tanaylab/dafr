// Per-column or per-row quantile at q (type-7) over dgCMatrix.
//
// Mirrors R's stats::quantile(x, q, type = 7) (and stats::median for q=0.5).
// Implicit zeros count toward the rank without being materialised.
// Explicit zeros in @x (which are rare after Matrix::drop0 but can occur) are
// also treated as part of the implicit-zero band for correctness.
//
// Type-7 quantile formula:
//   h    = q * (n - 1)   (0-based continuous rank)
//   lo   = floor(h)
//   hi   = ceil(h)
//   frac = h - lo
//   result = (1 - frac) * value[lo] + frac * value[hi]
//
// Negative / zero / positive partition (sorted order):
//   [ neg_0 .. neg_{k-1} | 0 ... 0 | pos_0 .. pos_{m-1} ]
//     ^--- n_neg ---^      ^n_zeros^ ^--- n_pos ---^
//
// axis == 1: per-column (ReduceToRow direction)
// axis == 0: per-row    (ReduceToColumn direction)

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <algorithm>
#include <cmath>
#include <vector>

namespace {

// Return the k-th smallest value (0-based) in the virtual sorted sequence
// [neg... | zeros... | pos...] without materialising zeros.
// nth_element partitions vec so that vec[k] is the value that would appear
// at position k in sorted order; values before it are <= it, after are >= it.
// This is destructive, so callers must rebuild neg/pos before a second call.
double pick_rank(std::vector<double>& neg, std::vector<double>& pos,
                 int n_zeros, int k) {
    const int n_neg = static_cast<int>(neg.size());
    if (k < n_neg) {
        std::nth_element(neg.begin(), neg.begin() + k, neg.end());
        return neg[k];
    }
    if (k < n_neg + n_zeros) return 0.0;
    const int k_pos = k - n_neg - n_zeros;
    std::nth_element(pos.begin(), pos.begin() + k_pos, pos.end());
    return pos[k_pos];
}

// Partition @x[start..end) into neg/pos vectors; return n_zeros.
// Explicit zeros are counted as structural zeros (conservative; drop0 not
// required by the caller but the kernel handles them correctly either way).
int partition_col(const double* px, int start, int end, int n_rows,
                  std::vector<double>& neg, std::vector<double>& pos) {
    neg.clear();
    pos.clear();
    for (int k = start; k < end; ++k) {
        if (px[k] < 0.0) neg.push_back(px[k]);
        else if (px[k] > 0.0) pos.push_back(px[k]);
        // px[k] == 0.0 falls through: counted in n_zeros
    }
    return n_rows - static_cast<int>(neg.size()) - static_cast<int>(pos.size());
}

double column_quantile(const double* px, int start, int end,
                       int n_rows, double q) {
    const int n = n_rows;
    if (n == 0) return 0.0;
    std::vector<double> neg, pos;
    neg.reserve(end - start);
    pos.reserve(end - start);
    const int n_zeros = partition_col(px, start, end, n, neg, pos);

    const double h = q * (n - 1);
    const int lo = static_cast<int>(std::floor(h));
    const int hi = static_cast<int>(std::ceil(h));
    const double frac = h - lo;

    if (lo == hi) {
        return pick_rank(neg, pos, n_zeros, lo);
    }
    const double v_lo = pick_rank(neg, pos, n_zeros, lo);
    // Rebuild after destructive nth_element for the hi pick.
    partition_col(px, start, end, n, neg, pos);
    const double v_hi = pick_rank(neg, pos, n_zeros, hi);
    return (1.0 - frac) * v_lo + frac * v_hi;
}

} // anonymous namespace

[[cpp11::register]]
cpp11::writable::doubles kernel_quantile_csc_cpp(
    cpp11::doubles x,
    cpp11::integers i,
    cpp11::integers p,
    int nrow,
    int ncol,
    int axis,
    double q,
    int threshold
) {
    const double* px = REAL(x.data());
    const int*    pi = INTEGER(i.data());
    const int*    pp = INTEGER(p.data());

    if (axis == 1) {
        // Per-column: CSC layout is already column-major.
        cpp11::writable::doubles out(ncol);
        double* pout = REAL(out.data());

        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            pout[j] = column_quantile(px, pp[j], pp[j + 1], nrow, q);
        }
        return out;
    }

    // axis == 0: per-row. Collect each row's non-zero values first, then
    // compute per-row quantile. Row-partition: each thread owns a disjoint
    // row range [r0, r1); writes to rows[pi[k]] are race-free because slot
    // ownership is fixed by r. The same thread that fills rows[r] also
    // computes out[r], so no cross-thread reads of rows[r].
    std::vector<std::vector<double>> rows(nrow);

    cpp11::writable::doubles out(nrow);
    double* pout = REAL(out.data());

    DAFR_OMP_PARALLEL_IF(nrow >= threshold)
    {
        const int tid = dafr_omp_get_thread_num();
        const int nt  = dafr_omp_get_num_threads();
        const int chunk = (nrow + nt - 1) / nt;
        const int r0 = std::min(nrow, tid * chunk);
        const int r1 = std::min(nrow, r0 + chunk);

        // Pass 1: fill rows[r] for r in [r0, r1).
        for (int j = 0; j < ncol; ++j) {
            const int k_end = pp[j + 1];
            for (int k = pp[j]; k < k_end; ++k) {
                const int r = pi[k];
                if (r < r0 || r >= r1) continue;
                rows[r].push_back(px[k]);
            }
        }

        // Pass 2: compute quantile for rows in [r0, r1).
        for (int r = r0; r < r1; ++r) {
            const int n = ncol;
            if (n == 0) { pout[r] = 0.0; continue; }
            const auto& rv = rows[r];
            std::vector<double> neg, pos;
            neg.reserve(rv.size());
            pos.reserve(rv.size());
            for (double v : rv) {
                if (v < 0.0) neg.push_back(v);
                else if (v > 0.0) pos.push_back(v);
            }
            const int n_zeros = n - static_cast<int>(neg.size()) -
                                    static_cast<int>(pos.size());
            const double h = q * (n - 1);
            const int lo = static_cast<int>(std::floor(h));
            const int hi = static_cast<int>(std::ceil(h));
            const double frac = h - lo;
            if (lo == hi) {
                pout[r] = pick_rank(neg, pos, n_zeros, lo);
            } else {
                const double v_lo = pick_rank(neg, pos, n_zeros, lo);
                // Rebuild for hi pick.
                neg.clear(); pos.clear();
                for (double v : rv) {
                    if (v < 0.0) neg.push_back(v);
                    else if (v > 0.0) pos.push_back(v);
                }
                const double v_hi = pick_rank(neg, pos, n_zeros, hi);
                pout[r] = (1.0 - frac) * v_lo + frac * v_hi;
            }
        }
    }
    return out;
}
