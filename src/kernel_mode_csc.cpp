// Per-column or per-row mode over numeric dgCMatrix, honouring implicit zeros.
//
// Tiebreak matches .op_mode in R/operations.R:
//   ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
// i.e., on equal counts, the value first encountered (lowest row/col index)
// wins. For a column, implicit zeros appear at all row positions not in @i;
// the first_seen_row for zero is the smallest row not in @i[start..end-1].
//
// axis == 1: per-column (ReduceToRow direction)
// axis == 0: per-row    (ReduceToColumn direction)

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <unordered_map>
#include <vector>
#include <limits>

namespace {

// Return the first row index in [0, n_rows) not present in the sorted array
// of stored row indices stored_rows[start..end-1].
// Returns n_rows if all rows are occupied (no implicit zeros).
inline int first_zero_row(const int *stored_rows, int start, int end, int n_rows) {
    if (start == end) return 0;           // empty column: first zero = row 0
    if (stored_rows[start] > 0) return 0; // first stored row > 0: gap at 0
    // Walk sorted stored rows, looking for first gap.
    for (int k = start; k < end; ++k) {
        int row = stored_rows[k];
        if (k + 1 < end) {
            if (stored_rows[k + 1] > row + 1) return row + 1;
        } else {
            // Last stored row; if it's not n_rows-1, there's a gap after it.
            if (row < n_rows - 1) return row + 1;
        }
    }
    return n_rows; // no zeros (fully occupied column)
}

// Per-column mode for a single column [start, end) of a CSC matrix.
// Tiebreak: first-seen row position (lowest row wins on equal counts).
double column_mode(const double *px, const int *pi,
                   int start, int end, int n_rows) {
    const int n_zeros = n_rows - (end - start);

    // Build count map and first-seen-row map for nonzero values.
    std::unordered_map<double, int> counts;
    std::unordered_map<double, int> first_row;
    counts.reserve(end - start);
    first_row.reserve(end - start);
    for (int k = start; k < end; ++k) {
        double v = px[k];
        auto it = counts.find(v);
        if (it == counts.end()) {
            counts[v] = 1;
            first_row[v] = pi[k]; // @i is sorted ascending -> first occurrence
        } else {
            it->second += 1;
        }
    }

    // Determine first_seen_row for implicit zero.
    int zero_first_row = (n_zeros > 0)
        ? first_zero_row(pi, start, end, n_rows)
        : std::numeric_limits<int>::max();

    // Start with zero as candidate if there are any zeros.
    double best_val = 0.0;
    int best_count = n_zeros;
    int best_first_row = (n_zeros > 0) ? zero_first_row
                                        : std::numeric_limits<int>::max();

    for (auto &kv : counts) {
        int cnt = kv.second;
        int fr  = first_row[kv.first];
        if (cnt > best_count ||
            (cnt == best_count && fr < best_first_row)) {
            best_count     = cnt;
            best_val       = kv.first;
            best_first_row = fr;
        }
    }
    return best_val;
}

} // namespace

[[cpp11::register]]
cpp11::writable::doubles kernel_mode_csc_cpp(
    cpp11::doubles x, cpp11::integers i, cpp11::integers p,
    int nrow, int ncol, int axis, int threshold
) {
    const double *px = REAL(x.data());
    const int    *pi = INTEGER(i.data());
    const int    *pp = INTEGER(p.data());

    if (axis == 1) {
        // Per-column (ReduceToRow): one result per column.
        cpp11::writable::doubles out(ncol);
        double *pout = REAL(out.data());
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            pout[j] = column_mode(px, pi, pp[j], pp[j + 1], nrow);
        }
        return out;
    }

    // axis == 0: per-row (ReduceToColumn): one result per row.
    // Row-partition: each thread owns a disjoint row range [r0, r1).
    // The same thread fills rows[r] and computes out[r] for r in that
    // range — no write race on rows[pi[k]], no cross-thread reads of
    // rows[r] during post-process.
    //
    // Entry list per row: (col, val) pairs. Ordered by column because
    // the owning thread scans j = 0..ncol-1 in ascending order. Mode's
    // first-column-wins tie-break relies on this ordering.
    struct Entry { int col; double val; };
    std::vector<std::vector<Entry>> rows(nrow);

    cpp11::writable::doubles out(nrow);
    double *pout = REAL(out.data());

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
                rows[r].push_back({j, px[k]});
            }
        }

        // Pass 2: compute mode for rows in [r0, r1).
        for (int r = r0; r < r1; ++r) {
            const auto &entries = rows[r];
            const int n_stored  = (int)entries.size();
            const int n_zeros   = ncol - n_stored;

            // Find first_seen_col for implicit zero.
            // entries are ordered by column (we inserted in j order).
            int zero_first_col = std::numeric_limits<int>::max();
            if (n_zeros > 0) {
                if (n_stored == 0 || entries[0].col > 0) {
                    zero_first_col = 0;
                } else {
                    zero_first_col = ncol; // default: no gap found yet
                    for (int k = 0; k < n_stored; ++k) {
                        if (k + 1 < n_stored) {
                            if (entries[k + 1].col > entries[k].col + 1) {
                                zero_first_col = entries[k].col + 1;
                                break;
                            }
                        } else {
                            if (entries[k].col < ncol - 1) {
                                zero_first_col = entries[k].col + 1;
                            }
                        }
                    }
                }
            }

            // Build count and first_col maps for nonzero values.
            std::unordered_map<double, int> counts;
            std::unordered_map<double, int> first_col;
            counts.reserve(n_stored);
            first_col.reserve(n_stored);
            for (const auto &e : entries) {
                auto it = counts.find(e.val);
                if (it == counts.end()) {
                    counts[e.val]    = 1;
                    first_col[e.val] = e.col;
                } else {
                    it->second += 1;
                }
            }

            double best_val      = 0.0;
            int    best_count    = n_zeros;
            int    best_first_col = zero_first_col;
            if (n_zeros == 0) {
                best_count     = 0;
                best_first_col = std::numeric_limits<int>::max();
            }

            for (auto &kv : counts) {
                int cnt = kv.second;
                int fc  = first_col[kv.first];
                if (cnt > best_count ||
                    (cnt == best_count && fc < best_first_col)) {
                    best_count     = cnt;
                    best_val       = kv.first;
                    best_first_col = fc;
                }
            }
            pout[r] = best_val;
        }
    }
    return out;
}
