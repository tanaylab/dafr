// src/kernel_grouped_mode_csc.cpp
// Grouped numeric mode over dgCMatrix. Per-(col, group) for G2 and
// per-(row, group) for G3.  Implicit zeros are honoured (they compete
// with explicit nonzero values).
//
// Tiebreak: matches .op_mode in R/operations.R —
//   ux <- unique(x); ux[which.max(tabulate(match(x, ux)))]
// i.e., on equal counts the FIRST-ENCOUNTERED value wins (lowest ordinal
// position within the (col, group) or (row, group) slice).
//
// "First position" within a (col, group) slice is the ordinal position
// within `rows_in_group[g]` for G2 — i.e. the index into the sorted list of
// rows whose group is g.  For the first implicit zero we find the first
// row in rows_in_group[g] that is NOT stored in pi[pp[j]..pp[j+1]).
//
// Similarly for G3 the slice is `dense[r, cols_in_group[g]]` and positions
// run over cols_in_group[g] in ascending column order.
//
// G2 (axis = 2, row-group):  output is ngroups x ncol  (ReduceToColumn)
// G3 (axis = 3, col-group):  output is nrow    x ngroups (ReduceToRow)

#include <cpp11.hpp>
#include "openmp_shim.h"
#include <algorithm>
#include <limits>
#include <unordered_map>
#include <vector>

namespace {

// Given the sorted list of stored rows for column j (pi[start..end)) and a
// sorted list `rows_in_g` of rows whose group is g, compute:
//   - counts per distinct nonzero value within this (col, group) cell and
//     their first_seen_pos (as an ordinal index into rows_in_g);
//   - n_zeros_cg and zero_first_pos (ordinal index into rows_in_g of the
//     first row not stored in this column).
// Returns the argmax value using first-encountered tiebreak.
//
// Walks rows_in_g and pi[start..end) as a merge join.
double cell_mode_g2(const double *px, const int *pi, int start, int end,
                    const std::vector<int> &rows_in_g) {
    const int n_g = static_cast<int>(rows_in_g.size());
    if (n_g == 0) return 0.0;  // empty group

    std::unordered_map<double, int> counts;
    std::unordered_map<double, int> first_pos;
    counts.reserve(n_g);
    first_pos.reserve(n_g);

    int n_zeros = 0;
    int zero_first_pos = std::numeric_limits<int>::max();
    bool zero_seen = false;

    int ki = start;        // pointer into pi[]
    int pos = 0;           // ordinal position within rows_in_g
    for (int r : rows_in_g) {
        // Advance ki to skip stored rows strictly less than r.
        while (ki < end && pi[ki] < r) ++ki;
        if (ki < end && pi[ki] == r) {
            // Explicit value at this row.
            const double v = px[ki];
            if (v == 0.0) {
                // Explicit zero: count as zero (rare after drop0).
                ++n_zeros;
                if (!zero_seen) { zero_first_pos = pos; zero_seen = true; }
            } else {
                auto it = counts.find(v);
                if (it == counts.end()) {
                    counts[v] = 1;
                    first_pos[v] = pos;
                } else {
                    it->second += 1;
                }
            }
            ++ki;
        } else {
            // Implicit zero at this row.
            ++n_zeros;
            if (!zero_seen) { zero_first_pos = pos; zero_seen = true; }
        }
        ++pos;
    }

    // Pick best: highest count, then smallest first_pos on tie.
    double best_val = 0.0;
    int best_count = n_zeros;
    int best_first_pos = zero_seen ? zero_first_pos
                                    : std::numeric_limits<int>::max();
    if (!zero_seen) {
        // No zeros at all -> seed with sentinel so any nonzero displaces.
        best_count = 0;
    }
    for (const auto &kv : counts) {
        const int cnt = kv.second;
        const int fp = first_pos[kv.first];
        if (cnt > best_count ||
            (cnt == best_count && fp < best_first_pos)) {
            best_count = cnt;
            best_val   = kv.first;
            best_first_pos = fp;
        }
    }
    return best_val;
}

} // anonymous namespace

[[cpp11::register]]
cpp11::writable::doubles_matrix<cpp11::by_column>
kernel_grouped_mode_csc_cpp(
    cpp11::doubles x, cpp11::integers i, cpp11::integers p,
    int nrow, int ncol,
    cpp11::integers group,       // 1-based; length nrow (G2) or ncol (G3)
    int ngroups,
    cpp11::integers n_in_group,  // length ngroups
    int axis,                    // 2 = G2 (row-group), 3 = G3 (col-group)
    int threshold
) {
    const double *px = REAL(x.data());
    const int    *pi = INTEGER(i.data());
    const int    *pp = INTEGER(p.data());
    const int    *pg = INTEGER(group.data());
    const int    *png = INTEGER(n_in_group.data());

    if (axis == 2) {
        // G2 (row-group): output is ngroups x ncol.
        // Precompute rows_in_group[g] = sorted list of rows whose group is g.
        std::vector<std::vector<int>> rows_in_group(ngroups);
        for (int g = 0; g < ngroups; ++g) {
            rows_in_group[g].reserve(png[g]);
        }
        for (int r = 0; r < nrow; ++r) {
            const int g = pg[r] - 1;
            rows_in_group[g].push_back(r);  // pushed in ascending order
        }

        cpp11::writable::doubles_matrix<cpp11::by_column> out(ngroups, ncol);
        DAFR_PARALLEL_FOR(ncol >= threshold)
        for (int j = 0; j < ncol; ++j) {
            for (int g = 0; g < ngroups; ++g) {
                if (png[g] <= 0) { out(g, j) = 0.0; continue; }
                out(g, j) = cell_mode_g2(px, pi, pp[j], pp[j + 1],
                                         rows_in_group[g]);
            }
        }
        return out;
    }

    // axis == 3 (G3, col-group): output is nrow x ngroups.
    //
    // For each (row r, col-group g) we need:
    //   - count of each distinct value in dense[r, cols_in_group[g]]
    //   - first-seen ordinal position within cols_in_group[g]
    // where cols_in_group[g] is the sorted list of columns whose group is g.
    //
    // Pass 1 (serial, rows independent but writes are per-(r,g) so we'd
    // need locks or thread buckets): collect (value, ord_pos) for each
    // explicit cell into per-(r, g) vectors.
    //
    // We use thread-local buckets of `Entry` = (value, ord_pos). After the
    // parallel pass we merge them into one vector per (r, g) and finalise.
    //
    // FIXME: memory is O(nthreads * nrow * ngroups) Entry pointers plus
    // O(nnz) Entry values.  For large nrow * ngroups consider a row-
    // partitioned fallback.
    struct Entry { double val; int pos; };

    // Precompute ordinal position per column within its group and
    // cols_in_group[g] for later use.
    std::vector<std::vector<int>> cols_in_group(ngroups);
    for (int g = 0; g < ngroups; ++g) cols_in_group[g].reserve(png[g]);
    std::vector<int> col_ord(ncol, -1);
    for (int j = 0; j < ncol; ++j) {
        const int g = pg[j] - 1;
        col_ord[j] = static_cast<int>(cols_in_group[g].size());
        cols_in_group[g].push_back(j);
    }

    cpp11::writable::doubles_matrix<cpp11::by_column> out(nrow, ngroups);

    const int nthreads = dafr_omp_get_max_threads_capped(ncol, threshold);
    // tbuf[tid][r + g * nrow] = vector<Entry>.
    std::vector<std::vector<std::vector<Entry>>> tbuf(nthreads,
        std::vector<std::vector<Entry>>((size_t)nrow * (size_t)ngroups));

    DAFR_PARALLEL_FOR(ncol >= threshold)
    for (int j = 0; j < ncol; ++j) {
        const int tid = dafr_omp_get_thread_num();
        const int g = pg[j] - 1;
        const int ord = col_ord[j];
        auto &buf = tbuf[tid];
        const size_t base = (size_t)g * (size_t)nrow;
        for (int k = pp[j]; k < pp[j + 1]; ++k) {
            buf[base + (size_t)pi[k]].push_back({px[k], ord});
        }
    }

    // Serial merge of thread buckets.
    std::vector<std::vector<Entry>> accs((size_t)nrow * (size_t)ngroups);
    for (int t = 0; t < nthreads; ++t) {
        auto &tb = tbuf[t];
        const size_t N = accs.size();
        for (size_t idx = 0; idx < N; ++idx) {
            if (!tb[idx].empty()) {
                auto &dst = accs[idx];
                dst.insert(dst.end(), tb[idx].begin(), tb[idx].end());
                tb[idx].clear();
                tb[idx].shrink_to_fit();
            }
        }
    }
    tbuf.clear();

    // Parallel post-process: compute mode per (r, g).
    DAFR_PARALLEL_FOR(nrow >= threshold)
    for (int r = 0; r < nrow; ++r) {
        for (int g = 0; g < ngroups; ++g) {
            const int n_total = png[g];
            if (n_total <= 0) { out(r, g) = 0.0; continue; }
            const size_t idx = (size_t)r + (size_t)g * (size_t)nrow;
            auto &entries = accs[idx];
            // Sort entries by ord so first-seen is correct (thread-bucket
            // merge may interleave).  ord is unique per entry in a cell
            // (one ord per column) so stability isn't an issue.
            std::sort(entries.begin(), entries.end(),
                      [](const Entry &a, const Entry &b) { return a.pos < b.pos; });

            std::unordered_map<double, int> counts;
            std::unordered_map<double, int> first_pos;
            counts.reserve(entries.size());
            first_pos.reserve(entries.size());

            int n_zeros = 0;
            int zero_first_pos = std::numeric_limits<int>::max();
            bool zero_seen = false;

            // Walk cols_in_group[g] in order; for each ord, either an
            // explicit entry appears (consume from entries) or it's an
            // implicit zero for row r.
            const auto &cg = cols_in_group[g];
            int ei = 0;
            const int ne = static_cast<int>(entries.size());
            for (int ord = 0; ord < static_cast<int>(cg.size()); ++ord) {
                if (ei < ne && entries[ei].pos == ord) {
                    const double v = entries[ei].val;
                    if (v == 0.0) {
                        ++n_zeros;
                        if (!zero_seen) { zero_first_pos = ord; zero_seen = true; }
                    } else {
                        auto it = counts.find(v);
                        if (it == counts.end()) {
                            counts[v]    = 1;
                            first_pos[v] = ord;
                        } else {
                            it->second += 1;
                        }
                    }
                    ++ei;
                } else {
                    // implicit zero
                    ++n_zeros;
                    if (!zero_seen) { zero_first_pos = ord; zero_seen = true; }
                }
            }

            double best_val = 0.0;
            int best_count = n_zeros;
            int best_first_pos = zero_seen ? zero_first_pos
                                             : std::numeric_limits<int>::max();
            if (!zero_seen) best_count = 0;
            for (const auto &kv : counts) {
                const int cnt = kv.second;
                const int fp  = first_pos[kv.first];
                if (cnt > best_count ||
                    (cnt == best_count && fp < best_first_pos)) {
                    best_count     = cnt;
                    best_val       = kv.first;
                    best_first_pos = fp;
                }
            }
            out(r, g) = best_val;
        }
    }
    return out;
}
