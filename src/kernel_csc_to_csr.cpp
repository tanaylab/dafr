#include <cpp11.hpp>
#include <cstring>
#include <vector>

using namespace cpp11::literals;

// Output lists: new_p (nrow+1), new_j (nnz), new_x (nnz) corresponding to
// a CSR representation.
[[cpp11::register]]
cpp11::writable::list kernel_csc_to_csr_cpp(
    cpp11::doubles x,    // nnz
    cpp11::integers i,   // nnz — 0-based row indices
    cpp11::integers p,   // ncol+1
    int nrow, int ncol
) {
    const R_xlen_t nnz = x.size();
    const double *px = REAL(x.data());
    const int *pi = INTEGER(i.data());
    const int *pp = INTEGER(p.data());

    cpp11::writable::integers new_p(nrow + 1);
    cpp11::writable::integers new_j(nnz);
    cpp11::writable::doubles new_x(nnz);

    int *pnp = INTEGER(new_p.data());
    int *pnj = INTEGER(new_j.data());
    double *pnx = REAL(new_x.data());

    std::memset(pnp, 0, (nrow + 1) * sizeof(int));

    // Count entries per row.
    for (R_xlen_t k = 0; k < nnz; ++k) ++pnp[pi[k] + 1];
    // Cumulative sum -> row pointers.
    for (int r = 0; r < nrow; ++r) pnp[r + 1] += pnp[r];

    // Scatter pass. Need a mutable cursor per row.
    std::vector<int> cur(nrow);
    std::memcpy(cur.data(), pnp, nrow * sizeof(int));

    for (int col = 0; col < ncol; ++col) {
        for (int k = pp[col]; k < pp[col + 1]; ++k) {
            int row = pi[k];
            int dest = cur[row]++;
            pnj[dest] = col;
            pnx[dest] = px[k];
        }
    }

    cpp11::writable::list out;
    out.push_back({"p"_nm = new_p});
    out.push_back({"j"_nm = new_j});
    out.push_back({"x"_nm = new_x});
    return out;
}
