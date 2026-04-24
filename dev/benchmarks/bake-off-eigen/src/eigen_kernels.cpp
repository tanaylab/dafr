// [[Rcpp::depends(RcppEigen)]]
#include <RcppEigen.h>
#include <cmath>

typedef Eigen::SparseMatrix<double, Eigen::ColMajor, int> SpMat;
typedef Eigen::SparseMatrix<double, Eigen::RowMajor, int> SpMatR;

// --- log(x) + y ---
// [[Rcpp::export]]
Eigen::VectorXd eigen_log_add(const Eigen::Map<Eigen::VectorXd> x,
                              const Eigen::Map<Eigen::VectorXd> y) {
    Eigen::VectorXd out = x.array().log() + y.array();
    return out;
}

// --- CSC colSums ---
// [[Rcpp::export]]
Eigen::VectorXd eigen_csc_colsums(
        const Eigen::Map<Eigen::SparseMatrix<double, Eigen::ColMajor, int>> m) {
    return m.transpose() * Eigen::VectorXd::Ones(m.rows());
}

// --- CSC -> CSR transpose ---
// [[Rcpp::export]]
Rcpp::List eigen_csc_to_csr(
        const Eigen::Map<Eigen::SparseMatrix<double, Eigen::ColMajor, int>> m) {
    SpMatR csr = m;
    csr.makeCompressed();
    return Rcpp::List::create(
        Rcpp::Named("p") = Rcpp::IntegerVector(csr.outerIndexPtr(),
                                               csr.outerIndexPtr() + csr.rows() + 1),
        Rcpp::Named("j") = Rcpp::IntegerVector(csr.innerIndexPtr(),
                                               csr.innerIndexPtr() + csr.nonZeros()),
        Rcpp::Named("x") = Rcpp::NumericVector(csr.valuePtr(),
                                               csr.valuePtr() + csr.nonZeros())
    );
}
