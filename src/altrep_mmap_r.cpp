#include <cpp11.hpp>
#include "altrep_mmap.h"
#include "mmap_region.h"

[[cpp11::init]]
void dafr_init_altrep_mmap(DllInfo* dll) {
    dafr::init_altrep_mmap(dll);
}

[[cpp11::register]]
SEXP mmap_real_altrep_cpp(std::string path, double length_double) {
    R_xlen_t length = static_cast<R_xlen_t>(length_double);
    auto region = dafr::MmapRegion::open_readonly(path);
    std::size_t required = static_cast<std::size_t>(length) * sizeof(double);
    if (region->nbytes() < required) {
        cpp11::stop("mmap_real: file '%s' has %zu bytes, need at least %zu for length=%lld",
                    path.c_str(), region->nbytes(), required,
                    static_cast<long long>(length));
    }
    return dafr::make_mmap_real_altrep(region, length);
}

[[cpp11::register]]
SEXP mmap_int_altrep_cpp(std::string path, double length_double) {
    R_xlen_t length = static_cast<R_xlen_t>(length_double);
    auto region = dafr::MmapRegion::open_readonly(path);
    std::size_t required = static_cast<std::size_t>(length) * sizeof(int);
    if (region->nbytes() < required) {
        cpp11::stop("mmap_int: file '%s' has %zu bytes, need at least %zu for length=%lld",
                    path.c_str(), region->nbytes(), required,
                    static_cast<long long>(length));
    }
    return dafr::make_mmap_int_altrep(region, length);
}

[[cpp11::register]]
SEXP mmap_lgl_altrep_cpp(std::string path, double length_double) {
    R_xlen_t length = static_cast<R_xlen_t>(length_double);
    auto region = dafr::MmapRegion::open_readonly(path);
    std::size_t required = static_cast<std::size_t>(length) * sizeof(int);
    if (region->nbytes() < required) {
        cpp11::stop("mmap_lgl: file '%s' has %zu bytes, need at least %zu for length=%lld",
                    path.c_str(), region->nbytes(), required,
                    static_cast<long long>(length));
    }
    return dafr::make_mmap_lgl_altrep(region, length);
}
