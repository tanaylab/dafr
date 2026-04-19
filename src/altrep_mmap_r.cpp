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
    return dafr::make_mmap_real_altrep(region, length);
}

[[cpp11::register]]
SEXP mmap_int_altrep_cpp(std::string path, double length_double) {
    R_xlen_t length = static_cast<R_xlen_t>(length_double);
    auto region = dafr::MmapRegion::open_readonly(path);
    return dafr::make_mmap_int_altrep(region, length);
}

[[cpp11::register]]
SEXP mmap_lgl_altrep_cpp(std::string path, double length_double) {
    R_xlen_t length = static_cast<R_xlen_t>(length_double);
    auto region = dafr::MmapRegion::open_readonly(path);
    return dafr::make_mmap_lgl_altrep(region, length);
}
