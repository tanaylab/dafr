// Slice 17 — MmapZipStore: C++ port of upstream
// DataAxesFormats.jl/src/mmap_zip_store.jl. Phase 1 lays the skeleton
// (struct fields, public class, cpp11 entry points). Subsequent phases
// fill in the parser (Phase 2), ALTREP RAW (Phase 3), writer (Phase 4),
// crash counter (Phase 5), recovery (Phase 6), reserve/patch (Phase 7),
// foreign-zip handling (Phase 9-12).

#include <cpp11.hpp>

#include "mmap_zip_store.h"

#include <stdexcept>
#include <string>
#include <vector>

namespace dafr {

class MmapZipStoreImpl {
public:
    std::string path;
    bool is_writable = false;
    uint64_t max_file_size = 0;
    uint64_t overlay_length = 0;
    const uint8_t* file_mmap = nullptr;
    std::vector<ZipEntry> entries;
    std::unordered_map<std::string, std::size_t> name_to_index;
    uint64_t central_directory_offset = 0;
    uint64_t central_directory_size = 0;
    std::vector<std::shared_ptr<AltrepRawSlot>> altrep_slots;
    SEXP crash_counter = R_NilValue;
    SEXP namespace_env = R_NilValue;
};

std::unique_ptr<MmapZipStore> MmapZipStore::open(const std::string& /*path*/,
                                                 bool /*writable*/, bool /*create*/, bool /*truncate*/,
                                                 uint64_t /*max_file_size*/) {
    throw std::runtime_error("MmapZipStore::open: slice-17 phase 1 stub");
}

MmapZipStore::MmapZipStore(MmapZipStoreImpl* impl) : impl_(impl) {}

MmapZipStore::~MmapZipStore() {
    delete impl_;
}

bool MmapZipStore::has(const std::string& /*key*/) const {
    throw std::runtime_error("MmapZipStore::has: slice-17 phase 1 stub");
}

bool MmapZipStore::stored_view(const std::string&, const uint8_t**, uint64_t*, uint16_t*, uint32_t*) const {
    throw std::runtime_error("MmapZipStore::stored_view: slice-17 phase 1 stub");
}

std::vector<uint8_t> MmapZipStore::read_decompressed(const std::string&) const {
    throw std::runtime_error("MmapZipStore::read_decompressed: slice-17 phase 1 stub");
}

std::vector<std::string> MmapZipStore::list_with_prefix(const std::string&) const {
    throw std::runtime_error("MmapZipStore::list_with_prefix: slice-17 phase 1 stub");
}

std::vector<std::string> MmapZipStore::all_keys() const {
    throw std::runtime_error("MmapZipStore::all_keys: slice-17 phase 1 stub");
}

void MmapZipStore::append(const std::string&, const uint8_t*, uint64_t) {
    throw std::runtime_error("MmapZipStore::append: slice-17 phase 1 stub");
}

uint8_t* MmapZipStore::reserve(const std::string&, uint64_t) {
    throw std::runtime_error("MmapZipStore::reserve: slice-17 phase 1 stub");
}

void MmapZipStore::patch_crc(const std::string&) {
    throw std::runtime_error("MmapZipStore::patch_crc: slice-17 phase 1 stub");
}

void MmapZipStore::close_store() {
    // Phase 1: no-op.
}

bool MmapZipStore::is_writable() const { return impl_ ? impl_->is_writable : false; }

const std::string& MmapZipStore::path() const {
    static const std::string empty;
    return impl_ ? impl_->path : empty;
}

const uint8_t* MmapZipStore::file_mmap_base() const { return impl_ ? impl_->file_mmap : nullptr; }
uint64_t MmapZipStore::overlay_length() const { return impl_ ? impl_->overlay_length : 0; }

std::shared_ptr<AltrepRawSlot> MmapZipStore::register_altrep_slot(uint64_t offset, uint64_t length) {
    auto slot = std::make_shared<AltrepRawSlot>(AltrepRawSlot{offset, length, false});
    impl_->altrep_slots.push_back(slot);
    return slot;
}

void MmapZipStore::set_crash_counter(SEXP counter, SEXP namespace_env) {
    impl_->crash_counter = counter;
    impl_->namespace_env = namespace_env;
}

}  // namespace dafr

// ---- cpp11 entry points (R-side names: dafr_mmap_zip_*) -------------------

namespace {
dafr::MmapZipStore* xptr_to_store(SEXP xptr) {
    if (TYPEOF(xptr) != EXTPTRSXP) {
        Rf_error("expected an external pointer to a MmapZipStore");
    }
    auto* store = static_cast<dafr::MmapZipStore*>(R_ExternalPtrAddr(xptr));
    if (!store) Rf_error("MmapZipStore has been closed");
    return store;
}

void mmap_zip_store_finalizer(SEXP xptr) {
    auto* store = static_cast<dafr::MmapZipStore*>(R_ExternalPtrAddr(xptr));
    if (store) {
        try {
            store->close_store();
        } catch (...) {
            // Finalizers must not throw.
        }
        delete store;
        R_ClearExternalPtr(xptr);
    }
}
}  // namespace

[[cpp11::register]]
SEXP dafr_mmap_zip_open(std::string path, std::string mode, double max_file_size_double) {
    bool writable = false;
    bool create = false;
    bool truncate = false;
    if (mode == "r") {
        // defaults
    } else if (mode == "r+") {
        writable = true;
    } else if (mode == "w+") {
        writable = true;
        create = true;
    } else if (mode == "w") {
        writable = true;
        create = true;
        truncate = true;
    } else {
        cpp11::stop("MmapZipStore mode must be one of 'r','r+','w+','w'; got '%s'", mode.c_str());
    }
    auto max_size = static_cast<uint64_t>(max_file_size_double);
    try {
        auto store = dafr::MmapZipStore::open(path, writable, create, truncate, max_size);
        SEXP xptr = PROTECT(R_MakeExternalPtr(store.release(), R_NilValue, R_NilValue));
        R_RegisterCFinalizerEx(xptr, mmap_zip_store_finalizer, TRUE);
        UNPROTECT(1);
        return xptr;
    } catch (const std::exception& e) {
        cpp11::stop("%s", e.what());
    }
}

[[cpp11::register]]
SEXP dafr_mmap_zip_close(SEXP xptr) {
    auto* store = static_cast<dafr::MmapZipStore*>(R_ExternalPtrAddr(xptr));
    if (store) {
        try {
            store->close_store();
        } catch (const std::exception& e) {
            cpp11::stop("%s", e.what());
        }
        delete store;
        R_ClearExternalPtr(xptr);
    }
    return R_NilValue;
}

[[cpp11::register]]
SEXP dafr_mmap_zip_get_bytes(SEXP xptr, std::string key) {
    (void)xptr_to_store(xptr);
    (void)key;
    cpp11::stop("dafr_mmap_zip_get_bytes: slice-17 phase 1 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_set_bytes(SEXP xptr, std::string key, cpp11::raws bytes) {
    (void)xptr_to_store(xptr);
    (void)key;
    (void)bytes;
    cpp11::stop("dafr_mmap_zip_set_bytes: slice-17 phase 1 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_exists(SEXP xptr, std::string key) {
    (void)xptr_to_store(xptr);
    (void)key;
    cpp11::stop("dafr_mmap_zip_exists: slice-17 phase 1 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_list(SEXP xptr, std::string prefix) {
    (void)xptr_to_store(xptr);
    (void)prefix;
    cpp11::stop("dafr_mmap_zip_list: slice-17 phase 1 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_reserve(SEXP xptr, std::string key, double size_double) {
    (void)xptr_to_store(xptr);
    (void)key;
    (void)size_double;
    cpp11::stop("dafr_mmap_zip_reserve: slice-17 phase 1 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_patch_crc(SEXP xptr, std::string key) {
    (void)xptr_to_store(xptr);
    (void)key;
    cpp11::stop("dafr_mmap_zip_patch_crc: slice-17 phase 1 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_set_crash_counter(SEXP xptr, SEXP counter, SEXP ns_env) {
    auto* store = xptr_to_store(xptr);
    store->set_crash_counter(counter, ns_env);
    return R_NilValue;
}
