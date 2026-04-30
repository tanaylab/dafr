// Slice 17 — MmapZipStore: C++ port of upstream
// DataAxesFormats.jl/src/mmap_zip_store.jl. Phase 1 laid the skeleton;
// Phase 2 implements the read side (parse existing zip, list, get bytes,
// decompress deflate). Writer / ALTREP / crash-counter / recovery /
// reserve-patch land in later phases.

#include <cpp11.hpp>

#include "mmap_zip_store.h"

#include <algorithm>
#include <cerrno>
#include <cstdio>
#include <cstring>
#include <stdexcept>
#include <string>
#include <vector>

#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <unistd.h>

#include <zlib.h>

namespace dafr {

// ---- Little-endian byte readers ------------------------------------------
//
// All ZIP integer fields are little-endian on disk. Use these helpers
// instead of unaligned memcpy + host-endian reinterpretation so the parser
// works portably and so bounds-violations show up as a single explicit
// check at every read site.

static inline uint16_t le16(const uint8_t* p) {
    return static_cast<uint16_t>(p[0]) |
           (static_cast<uint16_t>(p[1]) << 8);
}

static inline uint32_t le32(const uint8_t* p) {
    return static_cast<uint32_t>(p[0]) |
           (static_cast<uint32_t>(p[1]) << 8) |
           (static_cast<uint32_t>(p[2]) << 16) |
           (static_cast<uint32_t>(p[3]) << 24);
}

static inline uint64_t le64(const uint8_t* p) {
    uint64_t lo = le32(p);
    uint64_t hi = le32(p + 4);
    return lo | (hi << 32);
}

[[noreturn]] static void zip_corrupt(const std::string& reason) {
    throw std::runtime_error("MmapZipStore: corrupt zip — " + reason);
}

// Parser state lives entirely on the stack/in MmapZipStoreImpl during open;
// once parsing is done the impl owns the mmap and the parsed entries.
class MmapZipStoreImpl {
public:
    std::string path;
    bool is_writable = false;
    uint64_t max_file_size = 0;
    uint64_t overlay_length = 0;
    int fd = -1;                  // POSIX fd; -1 == not held
    void* mmap_base = nullptr;    // mmap region base (== file_mmap)
    uint64_t mmap_length = 0;     // length of mmap region (for munmap)
    const uint8_t* file_mmap = nullptr;
    std::vector<ZipEntry> entries;
    std::unordered_map<std::string, std::size_t> name_to_index;
    uint64_t central_directory_offset = 0;
    uint64_t central_directory_size = 0;
    uint64_t reservation_base = 0;     // unused for read-only opens
    uint64_t reservation_length = 0;   // unused for read-only opens
    std::vector<std::shared_ptr<AltrepRawSlot>> altrep_slots;
    SEXP crash_counter = R_NilValue;
    SEXP namespace_env = R_NilValue;

    ~MmapZipStoreImpl() {
        // Deactivate any outstanding ALTREP slots before tearing the mmap
        // down. This is also done in MmapZipStore::close_store(); the
        // duplication makes the destructor safe even if close_store() was
        // never called (e.g., open() throws after impl construction).
        for (auto& slot : altrep_slots) {
            if (slot) slot->deactivated = true;
        }
        altrep_slots.clear();
        if (mmap_base != nullptr && mmap_base != MAP_FAILED && mmap_length > 0) {
            ::munmap(mmap_base, static_cast<size_t>(mmap_length));
            mmap_base = nullptr;
            mmap_length = 0;
            file_mmap = nullptr;
        }
        if (fd >= 0) {
            ::close(fd);
            fd = -1;
        }
    }
};

namespace {

// POSIX read-only mmap of `path` covering the current file size. On error
// throws std::runtime_error with errno-derived detail. Returns the base
// pointer; sets *out_fd and *out_length. Caller (the impl destructor)
// owns munmap + close on cleanup.
const uint8_t* read_only_mmap(const std::string& path, int* out_fd, uint64_t* out_length) {
    int fd = ::open(path.c_str(), O_RDONLY);
    if (fd < 0) {
        throw std::runtime_error(
            "MmapZipStore: failed to open '" + path + "': " + std::strerror(errno));
    }
    struct stat st;
    if (::fstat(fd, &st) != 0) {
        int e = errno;
        ::close(fd);
        throw std::runtime_error(
            "MmapZipStore: fstat '" + path + "': " + std::strerror(e));
    }
    uint64_t length = static_cast<uint64_t>(st.st_size);
    if (length < END_OF_CENTRAL_DIRECTORY_SIZE) {
        ::close(fd);
        throw std::runtime_error(
            "MmapZipStore: file too small to be a zip archive: '" + path + "'");
    }
    void* p = ::mmap(nullptr, static_cast<size_t>(length),
                     PROT_READ, MAP_PRIVATE, fd, 0);
    if (p == MAP_FAILED) {
        int e = errno;
        ::close(fd);
        throw std::runtime_error(
            "MmapZipStore: mmap '" + path + "': " + std::strerror(e));
    }
    *out_fd = fd;
    *out_length = length;
    return static_cast<const uint8_t*>(p);
}

// Scan backward from `length - END_OF_CENTRAL_DIRECTORY_SIZE` for the EOCD
// signature. The legacy EOCD has a variable-length comment trailer of up to
// 0xFFFF bytes, so cap the search at 64 KiB + EOCD-size; that's the maximum
// possible distance from EOCD start to file end.
uint64_t find_eocd(const uint8_t* base, uint64_t length) {
    if (length < END_OF_CENTRAL_DIRECTORY_SIZE) {
        zip_corrupt("file shorter than EOCD record");
    }
    uint64_t max_back = static_cast<uint64_t>(0xFFFFu) + END_OF_CENTRAL_DIRECTORY_SIZE;
    uint64_t scan_start = length >= max_back
                              ? length - max_back
                              : 0;
    // Walk from latest possible EOCD position back toward scan_start.
    for (uint64_t i = length - END_OF_CENTRAL_DIRECTORY_SIZE; ; --i) {
        if (le32(base + i) == END_OF_CENTRAL_DIRECTORY_SIGNATURE) {
            // Sanity-check the comment-length field: comment must end at EOF.
            uint16_t comment_len = le16(base + i + 20);
            if (i + END_OF_CENTRAL_DIRECTORY_SIZE + comment_len == length) {
                return i;
            }
        }
        if (i == scan_start) break;
    }
    zip_corrupt("end-of-central-directory record not found");
}

// Re-read the local file header at `lfh_offset` to compute the data offset.
// Returns lfh_offset + 30 + name_len + extra_len. Validates the LFH
// signature and bounds.
uint64_t compute_data_offset(const uint8_t* base, uint64_t length,
                             uint64_t lfh_offset) {
    if (lfh_offset + LOCAL_FILE_HEADER_FIXED_SIZE > length) {
        zip_corrupt("local file header offset out of range");
    }
    if (le32(base + lfh_offset) != LOCAL_FILE_HEADER_SIGNATURE) {
        zip_corrupt("local file header signature missing");
    }
    uint16_t name_len  = le16(base + lfh_offset + 26);
    uint16_t extra_len = le16(base + lfh_offset + 28);
    uint64_t data_off  = lfh_offset + LOCAL_FILE_HEADER_FIXED_SIZE
                       + static_cast<uint64_t>(name_len)
                       + static_cast<uint64_t>(extra_len);
    if (data_off > length) {
        zip_corrupt("local file header extends past EOF");
    }
    return data_off;
}

// Parse `path`'s zip metadata into `impl`. Caller must have populated
// impl.file_mmap + impl.overlay_length. Throws on corrupt inputs.
void parse_central_directory(MmapZipStoreImpl& impl) {
    const uint8_t* base = impl.file_mmap;
    uint64_t length     = impl.overlay_length;

    uint64_t eocd_offset = find_eocd(base, length);

    // Legacy EOCD fields.
    uint64_t cd_size_legacy   = le32(base + eocd_offset + 12);
    uint64_t cd_offset_legacy = le32(base + eocd_offset + 16);
    uint64_t total_entries_legacy = le16(base + eocd_offset + 10);

    uint64_t cd_size   = cd_size_legacy;
    uint64_t cd_offset = cd_offset_legacy;
    uint64_t total_entries = total_entries_legacy;

    // Detect ZIP64. Either any of the legacy fields is the sentinel
    // 0xFFFF / 0xFFFFFFFF, OR the locator record at eocd-20 has the
    // ZIP64 locator signature. Use the latter as the authoritative test
    // because some upstream-written archives have ZIP64 unconditionally
    // even when sizes fit in 32 bits.
    bool have_zip64 = false;
    if (eocd_offset >= ZIP64_END_OF_CENTRAL_DIRECTORY_LOCATOR_SIZE) {
        uint64_t locator_offset = eocd_offset - ZIP64_END_OF_CENTRAL_DIRECTORY_LOCATOR_SIZE;
        if (le32(base + locator_offset) == ZIP64_END_OF_CENTRAL_DIRECTORY_LOCATOR_SIGNATURE) {
            uint64_t z64_eocd_offset = le64(base + locator_offset + 8);
            if (z64_eocd_offset + ZIP64_END_OF_CENTRAL_DIRECTORY_SIZE > length) {
                zip_corrupt("ZIP64 EOCD offset out of range");
            }
            if (le32(base + z64_eocd_offset) != ZIP64_END_OF_CENTRAL_DIRECTORY_SIGNATURE) {
                zip_corrupt("ZIP64 EOCD signature missing");
            }
            // ZIP64 EOCD layout (offsets from record start):
            //   +0  signature(4)
            //   +4  size of zip64 EOCD record(8)
            //  +12  version made by(2)
            //  +14  version needed(2)
            //  +16  this disk number(4)
            //  +20  start disk(4)
            //  +24  total entries this disk(8)
            //  +32  total entries(8)
            //  +40  cd size(8)
            //  +48  cd offset(8)
            total_entries = le64(base + z64_eocd_offset + 32);
            cd_size       = le64(base + z64_eocd_offset + 40);
            cd_offset     = le64(base + z64_eocd_offset + 48);
            have_zip64 = true;
        }
    }
    (void)have_zip64;  // currently informational only

    if (cd_offset > length || cd_size > length || cd_offset + cd_size > length) {
        zip_corrupt("central directory range out of file bounds");
    }

    impl.central_directory_offset = cd_offset;
    impl.central_directory_size = cd_size;

    impl.entries.clear();
    impl.entries.reserve(static_cast<std::size_t>(total_entries));
    impl.name_to_index.clear();

    uint64_t cursor = cd_offset;
    uint64_t cd_end = cd_offset + cd_size;
    for (uint64_t i = 0; i < total_entries; ++i) {
        if (cursor + CENTRAL_DIRECTORY_ENTRY_FIXED_SIZE > cd_end) {
            zip_corrupt("central directory entry truncated");
        }
        if (le32(base + cursor) != CENTRAL_DIRECTORY_ENTRY_SIGNATURE) {
            zip_corrupt("central directory entry signature missing");
        }
        uint16_t method   = le16(base + cursor + 10);
        uint32_t crc      = le32(base + cursor + 16);
        uint64_t comp_sz  = le32(base + cursor + 20);
        uint64_t uncomp_sz = le32(base + cursor + 24);
        uint16_t name_len = le16(base + cursor + 28);
        uint16_t extra_len = le16(base + cursor + 30);
        uint16_t comment_len = le16(base + cursor + 32);
        uint64_t lfh_off  = le32(base + cursor + 42);

        uint64_t name_offset  = cursor + CENTRAL_DIRECTORY_ENTRY_FIXED_SIZE;
        uint64_t extra_offset = name_offset + name_len;
        uint64_t comment_offset = extra_offset + extra_len;
        uint64_t entry_end = comment_offset + comment_len;
        if (entry_end > cd_end) {
            zip_corrupt("central directory entry name/extra/comment truncated");
        }

        std::string name(reinterpret_cast<const char*>(base + name_offset),
                         static_cast<std::size_t>(name_len));

        // Walk extra fields, looking for ZIP64 (0x0001). Other extras
        // (including DAF padding 0xDAF1) are ignored. The ZIP64 extra
        // contains uncompressed_size, compressed_size, lfh_offset only
        // for those of the legacy 32-bit fields that hold the sentinel
        // 0xFFFFFFFF — present in declaration order.
        bool size_is_sentinel = (uncomp_sz == 0xFFFFFFFFu);
        bool csize_is_sentinel = (comp_sz == 0xFFFFFFFFu);
        bool lfh_is_sentinel  = (lfh_off == 0xFFFFFFFFu);

        uint64_t ex_cursor = extra_offset;
        uint64_t ex_end    = extra_offset + extra_len;
        while (ex_cursor + 4 <= ex_end) {
            uint16_t hid  = le16(base + ex_cursor);
            uint16_t hlen = le16(base + ex_cursor + 2);
            if (ex_cursor + 4 + hlen > ex_end) break;
            if (hid == ZIP64_EXTRA_HEADER_ID) {
                uint64_t sub = ex_cursor + 4;
                uint64_t sub_end = sub + hlen;
                if (size_is_sentinel) {
                    if (sub + 8 > sub_end) zip_corrupt("ZIP64 extra: missing uncompressed size");
                    uncomp_sz = le64(base + sub);
                    sub += 8;
                }
                if (csize_is_sentinel) {
                    if (sub + 8 > sub_end) zip_corrupt("ZIP64 extra: missing compressed size");
                    comp_sz = le64(base + sub);
                    sub += 8;
                }
                if (lfh_is_sentinel) {
                    if (sub + 8 > sub_end) zip_corrupt("ZIP64 extra: missing LFH offset");
                    lfh_off = le64(base + sub);
                    sub += 8;
                }
            }
            ex_cursor += 4 + hlen;
        }

        uint64_t data_off = compute_data_offset(base, length, lfh_off);
        if (data_off + comp_sz > length) {
            zip_corrupt("entry data extends past EOF");
        }

        ZipEntry entry;
        entry.name = name;
        entry.local_file_header_offset = lfh_off;
        entry.data_offset = data_off;
        entry.compressed_size = comp_sz;
        entry.uncompressed_size = uncomp_sz;
        entry.crc32 = crc;
        entry.compression_method = method;
        entry.central_directory_offset = cursor;

        impl.name_to_index[name] = impl.entries.size();
        impl.entries.push_back(std::move(entry));

        cursor = entry_end;
    }
}

}  // namespace

// ---- MmapZipStore ---------------------------------------------------------

std::unique_ptr<MmapZipStore> MmapZipStore::open(const std::string& path,
                                                 bool writable, bool create, bool truncate,
                                                 uint64_t max_file_size) {
    if (writable || create || truncate) {
        throw std::runtime_error("MmapZipStore::open writable: slice-17 phase 4 stub");
    }
    (void)max_file_size;

    auto impl = std::unique_ptr<MmapZipStoreImpl>(new MmapZipStoreImpl());
    impl->path = path;
    impl->is_writable = false;
    impl->max_file_size = 0;

    int fd = -1;
    uint64_t length = 0;
    const uint8_t* mapped = read_only_mmap(path, &fd, &length);
    impl->fd = fd;
    impl->mmap_base = const_cast<uint8_t*>(mapped);
    impl->mmap_length = length;
    impl->file_mmap = mapped;
    impl->overlay_length = length;

    parse_central_directory(*impl);

    return std::unique_ptr<MmapZipStore>(new MmapZipStore(impl.release()));
}

MmapZipStore::MmapZipStore(MmapZipStoreImpl* impl) : impl_(impl) {}

MmapZipStore::~MmapZipStore() {
    delete impl_;
}

bool MmapZipStore::has(const std::string& key) const {
    return impl_ && impl_->name_to_index.find(key) != impl_->name_to_index.end();
}

bool MmapZipStore::stored_view(const std::string& key,
                               const uint8_t** out_ptr,
                               uint64_t* out_length,
                               uint16_t* out_method,
                               uint32_t* out_crc) const {
    if (!impl_) {
        if (out_ptr) *out_ptr = nullptr;
        if (out_length) *out_length = 0;
        return false;
    }
    auto it = impl_->name_to_index.find(key);
    if (it == impl_->name_to_index.end()) {
        if (out_ptr) *out_ptr = nullptr;
        if (out_length) *out_length = 0;
        return false;
    }
    const ZipEntry& entry = impl_->entries[it->second];
    if (out_method) *out_method = entry.compression_method;
    if (out_crc) *out_crc = entry.crc32;
    if (entry.compression_method != STORED_COMPRESSION_METHOD) {
        if (out_ptr) *out_ptr = nullptr;
        if (out_length) *out_length = 0;
        return false;
    }
    if (out_ptr) *out_ptr = impl_->file_mmap + entry.data_offset;
    if (out_length) *out_length = entry.compressed_size;
    return true;
}

std::vector<uint8_t> MmapZipStore::read_decompressed(const std::string& key) const {
    if (!impl_) throw std::runtime_error("MmapZipStore: store is closed");
    auto it = impl_->name_to_index.find(key);
    if (it == impl_->name_to_index.end()) {
        throw std::runtime_error("MmapZipStore: entry '" + key + "' not found");
    }
    const ZipEntry& entry = impl_->entries[it->second];
    const uint8_t* src = impl_->file_mmap + entry.data_offset;

    if (entry.compression_method == STORED_COMPRESSION_METHOD) {
        std::vector<uint8_t> out(static_cast<std::size_t>(entry.compressed_size));
        if (entry.compressed_size > 0) {
            std::memcpy(out.data(), src, static_cast<std::size_t>(entry.compressed_size));
        }
        return out;
    }

    if (entry.compression_method == DEFLATE_COMPRESSION_METHOD) {
        std::vector<uint8_t> out(static_cast<std::size_t>(entry.uncompressed_size));
        if (entry.uncompressed_size == 0) return out;

        z_stream zs{};
        zs.next_in = const_cast<Bytef*>(reinterpret_cast<const Bytef*>(src));
        zs.avail_in = static_cast<uInt>(entry.compressed_size);
        zs.next_out = reinterpret_cast<Bytef*>(out.data());
        zs.avail_out = static_cast<uInt>(entry.uncompressed_size);
        // Negative window bits: raw deflate (no zlib wrapper).
        int rc = inflateInit2(&zs, -MAX_WBITS);
        if (rc != Z_OK) {
            throw std::runtime_error("MmapZipStore: inflateInit2 failed for entry '" + key + "'");
        }
        rc = inflate(&zs, Z_FINISH);
        inflateEnd(&zs);
        if (rc != Z_STREAM_END) {
            throw std::runtime_error("MmapZipStore: inflate failed for entry '" + key + "'");
        }
        if (zs.total_out != entry.uncompressed_size) {
            throw std::runtime_error("MmapZipStore: inflate produced wrong byte count for entry '" + key + "'");
        }
        // CRC verification.
        uLong crc = ::crc32(0L, Z_NULL, 0);
        crc = ::crc32(crc, reinterpret_cast<const Bytef*>(out.data()),
                      static_cast<uInt>(out.size()));
        if (static_cast<uint32_t>(crc) != entry.crc32) {
            throw std::runtime_error("MmapZipStore: CRC32 mismatch for entry '" + key + "'");
        }
        return out;
    }

    char buf[64];
    std::snprintf(buf, sizeof(buf), "%u",
                  static_cast<unsigned>(entry.compression_method));
    throw std::runtime_error(
        "MmapZipStore: entry '" + key + "' uses unsupported compression method " + buf);
}

std::vector<std::string> MmapZipStore::list_with_prefix(const std::string& prefix) const {
    std::vector<std::string> out;
    if (!impl_) return out;
    if (prefix.empty()) {
        out.reserve(impl_->entries.size());
        for (const auto& e : impl_->entries) out.push_back(e.name);
        return out;
    }
    // Match `DirStore::store_list` shape: list all keys whose path lies
    // under the given prefix directory. The R-side wrapper passes a bare
    // prefix; we treat it as a directory (append '/' if missing) and emit
    // the full key path of every descendant. Slice 16 contract is simple
    // startsWith filtering against `<prefix>/`.
    std::string norm = prefix;
    if (!norm.empty() && norm.back() != '/') norm.push_back('/');
    for (const auto& e : impl_->entries) {
        if (e.name.size() > norm.size() &&
            std::memcmp(e.name.data(), norm.data(), norm.size()) == 0) {
            out.push_back(e.name);
        }
    }
    return out;
}

std::vector<std::string> MmapZipStore::all_keys() const {
    std::vector<std::string> out;
    if (!impl_) return out;
    out.reserve(impl_->entries.size());
    for (const auto& e : impl_->entries) out.push_back(e.name);
    return out;
}

void MmapZipStore::append(const std::string&, const uint8_t*, uint64_t) {
    throw std::runtime_error("MmapZipStore::append: slice-17 phase 4 stub");
}

uint8_t* MmapZipStore::reserve(const std::string&, uint64_t) {
    throw std::runtime_error("MmapZipStore::reserve: slice-17 phase 7 stub");
}

void MmapZipStore::patch_crc(const std::string&) {
    throw std::runtime_error("MmapZipStore::patch_crc: slice-17 phase 7 stub");
}

void MmapZipStore::close_store() {
    if (!impl_) return;
    // 1. Flip all outstanding ALTREP slots to deactivated BEFORE tearing
    //    down the mmap. The shared_ptrs keep the slots alive until the
    //    last ALTREP is GC'd, so any subsequent ALTREP method sees
    //    deactivated=true and short-circuits to inert behavior.
    for (auto& slot : impl_->altrep_slots) {
        if (slot) slot->deactivated = true;
    }
    // Drop the impl's references; the slots themselves live on inside
    // any surviving ALTREP state structs.
    impl_->altrep_slots.clear();
    // 2. munmap + close. Subsequent calls are no-ops (idempotent) since
    //    we null out the pointers and reset fd to -1.
    if (impl_->mmap_base != nullptr && impl_->mmap_base != MAP_FAILED &&
        impl_->mmap_length > 0) {
        ::munmap(impl_->mmap_base, static_cast<size_t>(impl_->mmap_length));
        impl_->mmap_base = nullptr;
        impl_->mmap_length = 0;
        impl_->file_mmap = nullptr;
    }
    if (impl_->fd >= 0) {
        ::close(impl_->fd);
        impl_->fd = -1;
    }
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
    auto* store = xptr_to_store(xptr);
    try {
        if (!store->has(key)) return R_NilValue;
        const uint8_t* ptr = nullptr;
        uint64_t length = 0;
        uint16_t method = 0;
        uint32_t crc = 0;
        bool stored = store->stored_view(key, &ptr, &length, &method, &crc);
        if (stored) {
            // Zero-copy ALTREP view onto file_mmap. Compute offset relative
            // to the mmap base so the make-time-cached pointer can be
            // recomputed if needed, and so the slot records a stable
            // (offset, length) pair instead of a raw pointer.
            const uint8_t* base = store->file_mmap_base();
            uint64_t offset = static_cast<uint64_t>(ptr - base);
            return dafr::make_zip_raw_altrep_with_xptr(store, offset, length, xptr);
        }
        // Compressed: decompress + verify CRC. Decompressed bytes are a
        // throwaway buffer — there's no zero-copy benefit, so a plain
        // RAWSXP is fine (and avoids holding the mmap longer than needed).
        std::vector<uint8_t> bytes = store->read_decompressed(key);
        SEXP out = PROTECT(Rf_allocVector(RAWSXP, static_cast<R_xlen_t>(bytes.size())));
        if (!bytes.empty()) {
            std::memcpy(RAW(out), bytes.data(), bytes.size());
        }
        UNPROTECT(1);
        return out;
    } catch (const std::exception& e) {
        cpp11::stop("%s", e.what());
    }
}

[[cpp11::register]]
SEXP dafr_mmap_zip_set_bytes(SEXP xptr, std::string key, cpp11::raws bytes) {
    (void)xptr_to_store(xptr);
    (void)key;
    (void)bytes;
    cpp11::stop("dafr_mmap_zip_set_bytes: slice-17 phase 4 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_exists(SEXP xptr, std::string key) {
    auto* store = xptr_to_store(xptr);
    try {
        bool present = store->has(key);
        SEXP out = PROTECT(Rf_allocVector(LGLSXP, 1));
        LOGICAL(out)[0] = present ? TRUE : FALSE;
        UNPROTECT(1);
        return out;
    } catch (const std::exception& e) {
        cpp11::stop("%s", e.what());
    }
}

[[cpp11::register]]
SEXP dafr_mmap_zip_list(SEXP xptr, std::string prefix) {
    auto* store = xptr_to_store(xptr);
    try {
        std::vector<std::string> keys = store->list_with_prefix(prefix);
        SEXP out = PROTECT(Rf_allocVector(STRSXP, static_cast<R_xlen_t>(keys.size())));
        for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(keys.size()); ++i) {
            SET_STRING_ELT(out, i, Rf_mkCharCE(keys[static_cast<size_t>(i)].c_str(), CE_UTF8));
        }
        UNPROTECT(1);
        return out;
    } catch (const std::exception& e) {
        cpp11::stop("%s", e.what());
    }
}

[[cpp11::register]]
SEXP dafr_mmap_zip_reserve(SEXP xptr, std::string key, double size_double) {
    (void)xptr_to_store(xptr);
    (void)key;
    (void)size_double;
    cpp11::stop("dafr_mmap_zip_reserve: slice-17 phase 7 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_patch_crc(SEXP xptr, std::string key) {
    (void)xptr_to_store(xptr);
    (void)key;
    cpp11::stop("dafr_mmap_zip_patch_crc: slice-17 phase 7 stub");
}

[[cpp11::register]]
SEXP dafr_mmap_zip_set_crash_counter(SEXP xptr, SEXP counter, SEXP ns_env) {
    auto* store = xptr_to_store(xptr);
    store->set_crash_counter(counter, ns_env);
    return R_NilValue;
}
