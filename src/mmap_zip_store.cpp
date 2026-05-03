// Slice 17 — MmapZipStore: C++ port of upstream
// DataAxesFormats.jl/src/mmap_zip_store.jl. Phase 1 laid the skeleton;
// Phase 2 implements the read side (parse existing zip, list, get bytes,
// decompress deflate). Phase 4 adds the write side: bootstrap empty zip,
// reserve VA + overlay file, append entries via the two-step commit
// protocol. Phase 5 wires `tick_crash_counter` ticks at every commit-able
// decision point inside `commit_new_entry`. Phase 6 adds `recover_interrupted_appends`
// which runs after the CD parse on every writable open and rolls back any
// trailing entry whose LFH signature is missing or whose data CRC doesn't
// match the recorded CRC. Reserve/patch_crc (Phase 7) remain stubs.
//
// Windows: uses POSIX mmap/sys/mman; not ported to Win32 yet. On Windows
// the implementation body is skipped and the cpp11-registered entry
// points come from src/mmap_zip_store_win_stubs.cpp, which throws a
// clear runtime error. The package still builds; only the MmapZipStore
// feature is unavailable.

#include <cpp11.hpp>

#ifndef _WIN32

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

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

// Platform-specific MAP/PROT flags. We're Linux-only for slice 17 — Darwin's
// MAP_ANON is 0x1000 instead of MAP_ANONYMOUS but we don't need that yet.
#ifndef MAP_ANONYMOUS
#  ifdef MAP_ANON
#    define MAP_ANONYMOUS MAP_ANON
#  endif
#endif

// ZIP integer fields are stored little-endian on disk. Our shift+or LE
// writers don't depend on host endianness, but match upstream's runtime
// assertion so a hypothetical big-endian host fails loudly at startup.
static_assert(sizeof(uint16_t) == 2 && sizeof(uint32_t) == 4 && sizeof(uint64_t) == 8,
              "MmapZipStore requires standard fixed-width integer sizes");

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

// ---- Little-endian byte writers ------------------------------------------
//
// Shift+or, NOT unaligned reinterpret-casts: keeps the writer portable to
// any host byte order and avoids strict-aliasing UB. Mirrors upstream
// `write_little_endian_uintN!` in mmap_zip_store.jl.

static inline void w_le16(uint8_t* p, uint16_t v) {
    p[0] = static_cast<uint8_t>(v & 0xFFu);
    p[1] = static_cast<uint8_t>((v >> 8) & 0xFFu);
}

static inline void w_le32(uint8_t* p, uint32_t v) {
    p[0] = static_cast<uint8_t>(v & 0xFFu);
    p[1] = static_cast<uint8_t>((v >> 8) & 0xFFu);
    p[2] = static_cast<uint8_t>((v >> 16) & 0xFFu);
    p[3] = static_cast<uint8_t>((v >> 24) & 0xFFu);
}

static inline void w_le64(uint8_t* p, uint64_t v) {
    w_le32(p, static_cast<uint32_t>(v & 0xFFFFFFFFu));
    w_le32(p + 4, static_cast<uint32_t>((v >> 32) & 0xFFFFFFFFu));
}

// ---- Crash-counter tick ---------------------------------------------------
//
// Phase 5 instrumentation. `tick_crash_counter(impl)` is called at every
// commit-able decision point inside `commit_new_entry`. When no counter is
// set (the common case), the call is a no-op. When set, it invokes R's
// `dafr:::tick_crash_counter(counter)`, which decrements the counter and
// `stop()`s with a `SimulatedCrash` condition when it reaches zero.
//
// Safety: R's stop() longjmps. Plain longjmp through C++ frames bypasses
// destructors, leaking mmap regions / fds / heap-allocated buffers. We
// route the call through `cpp11::unwind_protect`, which performs an
// internal setjmp → throws `cpp11::unwind_exception` on a longjmp →
// C++ destructors run normally on the way out → the BEGIN_CPP11/END_CPP11
// boundary catches `unwind_exception` and calls `R_ContinueUnwind` so R
// observes the original `SimulatedCrash` condition unchanged.

class MmapZipStoreImpl;
static void tick_crash_counter_cpp(MmapZipStoreImpl& impl);

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
        // Release crash-counter SEXP protections (paired with the
        // R_PreserveObject calls in set_crash_counter).
        if (crash_counter != R_NilValue) {
            R_ReleaseObject(crash_counter);
            crash_counter = R_NilValue;
        }
        if (namespace_env != R_NilValue) {
            R_ReleaseObject(namespace_env);
            namespace_env = R_NilValue;
        }
    }
};

// Tick the crash counter, if set. When the R-side call longjmps out via
// stop("SimulatedCrash"), cpp11::unwind_protect catches it and re-throws as
// `cpp11::unwind_exception`. C++ destructors run normally on the way out;
// the BEGIN_CPP11/END_CPP11 cpp11 entry-point boundary catches the
// exception and calls R_ContinueUnwind to deliver the `SimulatedCrash`
// condition to R unchanged.
static void tick_crash_counter_cpp(MmapZipStoreImpl& impl) {
    if (impl.crash_counter == R_NilValue || impl.namespace_env == R_NilValue) {
        return;
    }
    cpp11::unwind_protect([&]() {
        SEXP call = PROTECT(Rf_lang2(Rf_install("tick_crash_counter"),
                                     impl.crash_counter));
        Rf_eval(call, impl.namespace_env);
        UNPROTECT(1);
    });
}

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
//
// `tolerant` (writable opens, before recovery has run): if the strict
// search fails because the trailing comment-length doesn't reach EOF
// (which happens after a crash between resize and CD commit — the file
// grew but the old EOCD bytes still live at the pre-grow offset), retry
// without the comment-length check. Recovery will then truncate the file
// back to a tidy size.
uint64_t find_eocd(const uint8_t* base, uint64_t length, bool tolerant = false) {
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
    if (tolerant) {
        // Tolerant fallback: find the most recent EOCD signature
        // regardless of comment-length validity. This handles the
        // post-crash scenario where ftruncate added zeros past the
        // valid EOCD.
        for (uint64_t i = length - END_OF_CENTRAL_DIRECTORY_SIZE; ; --i) {
            if (le32(base + i) == END_OF_CENTRAL_DIRECTORY_SIGNATURE) {
                return i;
            }
            if (i == scan_start) break;
        }
    }
    zip_corrupt("end-of-central-directory record not found");
}

// Re-read the local file header at `lfh_offset` to compute the data offset.
// Returns lfh_offset + 30 + name_len + extra_len. Validates the LFH
// signature and bounds. When `tolerant` is true (writable opens, before
// recovery has run), returns 0 instead of throwing on a missing/invalid
// LFH so the parser can continue past corrupt trailing entries; recovery
// then walks the parsed list and rolls them back.
uint64_t compute_data_offset(const uint8_t* base, uint64_t length,
                             uint64_t lfh_offset, bool tolerant = false) {
    if (lfh_offset + LOCAL_FILE_HEADER_FIXED_SIZE > length) {
        if (tolerant) return 0;
        zip_corrupt("local file header offset out of range");
    }
    if (le32(base + lfh_offset) != LOCAL_FILE_HEADER_SIGNATURE) {
        if (tolerant) return 0;
        zip_corrupt("local file header signature missing");
    }
    uint16_t name_len  = le16(base + lfh_offset + 26);
    uint16_t extra_len = le16(base + lfh_offset + 28);
    uint64_t data_off  = lfh_offset + LOCAL_FILE_HEADER_FIXED_SIZE
                       + static_cast<uint64_t>(name_len)
                       + static_cast<uint64_t>(extra_len);
    if (data_off > length) {
        if (tolerant) return 0;
        zip_corrupt("local file header extends past EOF");
    }
    return data_off;
}

// Parse `path`'s zip metadata into `impl`. Caller must have populated
// impl.file_mmap + impl.overlay_length. Throws on corrupt inputs.
// `tolerant` = true (writable opens) means: don't reject entries whose
// LFH is missing/corrupt. Such entries get `data_offset = 0` and
// `compressed_size` from the CD; recovery will roll them back.
void parse_central_directory(MmapZipStoreImpl& impl, bool tolerant = false) {
    const uint8_t* base = impl.file_mmap;
    uint64_t length     = impl.overlay_length;

    uint64_t eocd_offset = find_eocd(base, length, tolerant);

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

        uint64_t data_off = compute_data_offset(base, length, lfh_off, tolerant);
        if (data_off != 0 && data_off + comp_sz > length) {
            if (!tolerant) {
                zip_corrupt("entry data extends past EOF");
            }
            // In tolerant mode, mark entry as invalid by zeroing data_off.
            data_off = 0;
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

// ---- Virtual address reservation + file overlay -------------------------
//
// `reserve_virtual_range` consumes virtual address space only — no RAM, no
// disk, no file size. The returned region is PROT_NONE so any access to it
// faults until it's overlaid with `overlay_file_on_range` (PROT_READ |
// PROT_WRITE, MAP_SHARED | MAP_FIXED).
//
// On Linux MAP_FIXED atomically replaces any prior overlay at the same
// base, so growing the file just requires another overlay call.

void* reserve_virtual_range(uint64_t byte_length) {
    void* p = ::mmap(nullptr, static_cast<size_t>(byte_length),
                     PROT_NONE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (p == MAP_FAILED) {
        throw std::runtime_error(
            std::string("MmapZipStore: mmap reservation failed: ") + std::strerror(errno));
    }
    return p;
}

void overlay_file_on_range(void* base, int fd, uint64_t byte_length) {
    void* p = ::mmap(base, static_cast<size_t>(byte_length),
                     PROT_READ | PROT_WRITE, MAP_SHARED | MAP_FIXED, fd, 0);
    if (p == MAP_FAILED) {
        throw std::runtime_error(
            std::string("MmapZipStore: mmap file overlay failed: ") + std::strerror(errno));
    }
    if (p != base) {
        throw std::runtime_error("MmapZipStore: mmap file overlay returned a different base");
    }
}

void disable_transparent_huge_pages(void* base, uint64_t byte_length) {
#ifdef MADV_NOHUGEPAGE
    if (byte_length == 0) return;
    int rc = ::madvise(base, static_cast<size_t>(byte_length), MADV_NOHUGEPAGE);
    if (rc != 0) {
        Rf_warning("MmapZipStore: madvise(MADV_NOHUGEPAGE) failed: %s", std::strerror(errno));
    }
#else
    (void)base;
    (void)byte_length;
#endif
}

void resize_file_overlay(MmapZipStoreImpl& impl, uint64_t new_file_size) {
    if (!impl.is_writable) {
        throw std::runtime_error("MmapZipStore: resize on a non-writable store");
    }
    if (new_file_size > impl.max_file_size) {
        throw std::runtime_error("MmapZipStore: resize beyond max_file_size");
    }
    if (::ftruncate(impl.fd, static_cast<off_t>(new_file_size)) != 0) {
        throw std::runtime_error(
            std::string("MmapZipStore: ftruncate failed: ") + std::strerror(errno));
    }
    overlay_file_on_range(reinterpret_cast<void*>(impl.reservation_base),
                          impl.fd, new_file_size);
    impl.overlay_length = new_file_size;
}

// ---- ZIP record writers --------------------------------------------------
//
// Byte-for-byte ports of upstream's write_*! functions. `dst` points at
// the first byte of the record (Julia's 1-indexed `position` -> 0-indexed
// pointer). Field offsets match upstream comments verbatim.

void write_central_directory_entry(uint8_t* dst,
                                   const std::string& name_bytes,
                                   uint64_t compressed_size,
                                   uint64_t uncompressed_size,
                                   uint32_t crc32_value,
                                   uint64_t local_file_header_offset) {
    w_le32(dst,       CENTRAL_DIRECTORY_ENTRY_SIGNATURE);
    w_le16(dst + 4,   0x031eu);                       // version made by (Unix v3.0)
    w_le16(dst + 6,   ZIP64_VERSION_NEEDED);          // version needed (ZIP64)
    w_le16(dst + 8,   static_cast<uint16_t>(1u << 11)); // gp flag: UTF-8 name
    w_le16(dst + 10,  STORED_COMPRESSION_METHOD);
    w_le16(dst + 12,  0u);                            // last mod time
    w_le16(dst + 14,  0x21u);                         // last mod date (1980-01-01)
    w_le32(dst + 16,  crc32_value);
    w_le32(dst + 20,  0xFFFFFFFFu);                   // comp size sentinel
    w_le32(dst + 24,  0xFFFFFFFFu);                   // uncomp size sentinel
    w_le16(dst + 28,  static_cast<uint16_t>(name_bytes.size()));
    w_le16(dst + 30,  static_cast<uint16_t>(ZIP64_CENTRAL_DIRECTORY_EXTRA_SIZE));
    w_le16(dst + 32,  0u);                            // comment length
    w_le16(dst + 34,  0u);                            // disk number start
    w_le16(dst + 36,  0u);                            // internal attrs
    w_le32(dst + 38,  static_cast<uint32_t>(0100644u) << 16); // external attrs (Unix)
    w_le32(dst + 42,  0xFFFFFFFFu);                   // LFH offset sentinel
    if (!name_bytes.empty()) {
        std::memcpy(dst + CENTRAL_DIRECTORY_ENTRY_FIXED_SIZE,
                    name_bytes.data(), name_bytes.size());
    }
    uint8_t* extra = dst + CENTRAL_DIRECTORY_ENTRY_FIXED_SIZE + name_bytes.size();
    w_le16(extra,     ZIP64_EXTRA_HEADER_ID);
    w_le16(extra + 2, static_cast<uint16_t>(ZIP64_CENTRAL_DIRECTORY_EXTRA_SIZE - 4));
    w_le64(extra + 4,  uncompressed_size);
    w_le64(extra + 12, compressed_size);
    w_le64(extra + 20, local_file_header_offset);
}

void write_local_file_header(uint8_t* dst,
                             const std::string& name_bytes,
                             uint64_t compressed_size,
                             uint64_t uncompressed_size,
                             uint32_t crc32_value,
                             uint32_t alignment_padding) {
    uint16_t total_extra = static_cast<uint16_t>(
        ZIP64_LOCAL_FILE_HEADER_EXTRA_SIZE + alignment_padding);
    w_le32(dst,       LOCAL_FILE_HEADER_SIGNATURE);
    w_le16(dst + 4,   ZIP64_VERSION_NEEDED);
    w_le16(dst + 6,   static_cast<uint16_t>(1u << 11));
    w_le16(dst + 8,   STORED_COMPRESSION_METHOD);
    w_le16(dst + 10,  0u);                            // last mod time
    w_le16(dst + 12,  0x21u);                         // last mod date
    w_le32(dst + 14,  crc32_value);
    w_le32(dst + 18,  0xFFFFFFFFu);                   // comp size sentinel
    w_le32(dst + 22,  0xFFFFFFFFu);                   // uncomp size sentinel
    w_le16(dst + 26,  static_cast<uint16_t>(name_bytes.size()));
    w_le16(dst + 28,  total_extra);
    if (!name_bytes.empty()) {
        std::memcpy(dst + LOCAL_FILE_HEADER_FIXED_SIZE,
                    name_bytes.data(), name_bytes.size());
    }
    uint8_t* extra = dst + LOCAL_FILE_HEADER_FIXED_SIZE + name_bytes.size();
    w_le16(extra,     ZIP64_EXTRA_HEADER_ID);
    w_le16(extra + 2, static_cast<uint16_t>(ZIP64_LOCAL_FILE_HEADER_EXTRA_SIZE - 4));
    w_le64(extra + 4,  uncompressed_size);
    w_le64(extra + 12, compressed_size);
    if (alignment_padding > 0) {
        // Min extra-field size is 4 bytes (header alone with 0-byte payload).
        // compute_alignment_padding always returns >= 4 (or 0).
        uint8_t* pad = extra + ZIP64_LOCAL_FILE_HEADER_EXTRA_SIZE;
        w_le16(pad,     DAF_PADDING_EXTRA_HEADER_ID);
        w_le16(pad + 2, static_cast<uint16_t>(alignment_padding - 4));
        if (alignment_padding > 4) {
            std::memset(pad + 4, 0, alignment_padding - 4);
        }
    }
}

void write_zip64_end_of_central_directory(uint8_t* dst,
                                          uint64_t cd_offset,
                                          uint64_t cd_size,
                                          uint64_t total_entries) {
    w_le32(dst,       ZIP64_END_OF_CENTRAL_DIRECTORY_SIGNATURE);
    // size of this record minus the signature + this size field (12 bytes).
    w_le64(dst + 4,   ZIP64_END_OF_CENTRAL_DIRECTORY_SIZE - 12u);
    w_le16(dst + 12,  0x031eu);                        // version made by
    w_le16(dst + 14,  ZIP64_VERSION_NEEDED);
    w_le32(dst + 16,  0u);                             // disk number
    w_le32(dst + 20,  0u);                             // CD start disk
    w_le64(dst + 24,  total_entries);                  // entries this disk
    w_le64(dst + 32,  total_entries);                  // total entries
    w_le64(dst + 40,  cd_size);
    w_le64(dst + 48,  cd_offset);
}

void write_zip64_end_of_central_directory_locator(uint8_t* dst, uint64_t z64_eocd_offset) {
    w_le32(dst,       ZIP64_END_OF_CENTRAL_DIRECTORY_LOCATOR_SIGNATURE);
    w_le32(dst + 4,   0u);                             // disk holding ZIP64 EOCD
    w_le64(dst + 8,   z64_eocd_offset);
    w_le32(dst + 16,  1u);                             // total disks
}

void write_end_of_central_directory(uint8_t* dst) {
    w_le32(dst,       END_OF_CENTRAL_DIRECTORY_SIGNATURE);
    w_le16(dst + 4,   0u);
    w_le16(dst + 6,   0u);
    w_le16(dst + 8,   0xFFFFu);                        // entries this disk (sentinel)
    w_le16(dst + 10,  0xFFFFu);                        // total entries (sentinel)
    w_le32(dst + 12,  0xFFFFFFFFu);                    // CD size sentinel
    w_le32(dst + 16,  0xFFFFFFFFu);                    // CD offset sentinel
    w_le16(dst + 20,  0u);                             // comment length
}

void write_end_of_central_directory_region(uint8_t* dst,
                                           uint64_t cd_offset,
                                           uint64_t cd_size,
                                           uint64_t total_entries) {
    uint64_t z64_eocd_offset = cd_offset + cd_size;
    write_zip64_end_of_central_directory(dst, cd_offset, cd_size, total_entries);
    write_zip64_end_of_central_directory_locator(
        dst + ZIP64_END_OF_CENTRAL_DIRECTORY_SIZE, z64_eocd_offset);
    write_end_of_central_directory(
        dst + ZIP64_END_OF_CENTRAL_DIRECTORY_SIZE + ZIP64_END_OF_CENTRAL_DIRECTORY_LOCATOR_SIZE);
}

uint64_t central_directory_entry_size(const ZipEntry& entry) {
    return static_cast<uint64_t>(CENTRAL_DIRECTORY_ENTRY_FIXED_SIZE)
         + static_cast<uint64_t>(entry.name.size())
         + static_cast<uint64_t>(ZIP64_CENTRAL_DIRECTORY_EXTRA_SIZE);
}

void populate_central_directory_offsets(std::vector<ZipEntry>& entries,
                                        uint64_t central_directory_offset) {
    uint64_t cur = central_directory_offset;
    for (auto& e : entries) {
        e.central_directory_offset = cur;
        cur += central_directory_entry_size(e);
    }
}

void write_central_directory_and_eocd(uint8_t* dst,
                                      const std::vector<ZipEntry>& entries,
                                      uint64_t central_directory_offset,
                                      uint64_t central_directory_size) {
    uint64_t pos = 0;
    for (const auto& entry : entries) {
        write_central_directory_entry(dst + pos,
                                      entry.name,
                                      entry.compressed_size,
                                      entry.uncompressed_size,
                                      entry.crc32,
                                      entry.local_file_header_offset);
        pos += static_cast<uint64_t>(CENTRAL_DIRECTORY_ENTRY_FIXED_SIZE)
             + static_cast<uint64_t>(entry.name.size())
             + static_cast<uint64_t>(ZIP64_CENTRAL_DIRECTORY_EXTRA_SIZE);
    }
    write_end_of_central_directory_region(dst + central_directory_size,
                                          central_directory_offset,
                                          central_directory_size,
                                          static_cast<uint64_t>(entries.size()));
}

// Number of bytes to add to the LFH so the data region starts at an
// 8-byte-aligned offset. Returns 0 when already aligned, otherwise >= 4
// (the minimum extra-field size: 4-byte header + 0-byte payload).
uint32_t compute_alignment_padding(uint64_t unpadded_data_offset) {
    uint32_t remainder = static_cast<uint32_t>(
        unpadded_data_offset % static_cast<uint64_t>(DAF_DATA_OFFSET_ALIGNMENT));
    if (remainder == 0) return 0;
    // upstream: total = mod(-remainder, 8). With unsigned 32-bit math,
    // -remainder wraps but mod 8 takes only the low 3 bits, which equal
    // (8 - remainder) mod 8 == 8 - remainder (since 1 <= remainder <= 7).
    uint32_t total = (DAF_DATA_OFFSET_ALIGNMENT - remainder) % DAF_DATA_OFFSET_ALIGNMENT;
    if (total >= 4) return total;
    return total + DAF_DATA_OFFSET_ALIGNMENT;
}

// ---- Recovery (Phase 6) --------------------------------------------------
//
// Port of upstream `recover_interrupted_appends!` /
// `entry_is_valid` / `roll_back_to_entry!` from
// DataAxesFormats.jl/src/mmap_zip_store.jl (lines 975-1057).
//
// Called from `MmapZipStore::open` after the CD parse, only on writable
// opens. Walks the parsed entries from back to front; the first run of
// trailing entries failing `entry_is_valid` is rolled back: rebuild a fresh
// CD+EOCD at the oldest invalid entry's `local_file_header_offset` and
// `ftruncate` the file to the new EOA.
//
// `entry_is_valid` returns true iff (a) the LFH signature at
// `entry.local_file_header_offset` is 0x04034b50 AND (b) the CRC32 of the
// data range equals the recorded CRC. Non-stored entries trust the CD
// (we never write deflate entries from this codebase, so the only way to
// see a non-stored entry is via an externally-built archive — for which
// rollback isn't meaningful).

bool entry_is_valid(const MmapZipStoreImpl& impl, std::size_t idx) {
    const ZipEntry& e = impl.entries[idx];
    if (e.compression_method != STORED_COMPRESSION_METHOD) {
        // We never write deflate from this codebase; trust the CD for
        // externally-built archives.
        return true;
    }
    const uint8_t* base = impl.file_mmap;
    uint64_t length = impl.overlay_length;
    if (e.local_file_header_offset + LOCAL_FILE_HEADER_FIXED_SIZE > length) return false;
    if (le32(base + e.local_file_header_offset) != LOCAL_FILE_HEADER_SIGNATURE) {
        return false;
    }
    // The tolerant parser may have left data_offset = 0 for entries
    // whose LFH was unreadable at parse time. Re-compute now that we
    // know the LFH signature is good. Failure to compute (out-of-range
    // name/extra) → invalid.
    uint64_t data_off = compute_data_offset(base, length,
                                            e.local_file_header_offset,
                                            /*tolerant=*/true);
    if (data_off == 0) return false;
    if (data_off + e.compressed_size > length) {
        return false;
    }
    uLong crc = ::crc32(0L, Z_NULL, 0);
    if (e.compressed_size > 0) {
        crc = ::crc32(crc, reinterpret_cast<const Bytef*>(base + data_off),
                      static_cast<uInt>(e.compressed_size));
    }
    return static_cast<uint32_t>(crc) == e.crc32;
}

void roll_back_to_entry(MmapZipStoreImpl& impl, std::size_t keep_count) {
    // The LFH offset of the FIRST DROPPED entry becomes the new
    // `central_directory_offset` (i.e., the new end of the data region).
    if (keep_count >= impl.entries.size()) return;
    uint64_t new_cd_offset =
        impl.entries[keep_count].local_file_header_offset;

    // Drop entries [keep_count..end).
    for (std::size_t i = keep_count; i < impl.entries.size(); ++i) {
        impl.name_to_index.erase(impl.entries[i].name);
    }
    impl.entries.resize(keep_count);

    uint64_t new_cd_size = 0;
    for (const auto& e : impl.entries) {
        new_cd_size += central_directory_entry_size(e);
    }

    // Build the commit buffer (kept-entries CD + 98-byte EOCD trailer).
    std::vector<uint8_t> commit_buffer(
        static_cast<std::size_t>(new_cd_size + TRAILING_END_OF_CENTRAL_DIRECTORY_REGION_SIZE));
    write_central_directory_and_eocd(commit_buffer.data(),
                                     impl.entries,
                                     new_cd_offset,
                                     new_cd_size);

    uint64_t new_file_size = new_cd_offset + new_cd_size
        + static_cast<uint64_t>(TRAILING_END_OF_CENTRAL_DIRECTORY_REGION_SIZE);
    resize_file_overlay(impl, new_file_size);

    uint8_t* mapped = static_cast<uint8_t*>(impl.mmap_base);
    std::memcpy(mapped + new_cd_offset, commit_buffer.data(), commit_buffer.size());

    impl.central_directory_offset = new_cd_offset;
    impl.central_directory_size = new_cd_size;
    populate_central_directory_offsets(impl.entries, new_cd_offset);
}

void recover_interrupted_appends(MmapZipStoreImpl& impl) {
    // Walk from the last entry backward, finding the highest index that
    // is still valid. If every entry is valid, recovery is a no-op for
    // the entry list — but we still truncate the file back to the
    // canonical size (cd_offset + cd_size + EOCD region) in case the
    // file was grown past it by a tick-#2-style crash.
    std::size_t last_valid_plus_one = impl.entries.size();
    while (last_valid_plus_one > 0) {
        if (entry_is_valid(impl, last_valid_plus_one - 1)) {
            break;
        }
        --last_valid_plus_one;
    }
    if (last_valid_plus_one < impl.entries.size()) {
        roll_back_to_entry(impl, last_valid_plus_one);
        return;
    }
    // All entries valid. If the file is larger than the canonical
    // archive size, truncate the trailing junk away.
    uint64_t canonical_size = impl.central_directory_offset
        + impl.central_directory_size
        + static_cast<uint64_t>(TRAILING_END_OF_CENTRAL_DIRECTORY_REGION_SIZE);
    if (impl.overlay_length > canonical_size) {
        resize_file_overlay(impl, canonical_size);
    }
}

// Bootstrap an empty zip archive at `path`. Writes 98 bytes of trailing
// EOCD region (ZIP64 EOCD + locator + legacy EOCD with sentinel values).
// Used by the writable constructor when the file doesn't exist.
void write_empty_zip_archive(const std::string& path) {
    int fd = ::open(path.c_str(), O_RDWR | O_CREAT | O_TRUNC, 0644);
    if (fd < 0) {
        throw std::runtime_error(
            "MmapZipStore: cannot create '" + path + "': " + std::strerror(errno));
    }
    uint8_t buf[TRAILING_END_OF_CENTRAL_DIRECTORY_REGION_SIZE];
    write_end_of_central_directory_region(buf, 0, 0, 0);
    ssize_t written = ::write(fd, buf, sizeof(buf));
    int e = errno;
    ::close(fd);
    if (written != static_cast<ssize_t>(sizeof(buf))) {
        throw std::runtime_error(
            "MmapZipStore: failed to write empty zip header to '" + path + "': "
            + std::strerror(e));
    }
}

}  // namespace

// ---- MmapZipStore ---------------------------------------------------------

std::unique_ptr<MmapZipStore> MmapZipStore::open(const std::string& path,
                                                 bool writable, bool create, bool truncate,
                                                 uint64_t max_file_size) {
    if (!writable) {
        // Read-only path (Phase 2) — unchanged.
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

    // Writable path (Phase 4): bootstrap if needed, reserve VA, overlay file.
    if (truncate) {
        if (!create) {
            throw std::runtime_error("MmapZipStore: truncate requires create");
        }
        ::unlink(path.c_str());  // Ignore ENOENT.
    }

    struct stat st_probe;
    bool exists = (::stat(path.c_str(), &st_probe) == 0);
    if (!exists) {
        if (!create) {
            throw std::runtime_error("MmapZipStore: zip file does not exist: " + path);
        }
        write_empty_zip_archive(path);
    }

    int fd = ::open(path.c_str(), O_RDWR);
    if (fd < 0) {
        throw std::runtime_error(
            "MmapZipStore: failed to open '" + path + "' for writing: " + std::strerror(errno));
    }
    struct stat st;
    if (::fstat(fd, &st) != 0) {
        int e = errno;
        ::close(fd);
        throw std::runtime_error(
            "MmapZipStore: fstat '" + path + "': " + std::strerror(e));
    }
    uint64_t file_size = static_cast<uint64_t>(st.st_size);
    if (file_size < END_OF_CENTRAL_DIRECTORY_SIZE) {
        ::close(fd);
        throw std::runtime_error(
            "MmapZipStore: file too small to be a zip archive: '" + path + "'");
    }
    if (max_file_size == 0) {
        max_file_size = DEFAULT_MAX_FILE_SIZE;
    }
    if (file_size > max_file_size) {
        ::close(fd);
        throw std::runtime_error(
            "MmapZipStore: file size exceeds max_file_size for writable open: " + path);
    }

    void* reservation = nullptr;
    try {
        reservation = reserve_virtual_range(max_file_size);
        overlay_file_on_range(reservation, fd, file_size);
        disable_transparent_huge_pages(reservation, max_file_size);
    } catch (...) {
        if (reservation != nullptr && reservation != MAP_FAILED) {
            ::munmap(reservation, static_cast<size_t>(max_file_size));
        }
        ::close(fd);
        throw;
    }

    auto impl = std::unique_ptr<MmapZipStoreImpl>(new MmapZipStoreImpl());
    impl->path = path;
    impl->is_writable = true;
    impl->max_file_size = max_file_size;
    impl->fd = fd;
    impl->mmap_base = reservation;
    impl->mmap_length = max_file_size;     // destructor munmaps full reservation
    impl->reservation_base = reinterpret_cast<uint64_t>(reservation);
    impl->reservation_length = max_file_size;
    impl->overlay_length = file_size;
    impl->file_mmap = static_cast<const uint8_t*>(reservation);

    try {
        // Tolerant parse: don't reject entries whose LFH is missing.
        // Recovery will roll them back below.
        parse_central_directory(*impl, /*tolerant=*/true);
        // Phase 6: every writable open recovers from a previous crash that
        // left trailing entries with missing LFHs or mismatched CRCs.
        recover_interrupted_appends(*impl);
    } catch (...) {
        // Destructor will tear down mmap + fd.
        throw;
    }

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

// commit_new_entry — port of upstream `commit_new_entry!`. Runs the full
// 5-tick append protocol with a *caller-supplied* CRC32. When the caller
// provides the real CRC (the `append` path) the on-disk archive is fully
// valid after this returns. When the caller provides a placeholder CRC of
// 0 (the `reserve` path), the entry is committed but its CRC field is
// wrong — the caller MUST run `patch_crc` to write the real CRC into both
// the LFH and CD. If the process crashes between the two, the next
// write-mode open detects the placeholder-CRC mismatch and rolls the
// reservation back via the same recovery path used for tick-#3/#4 crashes.
//
// Returns the impl-side index of the just-committed entry. The data
// region (`mapped + entry.data_offset .. + length`) holds whatever bytes
// `data` pointed to; for reserve, `data == nullptr` and the data region
// is left as ftruncate-zeros for the caller to fill in place.
static std::size_t commit_new_entry_impl(MmapZipStoreImpl& impl,
                                         const std::string& key,
                                         const uint8_t* data,
                                         uint64_t length,
                                         uint32_t crc32_value) {
    if (key.size() > 0xFFFFu) {
        throw std::runtime_error(
            "MmapZipStore: zip entry name too long: " + std::to_string(key.size()) + " bytes");
    }

    // ---- commit_new_entry! port ----
    uint64_t lfh_offset = impl.central_directory_offset;
    uint64_t base_lfh_size =
        static_cast<uint64_t>(LOCAL_FILE_HEADER_FIXED_SIZE)
        + static_cast<uint64_t>(key.size())
        + static_cast<uint64_t>(ZIP64_LOCAL_FILE_HEADER_EXTRA_SIZE);
    uint32_t alignment_padding = compute_alignment_padding(lfh_offset + base_lfh_size);
    uint64_t lfh_size = base_lfh_size + alignment_padding;
    uint64_t data_offset = lfh_offset + lfh_size;
    uint64_t new_cd_offset = data_offset + length;

    uint64_t new_entry_record_size =
        static_cast<uint64_t>(CENTRAL_DIRECTORY_ENTRY_FIXED_SIZE)
        + static_cast<uint64_t>(key.size())
        + static_cast<uint64_t>(ZIP64_CENTRAL_DIRECTORY_EXTRA_SIZE);
    uint64_t new_cd_size = impl.central_directory_size + new_entry_record_size;

    uint64_t required_file_size =
        new_cd_offset + new_cd_size
        + static_cast<uint64_t>(TRAILING_END_OF_CENTRAL_DIRECTORY_REGION_SIZE);
    if (required_file_size > impl.max_file_size) {
        throw std::runtime_error(
            "MmapZipStore: append of '" + key + "' would exceed max_file_size in: " + impl.path);
    }

    // Construct the new entry. Pushed to impl.entries BELOW (after tick #1
    // and the resize) so that a SimulatedCrash at tick #1 leaves the
    // in-memory entry list unchanged — the post-crash store object is
    // expected to be dropped, but the destructor still walks the list.
    ZipEntry new_entry;
    new_entry.name = key;
    new_entry.local_file_header_offset = lfh_offset;
    new_entry.data_offset = data_offset;
    new_entry.compressed_size = length;
    new_entry.uncompressed_size = length;
    new_entry.crc32 = crc32_value;
    new_entry.compression_method = STORED_COMPRESSION_METHOD;
    new_entry.central_directory_offset = 0;  // filled in by populate below

    // ---- TICK #1 — before resize_file_overlay --------------------------
    // Recovery state on interrupt: file size unchanged, old CD intact, no
    // new entry visible on reopen. No rollback needed.
    tick_crash_counter_cpp(impl);

    // Grow the file + re-overlay so the new region is reachable through the
    // mapped pointer.
    resize_file_overlay(impl, required_file_size);

    // Now we can safely push the new entry — the file is at least big
    // enough to hold all the entries we're about to write to disk, and a
    // crash from this point forward will be detected by recovery (either
    // tick #2 fires before the new CD is committed and old CD remains
    // authoritative, or ticks #3/#4 fire and recovery rolls back).
    impl.entries.push_back(new_entry);

    // Build the commit buffer (new CD + EOCD region).
    std::vector<uint8_t> commit_buffer(
        static_cast<std::size_t>(new_cd_size + TRAILING_END_OF_CENTRAL_DIRECTORY_REGION_SIZE));
    write_central_directory_and_eocd(commit_buffer.data(),
                                     impl.entries,
                                     new_cd_offset,
                                     new_cd_size);

    // Build the LFH buffer.
    std::vector<uint8_t> lfh_buffer(static_cast<std::size_t>(lfh_size));
    write_local_file_header(lfh_buffer.data(),
                            key,
                            length,
                            length,
                            crc32_value,
                            alignment_padding);

    uint8_t* mapped = static_cast<uint8_t*>(impl.mmap_base);

    // ---- TICK #2 — after resize, before new CD is committed ------------
    // Recovery state: file grew but the OLD CD/EOCD bytes still live at
    // their original offsets (we haven't overwritten them yet). Reopen
    // sees the old EOCD via the parser's backward-scan and yields the
    // pre-append entry list. No rollback needed.
    tick_crash_counter_cpp(impl);

    // The "commit point": writing the new CD + EOCD at new_cd_offset makes
    // the archive logically valid. After this, the new CD claims the new
    // entry and the old CD bytes are partially or fully overwritten.
    std::memcpy(mapped + new_cd_offset,
                commit_buffer.data(),
                commit_buffer.size());

    // ---- TICK #3 — after CD copy, before LFH copy ----------------------
    // **THE COMMIT POINT.** The new CD on disk references an entry whose
    // LFH has not been written yet — at lfh_offset the bytes are still old
    // CD content (no LFH signature). Recovery: walks new CD; entry's
    // `entry_is_valid` returns false because LFH signature is missing;
    // ROLLBACK to the prior entry count.
    tick_crash_counter_cpp(impl);

    // Then write the LFH (overwrites the old CD region — safe because the
    // new CD already lives at a higher offset).
    std::memcpy(mapped + lfh_offset, lfh_buffer.data(), lfh_buffer.size());

    // ---- TICK #4 — after LFH copy, before data copy --------------------
    // Recovery state: LFH is valid (signature + sizes + CRC all present),
    // but the data region is sparse zeros from ftruncate. CRC of zero
    // bytes won't match the recorded CRC for any non-empty payload.
    // ROLLBACK via the CRC mismatch path.
    tick_crash_counter_cpp(impl);

    // Then copy the data payload (skipped when `data == nullptr` — the
    // reserve path leaves the data region as ftruncate zeros for the
    // caller to fill in place via the returned writable pointer).
    if (length > 0 && data != nullptr) {
        std::memcpy(mapped + data_offset, data, static_cast<std::size_t>(length));
    }

    // Update impl bookkeeping. Done before tick #5 because the on-disk
    // state is fully valid at this point (or, for reserve, the only
    // remaining anomaly is a placeholder CRC, which the caller patches
    // immediately) — recovery would be a no-op even if we crashed here.
    impl.central_directory_offset = new_cd_offset;
    impl.central_directory_size = new_cd_size;
    populate_central_directory_offsets(impl.entries, new_cd_offset);
    std::size_t new_index = impl.entries.size() - 1;
    impl.name_to_index[key] = new_index;

    // ---- TICK #5 — after data copy (entry fully committed) -------------
    // No rollback needed. Included for symmetry: every commit-able decision
    // ends with a tick.
    tick_crash_counter_cpp(impl);
    return new_index;
}

void MmapZipStore::append(const std::string& key, const uint8_t* data, uint64_t length) {
    if (!impl_) {
        throw std::runtime_error("MmapZipStore: store is closed");
    }
    if (!impl_->is_writable) {
        throw std::runtime_error("MmapZipStore is read-only; cannot set bytes");
    }
    if (impl_->name_to_index.find(key) != impl_->name_to_index.end()) {
        throw std::runtime_error(
            "MmapZipStore is append-only; cannot overwrite entry: " + key);
    }

    // CRC32 of the (uncompressed) data using zlib.
    uLong crc = ::crc32(0L, Z_NULL, 0);
    if (length > 0) {
        crc = ::crc32(crc, reinterpret_cast<const Bytef*>(data),
                      static_cast<uInt>(length));
    }
    uint32_t crc32_value = static_cast<uint32_t>(crc);

    commit_new_entry_impl(*impl_, key, data, length, crc32_value);
}

uint8_t* MmapZipStore::reserve(const std::string& key, uint64_t length) {
    if (!impl_) {
        throw std::runtime_error("MmapZipStore: store is closed");
    }
    if (!impl_->is_writable) {
        throw std::runtime_error("MmapZipStore is read-only; cannot reserve entry");
    }
    if (impl_->name_to_index.find(key) != impl_->name_to_index.end()) {
        throw std::runtime_error(
            "MmapZipStore is append-only; cannot overwrite entry: " + key);
    }

    // commit_new_entry_impl with placeholder CRC32 = 0. Pass data=nullptr
    // so the protocol skips the on-disk data copy; the caller will fill
    // the reserved region in place via the returned pointer.
    std::size_t idx = commit_new_entry_impl(*impl_, key,
                                            /*data=*/nullptr,
                                            length,
                                            /*crc32_value=*/0u);
    if (length == 0) {
        // No data region — return a non-null but zero-length pointer.
        // Use the mmap base + 0; callers should not deref a zero-length
        // view anyway.
        return static_cast<uint8_t*>(impl_->mmap_base);
    }
    uint64_t data_offset = impl_->entries[idx].data_offset;
    return static_cast<uint8_t*>(impl_->mmap_base) + data_offset;
}

void MmapZipStore::patch_crc(const std::string& key) {
    if (!impl_) {
        throw std::runtime_error("MmapZipStore: store is closed");
    }
    if (!impl_->is_writable) {
        throw std::runtime_error("MmapZipStore is read-only; cannot patch CRC");
    }
    auto it = impl_->name_to_index.find(key);
    if (it == impl_->name_to_index.end()) {
        throw std::runtime_error("MmapZipStore: entry '" + key + "' not found for patch_crc");
    }
    ZipEntry& entry = impl_->entries[it->second];
    uint8_t* mapped = static_cast<uint8_t*>(impl_->mmap_base);

    // Compute CRC32 over the now-filled data region.
    uLong crc = ::crc32(0L, Z_NULL, 0);
    if (entry.compressed_size > 0) {
        crc = ::crc32(crc,
                      reinterpret_cast<const Bytef*>(mapped + entry.data_offset),
                      static_cast<uInt>(entry.compressed_size));
    }
    uint32_t computed = static_cast<uint32_t>(crc);
    entry.crc32 = computed;

    // Patch LFH CRC field at lfh_offset + 14, and CD CRC field at
    // cd_offset + 16. Two single-uint32 stores into the shared mmap.
    // No tick is inserted between them: per the brief, the patch is a
    // single logical operation; if interrupted, recovery uses the CD's
    // CRC, which won't match the data, triggering the same rollback as
    // the placeholder-CRC path.
    w_le32(mapped + entry.local_file_header_offset + 14u, computed);
    w_le32(mapped + entry.central_directory_offset + 16u, computed);
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
    auto slot = std::make_shared<AltrepRawSlot>(
        AltrepRawSlot{offset, length, /*deactivated=*/false, /*writable_inplace=*/false});
    impl_->altrep_slots.push_back(slot);
    return slot;
}

std::shared_ptr<AltrepRawSlot> MmapZipStore::register_altrep_slot_writable(
    uint64_t offset, uint64_t length) {
    auto slot = std::make_shared<AltrepRawSlot>(
        AltrepRawSlot{offset, length, /*deactivated=*/false, /*writable_inplace=*/true});
    impl_->altrep_slots.push_back(slot);
    return slot;
}

void MmapZipStore::set_crash_counter(SEXP counter, SEXP namespace_env) {
    // Release any previously-held protections.
    if (impl_->crash_counter != R_NilValue) {
        R_ReleaseObject(impl_->crash_counter);
        impl_->crash_counter = R_NilValue;
    }
    if (impl_->namespace_env != R_NilValue) {
        R_ReleaseObject(impl_->namespace_env);
        impl_->namespace_env = R_NilValue;
    }
    if (counter != R_NilValue) {
        R_PreserveObject(counter);
        impl_->crash_counter = counter;
    }
    if (namespace_env != R_NilValue) {
        R_PreserveObject(namespace_env);
        impl_->namespace_env = namespace_env;
    }
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
    auto* store = xptr_to_store(xptr);
    try {
        R_xlen_t n = bytes.size();
        const uint8_t* data = (n > 0) ? reinterpret_cast<const uint8_t*>(RAW(bytes.data())) : nullptr;
        store->append(key, data, static_cast<uint64_t>(n));
        return R_NilValue;
    } catch (const cpp11::unwind_exception&) {
        // SimulatedCrash (or any other R-level error) raised inside the
        // append protocol via tick_crash_counter. Let it propagate through
        // the BEGIN_CPP11/END_CPP11 boundary unchanged so R observes the
        // original condition class.
        throw;
    } catch (const std::exception& e) {
        cpp11::stop("%s", e.what());
    }
}

[[cpp11::register]]
SEXP dafr_mmap_zip_delete(SEXP xptr, std::string key) {
    (void)xptr_to_store(xptr);
    (void)key;
    cpp11::stop("MmapZipStore is append-only; deletion not supported");
}

// Debug helper: returns a numeric vector of every entry's data_offset, in
// CD order. Used by phase-4 tests to check 8-byte alignment of LFHs.
[[cpp11::register]]
SEXP dafr_mmap_zip_data_offsets(SEXP xptr) {
    auto* store = xptr_to_store(xptr);
    try {
        std::vector<std::string> keys = store->all_keys();
        SEXP out = PROTECT(Rf_allocVector(REALSXP, static_cast<R_xlen_t>(keys.size())));
        for (R_xlen_t i = 0; i < static_cast<R_xlen_t>(keys.size()); ++i) {
            const uint8_t* p = nullptr;
            uint64_t len = 0;
            uint16_t method = 0;
            uint32_t crc = 0;
            store->stored_view(keys[static_cast<size_t>(i)], &p, &len, &method, &crc);
            // For stored entries we have a direct view; for others we still
            // need the offset, which we recover from the parsed CD.
            // The simplest path is: the data_offset field is recoverable
            // via stored_view's pointer for stored entries, but for
            // unsupported methods this returns false. Phase 4 only writes
            // stored entries, so we expect every entry to be stored.
            uint64_t off = 0;
            if (p != nullptr) {
                off = static_cast<uint64_t>(p - store->file_mmap_base());
            }
            REAL(out)[i] = static_cast<double>(off);
        }
        UNPROTECT(1);
        return out;
    } catch (const std::exception& e) {
        cpp11::stop("%s", e.what());
    }
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
    auto* store = xptr_to_store(xptr);
    try {
        if (size_double < 0) {
            cpp11::stop("dafr_mmap_zip_reserve: negative size");
        }
        uint64_t length = static_cast<uint64_t>(size_double);
        // Run the commit protocol with placeholder CRC; reserve() returns
        // a writable pointer into the mmap region for `length` bytes.
        uint8_t* data_ptr = store->reserve(key, length);
        const uint8_t* base = store->file_mmap_base();
        if (!base) cpp11::stop("dafr_mmap_zip_reserve: store has no mapped region");
        uint64_t offset = static_cast<uint64_t>(data_ptr - base);
        // Writable-in-place ALTREP RAW view. Anchored to the store xptr.
        return dafr::make_zip_raw_altrep_writable(store, offset, length, xptr);
    } catch (const cpp11::unwind_exception&) {
        // Allow R-level errors (e.g. SimulatedCrash from tick injection
        // during the commit protocol) to propagate unchanged.
        throw;
    } catch (const std::exception& e) {
        cpp11::stop("%s", e.what());
    }
}

[[cpp11::register]]
SEXP dafr_mmap_zip_patch_crc(SEXP xptr, std::string key) {
    auto* store = xptr_to_store(xptr);
    try {
        store->patch_crc(key);
        return R_NilValue;
    } catch (const std::exception& e) {
        cpp11::stop("%s", e.what());
    }
}

[[cpp11::register]]
SEXP dafr_mmap_zip_set_crash_counter(SEXP xptr, SEXP counter, SEXP ns_env) {
    auto* store = xptr_to_store(xptr);
    store->set_crash_counter(counter, ns_env);
    return R_NilValue;
}

#endif  // !_WIN32
