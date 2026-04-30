#ifndef DAFR_MMAP_ZIP_STORE_H
#define DAFR_MMAP_ZIP_STORE_H

#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#define R_NO_REMAP
#include <Rinternals.h>

namespace dafr {

constexpr uint32_t LOCAL_FILE_HEADER_SIGNATURE                     = 0x04034b50u;
constexpr uint32_t CENTRAL_DIRECTORY_ENTRY_SIGNATURE               = 0x02014b50u;
constexpr uint32_t END_OF_CENTRAL_DIRECTORY_SIGNATURE              = 0x06054b50u;
constexpr uint32_t ZIP64_END_OF_CENTRAL_DIRECTORY_SIGNATURE        = 0x06064b50u;
constexpr uint32_t ZIP64_END_OF_CENTRAL_DIRECTORY_LOCATOR_SIGNATURE = 0x07064b50u;

constexpr uint32_t END_OF_CENTRAL_DIRECTORY_SIZE                   = 22u;
constexpr uint32_t ZIP64_END_OF_CENTRAL_DIRECTORY_SIZE             = 56u;
constexpr uint32_t ZIP64_END_OF_CENTRAL_DIRECTORY_LOCATOR_SIZE     = 20u;
constexpr uint32_t TRAILING_END_OF_CENTRAL_DIRECTORY_REGION_SIZE   =
    ZIP64_END_OF_CENTRAL_DIRECTORY_SIZE
    + ZIP64_END_OF_CENTRAL_DIRECTORY_LOCATOR_SIZE
    + END_OF_CENTRAL_DIRECTORY_SIZE;

constexpr uint32_t LOCAL_FILE_HEADER_FIXED_SIZE       = 30u;
constexpr uint32_t CENTRAL_DIRECTORY_ENTRY_FIXED_SIZE = 46u;
constexpr uint16_t ZIP64_EXTRA_HEADER_ID              = 0x0001u;
constexpr uint32_t ZIP64_LOCAL_FILE_HEADER_EXTRA_SIZE = 20u;
constexpr uint32_t ZIP64_CENTRAL_DIRECTORY_EXTRA_SIZE = 28u;
constexpr uint16_t STORED_COMPRESSION_METHOD          = 0u;
constexpr uint16_t DEFLATE_COMPRESSION_METHOD         = 8u;
constexpr uint16_t DEFLATE64_COMPRESSION_METHOD       = 9u;
constexpr uint16_t ZIP64_VERSION_NEEDED               = 45u;
constexpr uint16_t DAF_PADDING_EXTRA_HEADER_ID        = 0xDAF1u;
constexpr uint32_t DAF_DATA_OFFSET_ALIGNMENT          = 8u;

constexpr uint64_t DEFAULT_MAX_FILE_SIZE              = uint64_t{1} << 40;

struct ZipEntry {
    std::string name;
    uint64_t local_file_header_offset;
    uint64_t data_offset;
    uint64_t compressed_size;
    uint64_t uncompressed_size;
    uint32_t crc32;
    uint16_t compression_method;
    uint64_t central_directory_offset;
};

struct AltrepRawSlot {
    uint64_t offset;
    uint64_t length;
    bool deactivated;
    // Phase 7 (reserve): when true, Dataptr(writeable=TRUE) returns the
    // mmap pointer directly (cast away const) instead of materializing
    // a private copy. Callers using this contract MUST patch the entry's
    // CRC before reopening the store, otherwise recovery will roll the
    // entry back. Used only for vectors returned from `reserve`.
    bool writable_inplace;
};

class MmapZipStoreImpl;

class MmapZipStore {
public:
    static std::unique_ptr<MmapZipStore> open(const std::string& path,
                                              bool writable, bool create, bool truncate,
                                              uint64_t max_file_size);
    ~MmapZipStore();
    MmapZipStore(const MmapZipStore&) = delete;
    MmapZipStore& operator=(const MmapZipStore&) = delete;

    bool has(const std::string& key) const;
    bool stored_view(const std::string& key,
                     const uint8_t** out_ptr,
                     uint64_t* out_length,
                     uint16_t* out_method,
                     uint32_t* out_crc) const;
    std::vector<uint8_t> read_decompressed(const std::string& key) const;
    std::vector<std::string> list_with_prefix(const std::string& prefix) const;
    std::vector<std::string> all_keys() const;

    void append(const std::string& key, const uint8_t* data, uint64_t length);
    uint8_t* reserve(const std::string& key, uint64_t length);
    void patch_crc(const std::string& key);

    void close_store();
    bool is_writable() const;
    const std::string& path() const;
    const uint8_t* file_mmap_base() const;
    uint64_t overlay_length() const;

    std::shared_ptr<AltrepRawSlot> register_altrep_slot(uint64_t offset, uint64_t length);
    // Phase 7: register a slot whose ALTREP Dataptr(writeable=TRUE) returns
    // the mmap pointer in place (no materialization). Used for the buffer
    // returned by `reserve` so the caller can fill the data region in place.
    std::shared_ptr<AltrepRawSlot> register_altrep_slot_writable(uint64_t offset,
                                                                 uint64_t length);

    void set_crash_counter(SEXP counter, SEXP namespace_env);

private:
    explicit MmapZipStore(MmapZipStoreImpl* impl);
    MmapZipStoreImpl* impl_;
};

void init_altrep_zip_raw(DllInfo* dll);
SEXP make_zip_raw_altrep(MmapZipStore* store, uint64_t offset, uint64_t length);
// Variant that anchors the ALTREP's lifetime to the store's R-level xptr,
// so R's GC won't free the impl while the ALTREP is alive.
SEXP make_zip_raw_altrep_with_xptr(MmapZipStore* store, uint64_t offset,
                                   uint64_t length, SEXP store_xptr);
// Phase 7: writable-in-place variant. Dataptr(writeable=TRUE) returns the
// mmap pointer directly (cast away const). Caller must run patch_crc on
// the entry before reopening the store.
SEXP make_zip_raw_altrep_writable(MmapZipStore* store, uint64_t offset,
                                  uint64_t length, SEXP store_xptr);

}  // namespace dafr

#endif
