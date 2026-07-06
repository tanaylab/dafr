#ifndef DAFR_MMAP_REGION_H
#define DAFR_MMAP_REGION_H

#include <cstddef>
#include <memory>
#include <string>

namespace dafr {

// RAII wrapper: memory-map a file read-only. POSIX uses mmap + munmap;
// Windows uses CreateFileMappingW + MapViewOfFile + UnmapViewOfFile.
// Not copyable; share via std::shared_ptr from callers.
//
// The header intentionally does NOT include <windows.h>: that macro-
// pollutes TRUE/FALSE and breaks Rboolean conversions in the rest of
// the package. Windows handles (HANDLE) are just typedef'd void*, so
// we store them as void* here and reinterpret in the implementation.
class MmapRegion {
public:
    // Factory. Throws std::runtime_error on open/map failure, or if the
    // platform lacks a supported mmap implementation (DAFR_HAVE_MMAP=0).
    //
    // `offset` shifts the exposed view forward by that many bytes: the whole
    // file is still mapped from 0 (so the mapping stays page-aligned), but
    // data() returns base + offset and nbytes() the bytes from offset to EOF.
    // This lets a sub-region of a file (e.g. an HDF5 contiguous dataset at a
    // known byte offset) be mmap-backed. Throws if offset > file size.
    static std::shared_ptr<MmapRegion> open_readonly(const std::string &path,
                                                     std::size_t offset = 0);

#ifdef _WIN32
    MmapRegion(void *ptr, std::size_t nbytes,
               void *file_handle, void *mapping_handle, std::string path);
#else
    MmapRegion(void *ptr, std::size_t nbytes, int fd, std::string path);
#endif
    ~MmapRegion();

    MmapRegion(const MmapRegion&) = delete;
    MmapRegion& operator=(const MmapRegion&) = delete;

    // Both account for the view offset (see open_readonly). The destructor
    // still unmaps the full [ptr_, nbytes_] region, so it is unaffected.
    const void* data() const { return static_cast<const char*>(ptr_) + offset_; }
    std::size_t nbytes() const { return nbytes_ - offset_; }
    const std::string& path() const { return path_; }

private:
    void *ptr_;
    std::size_t nbytes_;
    std::size_t offset_ = 0;
#ifdef _WIN32
    void *file_handle_;     // HANDLE
    void *mapping_handle_;  // HANDLE
#else
    int fd_;
#endif
    std::string path_;
};

} // namespace dafr

#endif
