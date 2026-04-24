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
    static std::shared_ptr<MmapRegion> open_readonly(const std::string &path);

#ifdef _WIN32
    MmapRegion(void *ptr, std::size_t nbytes,
               void *file_handle, void *mapping_handle, std::string path);
#else
    MmapRegion(void *ptr, std::size_t nbytes, int fd, std::string path);
#endif
    ~MmapRegion();

    MmapRegion(const MmapRegion&) = delete;
    MmapRegion& operator=(const MmapRegion&) = delete;

    const void* data() const { return ptr_; }
    std::size_t nbytes() const { return nbytes_; }
    const std::string& path() const { return path_; }

private:
    void *ptr_;
    std::size_t nbytes_;
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
