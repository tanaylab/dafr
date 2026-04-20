#ifndef DAFR_MMAP_REGION_H
#define DAFR_MMAP_REGION_H

#include <cstddef>
#include <memory>
#include <string>

#ifdef _WIN32
  #ifndef NOMINMAX
    #define NOMINMAX
  #endif
  #ifndef WIN32_LEAN_AND_MEAN
    #define WIN32_LEAN_AND_MEAN
  #endif
  #include <windows.h>
#endif

namespace dafr {

// RAII wrapper: memory-map a file read-only. POSIX uses mmap + munmap;
// Windows uses CreateFileMappingW + MapViewOfFile + UnmapViewOfFile.
// Not copyable; share via std::shared_ptr from callers.
class MmapRegion {
public:
    // Factory. Throws std::runtime_error on open/map failure, or if the
    // platform lacks a supported mmap implementation (DAFR_HAVE_MMAP=0).
    static std::shared_ptr<MmapRegion> open_readonly(const std::string &path);

#ifdef _WIN32
    MmapRegion(void *ptr, std::size_t nbytes,
               HANDLE file, HANDLE mapping, std::string path);
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
    HANDLE file_handle_;
    HANDLE mapping_handle_;
#else
    int fd_;
#endif
    std::string path_;
};

} // namespace dafr

#endif
