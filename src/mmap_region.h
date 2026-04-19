#ifndef DAFR_MMAP_REGION_H
#define DAFR_MMAP_REGION_H

#include <cstddef>
#include <memory>
#include <string>

namespace dafr {

// RAII wrapper: mmap a file read-only into memory, release via munmap on
// destruction. Not copyable; share via std::shared_ptr from callers.
class MmapRegion {
public:
    // Factory. Throws std::runtime_error on open/fstat/mmap failure, or if
    // the platform lacks mmap (DAFR_HAVE_MMAP=0).
    static std::shared_ptr<MmapRegion> open_readonly(const std::string &path);

    MmapRegion(void *ptr, std::size_t nbytes, int fd, std::string path);
    ~MmapRegion();

    MmapRegion(const MmapRegion&) = delete;
    MmapRegion& operator=(const MmapRegion&) = delete;

    const void* data() const { return ptr_; }
    std::size_t nbytes() const { return nbytes_; }
    const std::string& path() const { return path_; }

private:
    void *ptr_;
    std::size_t nbytes_;
    int fd_;
    std::string path_;
};

} // namespace dafr

#endif
