#include "mmap_region.h"

#include <cerrno>
#include <cstring>
#include <stdexcept>

#if defined(DAFR_HAVE_MMAP) && DAFR_HAVE_MMAP
  #include <fcntl.h>
  #include <sys/mman.h>
  #include <sys/stat.h>
  #include <unistd.h>
#endif

namespace dafr {

MmapRegion::MmapRegion(void *ptr, std::size_t nbytes, int fd, std::string path)
    : ptr_(ptr), nbytes_(nbytes), fd_(fd), path_(std::move(path)) {}

MmapRegion::~MmapRegion() {
#if defined(DAFR_HAVE_MMAP) && DAFR_HAVE_MMAP
    if (ptr_ != nullptr && ptr_ != MAP_FAILED && nbytes_ > 0) {
        munmap(ptr_, nbytes_);
    }
    if (fd_ >= 0) {
        close(fd_);
    }
#else
    (void) ptr_; (void) fd_;
#endif
}

std::shared_ptr<MmapRegion> MmapRegion::open_readonly(const std::string &path) {
#if !defined(DAFR_HAVE_MMAP) || !DAFR_HAVE_MMAP
    (void) path;
    throw std::runtime_error("mmap not available on this platform");
#else
    int fd = ::open(path.c_str(), O_RDONLY);
    if (fd < 0) {
        throw std::runtime_error(
            "failed to open '" + path + "': " + std::strerror(errno));
    }
    struct stat st;
    if (fstat(fd, &st) != 0) {
        int e = errno;
        ::close(fd);
        throw std::runtime_error(
            "fstat '" + path + "': " + std::strerror(e));
    }
    std::size_t nbytes = static_cast<std::size_t>(st.st_size);
    if (nbytes == 0) {
        // Empty file: can't mmap a zero-length region. Treat as empty.
        ::close(fd);
        return std::make_shared<MmapRegion>(nullptr, 0, -1, path);
    }
    void *p = mmap(nullptr, nbytes, PROT_READ, MAP_SHARED, fd, 0);
    if (p == MAP_FAILED) {
        int e = errno;
        ::close(fd);
        throw std::runtime_error(
            "mmap '" + path + "': " + std::strerror(e));
    }
    return std::make_shared<MmapRegion>(p, nbytes, fd, path);
#endif
}

} // namespace dafr
