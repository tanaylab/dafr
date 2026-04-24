#include "mmap_region.h"

#include <cerrno>
#include <cstring>
#include <stdexcept>

#if defined(DAFR_HAVE_MMAP) && DAFR_HAVE_MMAP
  #ifdef _WIN32
    // windows.h is confined to this translation unit so its TRUE/FALSE
    // macros don't leak into other sources (they clash with R's Rboolean).
    #ifndef NOMINMAX
      #define NOMINMAX
    #endif
    #ifndef WIN32_LEAN_AND_MEAN
      #define WIN32_LEAN_AND_MEAN
    #endif
    #include <windows.h>
  #else
    #include <fcntl.h>
    #include <sys/mman.h>
    #include <sys/stat.h>
    #include <unistd.h>
  #endif
#endif

namespace dafr {

#ifdef _WIN32

namespace {

// Convert a UTF-8 path string to a UTF-16 wide string suitable for the
// W variant of the Win32 file APIs. Returns empty on conversion failure
// (the caller distinguishes the error via GetLastError after a failed
// conversion attempt).
std::wstring utf8_to_utf16(const std::string &s) {
    if (s.empty()) return std::wstring();
    int wlen = MultiByteToWideChar(CP_UTF8, 0, s.c_str(), -1, nullptr, 0);
    if (wlen <= 0) return std::wstring();
    std::wstring wbuf(static_cast<std::size_t>(wlen) - 1, L'\0');  // exclude NUL
    MultiByteToWideChar(CP_UTF8, 0, s.c_str(), -1, wbuf.data(), wlen);
    return wbuf;
}

std::string win_error_message(const std::string &prefix, DWORD code) {
    // Keep the message simple; users can look the code up. Including the
    // raw DWORD is sufficient for diagnostic purposes and avoids bringing
    // in FormatMessage's localization quirks.
    char buf[32];
    std::snprintf(buf, sizeof(buf), " (error 0x%08lX)",
                  static_cast<unsigned long>(code));
    return prefix + buf;
}

} // namespace

MmapRegion::MmapRegion(void *ptr, std::size_t nbytes,
                       void *file_handle, void *mapping_handle,
                       std::string path)
    : ptr_(ptr), nbytes_(nbytes),
      file_handle_(file_handle), mapping_handle_(mapping_handle),
      path_(std::move(path)) {}

MmapRegion::~MmapRegion() {
    if (ptr_ != nullptr && nbytes_ > 0) {
        UnmapViewOfFile(ptr_);
    }
    if (mapping_handle_ != nullptr) {
        CloseHandle(static_cast<HANDLE>(mapping_handle_));
    }
    if (file_handle_ != nullptr && file_handle_ != INVALID_HANDLE_VALUE) {
        CloseHandle(static_cast<HANDLE>(file_handle_));
    }
}

std::shared_ptr<MmapRegion> MmapRegion::open_readonly(const std::string &path) {
    std::wstring wpath = utf8_to_utf16(path);
    if (wpath.empty() && !path.empty()) {
        throw std::runtime_error(win_error_message(
            "failed to convert path to UTF-16 '" + path + "'", GetLastError()));
    }

    // Allow other processes read, write, and delete access while we hold
    // this mapping. We only need PROT_READ semantics ourselves; the
    // generous share flags match the relaxed semantics of POSIX MAP_SHARED.
    HANDLE h_file = CreateFileW(
        wpath.c_str(),
        GENERIC_READ,
        FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
        nullptr,
        OPEN_EXISTING,
        FILE_ATTRIBUTE_NORMAL,
        nullptr);
    if (h_file == INVALID_HANDLE_VALUE) {
        throw std::runtime_error(win_error_message(
            "failed to open '" + path + "'", GetLastError()));
    }

    LARGE_INTEGER size;
    if (!GetFileSizeEx(h_file, &size)) {
        DWORD code = GetLastError();
        CloseHandle(h_file);
        throw std::runtime_error(win_error_message(
            "GetFileSizeEx '" + path + "'", code));
    }

    std::size_t nbytes = static_cast<std::size_t>(size.QuadPart);
    if (nbytes == 0) {
        // CreateFileMapping rejects zero-size mappings. Mirror the POSIX
        // path: return a nullptr region that represents an empty file.
        CloseHandle(h_file);
        return std::make_shared<MmapRegion>(nullptr, 0, INVALID_HANDLE_VALUE,
                                            nullptr, path);
    }

    HANDLE h_map = CreateFileMappingW(
        h_file,
        nullptr,
        PAGE_READONLY,
        0, 0,            // entire file
        nullptr);
    if (h_map == nullptr) {
        DWORD code = GetLastError();
        CloseHandle(h_file);
        throw std::runtime_error(win_error_message(
            "CreateFileMapping '" + path + "'", code));
    }

    void *ptr = MapViewOfFile(h_map, FILE_MAP_READ, 0, 0, 0);
    if (ptr == nullptr) {
        DWORD code = GetLastError();
        CloseHandle(h_map);
        CloseHandle(h_file);
        throw std::runtime_error(win_error_message(
            "MapViewOfFile '" + path + "'", code));
    }

    return std::make_shared<MmapRegion>(ptr, nbytes, h_file, h_map, path);
}

#else // !_WIN32

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

#endif // _WIN32

} // namespace dafr
