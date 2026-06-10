#ifndef DAFR_CRC32C_H
#define DAFR_CRC32C_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
/* Castagnoli CRC-32C (poly 0x1EDC6F41, reflected 0x82F63B78), zlib-style:
   seed 0, returns the finalized checksum for buf[0..len). Used to validate
   Zarr v3 ZEP-0002 shard indices (index_codecs ends with `crc32c`). */
uint32_t dafr_crc32c(const unsigned char *buf, size_t len);
#ifdef __cplusplus
}
#endif
#endif
