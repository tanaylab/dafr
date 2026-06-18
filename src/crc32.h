#ifndef DAFR_CRC32_H
#define DAFR_CRC32_H
#include <stddef.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
/* Standard zlib CRC-32 (poly 0x04C11DB7, reflected 0xEDB88320), seed 0,
   finalized (~crc). Used for ZIP local-file-header / central-directory entry
   CRCs in dual-format packed shards (distinct from the crc32c shard index). */
uint32_t dafr_crc32(const unsigned char *buf, size_t len);
#ifdef __cplusplus
}
#endif
#endif
