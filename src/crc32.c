#include "crc32.h"

static uint32_t TBL[256];
static int READY = 0;

static void init_table(void) {
    uint32_t n, c;
    int k;
    for (n = 0; n < 256; n++) {
        c = n;
        for (k = 0; k < 8; k++)
            c = (c & 1) ? (0xEDB88320u ^ (c >> 1)) : (c >> 1);
        TBL[n] = c;
    }
    READY = 1;
}

uint32_t dafr_crc32(const unsigned char *buf, size_t len) {
    uint32_t crc;
    size_t i;
    if (!READY) init_table();
    crc = 0xFFFFFFFFu;
    for (i = 0; i < len; i++)
        crc = TBL[(crc ^ buf[i]) & 0xFFu] ^ (crc >> 8);
    return crc ^ 0xFFFFFFFFu;
}
