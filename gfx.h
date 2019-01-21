#include <stdint.h>
void fastestFillVStrip(int32_t *start, int32_t count, int32_t step, int32_t color);
int32_t sampler(int32_t x, int32_t y);
void fastestTextureVStrip(int32_t *start, int32_t *texPtr, int texWid, int texHt, int dPtr, int tx, int cy0, int cy1, double fty0, double dfty);
