#include <stdio.h>
#include "gfx.h"

void fastestFillVStrip(int32_t *start, int32_t count, int32_t step, int32_t color) {
  for (int32_t *end = start + (count * step); start < end; start += step) {
    *start = color;
  }
}

#define CHECKSIZE 8

int32_t sampler(int32_t x, int32_t y) {
  //return ((x / CHECKSIZE) % 2) == ((y / CHECKSIZE) % 2) ? 0xff0000ff : 0x00ff00ff;
  int check = ((x / CHECKSIZE) % 2) == ((y / CHECKSIZE) % 2) ? 0xff : 0x7f;
  return ((((x<<1)+0x80) & check) << 24) | (((((63-x)<<1)+0x80) & check) << 16) | ((((y<<1)+0x80) & check) << 8) | 0xff;
}

void fastestTextureVStrip(int32_t *start, int32_t *texPtr, int texWid, int texHt, int dPtr, int tx, int cy0, int cy1, double fty0, double dfty) {
  int32_t *p = start;
  double fty = fty0;

  if (tx < 0) {
    tx = 0;
  } else if (tx >= texWid) {
    tx = texWid-1;
  }

  for (int cy = cy0; cy <= cy1; cy++) {
    int ty = (int)fty;

    if (ty < 0) {
      ty = 0;
    } else if (ty >= texHt) {
      ty = texHt-1;
    }

    *p = *(texPtr + tx + (ty * texWid));
    p += dPtr;
    fty += dfty;
  }
}
