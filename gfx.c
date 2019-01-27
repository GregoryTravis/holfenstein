#include <stdio.h>
#include "gfx.h"

#define CHECKSIZE 8

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
