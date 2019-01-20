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
