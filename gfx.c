#include <stdio.h>
#include "gfx.h"

void gfx(int32_t *p, int a) {
  //printf("gfx %p %d!\n", p, a);
  *(p + 1200) = 0xffffffff;
}

int32_t *gfx2(int32_t *p, int a) {
  //printf("gfx2 %p %p %d!\n", p, p+1, a);
  return p + 1;
}

void fastestDrawVStrip(int32_t *start, int32_t count, int32_t step, int32_t color) {
  //printf("hi %d\n", count);
  for (int i = 0; i < count; ++i) {
    int32_t *p = start + (i * step);
    //printf("W %d %p %d\n", i, p, color);
    *p = color;
  }
}
