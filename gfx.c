#include <stdio.h>
#include "gfx.h"

void fastestDrawVStrip(int32_t *start, int32_t count, int32_t step, int32_t color) {
  for (int i = 0; i < count; ++i) {
    int32_t *p = start + (i * step);
    *p = color;
  }
}
