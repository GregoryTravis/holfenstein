#include <stdio.h>
#include "gfx.h"

void fastestDrawVStrip(int32_t *start, int32_t count, int32_t step, int32_t color) {
  for (int32_t *end = start + (count * step); start < end; start += step) {
    *start = color;
  }
}
