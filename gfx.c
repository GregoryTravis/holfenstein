#include <stdio.h>
#include "gfx.h"

void gfx(int32_t *p, int a) {
  printf("gfx %p %d!\n", p, a);
}

int32_t *gfx2(int32_t *p, int a) {
  printf("gfx2 %p %p %d!\n", p, p+1, a);
  return p + 1;
}
