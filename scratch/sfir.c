/* Simple FIR filter */

#include <stdio.h>

int sfir(int x) {
  static int a, b=0, c=0;

  /* Shuffle values around */
  a = b; b = c; c = x;

  return (a*1) + (b*2) + (c*3);
}

int main() {
  // prints "18 27 31 24 26"
  printf("%i ", sfir(6));
  printf("%i ", sfir(5));
  printf("%i ", sfir(5));
  printf("%i ", sfir(3));
  printf("%i ", sfir(5));

  printf("\n");
  return 0;
}
  
