#ifndef __LSPADC_H
#define __LSPADC_H

#include <p18f4431.h>

#define highbyte(x) ((x & 0xFF00) >> 8);
#define lowbyte(x) (x & 0xFF);

void setupADC0(void);
void setupADC1(void);
void setupADC2(void);
void setupADC3(void);
unsigned int performADC(void);

#endif
