#ifndef __MYPWM_H
#define __MYPWM_H

#include <p18f4431.h>

void setupPWM(void);
void setPWMDC0(unsigned int);
void setPWMDC1(unsigned int);
void setPWMDC2(unsigned int);
void setPWMDC3(unsigned int);

#endif
