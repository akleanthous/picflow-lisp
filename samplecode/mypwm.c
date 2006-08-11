#include "mypwm.h"

void setupPWM() {
	// Set up Power PWM timer
	PTCON0 = 0b00000000;
	PTCON1 = 0b10000000;

	// All PWM outputs enabled and independant of each other
	PWMCON0 = 0b01001111;
	PWMCON1 = 0b00000001;

	// Do NOT touch these numbers! Ever!
	PTPERL = 0xFF;
	PTPERH = 0x00;

	// A/D setup - not done
	TRISA = 0b00000000;
}

void setPWMDC0(unsigned int x) {
	PDC0L = x & 0xFF;
	PDC0H = x >> 8;
}

void setPWMDC1(unsigned int x) {
	PDC1L = x & 0xFF;
	PDC1H = x >> 8;
}

void setPWMDC2(unsigned int x) {
	PDC2L = x & 0xFF;
	PDC2H = x >> 8;
}

void setPWMDC3(unsigned int x) {
	PDC3L = x & 0xFF;
	PDC3H = x >> 8;
}
