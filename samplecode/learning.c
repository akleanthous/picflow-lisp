// This is source code! RAR!

#include <p18f4520.h>
#include <stdio.h>
#include <delays.h>

void main() {
	// Light up the LEDS on port B
	TRISD = 0;

//	LATD = 1;
	LATD = 0b1001;

	while (1) {
		Delay10KTCYx(10);
		LATD ^= 0xFF; // Invert bit pattern
//		LATD *= 2;
//		if (LATD > 0x0F) LATD = 1;
	}
}
