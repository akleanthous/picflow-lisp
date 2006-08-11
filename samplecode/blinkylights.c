#include <p18f4520.h>
#include <delays.h>

//#include "lspadc.h"

#define shortDelay() Delay10KTCYx(10)

void setupADC0() {
	ADCON0 = 0b00000001;
	ADCON1 = 0b00001101;
	ADCON2 = 0b10000100;
}

void setupADC1() {
	ADCON0 = 0b00000101;
	ADCON1 = 0b00001101;
	ADCON2 = 0b10000100;
}

#define highbyte(x) ((x & 0xFF00) >> 8);
#define lowbyte(x) (x & 0xFF);

unsigned int performADC() {
	ADCON0bits.GO_DONE = 1;
	while (ADCON0bits.GO_DONE); // Wait for ADC to finish
	return ((unsigned int)ADRESH << 8) | ADRESL;
}

void main() {
	unsigned char x;

	TRISB = 0;
	LATB = 0b1010;

/*	while (1) {
		setupADC1();
		LATD = (performADC() >> 6) & 0xFF;
		Delay10KTCYx(1);
	}*/
	

	while (1)
		for (x = 0; x <= (unsigned char)0b1111; x++) {
			LATB = x;
			shortDelay();
		}

}
