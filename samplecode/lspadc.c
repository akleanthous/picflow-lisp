/* This file contains library functions for doing analog to
digital conversions in a simple straightforward way. */

#include "lspadc.h"

/* Set up ADC on AN0, with all other pins of PORT[AE]
   set to digital inputs. */
void setupADC0() {
	TRISA = TRISE = 0xFF;
	ADCON0 = 0b00000001;
	ADCON1 = 0b00001000;
	ADCON2 = 0b10011011;
	ADCON3 = 0;
	ADCHS  = 0b00000000; // Select AN0, 1, 2, and 3.
	ANSEL0 = 0b00000001;
	ANSEL1 = 0;
}

/* Set up ADC on AN1, with all other pins of PORT[AE]
   set to digital inputs. */
void setupADC1() {
	TRISA = TRISE = 0xFF;
	ADCON0 = 0b00000101;
	ADCON1 = 0b00001000;
	ADCON2 = 0b10011011;
	ADCON3 = 0;
	ADCHS  = 0b00000000; // Select AN0, 1, 2, and 3.
	ANSEL0 = 0b00000010;
	ANSEL1 = 0;
}

/* Set up ADC on AN2, with all other pins of PORT[AE]
   set to digital inputs. */
void setupADC2() {
	TRISA = TRISE = 0xFF;
	ADCON0 = 0b00001001;
	ADCON1 = 0b00001000;
	ADCON2 = 0b10011011;
	ADCON3 = 0;
	ADCHS  = 0b00000000; // Select AN0, 1, 2, and 3.
	ANSEL0 = 0b00000100;
	ANSEL1 = 0;
}

/* Set up ADC on AN3, with all other pins of PORT[AE]
   set to digital inputs. */
void setupADC3() {
	TRISA = TRISE = 0xFF;
	ADCON0 = 0b00001101;
	ADCON1 = 0b00001000;
	ADCON2 = 0b10011011;
	ADCON3 = 0;
	ADCHS  = 0b00000000; // Select AN0, 1, 2, and 3.
	ANSEL0 = 0b00001000;
	ANSEL1 = 0;
}

/* Perform an ADC and return ADRESH:ADRESL as an unsigned int.
   You can get at the individual components using the highbyte()
   and lowbyte() macros or by accessing ADRESx directly. */
unsigned int performADC() {
	ADCON0bits.GO = 1; // Start ADC
	while (ADCON0bits.GO);  // Wait for ADC to finish
	
	return (((unsigned int)ADRESH) << 8) | ADRESL;
}
