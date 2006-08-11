/* This program controls multiple PWM outputs based on analog inputs between
  0V and 5V, with 0V being full off duty cycle and 5V being full off. */

#include <p18f4431.h>
#include <delays.h>
#include "mypwm.h"

void high_isr(void);
void low_isr(void);
void adc_complete_callback(void);

void main() {
	unsigned int x;

	// Set up clock at 8 MHz
	OSCCON = 0b01111110;

	TRISC = 0;
	// Use RB7 as a power-on LED
	TRISB = 0;
	LATBbits.LATB7 = 1;

	setupPWM();

	// Magical ADC configuration
	TRISE = 0x07; // RE0/1/2 are inputs
	ADCON0=0b00010101; // ADC set in multiple channel sequential mode2(SEQM2)
	ADCON1=0b00010000; // FIFO buffer enabled
	ADCON2=0b10110010; // Right justified result,12 TAD acquisition time, ADC clock = FOsc/32
	ADCON3=0b10000000; // Interrupt generated when 4th word is written to buffer
	ADCHS =0b01000100; // AN0,AN1,AN6,AN7 are selected for conversion  
	ANSEL0=0b11000011; // AN0,AN1,AN6,AN7 are selected as analog inputs  
	ANSEL1=0b00000000; // All other inputs set to digital

	// Enable peripheral and global interrupts. Dear God, no!
	INTCONbits.PEIE = 1;
	INTCONbits.GIE = 1;

	// Enable required interrupts
	PIE1bits.ADIE = 1; // ADC conversion interrupt
	
	while (1) {
/*		for (x=0; x <= 0b1111111111; x++) {
			setPWMDC0(x);
			setPWMDC1(x);
			setPWMDC2(x);
			setPWMDC3(x);
			Delay10KTCYx(5);
		}*/
		ADCON0bits.GO = 1;
		Delay10KTCYx(10);
	}
}

// SCARY EVIL INTERRUPT STUFF! RUN AWAY!
////////////////////////////////////////

#pragma code low_vector=0x18
void interrupt_at_low_vector(void) {
	_asm GOTO low_isr _endasm
}
#pragma code

#pragma interruptlow low_isr
void low_isr(void) {
	// Nothing here.
}

#pragma code high_vector=0x08
void interrupt_at_high_vector(void) {
	_asm GOTO high_isr _endasm
}
#pragma code

#pragma interrupt high_isr
void high_isr(void) {
	if (PIR1bits.ADIF)
		adc_complete_callback();
	// Ignore all other interrupts
}

// A macro to glue together two bytes into a 10-bit unsigned integer
#define TENBITS(H,L) ((((unsigned int)H) << 8) | L)

// Called when an analog to digital conversion is completed
void adc_complete_callback(void) {
	unsigned char high, low;

	high = ADRESH;
	low = ADRESL;
	setPWMDC0(TENBITS(high, low));

	high = ADRESH;
	low = ADRESL;
	setPWMDC1(TENBITS(high, low));

	high = ADRESH;
	low = ADRESL;
	setPWMDC2(TENBITS(high, low));

	high = ADRESH;
	low = ADRESL;
	setPWMDC3(TENBITS(high, low));

	// Clear interrupt flag so this code can be executed again
	PIR1bits.ADIF = 0;
}
