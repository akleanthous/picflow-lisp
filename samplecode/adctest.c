#include <p18f4431.h>
#include <delays.h>

void high_isr(void);
void low_isr(void);
void adc_complete_callback(void);

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

void main(void) {
	// Set up clock at 8 MHz
	OSCCON = 0b01111110;

	TRISE = 0x07; // RE0/1/2 are inputs
	ADCON0=0b00010101; // ADC set in multiple channel sequential mode2(SEQM2)
	ADCON1=0b00010000; // FIFO buffer enabled
	ADCON2=0b10110010; // Right justified result,12 TAD acquisition time, ADC clock = FOsc/32
	ADCON3=0b10000000; // Interrupt generated when 4th word is written to buffer
	ADCHS =0b01000100; // AN0,AN1,AN6,AN7 are selected for conversion  
	ANSEL0=0b11000011; // AN0,AN1,AN6,AN7 are selected as analog inputs  
	ANSEL1=0b00000000; // All other inputs set to digital

	TRISB = 0; // PORTB should be outputs
	LATB = 0b1111; // Set all output pins on PORTB low

	// Enable peripheral and global interrupts. What am I getting myself into?
	INTCONbits.PEIE = 1;
	INTCONbits.GIE = 1;

	// Enable required interrupts
	PIE1bits.ADIE = 1; // ADC conversion interrupt

	while (1) {
		ADCON0bits.GO = 1;
		Delay10KTCYx(30);
	}
}

#pragma interrupt high_isr
void high_isr(void) {
	if (PIR1bits.ADIF)
		adc_complete_callback();
	// Ignore all other interrupts
}

volatile unsigned char a;
volatile unsigned char b;
volatile unsigned char c;
volatile unsigned char d;
volatile unsigned char e;
volatile unsigned char f;
volatile unsigned char g;
volatile unsigned char h;

// A macro to glue together two bytes into a 10-bit unsigned integer
#define TENBITS(H,L) ((((unsigned int)H) << 8) | L)

// Called when an analog to digital conversion is completed
void adc_complete_callback(void) {
	unsigned int x; // bit bucket

//	LATB = (ADRESL >> 4);
	// Throw everything else away
//	b = ADRESH;
	a = ADRESL;
	b = ADRESH;
	LATB = (a >> 4);
	x = TENBITS(b,a);

	c = ADRESL;
	d = ADRESH;
	e = ADRESL;
	f = ADRESH;
	g = ADRESL;
	h = ADRESH;

	PIR1bits.ADIF = 0;
}
