#include <p18f4520.h>

#define ADCON0_BASE 0b00000001

// ADCON1 value which makes AN0 through AN7 analog inputs
#define ADCON1_AN0_THROUGH_AN7_ANALOG 0b00000111

// A default value for ADCON2. FIXME: make this customizable
#define ADCON2_DUMB_DEFAULT_VALUE 0b10000100

// These flags are to be OR'd with ADCON0_BASE to produce values for
// ADCON0. This makes things a little easier to code.
#define ADCON0_AN0  0b00000000
#define ADCON0_AN1  0b00000100
#define ADCON0_AN2  0b00001000
#define ADCON0_AN3  0b00001100
#define ADCON0_AN4  0b00010000
#define ADCON0_AN5  0b00010100
#define ADCON0_AN6  0b00011000
#define ADCON0_AN7  0b00011100

// A macro to glue together two bytes into a 10-bit unsigned integer
#define TENBITS(H,L) ((((unsigned int)H) << 8) | L)

void setupADC0() {
  ADCON0 = ADCON0_BASE | ADCON0_AN0;
  ADCON1 = ADCON1_AN0_THROUGH_AN7_ANALOG;
  ADCON2 = ADCON2_DUMB_DEFAULT_VALUE;
}

void setupADC1() {
  ADCON0 = ADCON0_BASE | ADCON0_AN1;
  ADCON1 = ADCON1_AN0_THROUGH_AN7_ANALOG;
  ADCON2 = ADCON2_DUMB_DEFAULT_VALUE;
}

void adc_callback(void) {
  LATB = ADRESL; // Do something with result

  // BOILERPLATE
  PIR1bits.ADIF = 0;
  // Get ADC started again. This should be optional, giving people the
  // option of setting off the next conversion manually.
  ADCON0bits.GO_DONE = 1;
}

#pragma code low_vector=0x18
void low_interrupt(void) {
  _asm goto interruptHandlerLow _endasm;
}

#pragma code
#pragma interrupt interruptHandlerLow
void interruptHandlerLow(void) {
  if (PIR1bits.ADIF) adc_callback();
}

void main(void) {
  TRISB = 0;
  LATB = 0;

  // Enable peripheral and global interrupts.
  INTCONbits.PEIE = 1;
  INTCONbits.GIE = 1;

  PIR1bits.ADIF = 0; // Clear ADC interrupt flag
  PIE1bits.ADIE = 1; // ADC conversion interrupt
  ADCON0bits.GO_DONE = 1; // Get ADC started

  while (1);
}
