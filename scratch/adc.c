#include <p18f4520.h>

#include "picflow_adc.h"

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
