#include <p18f4520.h>

#include "picflow_adc.h"

void interruptHandlerLow(void);

// Three nodes to process ADC results
void adc1(unsigned char adresx) { LATB = adresx; }
void adc2(unsigned char adresx) { LATB = ~adresx; }
void adc3(unsigned char adresx) { LATB = adresx ^ 0x42; }

void adc_callback(void) {
  // State variable controls which output is called
  static unsigned char adc_output_state = 0;

  // Do something with the result and switch to next state. Yes, we're
  // a finite-state machine!
  switch (adc_output_state) {
  case 0: // AN0
    adc1(ADRESL);
    adc_output_state++;
    setADC1();
    break;
  case 1: // AN1
    adc2(ADRESL);
    adc_output_state++;
    setADC2();
    break;
  case 2: // AN2
    adc3(ADRESL);
    adc_output_state = 0;
    setADC0();
    break;
  }

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

  // Set up the ADC
  setup_common_adc();
  setADC0();

  // Enable peripheral and global interrupts.
  INTCONbits.PEIE = 1;
  INTCONbits.GIE = 1;

  PIR1bits.ADIF = 0; // Clear ADC interrupt flag
  PIE1bits.ADIE = 1; // ADC conversion interrupt
  ADCON0bits.GO_DONE = 1; // Get ADC started

  while (1);
}
