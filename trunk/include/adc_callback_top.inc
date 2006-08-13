// The analog result in ten bits
overlay unsigned int adres_tenbits = TENBITS(ADRESH, ADRESL);

// State variable controls which output is called
static unsigned char adc_output_state = 0;

// Do something with the result and switch to next state. Yes, we're
// a finite-state machine!
switch (adc_output_state) {