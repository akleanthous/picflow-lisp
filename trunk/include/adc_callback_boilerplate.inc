
// Standard boilerplate
PIR1bits.ADIF = 0;
// Get ADC started again. This should be optional, giving people the
// option of setting off the next conversion manually.
ADCON0bits.GO_DONE = 1;