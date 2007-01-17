setup_common_adc(); // Set up ADC registers
PIR1bits.ADIF = 0; // Clear ADC interrupt flag
PIE1bits.ADIE = 1; // ADC conversion interrupt
ADCON0bits.GO_DONE = 1; // Get ADC started
