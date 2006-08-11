#ifndef __PICFLOW_ADC_H
#define __PICFLOW_ADC_H

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

#endif
