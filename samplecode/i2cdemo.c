#include <p18f4520.h>
#include <i2c.h>
#include <delays.h>

void main() {
	unsigned char wordaddr;

	OSCCON = 0x6B;
	TRISB = 0;
	SSPADD  = 0xA0;  // This device's address, in slave mode

	// Configure I2C for slave mode with a 7-bit address, running at 400 kHz
	OpenI2C(SLAVE_7, SLEW_ON);

	while (1) {
		while (!PIR1bits.SSPIF);
		PIR1bits.SSPIF = 0;
		wordaddr = ReadI2C();
		AckI2C();
		while (!PIR1bits.SSPIF);
		WriteI2C(wordaddr);
	}
}
