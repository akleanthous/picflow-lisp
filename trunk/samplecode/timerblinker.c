#include <p18f4520.h>
#include <timers.h>
#include <delays.h>

void main() {
	char x = 15;

	TRISB = 0; //sets port b to output
	PORTB = 15; //makes port b show 15 in binary
	OSCCON = 0x6B; // Sets internal timer to 4 MHz

	Delay10KTCYx(50); //creates a delay of 50 *10k clock cycles

	OpenTimer0(TIMER_INT_OFF & T0_16BIT & T0_SOURCE_INT &
			   T0_PS_1_64); //configures our timer
	WriteTimer0(0); //restarts timer at 0

	while (1) {
		while (ReadTimer0() < 15625); //ReadTimer gets value from timer
		x = x==0 ? 15 : 0; //switches x between 15 and 0
		PORTB = x; //changes port b to x
		WriteTimer0(0);
	}
}
