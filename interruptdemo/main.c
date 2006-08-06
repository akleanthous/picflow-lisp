#include <p18f4520.h>
#include <timers.h>
#include <usart.h>
#include <stdio.h>

void timer0_isr(void);
unsigned char count = 0;

#pragma code low_vector=0x18
void low_interrupt(void) {
	if (INTCONbits.TMR0IF) timer0_isr();
	// For some reason MCC18 refuses to put this here automatically.
	// I don't want to have a mess of retfie's throughout my handler
	// functions, so let's solve the problem once and for all:
	_asm retfie 0 _endasm
}

#pragma code

//#pragma interruptlow timer0_isr
void timer0_isr(void) {
	INTCONbits.TMR0IF = 0;

	LATB = count;
	printf("Tick! %i\n", count);
	count++;
}

void main(void) {
	TRISB = 0;
	PORTB = 0;

	OpenTimer0(TIMER_INT_ON & T0_SOURCE_INT & T0_16BIT & T0_PS_1_32);
	OpenUSART(USART_TX_INT_OFF &
			  USART_RX_INT_OFF &
			  USART_ASYNCH_MODE &
			  USART_EIGHT_BIT &
			  USART_CONT_RX &
			  USART_BRGH_HIGH, 25);

	INTCONbits.GIE = 1;

	while (1);
}
