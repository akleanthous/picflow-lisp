// The low-priority interrupt vector will be used for the ordinary
// operation of the program, leaving the high-priority interrupt
// vector free for more urgent things.
#pragma code low_vector=0x18
void low_interrupt(void) {
  _asm goto interruptHandlerLow _endasm;
}

#pragma code
#pragma interrupt interruptHandlerLow
void interruptHandlerLow(void) {
