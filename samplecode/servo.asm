	list p= 18F4680
	include <p18F4680.inc>
	global ATimeH, ATimeL, BTimeH, BTimeL, MainLine
	
	UDATA
SignalOn res 1
ATimeH res 1
ATimeL res 1
BTimeH res 1
BTimeL res 1
Temp res 1
Temp2 res 1

; Code to handle interrupts
IntHand1 CODE 8h
	goto IntHandler
IntHand2 CODE 18h
	goto IntHandler


	CODE
MainLine
	movlw b'11100000'
	movwf OSCCON

;Config Timer
;*************************
	movlw b'01100000'
	movwf OSCCON
	movlw b'10100000'    ;Enable Interrupts
	movwf INTCON   
	movlw b'10000000'  ;Config Timer enable
	movwf T0CON	      ;  no prescaler
	movlw b'11011000' 
	movwf TMR0H          ;set high
	movlw b'11111010'   ;
	movwf TMR0L           ;set low
 

;*************************
;Config Vars
	clrf SignalOn
	bcf SignalOn, 0
	bcf SignalOn, 1
	bcf SignalOn, 2


;************************
;Config PORTB
	clrf PORTB
	movlw b'00000000'
	movwf TRISB
	
	return



IntHandler
	btfsc INTCON, 2         ;if tmr0 trigered goto tmr0 

	btfss SignalOn, 2
		goto IntTMR0

	btfsc SignalOn, 2
		goto IntTMR1
EndRupt
	retfie FAST			;keep this




IntTMR0



	btfss SignalOn, 0  ;If signal is off turn on
			goto TurnAon

	btfsc SignalOn, 0 ;if signal is on turn off
			goto TurnAoff

	bra IntHandler


IntTMR1

	btfss SignalOn, 1  ;If signal is off turn on
			goto TurnBon

	btfsc SignalOn, 1 ;if signal is on turn off
			goto TurnBoff

	
	bra IntHandler








TurnAoff
;Set counter to Off Time

	movlw b'01100101' ;
	addwf ATimeL, 0	
	movwf Temp           ;set low
	movlw b'11101111' ;8.5 ms for interupt 
	addwfc ATimeH, 0 	;add the on time so it goes of earlier
	movwf TMR0H          ;set high
	movff Temp,TMR0L

	bcf PORTB, 0
	bcf SignalOn, 0
	bcf INTCON, 2

	bcf SignalOn, 0 ;turn signal
	bsf SignalOn, 2
	goto EndRupt
	retfie FAST	


TurnAon
;Set timer to trigger after On Time
	movlw b'11111111' ;set max time to 65535
	movwf Temp2
	clrf Temp

	movf ATimeL, 0    ;subtract the time we want it to be on
	subwf Temp2, 0
	movwf Temp

	movlw b'11111111' ;set max time to 65535
	movwf Temp2

	movf ATimeH, 0   ;subtract the time we want it to be on
	subwfb Temp2, 0
	movwf TMR0H

	movff Temp, TMR0L

	bsf PORTB, 0

	bcf INTCON, 2
	bsf SignalOn, 0 ;turn signal
	goto EndRupt	
	retfie FAST	


TurnBoff
;Set counter to Off Time

	movlw b'01100101' ;
	addwf BTimeL, 0	
	movwf Temp           ;set low
	movlw b'11101111' ;8.5 ms for interupt 
	addwfc BTimeH, 0 	;add the on time so it goes of earlier
	movwf TMR0H          ;set high
	movff Temp,TMR0L

	bcf PORTB, 1
	bcf INTCON, 2

	bcf SignalOn, 1
	bcf SignalOn, 2
	goto EndRupt
	retfie FAST	


TurnBon
;Set timer to trigger after On Time
	movlw b'11111111' ;set max time to 65535
	movwf Temp2
	clrf Temp

	movf BTimeL, 0    ;subtract the time we want it to be on
	subwf Temp2, 0
	movwf Temp

	movlw b'11111111' ;set max time to 65535
	movwf Temp2

	movf BTimeH, 0   ;subtract the time we want it to be on
	subwfb Temp2, 0
	movwf TMR0H

	movff Temp, TMR0L

	bsf PORTB, 1

	bcf INTCON, 2

	bsf SignalOn, 1 
	goto EndRupt
	retfie FAST	

	end
