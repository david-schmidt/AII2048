	.include "sos/sosconst.i"
	.include "sos/sosmacros.i"
	.include "ascreen.asm"

	.importzp CellTable, GameState, RestartLatch, P0ScoreBCD, CH, CV
	.import PRBYTE, COUT, CROUT, VTAB, SETUP, HOME, COL40
	.export READ_CHAR, DisplayBoard, InitScreen

InitScreen:
	lda E_REG			; Read the environment register
	and #$f7			; Turn $C000-$CFFF to R/W
	ora #$40			; Turn $C000-$CFFF to I/O
	sta E_REG			; Write the environment register
	jsr SETUP
	jsr COL40
	jsr HOME
	jsr DisplayScoreMsg
	rts

;---------------------------------------------------------
; READ_CHAR - Read a single character, no cursor
;---------------------------------------------------------
READ_CHAR:
	lda $C000         ;WAIT FOR NEXT COMMAND
	bit $C010
	sta tmp
	CONDITION_KEYPRESS
	cmp #$80
	bne :+
	pha
	lda #$01
	sta RestartLatch
	pla	
:	lda tmp
	rts

tmp:	.byte 00