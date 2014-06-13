	.include "prodos/prodosconst.i"
	.include "prodos/prodosmacros.i"
	.include "ascreen.asm"

	.importzp CellTable, GameState, RestartLatch, P0ScoreBCD

InitScreen:
	jsr HOME
	jsr DisplayScoreMsg
	rts
 
;---------------------------------------------------------
; READ_CHAR - Read a single character, no cursor
;---------------------------------------------------------
READ_CHAR:
	lda $C000         ; Read keyboard
	bit $c010
	cmp #$a0
	bne :+
	pha
	lda #$01
	sta RestartLatch
	pla	
:	rts