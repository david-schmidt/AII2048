	.include "sos/sosconst.i"
	.include "sos/sosmacros.i"

	.importzp CellTable, GameState, CH, CV, RestartLatch, P0ScoreBCD
	.import PRBYTE, COUT, CROUT, VTAB, SETUP, HOME, COL40
	.export READ_CHAR, DisplayBoard, DisplayState, InitScreen

InitScreen:
	lda E_REG			; Read the environment register
	and #$f7			; Turn $C000-$CFFF to R/W
	ora #$40			; Turn $C000-$CFFF to I/O
	sta E_REG			; Write the environment register
	jsr SETUP
	jsr COL40
	jsr HOME
	rts

;---------------------------------------------------------
; READ_CHAR - Read a single character, no cursor
;---------------------------------------------------------
READ_CHAR:
	lda $C000         ;WAIT FOR NEXT COMMAND
	bit $C010
	CONDITION_KEYPRESS
	cmp #$80
	bne :+
	pha
	lda #$01
	sta RestartLatch
	pla	
:	rts

DisplayBoard:
; Home cursor
	lda #$00
	sta CH
	sta CV
	jsr VTAB
; Paint top border
	lda #$20
	ldx #$0d
:	jsr COUT
	dex
	bne :-
	jsr CROUT
; Paint first data row
	lda #$20
	jsr COUT
	ldy #$05
:	lda CellTable,y
	jsr PRBYTE
	lda #$a0
	jsr COUT
	iny
	cpy #$09
	bne :-
	dec CH
	lda #$20
	jsr COUT
	lda #$a0
	jsr COUT
	jsr CROUT
	lda #$20
	jsr COUT
	iny
:	lda CellTable,y
	jsr PRBYTE
	lda #$a0
	jsr COUT
	iny
	cpy #$0e
	bne :-
	dec CH
	lda #$20
	jsr COUT
	jsr CROUT
	lda #$20
	jsr COUT
	iny
:	lda CellTable,y
	jsr PRBYTE
	lda #$a0
	jsr COUT
	iny
	cpy #$13
	bne :-
	dec CH
	lda #$20
	jsr COUT
	jsr CROUT
	lda #$20
	jsr COUT
	iny
:	lda CellTable,y
	jsr PRBYTE
	lda #$a0
	jsr COUT
	iny
	cpy #$18
	bne :-
	dec CH
	lda #$20
	jsr COUT
	jsr CROUT
	lda #$20
	ldx #$0d
:	jsr COUT
	dex
	bne :-
	lda #$14
	sta CH
	lda #$01
	sta CV
	jsr VTAB
	lda P0ScoreBCD
	jsr PRBYTE
	lda P0ScoreBCD+1
	jsr PRBYTE
	lda P0ScoreBCD+2
	jsr PRBYTE
	rts

DisplayState:
	pha
	lda #$25
	sta CH
	lda #$00
	sta CV
	jsr VTAB
	lda GameState
	jsr PRBYTE
	pla
	rts