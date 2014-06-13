	.include "prodos/prodosconst.i"
	.include "prodos/prodosmacros.i"

	.importzp CellTable, GameState, RestartLatch, P0ScoreBCD
	.export READ_CHAR, DisplayBoard, DisplayState, InitScreen

InitScreen:
	jsr HOME
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