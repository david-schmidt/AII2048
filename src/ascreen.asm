	.import FirstDataCellOffset
	.importzp CellTable, GameState, RestartLatch, P0ScoreBCD, P1ScoreBCD
	.export READ_CHAR, DisplayBoard, InitScreen, DisplayGameOverMsg, DisplayWonMsg

DisplayBoard:
; Home cursor
	lda #$06
	sta <CH
	lda #$06
	sta <CV
	jsr VTAB
; Paint bottom border
	lda #$20
	ldx #$0d
:	jsr COUT
	dex
	bne :-
; Paint top border
	lda #$01
	sta <CV
	jsr VTAB
	lda #$06
	sta <CH
	lda #$20
	ldx #$0d
:	jsr COUT
	dex
	bne :-


	lda #$01
	sta <CV
	ldy #$5		; FirstDataCellOffset
; Paint a data row
@Paint:	ldx #$00
	lda #$06
	sta <CH
	inc <CV
	lda <CV
	jsr VTAB
	lda #$20
	jsr COUT
:	lda CellTable,y
	jsr PRBYTE
	lda #$a0
	jsr COUT
	iny
	inx
	cpx #$04
	bne :-
	iny		; Skip over barrier
	dec <CH
	lda #$20
	jsr COUT
	lda #$a0
	jsr COUT
	lda CV
	cmp #$05
	bne @Paint

DisplayScore:
	lda #$1c
	sta <CH
	lda #$03
	sta <CV
	jsr VTAB
	lda P0ScoreBCD
	jsr PRBYTE
	lda P0ScoreBCD+1
	jsr PRBYTE
	lda P0ScoreBCD+2
	jsr PRBYTE
	lda #$1c
	sta <CH
	lda #$04
	sta <CV
	jsr VTAB
	lda P1ScoreBCD
	jsr PRBYTE
	lda P1ScoreBCD+1
	jsr PRBYTE
	lda P1ScoreBCD+2
	jsr PRBYTE
	rts

DisplayScoreMsg:
	lda #$16
	sta <CH
	lda #$03
	sta <CV
	jsr VTAB
	ldy #$00
@NextS:	lda ScoreMsg,y
	beq @High
	jsr COUT
	iny
	bne @NextS	
@High:	lda #$17
	sta <CH
	lda #$04
	sta <CV
	jsr VTAB
	ldy #$00
@NextH:	lda HighMsg,y
	beq @Done
	jsr COUT
	iny
	bne @NextH	
@Done:	rts

DisplayGameOverMsg:
	lda #$0f
	sta <CH
	lda #$0c
	sta <CV
	jsr VTAB
	ldy #$00
@Next:	lda GameOverMsg,y
	beq @Done
	jsr COUT
	iny
	bne @Next	
@Done:	rts

DisplayWonMsg:
	lda #$0d
	sta <CH
	lda #$0e
	sta <CV
	jsr VTAB
	ldy #$00
@Next:	lda WonMsg,y
	beq @Done
	jsr COUT
	iny
	bne @Next	
@Done:	rts

ScoreMsg:
	asc "SCORE:"
	.byte $00
HighMsg:
	asc "HIGH:"
	.byte $00
GameOverMsg:
	asc "GAME OVER!"
	.byte $00
WonMsg:
	asc "!YOU GOT 2048!"
	.byte $00