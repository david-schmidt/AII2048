.export CellTable, GameState
.exportzp RestartLatch, P0ScoreBCD, P1ScoreBCD
.import READ_CHAR, PRBYTE, DisplayBoard, DisplayState, InitScreen, VTAB, DisplayGameOverMsg

;
; 2048 2600
; =========
;
; A port of the 2048 game to the Atari 2600
;
; (C) 2014 Carlos Duarte do Nascimento (chesterbr)
; <cd@pobox.com | @chesterbr | http://chester.me>
;
; Latest version, contributors and general info:
;   http://github.com/chesterbr/2048-2060
;

; Building
; ---------
;
; You can build with DASM (http://dasm-dillon.sourceforge.net/), e.g.:
;
;   dasm 2048.asm -o2048.bin -f3
;
; However, you'll likely want to run graphics_gen.rb before. I've included
; my build script (build.sh), under the express condition that you don't
; mock my lame bash abilities :-P

; Cell Tables
; -----------
;
; The game store each player's tiles in a "cell table", which can contain one
; of these values:
;
;   0         = empty cell
;   1         = "2" tile
;   2         = "4" tile
;   3         = "8" tile
;   4         = "16" tile
;   ...
;   n         = "2n" tile (or, if you prefer: log2k = "k" tile)
;   ...
;   11        = "2048" tile
;   12        = "4096" tile
;   13        = "8192" tile
;	           (could go on, but try drawing a 5-digit 8x10 tile :-P )
;   128 ($FF) = sentinel tile (see below)
;
; In theory, we'd use 16 positions in memory for a 4x4 grid. Navigating
; left/right on the grid would mean subtracting/adding one position, and
; moving up/down would be done by subtracting 4 positions (that
; is, "cell table y offset" would be 4)
;
; However, we'd need to do complicated boundaries checking, so instead I
; surround the grid with "sentinel" or "wall" tiles. That would theoretically
; need 20 extra cells (bytes) to store the grid:
;
;   first cell -> SSSSSS       S = sentinel, . = data (a tile or empty cell)
;	 7th cell -> S....S
;	13rd cell -> S....S
;	19th cell -> S....S
;	25th cell -> S....S
;	             SSSSSS <- last (36th) cell
;
; But we can save some space by removing th left-side sentinels, since the
; memory position before those will be a sentinel anyway (the previous line's
; right-side sentinel).
;
; We can also cut the first and last sentinel (no movement can reach those),
; ending with with this layout in memory (notice how you still hit a
; sentinel if you try to leave the board in any direction):
;
;   first cell -> SSSSS        S = sentinel, . = data (a tile or empty cell)
;	 6th cell -> ....S
;	11th cell -> ....S
;	16th cell -> ....S
;	21st cell -> ....S
;	             SSSS <- last (29th) cell
;
; Only change from usual 4x4 is the cell table vertical y offset is now 5 (we
; add/subtract 5 to go down/up). The first data cell is still the first cell
; plus vertical y offset
;
;
; Grid Drawing
; ------------
;
; The grid itself will be drawn using the TIA playfield, and the tiles
; with player graphics. The Atari only allows two of those graphics per
; scanline (although they can be repeated up to 3 times by the hardware),
; and we have four tiles per row, meaning we have to trick it[2] by:
;
;	- Load the graphic for tiles A and B:          "A"     and "B"
;	- Ask TIA to repeat each player graphic:       "A   A" and "B   B"
;	- Overlap their horizontal positions:          "A B A B"
;	- Load graphic for tiles C and D when the      "A B C D"
;	  TV beam is right halfway, that is, here: --------^
;
; First three steps can be done just once when we start drawing the grid
; (TIA remembers the position), but the fourth must be repeated for every
; scanline. Timing is crucial on third and fourth steps, but that's Atari
; programming for you!
;
; To translate the cell table into a visual grid, we have to calculate, for
; each data cell, where the bitmap for its value (tile or empty space) is
; stored. We use the scanlines between each row of cells to do this calculation,
; meaning we need 8 RAM positions (4 cells per row x 2 bytes per address).
;
; We use the full address instead of a memory page offset to take advantage
; of the "indirect indexed" 6502 addressing mode [1], but we load the
; graphics table at a "page aligned" location (i.e., a "$xx00" address),
; so we only need to update the least significant byte on the positions above.
;
;
; Shifting
; --------
;
; The original 2048 sets a "vector" structure that points two variables with
; the direction in which the tiles will shift (e.g., vector.x = -1, vector.y
; = 0 means we'll move left). It also processes them from the opposite side
; (e.g., start from leftmost if it's a right shift), making the first one
; stop before the edge, the second stop before the first, etc.
;
; It also marks each merged tile as so (removing the marks between shifts),
; so it can block multiple merges (i.e., the row " 4 4 8 16" does not go
; straight to "32" with a left, but first becomes "8 8 16", then "16 16", then
; "32". Similarly, "2 2 2 2" would first become "4 4", then "8". Finally,
; it stores the previous position for each tile, and lets the awesomeness
; of CSS move them all at once with ease.
;
; We'll translate this idea by having a single-byte "vector" which can be
; -1/+1 for left/right, and -5/+5 for up/down (each row is 4 bytes plus a
; sentinel tile, see above). Each tile will be pushed (by adding the vector
; value) until the next cell is non-empty and does not match its value.
;
; The vector signal also tells us where to start to ensure they all get to
; the end: negative (left/up) start processing from the first cell and
; positive (right/down) start from the last.
;
; Merged tiles are marked by setting bit 7 on their values, which will be
; easy to check in upcoming pushed blocks without needing extra memory.
; We'll only miss the animations, but we can't have everything.
;
;
; Timings
; -------
;
; Since the shift routine can have unpredictable timing (and I wanted some
; freedom to move routines between overscan and vertical blank), I decided
; to use RIOT timers instead of the traditional scanline count. It is not
; the usual route for games (as they tend to squeeze every scanline of
; processing), but for this project it worked fine.
;
; [1] http://skilldrick.github.io/easy6502/
; [2] http://www.slideshare.net/chesterbr/atari-2600programming

	.org $2000
	jmp Initialize

;;;;;;;;;;;;;;;;;
;; DATA TABLES ;;
;;;;;;;;;;;;;;;;;

; Tile and digit graphics go in the beginning of the cart to keep page-aligned
; (that is, the address' MSB never changes and we only calculate the LSB)

; Score values of each tile, in BCD
TileValuesBCD:
	.byte $00,$04
	.byte $00,$08
	.byte $00,$16
	.byte $00,$32
	.byte $00,$64
	.byte $01,$28
	.byte $02,$56
	.byte $05,$12
	.byte $10,$24
	.byte $20,$48
	.byte $40,$96
	.byte $81,$92

; Values that change if we are on PAL mode (TV TYPE switch "B•W" position)
; Order: NTSC, PAL. (thanks @SvOlli)
VBlankTime64T:
	.byte 44,74
OverscanTime64T:
	.byte 35,65

;;;;;;;;;;;;;;;
;; CONSTANTS ;;
;;;;;;;;;;;;;;;

; Special cell values (see header)
CellEmpty    = 0
Cell2        = 1
Cell4        = 2
Cell2048     = 11
CellSentinel = 127          ; Can't use bit 7 (will be wiped)

MergedMask      = %10000000 ; Bit 7 is set to flag tiles as merged
ClearMergedMask = %01111111 ; We need to reset to get to original values

; The original 2048 puts 2s with 90% probability and 4s with 10%. Our random
; number range between 0-255, so we'll put a 4 if it is above 256 * 0.9 ? 230
ThresholdForTile4 = 230

; Values of GameState (it's a state machine!)
TitleScreen       = 0  ; => AddingRandomTitle
AddingRandomTile  = 1  ; => WaitingJoyRelease
WaitingJoyRelease = 2  ; => WaitingJoyPress
WaitingJoyPress   = 3  ; => Shifting
Shifting          = 4  ; => ShowingMerged OR WaitingJoyRelease
ShowingMerged     = 5  ; => AddingRandomTile OR GameOverFX
GameOverFX        = 6  ; => GameOver
GameOver          = 7  ; => TitleScreen

; Values of GameMode
OnePlayerGame = 0
TwoPlayerGame = 1

; Some relative positions on the cell table
; Notice how we go to last data cell: Top-Left + 3 rows down + 3 columns right
; (FYI: add another row down and you'd have the last sentinel)
FirstDataCellOffset = 5
LastDataCellOffset  = FirstDataCellOffset + (CellTableYOffset * 3) + 3
LastCellOffset      = LastDataCellOffset + CellTableYOffset
CellTableYOffset    = 5  ; How much we +/- to move up/down a line on the table

; Position of sentinels that might be picked as random tiles (for being the
; ones on the right "wall", see drawing on header), relative to the
; first data cell offset.
; We'll replace them with the last three, allowing our random nibble to
; effectively map a random data cell
Wall1 = 4
Wall2 = 9
Wall3 = 14
Wall1Repl = 16
Wall2Repl = 17
Wall3Repl = 18

ScoreColor         = $28 ; Colors were chosen to get equal or equally nice
InactiveScoreColor = $04 ; on both PAL and NTSC, avoiding adjust branches
GridColor          = $04
BackgroundColor    = $00


TileHeight = 11          ; Tiles have 11 scanlines (and are in graphics.asm)
GridBottomHeight = 5      ; Doesn't count the ones we use calculating P1's score

JoyUp    = %11100000      ; Masks to test SWCHA for joystick movement
JoyDown  = %11010000      ; (we'll shift P1's bits into P0s on his turn, so
JoyLeft  = %10110000      ;  it's ok to use P0 values)
JoyRight = %01110000
JoyMask  = %11110000

ColSwitchMask   = %00001000  ; Mask to test SWCHB for TV TYPE switch
SelectResetMask = %00000011  ; Mask to test SWCHB for GAME SELECT/RESET switches
GameSelect      = %00000001  ; Value for GAME SELECT pressed (after mask)
GameReset       = %00000010  ; Value for GAME RESET  pressed (after mask)

; Amount to add to move to a direciton in the cell grid, in two's complement
RightShiftVector = $01     ;  1
LeftShiftVector  = $FF     ; -1
DownShiftVector  = $05     ;  5
UpShiftVector    = $FB     ; -5

TurnIndicatorFrames = 60   ; Special display for this # of frames if turn switches

MergeNoise = 4             ; Value of TurnNoise (AUDC0) if a merge happened
ShiftNoise = 1             ; Value of TurnNoise (AUDC0) if no merge happened

;;;;;;;;;
;; RAM ;;
;;;;;;;;;

; Some positions are shared between different coroutines
; (think of them as local variables)

RestartLatch = $81
CellTable = $90              ; 29 ($1D) bytes (16 cells + 13 sentinels)

CellCursor = $aD             ; Table offset of the "current" cell on grid setup

; Frame count based RNG, used to add tiles and title screen rainbow
RandomNumber       = $aE

; Counter to display "animated" tiles (merged / new)
AnimationCounter   = $aF

; Added to the tile address to produce the merge animation
AnimationDelta     = $b0

; 6-digit score is stored in BCD (each nibble = 1 digit => 3 bytes)
ScoreBCD           = $b1

GameMode = $b4               ; One or Two players

DidMerge = $b5               ; Nonzero if a merge happened on the last move

; $A6-$A7 have different uses in various kernel routines:

TempVar1 = $b6               ; General use variable
TempVar2 = $b7               ; General use variable

LineCounter  = $b6           ; Counts lines while drawing the score
TempDigitBmp = $b7           ; Stores intermediate part of 6-digit score

DidShift = $b6               ; True if a shift happened

GameState = $b8;

; Tile shift routine variables
ShiftVector        = $b9     ; What to add to get to "next" tile in current direction
TilesLoopDirection = $bA     ; +1 for left/up, -1 for right/down
OffsetBeingPushed  = $bB     ; Position in cell table of the tile being pushed
ShiftEndOffset     = $bC     ; Position in which we'll stop processing
CurrentValue       = $bD     ; Value of that tile

; Address of the graphic for for each digit (6x2 bytes)
; or tile (4x2 bytes) currently being drawn

DigitBmpPtr = $c0            ; 6 bytes
TileBmpPtr  = $c0            ; 4 bytes (2 wasted)

; Colors of each tile on the current row (4 bytes)
RowTileColor = $cC

; Store each player score separatedly and copy
; from/to ScoreBCD as needed to display, add, etc.
; Note: P1 score will store (and show) the high-score in single-player games
P0ScoreBCD = $d0             ; 3 bytes
P1ScoreBCD = $d3             ; 3 bytes

ScoreBeingDrawn = $d6        ; 0 for P0 or 1 for P1
CurrentPlayer = $d7          ; 0 for P0 or 1 for P1

LastSWCHB = $d8              ; Avoid multiple detection of console switches

TurnIndicatorCounter = $d9   ; Controls the time spent changing player turn

GameOverEffectCounter = $dA  ; Controls the time spent on game over effect

CurrentBGColor = $dB         ; Ensures invisible score keeps invisible during
	                         ; game over "explosion"

DidMerge2048 = $dC           ; 0 if no 2048 was reached; 11 (Cell2048) if we did
Party2048Counter= $dD       ; 2048 effects counter (and ensures they play only once)

;;;;;;;;;;;;;;;
;; BOOTSTRAP ;;
;;;;;;;;;;;;;;;

Initialize:             ; Cleanup routine from macro.h (by Andrew Davie/DASM)
	sei
	cld
	ldx #0
	txa
	tay
	sta RestartLatch
	sta P0ScoreBCD
	sta P0ScoreBCD+1
	sta P0ScoreBCD+2
	sta P1ScoreBCD
	sta P1ScoreBCD+1
	sta P1ScoreBCD+2
	jsr InitScreen

;;;;;;;;;;;;;;;
;; TIA SETUP ;;
;;;;;;;;;;;;;;;

;	lda #%00000001      ; Playfield (grid) in mirror (symmetrical) mode
;	sta CTRLPF

;;;;;;;;;;;;;;;;;;;
;; PROGRAM SETUP ;;
;;;;;;;;;;;;;;;;;;;

; Pre-fill the graphic pointers' MSBs, so we only have to
; figure out the LSBs for each tile or digit
;	lda #>Tiles        ; MSB of tiles/digits page
;	ldx #11            ; 12-byte table (6 digits), zero-based
;FillMsbLoop:
;	sta TileBmpPtr,x
;	dex                ; Skip to the next MSB
;	dex
;	bpl FillMsbLoop

; Initialize the cell table with sentinels, then fill
; the interior with empty cells
	ldx #LastCellOffset
	lda #CellSentinel
InitCellTableLoop1:
	sta CellTable,x
	dex
	bpl InitCellTableLoop1

;;;;;;;;;;;;;;;;;;
;; TITLE SCREEN ;;
;;;;;;;;;;;;;;;;;;

ShowTitleScreen:
	lda #TitleScreen
	sta GameState

;	lda #BackgroundColor                     ; Hide the grid separator
;	sta COLUPF

	ldx #LastDataCellOffset                  ; Print using tiles
TitleScreenLoop:
	lda TitleTiles-FirstDataCellOffset,x
	sta CellTable,x
	dex
	cpx #FirstDataCellOffset-1
	bne TitleScreenLoop
	jsr DisplayBoard
	jmp StartFrame

TitleTiles:
	.byte $20, $48, $65, $02, CellSentinel
	.byte $20, $48, $65, $02, CellSentinel
	.byte $20, $48, $65, $02, CellSentinel
	.byte $20, $48, $65, $02, CellSentinel

;;;;;;;;;;;;;;
;; NEW GAME ;;
;;;;;;;;;;;;;;

StartNewGame:
	ldx #LastDataCellOffset       ; Last non-sentinel cell offset
	lda #CellEmpty
InitCellTableLoop2Outer:
	ldy #4                        ; We'll clean 4 cells at a time
InitCellTableLoop2Inner:
	sta CellTable,x
	dex
	dey
	bne InitCellTableLoop2Inner
	dex                           ; skip 1 cell (side sentinel)
	cpx #FirstDataCellOffset
	bcs InitCellTableLoop2Outer   ; and continue until we pass the top-left cell

; Reset score
	lda #0
	ldx #5                        ; Reset both scores if on two-player game
	ldy GameMode
	cpy #TwoPlayerGame
	beq LoopResetScore
	ldx #2                        ; Only reset P0 if single-player (P1=record)
LoopResetScore:
	sta P0ScoreBCD,x
	dex
	bpl LoopResetScore

; Reset other variables
	sta CurrentPlayer
	sta CurrentBGColor
	sta DidMerge2048
	sta Party2048Counter

; Set up the screen
	jsr InitScreen

; Start the game with a random tile
	lda #AddingRandomTile
	sta GameState

;;;;;;;;;;;;;;;;;
;; FRAME START ;;
;;;;;;;;;;;;;;;;;

StartFrame:

;;;;;;;;;;;;;;;;;;;;;
;; NEW RANDOM TILE ;;
;;;;;;;;;;;;;;;;;;;;;
	lda GameState
	cmp #AddingRandomTile
	bne EndRandomTile        ; No need for a random tile now

; Pick a random cell from a number from 0..15, mapping those that would
; hit sentinels on the right side ("walls") to the ones not covered by the range

	lda RandomNumber
	and #$0F
	cmp #Wall1
	bne NoWall1
	lda #Wall1Repl
NoWall1:
	cmp #Wall2
	bne NoWall2
	lda #Wall2Repl
NoWall2:
	cmp #Wall3
	bne CheckIfCellIsEmpty
	lda #Wall3Repl

CheckIfCellIsEmpty:
	tax                       ; Cell offset now in X
	lda CellTable+FirstDataCellOffset,x
	cmp #CellEmpty
	bne EndRandomTile         ; Tile not empty, let's try again next frame

PickTileType:
	ldy RandomNumber
	cpy #ThresholdForTile4
	bcc PickTile2
	lda #Cell4                ; >= threshold, A will be a "4" tile
	jmp AddTileToCell
PickTile2:
	lda #Cell2                ; < threshold, A will be a "2" tile

AddTileToCell:
	sta CellTable+FirstDataCellOffset,x
	lda #WaitingJoyRelease    ; Tile added, player can play as soon as they
	sta GameState             ; release the joystick

	jsr DisplayBoard

EndRandomTile:
	inc RandomNumber          ; Feed the random number generator

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SELECT, RESET AND P0 FIRE BUTTON ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	ldx GameMode              ; Remember if we were on one or two player mode
;	lda SWCHB                 ; We only want the switch presses once
;	and #SelectResetMask      ; (in particular GAME SELECT)
;	cmp LastSWCHB
;	beq NoSwitchChange
;	sta LastSWCHB             ; Store so we know if it's a repeat next time

;	cmp #GameSelect           ; GAME SELECT flips single/multiplayer...
;	bne NoSelect
;	lda GameMode
;	eor #1
;	sta GameMode
;	jmp StartNewGame          ; ...and restarts with no further game mode change
NoSelect:
;	cmp #GameReset            ; GAME RESET restarts the game at any time
;	beq Restart
NoSwitchChange:
;	lda INPT4
;	bpl ButtonPressed         ; P0 Fire button pressed?
;	ldx #1                    ; P1 fire button always starts two-player game
;	lda INPT5                 ; P1 fire button pressed?
;	bmi NoRestart
ButtonPressed:
;	lda GameState
;	cmp #TitleScreen
;	beq Restart               ; Start game if title screen
;	cmp #GameOver             ; or game over
;	bne NoRestart
Restart:
;	stx GameMode
;	jmp StartNewGame

	lda RestartLatch
	beq NoRestart
	lda #$00
	sta RestartLatch
	jmp StartNewGame

NoRestart:

	

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POST-SHIFT ACTION (MERGE ANIMATION, SCORE UPDATE, GAME OVER) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda GameState
	cmp #ShowingMerged
	bne ResetAnimationCounter     ; No animation: just keep the counter ready
	lda AnimationCounter          ; Steadly decrease the shift tone,
	lsr                           ; giving it a "wall hit" feeling
	lsr
	sta AUDV0
	dec AnimationCounter          ; Decrease counter until animation ends
	bne DoneCounterManagement
; Finished animation: update score and add new tile
	lda #AddingRandomTile         ; Animation done, let's add a tile...
	sta GameState
	ldx #FirstDataCellOffset      ; and count points for merged cells...
	lda #0
	sta DidMerge                  ; and set this flag if there were any
	sta DidMerge2048              ; and this to flag if any was a 2048
CountScoreLoop:
	lda CellTable,x
	cmp #MergedMask
	bmi ClearMergedBit            ; Not merged, just clear the bit
	sta DidMerge                  ; Flag that we had (at least one) merge
	and #ClearMergedMask          ; and get the value without the merge bit
	cmp #Cell2048
	bne CountScore
	sta DidMerge2048              ; If we merged a 2048, flag it
CountScore:
	asl
	tay                           ; Now Y = offset of tile values table
	sed                           ; We'll work in BCD
	lda CurrentPlayer
	bne AddScoreToP1
AddScoreToP0:
	clc
	lda TileValuesBCD-3,y
	adc P0ScoreBCD+2
	sta P0ScoreBCD+2                ; score "low byte" += table LSB

	lda TileValuesBCD-4,y
	adc P0ScoreBCD+1
	sta P0ScoreBCD+1                ; score "middle byte" += table MSB + carry

	lda #0
	adc P0ScoreBCD
	sta P0ScoreBCD                  ; score "high byte" += carry
	jmp DoneAddingScore
AddScoreToP1:
	clc
	lda TileValuesBCD-3,y
	adc P1ScoreBCD+2
	sta P1ScoreBCD+2                ; score "low byte" += table LSB

	lda TileValuesBCD-4,y
	adc P1ScoreBCD+1
	sta P1ScoreBCD+1                ; score "middle byte" += table MSB + carry

	lda #0
	adc P1ScoreBCD
	sta P1ScoreBCD                  ; score "high byte" += carry
DoneAddingScore:
	cld
	lda CellTable,x               ; Restore original value
ClearMergedBit:
	and #ClearMergedMask          ; Clear bit and store
	sta CellTable,x
	inx
	cpx #LastDataCellOffset+1
	bne CountScoreLoop            ; Go to the next, up to the last tile

; Change the turn to the other player, if appropriate
	lda GameMode
	cmp #TwoPlayerGame
	bne DoneCounterManagement     ; Single player always keeps their turn
	lda DidMerge
	bne DoneCounterManagement     ; Did merge, player keeps his turn
	lda CurrentPlayer
	eor #1                        ; Flip player (0<->1)
	sta CurrentPlayer
	lda #TurnIndicatorFrames      ; Show it's his turn
	sta TurnIndicatorCounter
ResetAnimationCounter:
DoneCounterManagement:
; Reached first 2048? Let's party!
	lda DidMerge2048
	beq NoParty                   ; No 2048
	lda Party2048Counter
	bne NoParty                   ; Already had a party
	inc Party2048Counter          ; Let's party!
NoParty:


;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME OVER DETECTION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

	lda GameState
	cmp #WaitingJoyRelease
	bne EndGameOverDetection

	ldx #FirstDataCellOffset
FindAMoveLoop:
	lda CellTable,x               ; A = current cell value
	cmp #CellEmpty
	beq FoundAMove                ; Empty cell => can move
	inx
	cmp #CellSentinel
	beq FindAMoveLoopEnd          ; Sentinel, check next cell
	cmp CellTable,x               ; X = offset of cell to the right
	beq FoundAMove                ; Cell matches right neighbour => can merge
	inx
	inx
	inx
	inx                           ; X = offset of cell to the bottom (1+4=5)
	cmp CellTable,x
	beq FoundAMove                ; Cell matches bottom neighbour => can merge
	dex
	dex
	dex
	dex                           ; X = offset of next cell (1+4-4=1)
FindAMoveLoopEnd:
	cpx #LastDataCellOffset+1
	bne FindAMoveLoop             ; Iterate all tiles
; If we get here, no move was found => game over
	lda #GameOverFX               ; Start the game over effects...
	sta GameState
	lda #127                      ; with a centered...
	sta AUDF0
	lda #8                        ; explosion-ish...
	sta AUDC0
	lda #15                       ; loud sound
	sta AUDV0
	ldx #0
	stx GameOverEffectCounter
; For two-player mode, find the winner and make it current (=highlighted)
; For one-player mode, just update the high score (P1 Score)
	ldy GameMode
FindHigherScoreLoop:              ; Iterate over score byte until we can
	lda P0ScoreBCD,x              ; define whether P0 or P1 is higher
	cmp P1ScoreBCD,x
	beq ContinueFindHigherScore   ; Can't tell from this byte, keep looping
	bcc P1Higher
P0Higher:
	lda #0
	cpy #TwoPlayerGame
	beq SetTurnToWinner           ; Two-player game: P0 wins, make it current.
	ldx P0ScoreBCD                ; One-player game: we have a new record,
	stx P1ScoreBCD                ; copy it to the P1 (high score)
	ldx P0ScoreBCD+1
	stx P1ScoreBCD+1
	ldx P0ScoreBCD+2
	stx P1ScoreBCD+2
	jmp EndGameOverDetection
P1Higher:
	cpy #OnePlayerGame
	beq EndGameOverEffects        ; One-player game: no new record, we're done
	lda #1                        ; Two-Player game: P1 wins, make it current.
	jmp SetTurnToWinner
ContinueFindHigherScore:
	inx
	cpx #3
	bne FindHigherScoreLoop
BothAreLosers:
	lda #99                       ; In a tie, no one will be bright
SetTurnToWinner:
	sta CurrentPlayer
FoundAMove:
EndGameOverDetection:

;;;;;;;;;;;;;;;;;;;;;;;
;; GAME OVER EFFECTS ;;
;;;;;;;;;;;;;;;;;;;;;;;
	lda GameState
	cmp #GameOverFX
	bne EndGameOverEffects
	lda RandomNumber              ; Flash the background
;	sta COLUBK
	sta CurrentBGColor
	inc GameOverEffectCounter     ; Keep on for ~2.3s (+/-0.2 for PAL/NTSC diff)
	bpl EndGameOverEffects
	lda #BackgroundColor          ; Now game is *really* over, cut the effects
;	sta COLUBK
	sta CurrentBGColor
	lda #0
	sta AUDV0
	lda #GameOver
	sta GameState
	jsr DisplayGameOverMsg
EndGameOverEffects:

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2048 "PARTY" EFFECTS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

	lda Party2048Counter
	beq DonePartyCheck           ; No party yet
	bmi DonePartyCheck           ; Party is over
	lda #04                      ; Party!
;	sta AUDC1
	lda Party2048Counter
;	sta AUDF1
;	sta COLUPF
	lda #10
;	sta AUDV1
	inc Party2048Counter
	bpl DonePartyCheck
	lda #0                       ; End of party
;	sta AUDV1
	lda GridColor
;	sta COLUPF
DonePartyCheck:

;;;;;;;;;;;;;;;;;;;;
;; INPUT CHECKING ;;
;;;;;;;;;;;;;;;;;;;;

; Joystick
	jsr READ_CHAR

VerifyGameStateForJoyCheck:
	ldx GameState            ; We only care for states in which we are waiting
	cpx #WaitingJoyRelease   ; for a joystick press or release
	beq CheckJoyRelease
	cpx #WaitingJoyPress
	bne EndJoyCheck

; If the joystick is in one of these directions, trigger the shift by
; setting the ShiftVector and changing mode
CheckJoyUp:
	cmp #$8b
	bne CheckJoyDown
	lda #UpShiftVector
	jmp TriggerShift

CheckJoyDown:
	cmp #$8a
	bne CheckJoyLeft
	lda #DownShiftVector
	jmp TriggerShift

CheckJoyLeft:
	cmp #$88
	bne CheckJoyRight
	lda #LeftShiftVector
	jmp TriggerShift

CheckJoyRight:
	cmp #$95
	bne EndJoyCheck
	lda #RightShiftVector

TriggerShift:
	sta ShiftVector            ; We'll need the direction vector on the shift
	lda #Shifting
	sta GameState
	jmp EndJoyCheck

CheckJoyRelease:
	lda tmp
	bmi EndJoyCheck

	lda #WaitingJoyPress       ; Joystick released, can accept shifts again
	sta GameState

EndJoyCheck:

;;;;;;;;;;;;;;;;;
;; SHIFT BOARD ;;
;;;;;;;;;;;;;;;;;

	lda GameState
	cmp #Shifting
	beq StartShift             ; Have to do this instead of bne EndShift
	jmp EndShift               ; because the routine is > 128 bytes!

; Outer loop will traverse the entire cell map in the *opposite* order of the
; movement, that is, from the beginning for right/up and from end for left/down,
; so they stack and merge as expected. Let's setup these parameters

StartShift:
	lda #0                     ; So far, no merges happened
	sta DidMerge

SetDirection:
	lda ShiftVector
	bmi NegativeVector

PositiveVector:
	ldx #LastDataCellOffset    ; Start from the last cell
	ldy #FirstDataCellOffset-1 ; Stop when we pass the first one
	lda #$FF                   ; Go backwards (-1)
	jmp SetShiftParams

NegativeVector:
	ldx #FirstDataCellOffset   ; Start from the first cell
	ldy #LastDataCellOffset+1  ; Stop when we pass the last one
	lda #$01                   ; Go forward (+1)

SetShiftParams:
	sty ShiftEndOffset
	sta TilesLoopDirection

	lda #WaitingJoyRelease     ; We won't animate or add tiles unless some
	sta GameState              ; movement happens

; Notice that X will keep the offset of the cell being processed (main loop),
; and will advance until we have no more processable tile positions.
;
; Whenever we have a "good" tile on X (not empty or sentinel), we'll start
; pushing it on the vector direciton (inner/"push" loop), until we can't
; push anyomre.

CheckIfXIsPushable:
	lda CellTable,x
	cmp #CellSentinel
	beq AdvanceToNext             ; Skip empty cells
	cmp #CellEmpty
	beq AdvanceToNext             ; Skip sentinels
	stx OffsetBeingPushed
	jmp StartPush                 ; This one is good, start pushing it

AdvanceToNext:
	txa                           ; Move X to the next candidate offset
	clc
	adc TilesLoopDirection
	tax
	cpx ShiftEndOffset
	beq TurnNoiseSetup            ; Processed all tiles, shift is done!
	jmp CheckIfXIsPushable        ; Check the new candidate
TurnNoiseSetup:
	lda GameState
	cmp #WaitingJoyRelease
	beq EndShift                  ; No shift => no noise
	ldx #ShiftNoise
	lda DidMerge
	beq StartNoise                ; Shift with no merge => shift noise
	ldx #MergeNoise               ; Shift & merge => merge noise
StartNoise:
	stx AUDC0
	lda #90
	sta AUDF0
	jmp EndShift

; Inner loop will push the tile currenlty picked by the outer loop towards
; the desired direction, until hitting an unmergeable tile

StartPush:
	lda CellTable,x
	sta CurrentValue              ; Keep the current tile's value
	stx OffsetBeingPushed         ; Initialize inner loop counter

PushCurrentTile:                  ; Inner loop begins here
	clc
	lda OffsetBeingPushed
	adc ShiftVector
	tay
	lda CellTable,y          ; A <= value of next cell in the vector direction

	cmp #CellEmpty
	bne NotEmpty             ; We won't move if the next cell is not empty

MoveCurrentToNext:
	lda #ShowingMerged       ; If we move at least once, we can show merged
	sta GameState            ; animations (if any)

	lda CurrentValue
	sta CellTable,y          ; Set next cell to current value

	lda #CellEmpty
	ldy OffsetBeingPushed
	sta CellTable,y          ; Clear current cell

	clc
	lda OffsetBeingPushed
	adc ShiftVector
	sta OffsetBeingPushed    ;   - make next the one being pushed

	jmp PushCurrentTile      ; Keep pushing

NotEmpty:
	cmp #MergedMask
	bcs AdvanceToNext        ; Can't merge if next cell has already been merged
	cmp CurrentValue
	bne AdvanceToNext        ; Only merge if value matches

Merge:
	inc CurrentValue         ; Multiply by 2 in log (that is, add 1 to exponent)
	lda #MergedMask
	ora CurrentValue         ; Add the "merged" bit (so it doesn't match others)
	sta CurrentValue
	sta DidMerge             ; Signal the merge
	jmp MoveCurrentToNext    ; Move the multiplied cell to the target position

EndShift:
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REMAINDER OF OVERSCAN ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

WaitForOverscanEndLoop:
;	lda INTIM                   ; Wait until the timer signals the actual end
;	bne WaitForOverscanEndLoop  ; of the overscan period

;	sta WSYNC
	jmp StartFrame

tmp:	.byte 0

; The MIT License (MIT)

; Copyright (c) 2014 Carlos Duarte do Nascimento (Chester)
;
; Original 2048 game Copyright (c) 2014 Gabriele Cirulli

; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:

; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

AUDV0 = $c030
AUDF0 = $c030
AUDC0 = $c030
