;-----------------------------------------------------------------------------
; x16-life.asm
;
; Conway's game of life for Commander X16.  This sets the neighbour counts
; in cells and thus does not need to check the neighbours to generate
; generations.  This version doesn't do wrapping - I spent a bit of time
; thinking about how to do wrapping but nothing came to mind that would
; be relatively easy, which is too bad.
;
; Stefan Wessels, 2019
; This is free and unencumbered software released into the public domain.

;-----------------------------------------------------------------------------
NUMBER_PART   = %00001111                       ; number of neighbours mask
ALIVE_NOW     = %01000000                       ; bit set for alive this generation
ALIVE_NEXT    = %10000000                       ; bit set alive next generation
THIS_GEN      = %01111111                       ; mask to unset next generation
COLUMNS       = 82                              ; size of grid X
ROWS          = 62                              ; size of grid Y
CELL_ALIVE    = $A0                             ; display code for alive (block)
CELL_DEAD     = $20                             ; diplay code for dead (space)
CELL_COLOR    = $6E                             ; color for display grid

;-----------------------------------------------------------------------------
VERA_ADDR_LO  = $9F20
VERA_ADDR_MID = $9F21
VERA_ADDR_HI  = $9F22
VERA_DATA0    = $9F23
VERA_CTRL     = $9F25

GETIN         = $FFE4

;-----------------------------------------------------------------------------
; .debuginfo on
; .listbytes unlimited

;-----------------------------------------------------------------------------
.segment "ZEROPAGE"

zCellPtr:      .res 2                           ; ptr into life grid
zTempPtr:      .res 2                           ; temporary ptr used in alive/die
zRows:         .res 1                           ; row being processed
zTempY:        .res 1                           ; temporary storage of y index register
zAliveNowBit:  .res 1                           ; ALIVE_NOW bit pattern for bit instruction
zAliveNextBit: .res 1                           ; ALIVE_NEXT bit pattern for bit instruction

;-----------------------------------------------------------------------------
.segment "CODE"

jsr setup                                       ; get ready
jmp runLife                                     ; run the cycles

;-----------------------------------------------------------------------------
.proc setup

    lda #ALIVE_NOW                              ; set the bit pattern
    sta zAliveNowBit                            ; in memory

    lda #ALIVE_NEXT
    sta zAliveNextBit

    jsr clearData                               ; clear the data section to 0's
    jsr setupPattern                            ; put a pattern into the data grid
    jmp showCanvas                              ; fill in neighbours and display grid

.endproc 

;-----------------------------------------------------------------------------
; Clear the cell grid to all 0's
.proc clearData

    lda #<patternData                           ; point at pattern
    sta zCellPtr
    lda #>patternData
    sta zCellPtr + 1

    ldy #ROWS                                   ; rows to process
    sty zRows

    clc 

outerLoop:
    lda #0                                      ; value to fill grid with
    ldy #COLUMNS - 1                            ; columns to fill

loop:
    sta (zCellPtr), y                           ; write the value
    dey                                         ; next cell
    bpl loop 
    dec zRows                                   ; done a row
    beq done                                    ; all rows doen

    lda zCellPtr                                ; move to next row
    adc #COLUMNS
    sta zCellPtr
    bcc outerLoop
    inc zCellPtr +  1
    clc 
    bcc outerLoop

done:
    rts 

.endproc

;-----------------------------------------------------------------------------
; Set a pattern to run in the cell grid
.proc setupPattern

    lda #ALIVE_NOW                              ; value to set a cell "alive"

	sta patternData + (30 * COLUMNS) + 35       ; make a pattern
	sta patternData + (30 * COLUMNS) + 39
	sta patternData + (30 * COLUMNS) + 40
	sta patternData + (30 * COLUMNS) + 41
	sta patternData + (31 * COLUMNS) + 35
	sta patternData + (31 * COLUMNS) + 36
	sta patternData + (31 * COLUMNS) + 37
	sta patternData + (31 * COLUMNS) + 40
	sta patternData + (32 * COLUMNS) + 36

    rts 

.endproc 

;-----------------------------------------------------------------------------
; Show the pattern set in setupPattren and also init the cell grid with the
; neighbour counts for the pattern
.proc showCanvas

    lda #0                                      ; data0
    sta VERA_CTRL

    lda #$10                                    ; step 1
    sta VERA_ADDR_HI

    lda #0                                      ; vera memory at $00000
    sta VERA_ADDR_LO
    sta VERA_ADDR_MID 

    lda #<(patternData + COLUMNS + 1)           ; point at the cell grid
    sta zCellPtr
    lda #>(patternData + COLUMNS + 1)
    sta zCellPtr + 1

    lda #ROWS-2                                 ; skip the two non-visible rows
    sta zRows

    ldx #CELL_COLOR                             ; color

outerLoop:
    ldy #0                                      ; process columns left to right

loop:
    lda (zCellPtr), y                           ; get the cell
    bit zAliveNowBit                            ; is it alive
    beq notAlive 

    jsr cellAlive                               ; set the neighbour counts
    lda #CELL_ALIVE                             ; VERA alive cell for display
    sta VERA_DATA0                              ; show the cell
    bne color

notAlive:
    lda #CELL_DEAD                              ; VERA dead cell for display
    sta VERA_DATA0                              ; show the dead cell

color:
    stx VERA_DATA0                              ; and set the color

    iny                                         ; go to next cell
    cpy #COLUMNS - 2                            ; do all columns in a row
    bne loop                                    

nextRow:
    dec zRows                                   ; done with a row
    beq done                                    ; all rows done?

    lda #0                                      ; VERA rows start 0x100 bytes apart (aligned)
    sta VERA_ADDR_LO                            ; so set the lo to 0
    inc VERA_ADDR_MID                           ; and inc mid to go to next row

    clc                                         ; move the grid ptr to the next row also
    lda zCellPtr 
    adc #COLUMNS 
    sta zCellPtr
    bcc outerLoop
    inc zCellPtr + 1
    bne outerLoop

done:
    ; jsr GETIN                                 ; See the initial pattern
    ; beq done
    rts 

.endproc 

;-----------------------------------------------------------------------------
; When a cell comes alive, increment the neighbour counts in all of its neighbours
.proc cellAlive

    sty zTempY                                  ; save Y so it can be restored before exit
    
    lda zCellPtr + 1                            ; copy the hi to a temporary
    sta zTempPtr + 1

    clc 
    tya                                         ; normalize the temp ptr by adding the current
    adc zCellPtr                                ; y offset to the grid ptr
    bcc :+
    inc zTempPtr + 1 
:
    sec                                         ; move the temp ptr up a row and left a column
    sbc #(COLUMNS + 1)                          ; so it points at the 1st neighbour (upper left)
    sta zTempPtr
    bcs :+
    dec zTempPtr + 1

:
    clc                                         ; clear carry and it will stay clear throughout
    ldy #0                                      ; 1st neighbour is at offset 0 from temp ptr
    lda (zTempPtr), y                           ; get the cell
    adc #1                                      ; and increment the neighbour count
    sta (zTempPtr), y                           ; and save that

    iny                                         ; cell above the newly alive cell
    lda (zTempPtr), y
    adc #1
    sta (zTempPtr), y

    iny                                         ; upper right
    lda (zTempPtr), y
    adc #1
    sta (zTempPtr), y

    ldy #COLUMNS                                ; cell to the left of newly alive cell

    lda (zTempPtr), y
    adc #1
    sta (zTempPtr), y

    iny                                         ; new cell itself
    lda (zTempPtr), y
    and #NUMBER_PART                            ; start with the neighbour count only
    ora #ALIVE_NOW                              ; and set it alive now
    sta (zTempPtr), y

    iny                                         ; cell to the right of new cell
    lda (zTempPtr), y
    adc #1
    sta (zTempPtr), y

    ldy #(2 * COLUMNS)                          ; lower left

    lda (zTempPtr), y
    adc #1
    sta (zTempPtr), y

    iny                                         ; directly below
    lda (zTempPtr), y
    adc #1
    sta (zTempPtr), y

    iny                                         ; lower right
    lda (zTempPtr), y
    adc #1
    sta (zTempPtr), y

    ldy zTempY                                  ; restore the Y index register

    rts 

.endproc

;-----------------------------------------------------------------------------
; Decrement the neighbour count in all neighbours of the cell dying
; exact inverse of cellAlive
.proc cellDie

    sty zTempY
    
    lda zCellPtr + 1
    sta zTempPtr + 1

    clc 
    tya 
    adc zCellPtr
    bcc :+
    inc zTempPtr + 1 
:
    sec                                         ; carry will stay set
    sbc #(COLUMNS + 1)
    sta zTempPtr
    bcs :+
    dec zTempPtr + 1

:
    sec 
    ldy #0
    lda (zTempPtr), y
    sbc #1
    sta (zTempPtr), y

    iny 
    lda (zTempPtr), y
    sbc #1
    sta (zTempPtr), y

    iny 
    lda (zTempPtr), y
    sbc #1
    sta (zTempPtr), y

    ldy #COLUMNS

    lda (zTempPtr), y
    sbc #1
    sta (zTempPtr), y

    iny 
    lda (zTempPtr), y
    and #NUMBER_PART                            ; strip down to only the neighbours
    sta (zTempPtr), y

    iny 
    lda (zTempPtr), y
    sbc #1
    sta (zTempPtr), y

    ldy #(2 * COLUMNS)

    lda (zTempPtr), y
    sbc #1
    sta (zTempPtr), y

    iny 
    lda (zTempPtr), y
    sbc #1
    sta (zTempPtr), y

    iny 
    lda (zTempPtr), y
    sbc #1
    sta (zTempPtr), y

    ldy zTempY 

    rts 

.endproc

;-----------------------------------------------------------------------------
; For debugging - will show the neigbour counts of cells
.proc showGridNumbers

    lda #<(patternData+COLUMNS+1)               ; start at the 1st visible cell
    sta zCellPtr
    lda #>(patternData+COLUMNS+1)
    sta zCellPtr + 1

    lda #$20
    sta VERA_ADDR_HI                            ; set VERA to step in 2's (skip color)

    lda #0                                      ; for display, need to count up
    sta VERA_ADDR_LO                            ; start at the beginning of screen memory
    sta VERA_ADDR_MID
    sta zRows                                   ; the rows

outer:
    ldy #0                                      ; and need to work from left to right
    clc 

loop:
    lda (zCellPtr), y                           ; get the cell
    beq :+                                      ; if 0 display as a space
    and #NUMBER_PART                            ; get the number of neighbours
    adc #'0'                                    ; turn to a visible digit
    bne num                                     ; JuMP
:
    lda #CELL_DEAD                              ; show as a space if 0 neighbours

num:
    sta VERA_DATA0                              ; show the space or number

next:
    iny                                         ; next column
    cpy #COLUMNS-2                              ; do for all visible columns
    bcc loop 
    
    inc zRows                                   ; move to next row after all columns done
    lda zRows 
    cmp #ROWS-2                                 ; all rows also done?
    bcs done

    lda #0
    sta VERA_ADDR_LO                            ; move VERA to next row
    inc VERA_ADDR_MID

    lda zCellPtr                                ; move the pattern ptr to next row
    clc 
    adc #COLUMNS 
    sta zCellPtr
    bcc outer 
    inc zCellPtr + 1
    bne outer 

done:
    rts 

.endproc


;-----------------------------------------------------------------------------
; 2 pass simulation.
; Pass 1, see who lives and dies
; Pass 2, update neighbour counts and display the pattern
.proc runLife

    lda #<(patternData + COLUMNS + 1)           ; set ptr to 1st visible cell
    sta zCellPtr
    lda #>(patternData + COLUMNS + 1)
    sta zCellPtr + 1

    lda #ROWS-2                                 ; process visible number of rows
    sta zRows

p1_outer:
    ldy #COLUMNS - 2                            ; process visible number of columns

p1_loop:
    lda (zCellPtr), y                           ; get a cell
    beq p1_next                                 ; if no neighbours, move on
    bit zAliveNowBit                            ; is it alove now
    beq p1_notAliveNow                          ; 0 means did not match, so not alive now

    and #NUMBER_PART                            ; it is alive, get the number
    cmp #4                                      ; 4 or bigger, won't be alive next time
    bcs p1_next 
    cmp #2                                      ; 2 or less, also won't be alive
    bcc p1_next 

    ora #(ALIVE_NEXT | ALIVE_NOW)               ; 2 or 3 means alive again next time
    sta (zCellPtr), y                           ; save that info
    bne p1_next                                 ; JuMP

p1_notAliveNow:
    cmp #3                                      ; does the dead cell have exactly 3 neighbours
    bne p1_next                                 ; no, then not coming alive
    ora #ALIVE_NEXT                             ; yes, 3.  So this will be alive next time
    sta (zCellPtr), y                           ; save that

p1_next:
    dey                                         ; next cell
    bpl p1_loop 

    dec zRows                                   ; done a row
    beq p2_start                                ; all rows done

    lda zCellPtr                                ; move to next row in cell grid
    clc 
    adc #COLUMNS
    sta zCellPtr
    bcc p1_outer
    inc zCellPtr + 1
    bne p1_outer                                ; juMP

p2_start:                                       ; second pass starts here

    lda #<(patternData + COLUMNS + 1)           ; point at the 1st visible cell again
    sta zCellPtr
    lda #>(patternData + COLUMNS + 1)
    sta zCellPtr + 1

    lda #0                                      ; for display, need to process rows incrementally
    sta zRows

p2_outer:
    ldy #COLUMNS - 2                            ; start at the last visible colummn (ptr points at 1st)

p2_loop:

    lda (zCellPtr), y                           ; get the cell
    beq p2_next                                 ; if 0 then nothing to do
    bit zAliveNextBit                           ; does it need to come alive?
    beq p2_notAliveNext

    bit zAliveNowBit                            ; yes, needs to come alive.  Is it alive already?
    beq p2_notAliveNow                          ; 0 is no, not alive already

    and #THIS_GEN                               ; strip only the next generation bit
    sta (zCellPtr), y                           ; save this alive now cell
    jmp p2_next                                 ; and move on - no need to draw

p2_notAliveNow:
    jsr cellAlive                               ; dead cell comes alive.  Update neighbours
    ldx #CELL_ALIVE                             ; save the draw code in x register
    bne draw                                    ; and go draw

p2_notAliveNext:
    bit zAliveNowBit                            ; is this cell alive now
    beq p2_next                                 ; if not, then move on
    jsr cellDie                                 ; this cell is now dead.  update neighbours
    ldx #CELL_DEAD                              ; save the draw code in x register

draw:
    tya                                         ; get the column into accumulator
    asl                                         ; VERA is 2x the grid y due to color
    sta VERA_ADDR_LO                            ; set the VERA address
    lda zRows                                   ; rows maps nicely to mid
    sta VERA_ADDR_MID 
    stx VERA_DATA0                              ; and set the cell display "block"

p2_next:
    dey                                         ; next column
    bpl p2_loop 

    inc zRows                                   ; done a row, next row
    lda zRows
    cmp #ROWS-2                                 ; all rows done?
    beq done

    lda zCellPtr                                ; move cell ptr to next row in cell grid
    clc 
    adc #COLUMNS
    sta zCellPtr
    bcc p2_outer
    inc zCellPtr + 1
    bne p2_outer                                ; juMP

done:
    ; jsr showGridNumbers                       ; debugging 
    ; jsr GETIN                                 ; single step
    ; beq done
    jmp runLife                                 ; loop forever

.endproc

;-----------------------------------------------------------------------------
.segment "DATA"

patternData: 
    .res COLUMNS * ROWS 
