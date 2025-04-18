; ===================================================================
; Game subroutines
; ===================================================================

; --------------------------------------------------
; Play beep
; INPUT: 
;   B: Number of toggles
;   C: Delay loop
; --------------------------------------------------
PlayBeep:
    PUSH BC
    LD   A, 16          ; 0001 0000 → beeper ON
.BeepLoop:
    OUT  (254), A       ; accendi
    CALL DelayHalf
    XOR  A              ; A=0 → beeper OFF
    OUT  (254), A       ; spegni
    CALL DelayHalf
    DEC  B
    JR   NZ, .BeepLoop
    POP  BC
    RET

DelayHalf:
    LD   D, C
.DH:
    DEC  D
    JR   NZ, .DH
    RET

; --------------------------------------------------
; Line completed sound
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
; --------------------------------------------------
GameOverSound:
    ; Primo beep grave, lungo
    ld   b, 80      ; durata: 80 toggles
    ld   c, 160     ; frequenza: delay lungo → tono molto grave
    call PlayBeep

    ; Secondo beep medio, più corto
    ld   b, 60      ; durata: 60 toggles
    ld   c, 100     ; frequenza: delay medio → tono intermedio
    call PlayBeep

    ; Terzo beep acuto, breve
    ld   b, 40      ; durata: 40 toggles
    ld   c,  40     ; frequenza: delay breve → tono acuto
    call PlayBeep

    ret
; --------------------------------------------------
; Line completed sound
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
; --------------------------------------------------
LineCompleteSound:
    LD   B,  40         ; durata: 40 toggles
    LD   C,  15         ; frequenza: delay breve → tono acuto
    CALL PlayBeep
    RET

; --------------------------------------------------
; Bkloc land sound
; --------------------------------------------------
BlockLandSound:
    LD   b,  60         ; durata: 60 toggles
    LD   c, 120         ; frequenza: delay lungo → tono grave
    CALL PlayBeep
    RET
;--------------------------------------------------
; Play menu music
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;--------------------------------------------------
PlayInGameMusic:
        ; The next section of code plays a note of the in-game music.

    LD A,(MusicNoteIndex)                    ; Increment the in-game music note index.
    INC A
    LD (MusicNoteIndex),A
    AND 126
    RRCA
    LD E,A
    LD D,$00
    LD HL,InGameTuneData                     ; Point HL at the appropriate entry in the tune data table at
    ADD HL,DE                                ; InGameTuneData.
    LD A,(0x20)                       ; Pick up the border colour for the current cavern.
    LD E,(HL)                                ; Initialise the pitch delay counter in E.
    LD BC,$03                                ; Initialise the duration delay counters in B (0) and C (3).
PlayInGameMusic12:                 
    OUT (254),A                              ; Produce a note of the in-game music.
    DEC E
    JR NZ,PlayInGameMusic13
    LD E,(HL)
    XOR 24
PlayInGameMusic13:                        
    DJNZ PlayInGameMusic12
    DEC C
    JR NZ,PlayInGameMusic12
    RET

;---------------------------------------------------------------------
; Fill score area
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;---------------------------------------------------------------------
FillScoreArea:
    LD      A, (CurrentMatrixOrientation)
    CP      ORIENT_EAST
    JP      Z, FillScoreAreaEast
    CP      ORIENT_WEST
    JP      Z, FillScoreAreaWest
    CP      ORIENT_NORTH
    JP      Z, FillScoreAreaNorth
FillScoreAreaSouth:
    LD      HL, TEXT_SCORE
    LD      D, 2
    LD      E, 26
    CALL    Screen_PrintString

    LD      HL, TEXT_HIGH_SCORE_1
    LD      D, 6
    LD      E, 26
    CALL    Screen_PrintString
    LD      HL, TEXT_HIGH_SCORE_2
    LD      D, 7
    LD      E, 26
    CALL    Screen_PrintString


    LD      HL, TEXT_BLOCKS
    LD      D, 11
    LD      E, 26
    CALL    Screen_PrintString

    LD      HL, TEXT_MAX_BLOCKS
    LD      D, 15
    LD      E, 26
    CALL    Screen_PrintString
    LD      HL, TEXT_BLOCKS
    LD      D, 16
    LD      E, 26
    CALL    Screen_PrintString

    LD      HL, TEXT_LEVEL
    LD      D, 20
    LD      E, 26
    CALL    Screen_PrintString

    CALL    ScoreUpdate
    RET
FillScoreAreaEast:
    LD      HL, TEXT_SCORE
    LD      D, 1
    LD      E, 1
    CALL    Screen_PrintString
    LD      HL, TEXT_EMPTY5
    LD      D, 2
    LD      E, 1
    CALL    Screen_PrintString


    LD      HL, TEXT_HIGH_SCORE_1
    LD      D, 1
    LD      E, 10
    CALL    Screen_PrintString
    LD      HL, TEXT_HIGH_SCORE_2
    LD      D, 2
    LD      E, 10
    CALL    Screen_PrintString


    LD      HL, TEXT_BLOCKS
    LD      D, 1
    LD      E, 18
    CALL    Screen_PrintString
    LD      HL, TEXT_EMPTY5
    LD      D, 2
    LD      E, 18
    CALL    Screen_PrintString

    LD      HL, TEXT_MAX_BLOCKS
    LD      D, 1
    LD      E, 26
    CALL    Screen_PrintString
    LD      HL, TEXT_BLOCKS
    LD      D, 2
    LD      E, 26
    CALL    Screen_PrintString

    LD      HL, TEXT_LEVEL
    LD      D, 21
    LD      E, 1
    CALL    Screen_PrintString

    CALL    ScoreUpdate
    RET
FillScoreAreaWest:
    LD      HL, TEXT_SCORE
    LD      D, 20
    LD      E, 1
    CALL    Screen_PrintString
    LD      HL, TEXT_EMPTY5
    LD      D, 21
    LD      E, 1
    CALL    Screen_PrintString


    LD      HL, TEXT_HIGH_SCORE_1
    LD      D, 20
    LD      E, 10
    CALL    Screen_PrintString
    LD      HL, TEXT_HIGH_SCORE_2
    LD      D, 21
    LD      E, 10
    CALL    Screen_PrintString


    LD      HL, TEXT_BLOCKS
    LD      D, 20
    LD      E, 18
    CALL    Screen_PrintString
    LD      HL, TEXT_EMPTY5
    LD      D, 21
    LD      E, 18
    CALL    Screen_PrintString

    LD      HL, TEXT_MAX_BLOCKS
    LD      D, 20
    LD      E, 26
    CALL    Screen_PrintString
    LD      HL, TEXT_BLOCKS
    LD      D, 21
    LD      E, 26
    CALL    Screen_PrintString

    LD      HL, TEXT_LEVEL
    LD      D, 1
    LD      E, 26
    CALL    Screen_PrintString

    CALL    ScoreUpdate
    RET
FillScoreAreaNorth:
    LD      HL, TEXT_SCORE
    LD      D, 2
    LD      E, 1
    CALL    Screen_PrintString

    LD      HL, TEXT_HIGH_SCORE_1
    LD      D, 6
    LD      E, 1
    CALL    Screen_PrintString
    LD      HL, TEXT_HIGH_SCORE_2
    LD      D, 7
    LD      E, 1
    CALL    Screen_PrintString


    LD      HL, TEXT_BLOCKS
    LD      D, 11
    LD      E, 1
    CALL    Screen_PrintString

    LD      HL, TEXT_MAX_BLOCKS
    LD      D, 15
    LD      E, 1
    CALL    Screen_PrintString
    LD      HL, TEXT_BLOCKS
    LD      D, 16
    LD      E, 1
    CALL    Screen_PrintString

    LD      HL, TEXT_LEVEL
    LD      D, 20
    LD      E, 1
    CALL    Screen_PrintString

    CALL    ScoreUpdate
    RET

;---------------------------------------------------------------------
; Display score update
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;---------------------------------------------------------------------
DisplayScoreUpdate:
    LD      A, (CurrentMatrixOrientation)
    CP      ORIENT_EAST
    JP      Z, DisplayScoreUpdateEast
    CP      ORIENT_WEST
    JP      Z, DisplayScoreUpdateWest
    CP      ORIENT_NORTH
    JP      Z, DisplayScoreUpdateNorth

DisplayScoreUpdateSouth:
    LD      HL, (Score)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 3
    LD      E, 26
    CALL    Screen_PrintString

    LD      HL, (HighScore)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 8
    LD      E, 26
    CALL    Screen_PrintString


    LD      HL, (PlacedBlocks)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 12
    LD      E, 26
    CALL    Screen_PrintString

    LD      HL, (MaxPlacedBlocks)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 1
    LD      E, 26
    CALL    Screen_PrintString

    LD      A, (Level)
    LD      H, 0
    LD      L, A
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 21
    LD      E, 26
    CALL    Screen_PrintString
    RET
DisplayScoreUpdateNorth:
    LD      HL, (Score)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 3
    LD      E, 1
    CALL    Screen_PrintString

    LD      HL, (HighScore)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 8
    LD      E, 1
    CALL    Screen_PrintString


    LD      HL, (PlacedBlocks)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 12
    LD      E, 1
    CALL    Screen_PrintString

    LD      HL, (MaxPlacedBlocks)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 17
    LD      E, 1
    CALL    Screen_PrintString

    LD      A, (Level)
    LD      H, 0
    LD      L, A
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 21
    LD      E, 1
    CALL    Screen_PrintString

    RET
DisplayScoreUpdateEast:
    LD      HL, (Score)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 3
    LD      E, 1
    CALL    Screen_PrintString

    LD      HL, (HighScore)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 3
    LD      E, 10
    CALL    Screen_PrintString


    LD      HL, (PlacedBlocks)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 3
    LD      E, 18
    CALL    Screen_PrintString

    LD      HL, (MaxPlacedBlocks)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 3
    LD      E, 26
    CALL    Screen_PrintString

    LD      A, (Level)
    LD      H, 0
    LD      L, A
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 22
    LD      E, 1
    CALL    Screen_PrintString
    RET
DisplayScoreUpdateWest:
    LD      HL, (Score)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 22
    LD      E, 1
    CALL    Screen_PrintString

    LD      HL, (HighScore)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 22
    LD      E, 10
    CALL    Screen_PrintString


    LD      HL, (PlacedBlocks)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 22
    LD      E, 18
    CALL    Screen_PrintString

    LD      HL, (MaxPlacedBlocks)
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 22
    LD      E, 26
    CALL    Screen_PrintString

    LD      A, (Level)
    LD      H, 0
    LD      L, A
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      HL, NumberValue
    LD      D, 2
    LD      E, 26
    CALL    Screen_PrintString

    RET

;---------------------------------------------------------------------
; Score update
; INPUT:
;   A: Amount of completed rows
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;---------------------------------------------------------------------
ScoreUpdate:
    CP      0
    RET     Z
    CALL    LineCompleteSound
    LD      C, 0    ; Score to add
    LD      B, A    ; B = number of completed rows
    CP      1
    JR      NZ, .Continue1
    LD      C, 10
    JR      .Continue4
.Continue1:
    CP      2
    JR      NZ, .Continue2
    LD      C, 22
    JR      .Continue4
.Continue2:
    CP      3
    JR      NZ, .Continue3
    LD      C, 35
    JR      .Continue4
.Continue3:
    LD      C, 50
.Continue4:
    LD      HL, (Score)
    LD      A, C
    CALL    Math_AddAToHL
    LD      (Score), HL


    LD      HL, (Score)
    LD      BC, (HighScore)
    CALL    Math_CompareHLtoBC
    CP      1
    RET     NZ
    LD      (HighScore), HL  ; Set the maximum number of blocks to the current number of blocks      
    RET

;--------------------------------------------------------------------------
; Compare and update DE and HL (Update HL if DE > HL)
; INPUT: 
;   DE: First number (16-bit)
;   HL: Second number (16-bit)
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;--------------------------------------------------------------------------
CompareAndUpdateHLAndDE:
    LD   A, D            ; Load the high byte of DE into A.
    CP   H               ; Compare A with the high byte of HL.
    JR   Z, CompareLow   ; If equal, move to compare the low bytes.
    JR   C, SkipUpdate   ; If A < H (i.e. D < H), then DE < HL; do not update.
    ; If we reach here, D > H, so DE > HL.
UpdateHL:
    EX   DE, HL         ; Swap DE and HL so that HL becomes DE.
    RET

CompareLow:
    LD   A, E           ; Load the low byte of DE into A.
    CP   L              ; Compare A with the low byte of HL.
    JR   NC, SkipUpdate  ; If A <= L then DE <= HL; no update.
    JR   UpdateHL       ; Otherwise (A > L), update HL = DE.

SkipUpdate:
    RET



;------------------------------------------------------------------------
; Reset next block position
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;------------------------------------------------------------------------
SetNextBoxPosition:
    LD      A, (Bug)
    LD      A, (CurrentMatrixOrientation)
    CP      ORIENT_NORTH
    JP      Z, SetNextBoxPositionNorth
    CP      ORIENT_SOUTH
    JP      Z, SetNextBoxPositionSouth
    CP      ORIENT_EAST
    JP      Z, SetNextBoxPositionEast

SetNextBoxPositionWest:
    LD      A, 2
    LD      (NextBlockYPosition), A
    LD      A, (NextBlock)
    CP      BLOCK_O
    JR      NZ, SetNextBoxPositionWestContinue1
    LD      A, 3
    LD      (NextBlockYPosition), A
SetNextBoxPositionWestContinue1:
    LD      A, (NextBlock)
    CP      BLOCK_I
    JR      NZ, SetNextBoxPositionWestContinue2
    LD      A, 1
    LD      (NextBlockYPosition), A    
SetNextBoxPositionWestContinue2:
    LD      A, 7
    LD      (NextBlockXPosition), A
    RET
SetNextBoxPositionNorth:
    LD      A, 23
    LD      (NextBlockXPosition), A
    LD      A, 3
    LD      (NextBlockYPosition), A
    RET
SetNextBoxPositionSouth:
    LD      A, 6
    LD      (NextBlockXPosition), A
    LD      A, (NextBlock)
    CP      BLOCK_O
    JR      NZ, SetNextBoxPositionSouthContinue1
    LD      A, 7
    LD      (NextBlockXPosition), A
SetNextBoxPositionSouthContinue1:
    LD      A, (NextBlock)
    CP      BLOCK_I
    JR      NZ, SetNextBoxPositionSouthContinue2
    LD      A, 5
    LD      (NextBlockXPosition), A
    LD      A, 20
    LD      (NextBlockYPosition), A
    RET
SetNextBoxPositionSouthContinue2:
    LD      A, 19
    LD      (NextBlockYPosition), A
    RET
SetNextBoxPositionEast:
    LD      A, 23
    LD      (NextBlockXPosition), A
    LD      A, (NextBlock)
    CP      BLOCK_I
    JR      NZ, SetNextBoxPositionEastContinue
    LD      A, 24
    LD      (NextBlockXPosition), A
SetNextBoxPositionEastContinue:

    LD      A, 19
    LD      (NextBlockYPosition), A
    RET

;------------------------------------------------------------------------
; Update next block info
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;------------------------------------------------------------------------
UpdateNextBlockInfo:
    LD      A, (CurrentMatrixOrientation)
    CP      ORIENT_NORTH
    JP      Z, UpdateNextBlockInfoNorth
    CP      ORIENT_SOUTH
    JP      Z, UpdateNextBlockInfoSouth
    CP      ORIENT_EAST
    JP      Z, UpdateNextBlockInfoEast
    CP      ORIENT_WEST
    JP      Z, UpdateNextBlockInfoWest
UpdateNextBlockInfoNorth:
    XOR     A
    LD      (CurrentBlockRotation), A
    JR      UpdateNextBlockInfoContinue
UpdateNextBlockInfoSouth:
    LD      A, 2
    LD      (CurrentBlockRotation), A
    JR      UpdateNextBlockInfoContinue
UpdateNextBlockInfoEast:
    LD      A, 1
    LD      (CurrentBlockRotation), A
    JR      UpdateNextBlockInfoContinue
UpdateNextBlockInfoWest:
    LD      A, 3
    LD      (CurrentBlockRotation), A
UpdateNextBlockInfoContinue:
    LD      A, (NextBlock) 
    LD      C, A
    LD      A, (CurrentBlock)
    LD      B, A
    LD      A, C
    LD      (CurrentBlock), A
    LD      A, (ColorOfBlock)
    LD      E, A
    PUSH    DE
    PUSH    BC
   
    CALL    UpdateCurrentBlockTable

    LD      A, (ColorOfBlock)
    LD      (NextBlockColor), A
    LD      (NextBlockTable), HL

    POP     BC
    POP     DE
    LD      A, E
    LD      (ColorOfBlock), A
    LD      A, C
    LD      (NextBlock), A
    LD      A, B
    LD      (CurrentBlock), A
    RET
;------------------------------------------------------------------------
; Show next block
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;------------------------------------------------------------------------
ShowNextBlockOnBox:
    CALL    ClearNextBlockArea
    LD      HL, (NextBlockTable)
ShowNextBlockLoop:
    LD      A, (HL)
    CP      0xFF
    JR      Z, ShowNextBlockEnd
    LD      A, (NextBlockYPosition)
    LD      B, A
    LD      A, (HL)
    ADD     B
    LD      D, A
    INC     HL
    LD      A, (NextBlockXPosition)
    LD      B, A
    LD      A, (HL)
    ADD     B
    LD      E, A
    PUSH    HL
    LD      HL, NextBlockColor
    LD      A, (HL)
    CALL    SetAttributeAtDE
    LD      A, TILE_BLOCK
    CALL    Screen_PrintRamChar
    POP     HL
    INC     HL
    JR      ShowNextBlockLoop
ShowNextBlockEnd:
    RET
;------------------------------------------------------------------------
; Draw menu title
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;------------------------------------------------------------------------
DrawMenuTitle:
    LD   HL, ZXETRISMATRIX  ; Punto di partenza della matrice
    LD   D, 1               ; Riga = 0

NextRow:
    LD   E, 3               ; Colonna = 0

NextColumn:
    LD   A, (HL)            ; Legge il byte corrente
    CP   0
    JR   Z, SkipSet         ; Se zero, non chiamare SetAttributeAtDE

    ; Valore ≠ 0: esegue la subroutine
    PUSH HL                 ; Salva HL prima della chiamata
    CALL SetAttributeAtDE   ; A = valore, D = riga, E = colonna
    POP  HL                 ; Ripristina HL

SkipSet:
    INC  HL                 ; Prossimo byte
    INC  E                  ; Prossima colonna
    LD   A, E
    CP   28
    JR   NZ, NextColumn     ; Continua finché E < 25

    INC  D                  ; Riga successiva
    LD   A, D
    CP   8
    JR   NZ, NextRow        ; Continua finché D < 7

    RET
;------------------------------------------------------------------------
; Initialize matrix
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;------------------------------------------------------------------------
MatrixInit:
    LD      HL,Matrix       ; HL -> start of the area to fill
    LD      DE,200          ; number of bytes to fill
MatrixInitLoop:
    LD      (HL),COLOR_BLACK          ; store A into (HL)
    INC     HL              ; move to next byte
    DEC     DE              ; decrement counter
    LD      A,D             ; check if DE = 0
    OR      E
    JP      NZ,MatrixInitLoop 
    LD      A,1
    LD      (ForcePrintMatrixRepaintAll), A
    RET

;------------------------------------------------------------------------
; Show game menu
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;------------------------------------------------------------------------
ShowMenu:
    CALL    MatrixInit
    XOR     A
    LD      (InGame), A
    DI                                      ; Disable interrupts
    IM      2                               ; Set the interrupt mode
    EI                                      ; Enable interrupts
    CALL    Screen_Clear            ; Clear screen
    LD      A, TILE_VERTICAL_WALL
    LD      (CurrentWallTile), A
    CALL    FillWallArea
    CALL    FillOptionsArea
    LD      A, 10
    LD      (SelectedOption), A
    CALL    UpdateOptions
    LD      HL, TEXT_COPYRIGHT
    LD      D, 22
    LD      E, 7
    CALL    Screen_PrintString
    LD      HL, TEXT_COMMANDS
    LD      D, 17
    LD      E, 3
    CALL    Screen_PrintString
    LD      HL, TEXT_COMMANDS_SPACE
    LD      D, 18
    LD      E, 8
    CALL    Screen_PrintString
    LD      HL, TEXT_COMMANDS_SPACE_TO_START
    LD      D, 20
    LD      E, 6
    CALL    Screen_PrintString
    LD      A, 6
    LD      D, 20
    LD      B, 0xC7
ShowMenuBlinkingLoop:
    CP      26
    JR      Z, ShowMenuKeyboardLoop
    PUSH    AF
    LD      E, A
    LD      A, B
    CALL    SetAttributeAtDE
    POP     AF
    INC     A
    JR      ShowMenuBlinkingLoop
ShowMenuKeyboardLoop:
    LD      A, (TitleTimer)
    CP      250
    JR      NZ, ShowMenuKeyboardLoopContinue
    CALL    RandomizeMatrixBlocks
    CALL    DrawMenuTitle
    XOR     A
    LD      (TitleTimer), A
ShowMenuKeyboardLoopContinue:
    LD      A, (TitleTimer)
    INC     A
    LD      (TitleTimer), A
    CALL    ReadKeyboard
    LD      A, (KeyPressed)
    CP      32
    JP      Z, ShowMenuExit
    CP      52
    JP      Z, ChangeCurrentOptionValueLeft
    CP      53
    JP      Z, ShowMenuKeyboardChangeSelectedOption
    CP      54
    JP      Z, ChangeCurrentOptionValueRight
    LD      A, (Seed)
    CP      255
    JR      NZ, ShowMenuKeyboardLoopSeed
    XOR     A
ShowMenuKeyboardLoopSeed:
    INC     A
    LD      (Seed), A
    JR      ShowMenuKeyboardLoop
ShowMenuExit:
    DI                                      ; Disable interrupts
    IM      1                               ; Set the interrupt mode
    EI                                      ; Enable interrupts
    RET
ChangeCurrentOptionValueLeft:
    XOR     A
    LD      (KeyPressed), A
    LD      A, (SelectedOption)
    CP      10
    JR      Z, ChangeCurrentOptionValueLeftLevel
    CP      12
    JR      Z, ChangeCurrentOptionValueLeftCanGrow
    CP      14
    JR      Z, ChangeCurrentOptionValueLeftOrientation
ChangeCurrentOptionValueLeftLevel:
    LD      A, (SelectedLevel)
    DEC     A
    CP      0
    JP      Z, ShowMenuKeyboardLoop
    LD      (SelectedLevel), A
    CALL    UpdateOptions
    JP      ShowMenuKeyboardLoop

ChangeCurrentOptionValueLeftCanGrow:
    XOR     A
    LD      (SelectedCanGrow), A
    CALL    UpdateOptions
    JP      ShowMenuKeyboardLoop
ChangeCurrentOptionValueLeftOrientation:
    LD      A, (SelectedOrientation)
    CP      0
    JP      Z, ShowMenuKeyboardLoop
    DEC     A
    LD      (SelectedOrientation), A
    CALL    UpdateOptions
    JP      ShowMenuKeyboardLoop




ChangeCurrentOptionValueRight:
    XOR     A
    LD      (KeyPressed), A
    LD      A, (SelectedOption)
    CP      10
    JR      Z, ChangeCurrentOptionValueRightLevel
    CP      12
    JR      Z, ChangeCurrentOptionValueRightCanGrow
    CP      14
    JR      Z, ChangeCurrentOptionValueRightOrientation
ChangeCurrentOptionValueRightLevel:
    LD      A, (SelectedLevel)
    INC     A
    CP      11
    JP      Z, ShowMenuKeyboardLoop
    LD      (SelectedLevel), A
    CALL    UpdateOptions
    JP      ShowMenuKeyboardLoop

ChangeCurrentOptionValueRightCanGrow:
    LD      A, 1
    LD      (SelectedCanGrow), A
    CALL    UpdateOptions
    JP      ShowMenuKeyboardLoop
ChangeCurrentOptionValueRightOrientation:
    LD      A, (SelectedOrientation)
    INC     A
    CP      5
    JP      Z, ShowMenuKeyboardLoop
    LD      (SelectedOrientation), A
    CALL    UpdateOptions
    JP      ShowMenuKeyboardLoop

ShowMenuKeyboardChangeSelectedOption:
    XOR     A
    LD      (KeyPressed), A
    LD      A, (SelectedOption)
    INC     A
    INC     A
    CP      16
    JR      NZ, ShowMenuKeyboardChangeRefreshScreen
    LD      A, 10
ShowMenuKeyboardChangeRefreshScreen:
    LD      (SelectedOption), A
    CALL    UpdateOptions
    JP      ShowMenuKeyboardLoop
ShowMenuGameStart:
    LD      A, ORIENT_NORTH  ; ****************  DIRECTION  ****************
    LD      (CurrentMatrixOrientation), A  ; Set the orientation to North

    RET
;----------------------------------------------------------------------
; Update options
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;----------------------------------------------------------------------
UpdateOptions:
    LD      D, 10
    LD      E, 23
    LD      HL, TEXT_SPACE
    CALL    Screen_PrintString
    LD      A, (SelectedLevel)
    LD      H, 0
    LD      L, A
    LD      DE, NumberValue
    CALL    String_NumberToASCII
    LD      HL, NumberValue
    CALL    String_RemoveLeadingZeros
    LD      D, 10
    LD      E, 24
    LD      B, 0
    LD      A, (SelectedOption)
    CP      10
    JR      NZ, UpdateOptionsNoYello1
    LD      B, 1
UpdateOptionsNoYello1:

    LD      A, (SelectedLevel)
    CP      10
    JR      NZ, UpdateOptionsNoYello1_1
    LD      E, 23
UpdateOptionsNoYello1_1:
    CALL    Screen_PrintString
    LD      HL, TEXT_YES
    LD      A, (SelectedCanGrow)
    CP      1
    JR      Z, UpdateOptionsContinue1
    LD      HL, TEXT_NO

UpdateOptionsContinue1:
    LD      D, 12
    LD      E, 22
    LD      B, 0
    LD      A, (SelectedOption)
    CP      12
    JR      NZ, UpdateOptionsNoYello2
    LD      B, 1
UpdateOptionsNoYello2:
    CALL    Screen_PrintString
    LD      A, (SelectedOrientation)
    CP      ORIENT_NORTH
    JR      Z, UpdateOptionsContinueNorth
    CP      ORIENT_SOUTH
    JR      Z, UpdateOptionsContinueSouth
    CP      ORIENT_EAST
    JR      Z, UpdateOptionsContinueEast
    CP      ORIENT_WEST
    JR      Z, UpdateOptionsContinueWest
    LD      HL, TEXT_CHAOS
    JR      UpdateOptionsContinue2:
UpdateOptionsContinueNorth:
    LD      HL, TEXT_NORTH
    JR      UpdateOptionsContinue2
UpdateOptionsContinueSouth:
    LD      HL, TEXT_SOUTH
    JR      UpdateOptionsContinue2
UpdateOptionsContinueEast:
    LD      HL, TEXT_EAST
    JR      UpdateOptionsContinue2
UpdateOptionsContinueWest:
    LD      HL, TEXT_WEST
    JR      UpdateOptionsContinue2
UpdateOptionsContinue2:
    LD      D, 14
    LD      E, 20
    LD      B, 0
    LD      A, (SelectedOption)
    CP      14
    JR      NZ, UpdateOptionsNoYello3
    LD      B, 1
UpdateOptionsNoYello3:
    CALL    Screen_PrintString
    RET
;----------------------------------------------------------------------
; Wait for enter key press
; INPUT: -
; OUTPUT: - 
; MODIFY: AF, DE, HL, BC
;----------------------------------------------------------------------
WaitForSpaceKey:
    CALL    ReadKeyboard
    LD      A, (KeyPressed)
    CP      32
    JR      NZ, WaitForSpaceKey
    LD      A, 0
    LD      (KeyPressed), A
    RET
;------------------------------------------------------------------------
; Fill next block area (Bottom Right)
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;-------------------------------------------------------------------------
FillNextBlockAreaBottomRight:
    LD      A, 18
FillNextBlockAreaBottomRightRowLoop:
    CP      23
    RET     Z
    PUSH    AF
    LD      D, A
    LD      A, 22
FillNextBlockAreaBottomRightColLoop:
    CP      26
    JR      Z, FillNextBlockAreaBottomRightRowLoopContinue
    PUSH    AF
    LD      E, A

    LD      A, COLOR_BLACK
    CALL    SetAttributeAtDE
    LD      A, TILE_BLOCK_EMPTY
    CALL    Screen_PrintRamChar

    POP     AF
    INC     A
    JR      FillNextBlockAreaBottomRightColLoop
FillNextBlockAreaBottomRightRowLoopContinue:
    POP     AF
    INC     A
    JP      FillNextBlockAreaBottomRightRowLoop




;------------------------------------------------------------------------
; Fill next block area (Top Left)
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;-------------------------------------------------------------------------
FillNextBlockAreaTopLeft:
    LD      A, 1
FillNextBlockAreaTopLeftRowLoop:
    CP      6
    RET     Z
    PUSH    AF
    LD      D, A
    LD      A, 6
FillNextBlockAreaTopLeftColLoop:
    CP      10
    JR      Z, FillNextBlockAreaTopLeftRowLoopContinue
    PUSH    AF
    LD      E, A

    LD      A, COLOR_BLACK
    CALL    SetAttributeAtDE
    LD      A, TILE_BLOCK_EMPTY
    CALL    Screen_PrintRamChar

    POP     AF
    INC     A
    JR      FillNextBlockAreaTopLeftColLoop
FillNextBlockAreaTopLeftRowLoopContinue:
    POP     AF
    INC     A
    JP      FillNextBlockAreaTopLeftRowLoop


;------------------------------------------------------------------------
; Fill next block area (Top Right)
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;-------------------------------------------------------------------------
FillNextBlockAreaTopRight:
    LD      A, 2
FillNextBlockAreaTopRightRowLoop:
    CP      6
    RET     Z
    PUSH    AF
    LD      D, A
    LD      A, 22
FillNextBlockAreaTopRightColLoop:
    CP      27
    JR      Z, FillNextBlockAreaTopRightRowLoopContinue
    PUSH    AF
    LD      E, A

    LD      A, COLOR_BLACK
    CALL    SetAttributeAtDE
    LD      A, TILE_BLOCK_EMPTY
    CALL    Screen_PrintRamChar

    POP     AF
    INC     A
    JR      FillNextBlockAreaTopRightColLoop
FillNextBlockAreaTopRightRowLoopContinue:
    POP     AF
    INC     A
    JP      FillNextBlockAreaTopRightRowLoop
;------------------------------------------------------------------------
; Fill next block area (Bottom Left)
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;-------------------------------------------------------------------------
FillNextBlockAreaBottomLeft:
    LD      A, 18
FillNextBlockAreaBottomLeftRowLoop:
    CP      22
    RET     Z
    PUSH    AF
    LD      D, A
    LD      A, 5
FillNextBlockAreaBottomLeftColLoop:
    CP      10
    JP      Z, FillNextBlockAreaBottomLeftRowLoopContinue
    PUSH    AF
    LD      E, A

    LD      A, COLOR_BLACK
    CALL    SetAttributeAtDE
    LD      A, TILE_BLOCK_EMPTY
    CALL    Screen_PrintRamChar

    POP     AF
    INC     A
    JR      FillNextBlockAreaBottomLeftColLoop
FillNextBlockAreaBottomLeftRowLoopContinue:
    POP     AF
    INC     A
    JR      FillNextBlockAreaBottomLeftRowLoop





;------------------------------------------------------------------------
; Fill options area 
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;-------------------------------------------------------------------------
FillOptionsArea:
    LD      E, 0
    LD      D, 0
    LD      A, 9
FillOptionsAreaRowLoop:
    CP      16
    JP      Z, FillOptionsAreaContinue
    PUSH    AF
    LD      D, A
    LD      A, 5
FillOptionsAreaColLoop:
    CP      26
    JR      Z, FillOptionsAreaRowLoopContinue
    PUSH    AF
    LD      E, A


    LD      A, COLOR_BLACK
    CALL    SetAttributeAtDE
    LD      A, TILE_BLOCK_EMPTY
    CALL    Screen_PrintRamChar


    POP     AF
    INC     A
    JR      FillOptionsAreaColLoop
FillOptionsAreaRowLoopContinue:
    POP     AF
    INC     A
    JR      FillOptionsAreaRowLoop
FillOptionsAreaContinue:
    LD      HL, TEXT_START_LEVEL
    LD      D, 10
    LD      E, 6
    CALL    Screen_PrintString

    LD      HL, TEXT_LEVEL_GROWING
    LD      D, 12
    LD      E, 6
    CALL    Screen_PrintString

    LD      HL, TEXT_ORIENTATION
    LD      D, 14
    LD      E, 6
    CALL    Screen_PrintString
    RET

;------------------------------------------------------------------------
; Fill game area (Horizontal)
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;-------------------------------------------------------------------------
FillGameAreaH:
    LD      E, 0
    LD      D, 0
    LD      A, 7
FillGameAreaHRowLoop:
    CP      17
    RET     Z
    PUSH    AF
    LD      D, A
    LD      A, 6
FillGameAreaHColLoop:
    CP      26
    JR      Z, FillGameAreaHRowLoopContinue
    PUSH    AF
    LD      E, A


    LD      A, COLOR_BLACK
    CALL    SetAttributeAtDE
    LD      A, TILE_BLOCK_EMPTY
    CALL    Screen_PrintRamChar


    POP     AF
    INC     A
    JR      FillGameAreaHColLoop
FillGameAreaHRowLoopContinue:
    POP     AF
    INC     A
    JR      FillGameAreaHRowLoop

;------------------------------------------------------------------------
; Fill game area (Vertical)
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;-------------------------------------------------------------------------
FillGameAreaV:
    LD      E, 0
    LD      D, 0
    LD      A, 2
FillGameAreaVRowLoop:
    CP      22
    RET     Z
    PUSH    AF
    LD      D, A
    LD      A, 11
FillGameAreaVColLoop:
    CP      21
    JR      Z, FillGameAreaVRowLoopContinue
    PUSH    AF
    LD      E, A


    LD      A, COLOR_BLACK
    CALL    SetAttributeAtDE
    LD      A, TILE_BLOCK_EMPTY
    CALL    Screen_PrintRamChar


    POP     AF
    INC     A
    JR      FillGameAreaVColLoop
FillGameAreaVRowLoopContinue:
    POP     AF
    INC     A
    JR      FillGameAreaVRowLoop

;------------------------------------------------------------------------
; Fill an area with a tile
; INPUT: -
; OUTPUT: -
; MODIFY: AF, DE, HL, BC
;-------------------------------------------------------------------------
FillWallArea:
    LD      A, (CurrentWallTile)
    LD      B, A
    LD      E, 0
    LD      D, 0
    XOR     A
FillAreaWithTileRowLoop:
    CP      24
    RET     Z
    PUSH    AF
    LD      D, A
    XOR     A
FillAreaWithTileColLoop:
    CP      32
    JR      Z, FillAreaWithTileRowLoopContinue
    PUSH    AF
    LD      E, A

    PUSH    DE
    LD      A, 0x57
    CALL    SetAttributeAtDE
    POP     DE
    PUSH    DE
    LD      A, B
    CALL    Screen_PrintRamChar
    POP     DE

    POP     AF
    INC     A
    JR      FillAreaWithTileColLoop
FillAreaWithTileRowLoopContinue:
    POP     AF
    INC     A
    JR      FillAreaWithTileRowLoop






;----------------------------------------------------------------------
;  Show game field
;  INPUT: -
;  OUTPUT: -
;  MODIFY: AF, BC, DE, HL
;----------------------------------------------------------------------
ShowGameField:
    LD      A, (CurrentMatrixOrientation)
    EX      AF, AF'                 ; Save AF in AF'
    CALL    Screen_Clear            ; Clear screen
    EX      AF, AF'                 ; Restore AF
    CP      ORIENT_EAST
    JP      Z, ShowGameFieldHorizontal
    CP      ORIENT_WEST
    JP      Z, ShowGameFieldHorizontal
    CP      ORIENT_NORTH
    JP      Z, ShowGameFieldVertical
    CP      ORIENT_SOUTH
    JP      Z, ShowGameFieldVertical
    RET
ShowGameFieldVertical:
    LD      A, TILE_VERTICAL_WALL
    LD      (CurrentWallTile), A
    CALL    FillWallArea
    CALL    FillGameAreaV
    CALL    ClearNextBlockArea
    CALL    FillScoreArea
    RET
ShowGameFieldHorizontal:
    LD      A, TILE_HORIZONTAL_WALL
    LD      (CurrentWallTile), A
    CALL    FillWallArea
    CALL    FillGameAreaH
    CALL    ClearNextBlockArea
    CALL    FillScoreArea
    RET

;----------------------------------------------------------------------
;  Clear next block area
;  INPUT: -
;  OUTPUT: -
;  MODIFY: AF, BC, DE, HL
;----------------------------------------------------------------------
ClearNextBlockArea:
    LD      A, (CurrentMatrixOrientation)
    CP      ORIENT_EAST
    JP      Z, ClearNextBlockAreaEast
    CP      ORIENT_WEST
    JP      Z, ClearNextBlockAreaWest
    CP      ORIENT_NORTH
    JP      Z, ClearNextBlockAreaNorth
    CALL    FillNextBlockAreaBottomLeft
    RET
ClearNextBlockAreaNorth:
    CALL    FillNextBlockAreaTopRight
    RET
ClearNextBlockAreaEast:
    CALL    FillNextBlockAreaBottomRight
    RET
ClearNextBlockAreaWest:
    CALL    FillNextBlockAreaTopLeft
    RET
;----------------------------------------------------------------------
;  Print matrix
;  INPUT: 
;    A: 250 Refresh all matrix
;  OUTPUT: -
;  MODIFY: AF, BC, DE, HL
;----------------------------------------------------------------------
PrintMatrix:
    LD      A, (ForcePrintMatrixRepaintAll)
    CP      1
    JR      NZ, PrintMatrixSetLimits
    LD      A, 0
    LD      (ForcePrintMatrixRepaintAll), A
    LD      A, 20
    LD      (PrintMatrixMaxRow), A
    LD      B, 0
    JR      PrintMatrixContinue2
PrintMatrixSetLimits:
    

    LD      A, (SavedY)
    CP      0
    JR      Z, PrintMatrixContinue
    DEC     A
PrintMatrixContinue:
    LD      B, A           ; C = column (0..9)
    INC     A
    INC     A
    INC     A
    INC     A
    INC     A
    LD      (PrintMatrixMaxRow), A
    CP      21
    JR      C, PrintMatrixContinue2
    LD      A, 20
    LD      (PrintMatrixMaxRow), A
PrintMatrixContinue2:
    LD      A, (CurrentMatrixOrientation)
    CP      ORIENT_NORTH
    JP      Z, PrintMatrixNorth
    CP      ORIENT_SOUTH
    JP      Z, PrintMatrixSouth
    CP      ORIENT_EAST
    JP      Z, PrintMatrixEast
    CP      ORIENT_WEST
    JP      Z, PrintMatrixWest
    RET
;----------------------------------------------------------------------
;  PrintMatrixNorth
;  - Shows the matrix as is (20 rows, 10 columns).
;  - We center it: offset row = 2, offset col = 11.
;  - For each (r,c), we read matrix[r*10 + c].
;    Then we print at (2 + r, 11 + c).
;----------------------------------------------------------------------
PrintMatrixNorth:
    ;LD      B, 0           ; B = row (0..19)
North_RowLoop:
    LD      C, 0           ; C = column (0..9)
North_ColLoop:
    ; Calculate address = Matrix + (B*10 + C)
    PUSH    BC
    LD      A, B
    LD      H, 0
    LD      L, A
    LD      DE, 10
    CALL    MultiplyHLbyDE     ; HL = B*10
    LD      A, C
    LD      E, A
    LD      D, 0
    ADD     HL, DE             ; HL = B*10 + C
    LD      DE, Matrix          ; <-- Load base address of Matrix in DE
    ADD     HL, DE              ; HL = Matrix + (B*10 + C)
    POP     BC

    ;LD      A, (HL)             ; A = cell value (0..8)

    ; Calculate screen row/column
    ; Row (D) = 2 + B
    LD      D, B
    INC     D
    INC     D
    ; Col (E) = 11 + C
    LD      E, C
    LD      A, E
    ADD     A, 11
    LD      E, A

    ; Restore A with the cell color
    LD      A, (HL)
    PUSH    DE
    PUSH    BC


    PUSH    AF
    CALL    SetAttributeAtDE
    POP     AF
    CP      0xFF
    LD      A, TILE_BLOCK
    JR      NZ, PrintMatrixNorthEndContinue
    LD      A, TILE_BLOCK_ROW_COMPETED
PrintMatrixNorthEndContinue:
    CALL    Screen_PrintRamChar
    POP     BC
    POP     DE
    INC     C
    LD      A, C
    CP      10
    JR      NZ, North_ColLoop

    INC     B
    LD      A, (PrintMatrixMaxRow)
    PUSH    DE
    LD      D, A
    LD      A, B
    CP      D
    POP     DE
    JR      NZ, North_RowLoop
    RET

;----------------------------------------------------------------------
;  PrintMatrixSouth
;  - Shows the matrix upside down (180°).
;  - We center it the same way (offset row=2, col=11).
;  - For (r,c), screen row = 2 + (19 - r), col = 11 + (9 - c).
;----------------------------------------------------------------------
PrintMatrixSouth:
    ;LD      B, 0
South_RowLoop:
    LD      C, 0
South_ColLoop:
    PUSH    BC
    LD      A, B
    LD      H, 0
    LD      L, A
    LD      DE, 10
    CALL    MultiplyHLbyDE
    LD      A, C
    LD      E, A
    LD      D, 0
    ADD     HL, DE
    LD      DE, Matrix
    ADD     HL, DE
    POP     BC

    LD      A, (HL)

    ; R = 2 + (19 - B)
    LD      D, B
    LD      A, 19
    SUB     D
    ADD     A, 2
    LD      D, A

    ; C = 11 + (9 - C)
    LD      E, C
    LD      A, 9
    SUB     E
    ADD     A, 11
    LD      E, A

    LD      A, (HL)
    PUSH    DE
    PUSH    BC
    PUSH    AF
    CALL    SetAttributeAtDE
    POP     AF
    CP      0xFF
    LD      A, TILE_BLOCK
    JR      NZ, PrintMatrixSouthEndContinue
    LD      A, TILE_BLOCK_ROW_COMPETED
PrintMatrixSouthEndContinue:
    CALL    Screen_PrintRamChar
    POP     BC
    POP     DE

    INC     C
    LD      A, C
    CP      10
    JR      NZ, South_ColLoop

    INC     B
    LD      A, (PrintMatrixMaxRow)
    LD      D, A
    LD      A, B
    CP      D
    JR      NZ, South_RowLoop
    RET

;----------------------------------------------------------------------
;  PrintMatrixEast
;  - Shows the matrix rotated 90° right.
;  - Now it becomes 10 rows x 20 columns.
;  - Center offsets: row=7, col=6.
;  - For (r,c): new_row = c + 7, new_col = (19 - r) + 6.
;----------------------------------------------------------------------
PrintMatrixEast:
    ;LD      B, 0         ; B = r
East_RowLoop:
    LD      C, 0         ; C = c
East_ColLoop:
    PUSH    BC
    LD      A, B
    LD      H, 0
    LD      L, A
    LD      DE, 10
    CALL    MultiplyHLbyDE
    LD      A, C
    LD      E, A
    LD      D, 0
    ADD     HL, DE
    LD      DE, Matrix          ; <-- Load base address of Matrix in DE
    ADD     HL, DE
    POP     BC

    LD      A, (HL)

    ; R = 7 + C
    LD      D, C
    LD      A, D
    ADD     A, 7
    LD      D, A

    ; C = 6 + (19 - B)
    LD      E, B
    LD      A, 19
    SUB     E
    ADD     A, 6
    LD      E, A

    LD      A, (HL)
    PUSH    DE
    PUSH    BC
    PUSH    AF
    CALL    SetAttributeAtDE
    POP     AF
    CP      0xFF
    LD      A, TILE_BLOCK
    JR      NZ, PrintMatrixEastEndContinue
    LD      A, TILE_BLOCK_ROW_COMPETED
PrintMatrixEastEndContinue:
    CALL    Screen_PrintRamChar
    POP     BC
    POP     DE

    INC     C
    LD      A, C
    CP      10
    JR      NZ, East_ColLoop

    INC     B
    LD      A, (PrintMatrixMaxRow)
    LD      D, A
    LD      A, B
    CP      D
    JR      NZ, East_RowLoop
    RET

;----------------------------------------------------------------------
;  PrintMatrixWest
;  - Shows the matrix rotated 90° left.
;  - Now it becomes 10 rows x 20 columns.
;  - Center offsets: row=7, col=6.
;  - For (r,c): new_row = (9 - c) + 7, new_col = r + 6.
;----------------------------------------------------------------------
PrintMatrixWest:
    ;LD      B, 0
West_RowLoop:
    LD      C, 0
West_ColLoop:
    PUSH    BC
    LD      A, B
    LD      H, 0
    LD      L, A
    LD      DE, 10
    CALL    MultiplyHLbyDE
    LD      A, C
    LD      E, A
    LD      D, 0
    ADD     HL, DE
    LD      DE, Matrix          ; <-- Load base address of Matrix in DE
    ADD     HL, DE
    POP     BC

    LD      A, (HL)

    ; R = 7 + (9 - C)
    LD      D, C
    LD      A, 9
    SUB     D
    ADD     A, 7
    LD      D, A

    ; C = 6 + B
    LD      E, B
    LD      A, E
    ADD     A, 6
    LD      E, A

    LD      A, (HL)
    PUSH    DE
    PUSH    BC
    PUSH    AF
    CALL    SetAttributeAtDE
    POP     AF
    CP      0xFF
    LD      A, TILE_BLOCK
    JR      NZ, PrintMatrixWestEndContinue
    LD      A, TILE_BLOCK_ROW_COMPETED
PrintMatrixWestEndContinue:
    CALL    Screen_PrintRamChar
    POP     BC
    POP     DE

    INC     C
    LD      A, C
    CP      10
    JR      NZ, West_ColLoop

    INC     B
    LD      A, (PrintMatrixMaxRow)
    LD      D, A
    LD      A, B
    CP      D
    JR      NZ, West_RowLoop
    RET

;--------------------------------------------------
; Check blocks collision
; INPUT: -
; OUTPUT:
;   A: 1=Yes - 0=No
; MODIFY: AF, HL, BC, DE
;--------------------------------------------------
CheckBlocksCollision:
    LD      A, (SavedX)
    LD      (TempXBlocPosition), A
    LD      HL,(CurrentBlockTable)
    LD      A, (BlockLeftMoving)
    CP      0
    JP      Z, CheckBlocksCollisionLoop
    CP      1
    JP      Z, CheckBlocksCollisionLoopLeft
    LD      A, (SavedX)
    INC     A
    LD      (TempXBlocPosition), A
    JP      CheckBlocksCollisionLoop
CheckBlocksCollisionLoopLeft:
    LD      A, (SavedX)
    DEC     A
    LD      (TempXBlocPosition), A
CheckBlocksCollisionLoop:
    LD      A, (HL)
    CP      0xFF
    JP      Z, CheckBlocksCollisionNo
    LD      C, A        ; dy
    INC     HL
    LD      A, (HL)     ; dx
    INC     HL
    LD      B, A
    PUSH    HL
    ; r = SavedY + dy
    LD      A, (SavedY)
    ADD     A, C
    LD      C, A

    ; c = SavedX + dx
    LD      A, (TempXBlocPosition)
    ADD     A, B
    LD      B, A

    ; offset = r*10 + c => HL
    CALL    ComputeOffset
    LD      DE, Matrix
    ADD     HL, DE

    LD      A, (HL)
    CP      COLOR_BLACK
    JR      NZ, CheckBlocksCollisionYes
    POP     HL
    JR      CheckBlocksCollisionLoop

CheckBlocksCollisionYes:
    POP     HL
    LD      A, 1
    RET


CheckBlocksCollisionNo:
    XOR A
    RET
;--------------------------------------------------
; Update current block table
; INPUT:
;   D: Y position
;   E: X position
; OUTPUT: -
; MODIFY: AF, BC, DE, HL
;--------------------------------------------------
UpdateCurrentBlockTable:
    LD      A, (CurrentBlock)
    LD      B, A                ; Salvo ID in B
    LD      L, A                ; L = ID
    LD      A, D                
    LD      (SavedY), A
    LD      A, E
    LD      (SavedX), A        
    LD      H, 0
    LD      DE, BLOCK_COLORS
    ADD     HL, DE              ; HL -> &BLOCK_COLORS[ID]
    LD      A, (HL)             ; A = colore
    LD      (ColorOfBlock), A   ; mettiamo in una variabile
    LD      A, (CurrentBlockRotation) ; A = rotazione (0..3)
    LD      L, B                       ; L = ID salvato prima
    SLA     L
    SLA     L                          ; L = ID*4
    ADD     A, L                       ; A = (ID*4) + rot
    LD      L, A
    LD      H, 0
    ADD     HL, HL                     ; HL *= 2 (ogni voce della tabella = 2 byte)
    LD      DE, BLOCKS_TABLE
    ADD     HL, DE                     ; HL => DW con l'indirizzo giusto
    LD      E, (HL)
    INC     HL
    LD      D, (HL)
    EX      DE, HL                   
    LD      (CurrentBlockTable), HL
    RET
;------------------------------------------------------------------------
; Delay
; INPUT: -
; OUTPUT: -
; MODIFY: AF, BC
;------------------------------------------------------------------------
Delay:
    LD      BC, 0x0120              ; Set a counter
.DelayLoop:
    DEC     BC
    LD      A, B
    OR      C
    JR      NZ, .DelayLoop          ; Wait until BC is 0
    RET
; ---------------------------------------------------------------------
; Remove completed rows
; ---------------------------------------------------------------------
RemoveCompletedRows:
    LD      B, 19               ; Partiamo dalla riga più bassa (19)
RemoveCompletedLoop:
    LD      A, B
    CP      0                   ; Se siamo alla riga 0, fermati
    JR      Z, RemoveCompletedEnd

    ; Controlla la prima colonna della riga corrente
    PUSH    BC
    CALL    CheckFirstColumnFF  ; Z=1 se la prima colonna è 0xFF
    POP     BC
    CP      0
    JR      Z, RemoveCompletedSkipRow         ; Se Z=0, passa alla riga successiva

    ; Se la prima colonna è 0xFF, sposta le righe superiori verso il basso
    CALL    ShiftRowsDown
    
RemoveCompletedSkipRow:
    DEC     B                   ; Passa alla riga superiore
    JR      RemoveCompletedLoop            ; Continua il ciclo

RemoveCompletedEnd:
    LD      A, 0x1
    LD      (ForcePrintMatrixRepaintAll), A
    RET

; ---------------------------------------------------------------------
; CheckFirstColumnFF
; Controlla se la prima colonna della riga specificata è 0xFF.
; Input:
;   B = numero della riga (0..19)
; Output:
;   Z=1 se la prima colonna è 0xFF, Z=0 altrimenti
; ---------------------------------------------------------------------
CheckFirstColumnFF:

    LD      C, B
    LD      B, 0
    CALL    ComputeOffset
    LD      DE, Matrix
    ADD     HL, DE
    LD      A, (HL)
    CP      0xFF                ; Confronta con 0xFF
    JR      Z, CheckFirstColumnFFContinue
    XOR     A
    JR      CheckFirstColumnFFEnd
CheckFirstColumnFFContinue:
    LD      A, 1
CheckFirstColumnFFEnd:
    RET

; ---------------------------------------------------------------------
; ShiftRowsDown
; Sposta tutte le righe superiori verso il basso, riempie la riga 0
; con COLOR_BLACK.
; Input:
;   B = numero della riga corrente (da spostare verso il basso)
; ---------------------------------------------------------------------
ShiftRowsDown:
    PUSH    BC
    DEC     B                   ; Passa alla riga corrente
    LD      C, B
    LD      B, 0
    PUSH    BC
    CALL    ComputeOffset
    POP     BC
    LD      DE, Matrix
    ADD     HL, DE   ; HL=Start position to copy from
    PUSH    HL
    XOR     A
ShiftRowsDownLoop:
    LD      C, A
    LD      A, (HL) ; Value to copy
    INC     HL
    INC     HL
    INC     HL
    INC     HL
    INC     HL
    INC     HL
    INC     HL
    INC     HL
    INC     HL
    INC     HL
    LD      (HL), A ; Copy value to the next row
    DEC     HL
    DEC     HL
    DEC     HL
    DEC     HL
    DEC     HL
    DEC     HL
    DEC     HL
    DEC     HL
    DEC     HL
    LD      A, C
    INC     A
    CP      10
    JR      Z, ShiftRowsDownLoopEnd
    JR      ShiftRowsDownLoop
ShiftRowsDownLoopEnd:
    POP     HL
    POP     BC
    LD      A, B
    DEC     A
    CP      0
    RET     Z
    ;LD      B, 20
    LD      A, 0xFF
    LD      (HL), A ; Fill the first row with 0xFF
    RET
FillTopRow:
    ; Riempie la riga 0 con COLOR_BLACK
    LD      D, 0
    LD      E, 0
    CALL    ComputeRowAddress   ; HL = indirizzo della riga 0
    LD      C, 10               ; Numero di colonne
FillRowLoop:
    LD      (HL), COLOR_BLACK   ; Riempie con COLOR_BLACK
    INC     HL
    DEC     C
    JR      NZ, FillRowLoop
    RET

; ---------------------------------------------------------------------
; ComputeRowAddress
; Calcola l'indirizzo della riga in Matrix.
; Input:
;   D = 0
;   E = numero della riga (0..19)
; Output:
;   HL = indirizzo della riga in Matrix
; ---------------------------------------------------------------------
ComputeRowAddress:
    LD      HL, Matrix          ; Base della matrice
    LD      A, E
    CALL    MulByteBy10         ; HL = E * 10
    ADD     HL, HL              ; HL = Matrix + (E * 10)
    RET
;--------------------------------------------------
; Place block into matrix (with collision check)
; INPUT:
;   D: Y position
;   E: X position
; OUTPUT: -
; MODIFY: AF, BC, HL
;--------------------------------------------------
PlaceBlock:
    CALL    UpdateCurrentBlockTable
    CALL    CheckBlocksCollision
    CP      0
    JR      Z, CheckOk
Collision:

    LD      (ShowNextBlock), A  
    LD      A, (SavedY)
    CP      0
    JP      Z, NoMoreBlocks
    DEC     A
    LD      (SavedY), A

CheckOk:
    ; => arrivati a 0xFF => nessuna collisione
    ;XOR     A
    ;ADC     A, A      ; forziamo CF=0 (modo classico: "OR A" + "Ccf" e simili)
    LD      HL,(CurrentBlockTable)
    ;LD      A, (ColorOfBlock)
PlaceBlockLoop:
    LD      A, (HL)
    CP      0xFF
    RET     Z
    LD      C, A        ; dy
    INC     HL
    LD      A, (HL)     ; dx
    INC     HL
    LD      B, A
    PUSH    HL
    ; r = SavedY + dy
    LD      A, (SavedY)
    ADD     A, C
    LD      C, A

    ; c = SavedX + dx
    LD      A, (SavedX)
    ADD     A, B
    LD      B, A

    ; offset = r*10 + c => HL
    CALL    ComputeOffset
    LD      DE, Matrix
    ADD     HL, DE

    LD      A, (ColorOfBlock)
    LD      (HL), A
    POP     HL
    JR      PlaceBlockLoop

NoMoreBlocks: ; GAME OVER
    LD      A, 1
    LD      (GameOver), A
    RET

;--------------------------------------------------
; Multiply HL by DE 
; INPUT:
;   DE: First value to multiply (16-bit)
;   HL: Second value to multiply (16-bit)
; OUTPUT:
;   HL: result of multiplication
; MODIFIY: DE, HL, BC
;--------------------------------------------------
MultiplyHLbyDE:
    PUSH    BC
    LD      B, H
    LD      C, L
    XOR     A
    LD      H, A
    LD      L, A
    LD      A, 16
MulLoop:
    BIT     0, C
    JR      Z, SkipAdd
    ; Se e' 1 => HL = HL + DE
    ADD     HL, DE
SkipAdd:
    SRL     C
    RR      B
    SLA     E
    RL      D
    DEC     A
    JR      NZ, MulLoop
    POP     BC
    RET
;--------------------------------------------------
; Game over
; INPUT: -
; OUTPUT: -
; MODIFY: AF, BC, DE, HL
;--------------------------------------------------
ShowGameOver:

    DI
    IM 1       ; Torna alla modalità interrupt standard
    EI

    LD   HL, Matrix            ; HL punta all'inizio della matrice
    LD   B,0
    LD   C, 200                 ; BC = contatore di byte da scorrere

ShowGameOverNextByte:
    LD   A, (HL)               ; Carica il valore corrente
    CP   COLOR_BLACK           ; È nero?
    JR   Z, ShowGameOverSkipReplace        ; Se sì, salta

    LD   (HL), COLOR_WHITE     ; Altrimenti, imposta bianco

ShowGameOverSkipReplace:
    INC  HL                    ; Passa al prossimo byte
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, ShowGameOverNextByte          ; Finché BC ≠ 0

    LD     A, 1
    LD     (ForcePrintMatrixRepaintAll), A
    CALL   PrintMatrix
    


    LD     E, 11
    LD     D, 11
    LD     HL, TEXT_GAME_OVER_1
    CALL   Screen_PrintString

    LD     E, 11
    LD     D, 12
    LD     HL, TEXT_GAME_OVER_2
    CALL   Screen_PrintString

    LD     E, 11
    LD     D, 13
    LD     HL, TEXT_GAME_OVER_3
    CALL   Screen_PrintString

    LD     E, 11
    LD     D, 14
    LD     HL, TEXT_GAME_OVER_1
    CALL   Screen_PrintString
    CALL   GameOverSound
ShowGameOverKeyPress:
    CALL   ReadKeyboard
    LD     A, (KeyPressed)
    CP     32
    JP     Z, InitMenu
    JR     ShowGameOverKeyPress
;--------------------------------------------------
; Init new game
;--------------------------------------------------
InitNewGame:
    XOR     A
    LD      (MusicNoteIndex),A
    LD      (GameOver), A
    LD      (CompletedRows), A
    LD      (FrameCounter), A
    LD      (SecondsCounter), A
    LD      (MinutesCounter), A
    LD      (Score), A
    LD      (ShowNextBlock), A
    LD      (BlockSpeedCounter), A  
    LD      H, 0
    LD      L, 0
    LD      (PlacedBlocks), HL              ; Set the number of blocks to 0
    LD      (Score), HL                     ; Set the score to 0

    DI                                      ; Disable interrupts
    IM      2                               ; Set the interrupt mode
    EI                                      ; Enable interrupts
    RET

;--------------------------------------------------
; Set M2 interrupt routine
; INPUT: -
; OUTPUT: -
; MODIFY: AF, BC, DE, HL
;--------------------------------------------------
SetM2RRoutine:
    LD      HL,M2Routine
    LD      IX,0xFFF0                       ; This code is to be written at 0xFF
    LD      (IX+04h),0xC3                   ; Opcode for JP
    LD      (IX+05h),L                      ; Store the address of the interrupt routine in
    LD      (IX+06h),H
    LD      (IX+0Fh),0x18                   ; Opcode for JR; this will do JR to FFF4h
    LD      A,0x39                          ; Interrupt table at page 0x3900 (ROM)
    LD      I,A                             ; Set the interrupt register to that page
    RET
;----------------------------------------------------
; Move block down
;----------------------------------------------------
MoveBlockDown:
    LD      C, 19
    LD      A, (CurrentBlock)
    CP      0
    JR      NZ, MoveBlockDownNoIBlock
    LD      C, 20
MoveBlockDownNoIBlock:
    LD      B, A
    LD      A, (CurrentBlockRotation) ; A = rotazione (0..3)
    LD      L, B                       ; L = ID salvato prima
    SLA     L
    SLA     L                          ; L = ID*4
    ADD     A, L                       ; A = (ID*4) + rot
    LD      L, A
    LD      H, 0
    LD      DE, BLOCKS_HEIGHT
    ADD     HL, DE                      ; HL => DW con l'indirizzo giusto
    LD      B, (HL)                     ; Current block height
    LD      A, (SavedY)
    ADD     B
    CP      20
    JR      NZ, MoveBlockDownCheckBottomLimit       ; Continue moving if not at the bottom
    XOR     A
    LD      (BlockSpeedCounter), A
    LD      A, 1                      
    LD      (ShowNextBlock), A  
    RET
MoveBlockDownCheckBottomLimit:
    XOR     A   
    LD      (BlockSpeedCounter), A 
    LD      A, (SavedY)
    INC     A
    CP      C
    JR      NZ, MoveBlockDownContinue       ; Continue moving if not at the bottom
    LD      A, 1                      
    LD      (ShowNextBlock), A 
    RET
MoveBlockDownContinue:
    PUSH    AF
    CALL    RemoveCurrentBlock
    POP     AF
    LD      (SavedY), A           ; Increment the Y position
    LD      D, A
    LD      A, (SavedX)           ; Increment the Y position
    LD      E, A           ; Get the X position
    CALL    PlaceBlock
    RET

;---------------------------------------------------
; M2 routine
;---------------------------------------------------        
M2Routine:
    DI                                      ; Disable interrupts 
    PUSH    AF                                 ; Save all the registers on the stack
    PUSH    BC                                 ; This is probably not necessary unless
    PUSH    DE                                 ; we're looking at returning cleanly
    PUSH    HL                                 ; back to BASIC at some point
    PUSH    IX
    EXX
    EX      AF,AF'
    PUSH    AF
    PUSH    BC
    PUSH    DE
    PUSH    HL
    PUSH    IY

    ;

        

    LD      A, (FrameCounter)
    INC     A
    CP      51
    JR      NZ, M2RoutineContinue
    LD      A, (SecondsCounter)
    INC     A
    CP      61
    JR      NZ, M2RoutineNoNewMinute
    LD      A, (MinutesCounter)
    INC     A
    LD      (MinutesCounter), A
    XOR     A                            ; Reset the seconds counter
M2RoutineNoNewMinute:
    LD      (SecondsCounter), A
    LD      A, 0                            ; Reset the frame counter
M2RoutineContinue:
    LD      (FrameCounter), A

    LD      A, (InGame)
    CP      1
    JR      Z, M2RoutineInGame
    LD      A, (FrameCounter)
    CP      0
    JP      Z, PlayMenuMusic
    CP      0
    JP      Z, PlayMenuMusic
    CP      10
    JP      Z, PlayMenuMusic
    CP      20
    JP      Z, PlayMenuMusic
    CP      30
    JP      Z, PlayMenuMusic
    CP      40
    JP      Z, PlayMenuMusic
    

    CP      5
    JP      Z, PlayMenuMusic
    CP      15
    JP      Z, PlayMenuMusic
    CP      25
    JP      Z, PlayMenuMusic
    CP      35
    JP      Z, PlayMenuMusic
    CP      45
    JP      Z, PlayMenuMusic

    JP      M2RoutineBlockMoveExit
PlayMenuMusic:
    CALL    PlayInGameMusic
    JP      M2RoutineBlockMoveExit

M2RoutineInGame:

    LD      A, (KeyPressed)
    CP      32
    JP      Z, MoveBlockToBottom
    CP      51
    JP      Z, MoveBlockToBottom
    CP      52
    JP      Z, MoveBlockLeft
    CP      53
    JP      Z, BlockRotation
    CP      54
    JP      Z, MoveBlockRight
    CP      0
    JR      Z, M2RoutineBlockMoveDown
    JR      M2RoutineBlockMoveEnd
M2RoutineKeyOk:
    LD      A, (SavedY)
    LD      D, A
    LD      A, (SavedX)           ; Increment the Y position
    LD      E, A           ; Get the X position
    CALL    PlaceBlock
    
    JR      M2RoutineBlockMoveEnd
M2RoutineBlockMoveDown:
    XOR     A
    LD      (BlockLeftMoving), A
    LD      HL, BlockSpeed
    LD      A, (BlockSpeedCounter)
    CP      (HL)
    CALL    Z, MoveBlockDown
    LD      A, (BlockSpeedCounter)
    INC     A
    LD      (BlockSpeedCounter), A
M2RoutineBlockMoveEnd:
    XOR     A
    LD      (KeyPressed), A
    CALL    PrintMatrix             ; Display the initial matrix
M2RoutineBlockMoveExit:
    POP     IY                                  ; Restore all the registers
    POP     HL
    POP     DE
    POP     BC
    POP     AF
    EXX
    EX      AF,AF'
    POP     IX
    POP     HL
    POP     DE
    POP     BC
    POP     AF
    EI                                      ; Enable interrupts
    RET                                     ; And return
BakcupMatrix:
    LD      HL, Matrix
    LD      DE, TempMatrix
    LD      BC, 200
    LDIR
    RET
RestoreMatrix:
    LD      HL, TempMatrix
    LD      DE, Matrix
    LD      BC, 200
    LDIR
    RET
MoveBlockLeft:
    LD      A, (CurrentMatrixOrientation)
    CP      ORIENT_WEST
    JP      Z, MoveBlockLeftContinue
    CP      ORIENT_NORTH
    JP      Z, MoveBlockLeftContinue
    JP      MoveBlockRightContinue
MoveBlockLeftContinue:
    CALL    BakcupMatrix
    LD      A, 1
    LD      (BlockLeftMoving), A
    CALL    RemoveCurrentBlock
    LD      A, (SavedX)
    CP      0
    JP      Z, M2RoutineKeyOk
    DEC     A
    LD      E, A
    LD      A, (SavedY)
    LD      D, A
    CALL    UpdateCurrentBlockTable
    LD      A, (SavedX)
    INC     A
    LD      (SavedX), A
    CALL    CheckBlocksCollision
    CP      0
    JP      Z, MoveBlockLeftContinueEnd
    CALL    RestoreMatrix
    JP      M2RoutineBlockMoveDown
MoveBlockLeftContinueEnd:

    XOR     A
    LD      (BlockLeftMoving), A
    LD      A, (SavedX)
    DEC     A
    LD      (SavedX), A
    JP      M2RoutineKeyOk
MoveBlockRight:
    LD      A, (CurrentMatrixOrientation)
    CP      ORIENT_WEST
    JP      Z, MoveBlockRightContinue
    CP      ORIENT_NORTH
    JP      Z, MoveBlockRightContinue
    JP      MoveBlockLeftContinue
MoveBlockRightContinue:
    CALL    BakcupMatrix
    LD      A, 2
    LD      (BlockLeftMoving), A
    CALL    RemoveCurrentBlock
    LD      A, (SavedY)
    LD      D, A
    LD      A, (SavedX)           ; Increment the Y position
    LD      E, A           ; Get the X position
    CALL    UpdateCurrentBlockTable
    CALL    CheckBlocksCollision
    CP      0
    JP      Z, MoveBlockRightContinueEnd
    CALL    RestoreMatrix
    JP      M2RoutineBlockMoveDown
MoveBlockRightContinueEnd:
    XOR     A
    LD      (BlockLeftMoving), A
    LD      A, (CurrentBlock)
    LD      B, A
    LD      A, (CurrentBlockRotation) ; A = rotazione (0..3)
    LD      L, B                       ; L = ID salvato prima
    SLA     L
    SLA     L                          ; L = ID*4
    ADD     A, L                       ; A = (ID*4) + rot
    LD      L, A
    LD      H, 0
    LD      DE, BLOCKS_WIDTH
    ADD     HL, DE                      ; HL => DW con l'indirizzo giusto
    LD      B, (HL)                     ; Current block width
    LD      A, (SavedX)
    ADD     B
    CP      10
    JP      Z, M2RoutineKeyOk
    LD      A, (SavedX)
    INC     A
    LD      (SavedX), A
    JP      M2RoutineKeyOk
BlockRotation:
    CALL    RemoveCurrentBlock
    LD      A, (SavedX)
    ;CP      0
    ;JP      Z, M2RoutineKeyOk
    LD      E, A
    LD      A, (SavedY)
    LD      D, A
    CALL    UpdateCurrentBlockTable
    CALL    CheckBlocksCollision
    CP      1
    JP      Z, M2RoutineBlockMoveDown
    LD      A,  (CurrentBlockRotation)
    INC     A
    CP      4
    JR      NZ, BlockRotationContinue
    LD      A, 0
BlockRotationContinue:
    
    LD      D, A
    LD      A, (SavedX)
    CP      7
    JP      NC, BlockRotationIBlock
    CP      8
    JP      C, BlockRotationContinueEnd
BlockRotationContinue1:
    PUSH    DE
    LD      A, (CurrentBlock)
    LD      B, A

    LD      A, D

    LD      L, B                       ; L = ID salvato prima
    SLA     L
    SLA     L                          ; L = ID*4
    ADD     A, L                       ; A = (ID*4) + rot
    LD      L, A
    LD      H, 0
    LD      DE, BLOCKS_WIDTH
    ADD     HL, DE                      ; HL => DW con l'indirizzo giusto
    LD      B, (HL)                     ; Current block width
    LD      A, (SavedX)
    ADD     B
    POP     DE
    CP      13
    JR      C, BlockRotationContinue2
    CP      12
    JR      C, BlockRotationContinue2
    CP      12
    JP      NC, M2RoutineKeyOk
BlockRotationContinue2:
    LD      A, 10
    SUB     B
    LD      (SavedX), A
BlockRotationContinueEnd:
    LD      A, (CurrentBlockRotation)
    LD      E, A
    LD      A, D
    LD      (CurrentBlockRotation), A
    PUSH    DE
    LD      A, (SavedX)
    LD      E, A
    LD      A, (SavedY)
    LD      D, A
    LD      HL, BLOCK_3x3
    LD      (CurrentBlockTable), HL
    LD      A, (CurrentBlock)
    CP      BLOCK_I
    JR      NZ, BlockRotationContinueEnd1
    LD      HL, BLOCK_4x4
    LD      (CurrentBlockTable), HL
BlockRotationContinueEnd1:

    CALL    CheckBlocksCollision
    POP     DE
    CP      1
    JP      NZ, M2RoutineKeyOk
    LD      A, E
    LD      (CurrentBlockRotation), A
    JP      M2RoutineKeyOk
BlockRotationIBlock:
    LD      A, (CurrentBlock)
    CP      BLOCK_I
    JR      NZ, BlockRotationContinue1
    LD      A, D
    CP      2
    JP      Z, BlockRotationContinue1
    CP      4
    JP      Z, BlockRotationContinue1
    ;LD      A, (SavedY)
    ;CP      16
    ;JP      NC, BlockRotationContinueEnd
    LD      A, (SavedX)
    CP      7
    JP      C, BlockRotationContinue1
    LD      A, 6
    LD      (SavedX), A
    JP      BlockRotationContinue1
MoveBlockToBottom:
    LD      A, 1
    LD      (MoveToBottomInAction), A
    LD      A, (SavedY)
MoveBlockToBottomLoop:
    CP      19
    JP      Z, MoveBlockToBottomLoopEnd
    PUSH    AF
    CALL    MoveBlockDown
    POP     AF
    INC     A
    JP      MoveBlockToBottomLoop
MoveBlockToBottomLoopEnd:
    LD      A, 1 ; Force all matrix repaint
    LD      (ForcePrintMatrixRepaintAll), A
    JP      M2RoutineBlockMoveEnd
;--------------------------------------------------
; Fill M2 table
; INPUT: -
; OUTPUT: -
; MODIFY: AF, HL, DE, BC
;--------------------------------------------------
FillM2Table:
    LD      B, 129          ; 129*2 = 258 byte, uno in piu'
FillM2TableLoop:
    LD      (HL), E         ; low byte
    INC     HL
    LD      (HL), D         ; high byte
    INC     HL

    DJNZ    FillM2TableLoop
    RET
;--------------------------------------------------
; Remove block from matrix (with collision check)
; INPUT:
;   A: ID del pezzo (0..6)
;   D: Y di posizionamento (0..19)
;   E: X di posizionamento (0..9)
; OUTPUT:
;   A: Collision (0=NO, 1=YES);
; MODIFY: AF, BC, HL
;--------------------------------------------------
RemoveCurrentBlock:
    LD      A, (CurrentBlock)       ; Ottieni il colore del pezzo
    LD      B, A                ; Salvo ID in B
    LD      L, A                ; L = ID
    LD      A, (CurrentBlockRotation) ; A = rotazione (0..3)
    LD      L, B                       ; L = ID salvato prima
    SLA     L
    SLA     L                          ; L = ID*4
    ADD     A, L                       ; A = (ID*4) + rot
    LD      L, A
    LD      H, 0
    ADD     HL, HL                     ; HL *= 2 (ogni voce della tabella = 2 byte)
    LD      DE, BLOCKS_TABLE
    ADD     HL, DE                     ; HL => DW con l'indirizzo giusto
    LD      E, (HL)
    INC     HL
    LD      D, (HL)
    EX      DE, HL                   
RemoveCurrentBlockLoop:
    LD      A, (HL)
    CP      0xFF
    JR      Z, RemoveCurrentBlockDone
    LD      C, A        ; dy
    INC     HL
    LD      A, (HL)     ; dx
    INC     HL
    LD      B, A
    PUSH    HL
    ; r = SavedY + dy
    LD      A, (SavedY)
    ADD     A, C
    LD      C, A

    ; c = SavedX + dx
    LD      A, (SavedX)
    ADD     A, B
    LD      B, A

    ; offset = r*10 + c => HL
    CALL    ComputeOffset
    LD      DE, Matrix
    ADD     HL, DE

    LD      A, COLOR_BLACK
    LD      (HL), A
    POP     HL
    JR      RemoveCurrentBlockLoop

RemoveCurrentBlockDone:
    ; => Piazzato, CF=0
    XOR     A
    RET
;--------------------------------------------------
; Compute Place block offset
; INPUT:  
;   B: Column
;   C: Row
; OUTPUT:
;   HL: Row*10 + column
; MODIFY: AF, BC, HL
;--------------------------------------------------
ComputeOffset:
        LD   A, C
        PUSH  BC
        CALL MulByteBy10  ; => HL = row*10
        POP  BC
        LD   A, B         ; col
        ADD  A, L         ; L += col
        LD   L, A
        ; Se si generasse carry (non dovrebbe in 20x10), 
        ; potrebbe influire su H, ma in Tetris reale (r<20, c<10) siamo tranquilli.
        RET

;--------------------------------------------------
; Multiply A by 10
; INPUT:
;   A: Value 0..255
; OUTPUT:
;   HL: A * 10 (16 bit)
;   MODIFY: AF, BC, HL
;--------------------------------------------------
MulByteBy10:
    LD   B, A         ; salviamo in B
    LD   H, 0
    LD   L, B         ; HL = val
    ADD  HL, HL       ; *2
    ADD  HL, HL       ; *4
    ADD  HL, HL       ; *8
    LD   A, B
    ADD  A, A         ; *2
    LD   B, A
    LD   A, L
    ADD  A, B         ; HL.low + r*2 => r*8 + r*2 = r*10
    LD   L, A
    ; Se ci fosse carry (r>25), potremmo toccare H, ma in Tetris r<20
    RET

;--------------------------------------------------
; Randomize matrix blocks
; INPUT: -
; OUTPUT: -
; MODIFY: HL, AF, BC
;--------------------------------------------------
RandomizeMatrixBlocks:
    LD   HL, ZXETRISMATRIX      ; Punto iniziale della matrice
    LD   BC, 175        ; Contatore di byte

NextByte:
    LD   A, (HL)                ; Legge il valore attuale
    OR   A
    JR   Z, SkipReplace         ; Salta se è zero

    ; Chiamata alla routine random
    CALL ChooseRandomBlock      ; A = 0-6
    LD   E, A
    LD   D, 0
    LD   IX, COLORS_TABLE
    ADD  IX, DE
    LD   A, (IX)                ; A = attributo colore
    LD   (HL), A                ; Scrive il nuovo valore nella matrice

SkipReplace:
    INC  HL                     ; Prossimo byte
    DEC  BC
    LD   A, B
    OR   C
    JP   NZ, NextByte           ; Finché BC ≠ 0, continua

    RET

;------------------------------------------------------------------------
; Choose random block
; INPUT: -
; OUTPUT:
;   A: Block ID (0..6)
; MODIFY: -
;------------------------------------------------------------------------
ChooseRandomBlock:
    LD      A, (Seed)
    ADD     A, 17
    LD      (Seed), A
ChooseRandomBlockLoop:
    CP      7                           ; confronta A con 7
    JR      C, ChooseRandomBlockDone    ; se A < 7, abbiamo finito
    SUB     7                           ; altrimenti sottrai 7
    JR      ChooseRandomBlockLoop       ; e ripeti

ChooseRandomBlockDone:
    ;LD      A, 0 ; RIMUOVI DOPO TEST    FP_USER
        ; Al termine A è compreso tra 0 e 6
    LD      (NextBlock), A       ; Salva il pezzo scelto (ora è il colore)
    RET
;------------------------------------------------------------------------
; Choose random orientation
; INPUT: 
;   B: Max value
; OUTPUT:
;   A: Result
; MODIFY: -
;------------------------------------------------------------------------
GetRandomValue:
    LD      A, (Seed)
    ADD     A, 17
    LD      (Seed), A
GetRandomValueLoop:
    CP      B                           
    JR      C, GetRandomValueDone    
    SUB     B                           
    JR      GetRandomValueLoop       

GetRandomValueDone:
    RET
;------------------------------------------------------------------------
; Get block color
; INPUT:
;   A: Block ID (0..6)
; OUTPUT:
;   A: Block color
; MODIFY: -
;------------------------------------------------------------------------
GetBlockColor:
    CP      0
    JR      Z, .SetBlue
    CP      1
    JR      Z, .SetYellow
    CP      2
    JR      Z, .SetRed
    CP      3
    JR      Z, .SetCyan
    CP      4
    JR      Z, .SetMagenta
    CP      5
    JR      Z, .SetWhite
    CP      6
    JR      Z, .SetGreen
    RET

.SetBlue:
    LD      A, COLOR_BLUE
    RET
.SetYellow:
    LD      A, COLOR_YELLOW
    RET
.SetRed:
    LD      A, COLOR_RED
    RET
.SetCyan:
    LD      A, COLOR_CYAN
    RET
.SetMagenta:
    LD      A, COLOR_MAGENTA
    RET
.SetWhite:
    LD      A, COLOR_WHITE
    RET
.SetGreen:
    LD      A, COLOR_GREEN
    RET

;------------------------------------------------------------------------
; Set speed by level
; INPUT: -
; OUTPUT: -
; MODIFY: HL, AF
;------------------------------------------------------------------------
SetSpeedByLevel:
    LD      A, (Level)
    CP      10
    RET     Z
    LD      B, A
    LD      A, 10
    SUB     B
    LD      (BlockSpeed), A
    XOR     A
    LD      (LevelBlocksCounter), A
    RET
;------------------------------------------------------------------------
; Change matrix the orientation
; INPUT: -
; OUTPUT: -
; MODIFY: HL, AF
;------------------------------------------------------------------------
ChangeOrientation:
    LD      B, 3
    CALL    GetRandomValue
    LD      B, A
    LD      A, (CurrentMatrixOrientation)
    CP      B
    RET     Z
    LD      A, B
    LD      (CurrentMatrixOrientation), A
    CALL    ShowGameField
    LD      A, 1
    LD      (ForcePrintMatrixRepaintAll), A
    CALL    PrintMatrix
    CALL    DisplayScoreUpdate
    LD      A, (NextBlock)
    CALL    UpdateNextBlockInfo
    CALL    SetNextBoxPosition
    CALL    ShowNextBlockOnBox
    RET
;------------------------------------------------------------------------
; Place block at top
; INPUT: -
; OUTPUT: -
; MODIFY: DE, HL, AF
;------------------------------------------------------------------------
PlaceBlockAtTop:

    LD      A, (LevelBlocksCounter)
    CP      5
    CALL    Z, ChangeOrientation
    CP      15
    CALL    Z, ChangeOrientation
    CP      25
    CALL    Z, ChangeOrientation
    CP      35
    CALL    Z, ChangeOrientation
    CP      45
    CALL    Z, ChangeOrientation
    CP      55
    CALL    Z, ChangeOrientation
    CP      65
    CALL    Z, ChangeOrientation
    CP      75
    CALL    Z, ChangeOrientation
    CP      85
    CALL    Z, ChangeOrientation
    CP      95
    CALL    Z, ChangeOrientation
    XOR     A
    LD      (CurrentBlockRotation), A
    LD      (MoveToBottomInAction), A
    LD      HL, Matrix              ; Inizia dalla cima della matrice
    CALL    GetBlockXStartPosition  ; Ottieni la posizione iniziale del pezzo
    LD      E, A                    ; Riga iniziale
    LD      D, 0                    ; First row
    EI
    CALL    PlaceBlock              ; Disegna il pezzo nella matrice
    DI
    ;CALL    PrintMatrix             ; Mostra la matrice
    XOR     A                       ; A = 0
    LD      (ShowNextBlock), A      ; Mostra il pezzo successivo
    LD      A, (LevelBlocksCounter)
    INC     A
    CP      100                     ; Check for level up
    JR      NZ, PlaceBlockAtTopEnd
    LD      A, (SelectedCanGrow)
    CP      1
    JR      NZ, PlaceBlockAtTopContinue
    LD      A, (Level)
    CP      10
    JR      Z, PlaceBlockAtTopContinue
    INC     A
    LD      (Level), A
    CALL    SetSpeedByLevel
PlaceBlockAtTopContinue:
    XOR     A
PlaceBlockAtTopEnd:
    LD      (LevelBlocksCounter), A  ; Incrementa il contatore dei pezzi
    CALL    DisplayScoreUpdate
    LD      A, (PlacedBlocks)
    CP      0
    RET     Z
    CALL    BlockLandSound 
    RET
;------------------------------------------------------------------------
; Get new block start position
; INPUT:
;   A: Block ID
; OUTPUT:
;   A: Block start position in the matrix
; MODIFY: -
;------------------------------------------------------------------------
GetBlockXStartPosition:
    CP      BLOCK_I
    JP      Z, .PosI
    CP      BLOCK_O
    JP      Z, .PosO
    CP      BLOCK_S
    JP      Z, .PosS
    CP      BLOCK_T
    JP      Z, .PosT
    CP      BLOCK_Z
    JP      Z, .PosZ
    CP      BLOCK_J
    JP      Z, .PosJ
    CP      BLOCK_L
    JP      Z, .PosL
    RET

.PosI: 
    LD      A, 3
    RET

.PosO: 
    LD      A, 4
    RET

.PosT: 
    LD      A, 0
    RET
.PosS: 
    LD      A, 3
    RET
.PosZ: 
    LD      A, 3
    RET

.PosJ: 
    LD      A, 4
    RET

.PosL: 
    LD      A, 4
    RET
;------------------------------------------------------------------------
; Read the keyboard
; INPUT: -
; OUTPUT:
;   A: ASCII code of key pressed
; MODIFY: HL, BC, AF
;------------------------------------------------------------------------
ReadKeyboard:          
    LD  HL,KEYBOARD_MAP     ; Point HL at the keyboard list
    LD  D, 8                ; This is the number of ports (rows) to check
    LD  C, #FE              ; C is always FEh for reading keyboard ports
ReadKeyboard0:        
    LD  B, (HL)             ; Get the keyboard port address from table
    INC HL                  ; Increment to list of keys
    IN  A, (C)              ; Read the row of keys in
    AND #1F                 ; We are only interested in the first five bits
    LD  E, 5                ; This is the number of keys in the row
ReadKeyboard1:        
    SRL A                   ; Shift A right; bit 0 sets carry bit
    JR  NC, ReadKeyboard2   ; If the bit is 0, we've found our key
    INC HL                  ; Go to next table address
    DEC E                   ; Decrement key loop counter
    JR  NZ, ReadKeyboard1   ; Loop around until this row finished
    DEC D                   ; Decrement row loop counter
    JR  NZ, ReadKeyboard0   ; Loop around until we are done
    AND A                   ; Clear A (no key found)
    LD  (LastKeyPressed), A
    RET
ReadKeyboard2:        
    LD  A, (HL)             ; We've found a key at this point; fetch the character code!
    LD  B, A
    LD  A,(LastKeyPressed)
    CP  0
    JR  Z, ReadKeyboard3    ; If no key pressed before, skip the check
    CP  B                   ; Check if the key is the same as the last one pressed
    JR  NZ, ReadKeyboard3    ; If it is, skip the rest of this routine
    XOR A               ; If it is the same, clear A (no key found)
    RET
ReadKeyboard3:
    LD  A, B
    LD  (LastKeyPressed), A
    LD  (KeyPressed), A
    RET
;--------------------------------------------------
; Check and mark completed rows
; INPUT: -
; OUTPUT: -
; MODIFY: AF, BC, DE, HL
;--------------------------------------------------
CheckAndMarkCompletedRows:
    XOR     A                ; A = 0 (nessuna riga completata)
    LD      (CompletedRows), A
    LD      HL, Matrix          ; HL punta all'inizio della matrice
    LD      B, 20               ; Contatore righe (20 righe)
CheckRowLoop:
    PUSH    HL                  ; Salva HL per ripristinarlo dopo
    LD      C, 10               ; Contatore colonne (10 colonne)
    LD      A, 0                ; Flag per riga completata (A=0: completata, A!=0: non completata)
CheckColumnLoop:
    LD      D, (HL)             ; Legge il byte corrente
    CP      D                   ; Confronta con 0
    JR      Z, RowNotCompleted  ; Se un byte è 0, la riga non è completata
    INC     HL                  ; Passa al prossimo byte
    DEC     C                   ; Decrementa il contatore colonne
    JR      NZ, CheckColumnLoop ; Continua a controllare la riga
    ; Se tutte le colonne sono diverse da 0, marca la riga come completata
    POP     HL                  ; Ripristina HL all'inizio della riga
    LD      C, 10               ; Reset contatore colonne
    LD      A, 0XFF ; Valore per riga completata
MarkRowCompleted:
    LD      (HL), A             ; Imposta il byte corrente a CHAR_ROW_COMPLETED
    INC     HL                  ; Passa al prossimo byte
    DEC     C                   ; Decrementa il contatore colonne
    JR      NZ, MarkRowCompleted ; Continua a marcare la riga
    LD      A, (CompletedRows)     ; A = valore corrente di CompletedRows
    ADD     1                   ; Incrementa il contatore righe completate
    LD      (CompletedRows), A  ; Marca la riga come completata
    JR      NextTitleRow             ; Passa alla riga successiva
RowNotCompleted:
    POP     HL                  ; Ripristina HL all'inizio della riga
    LD      DE, 10              ; DE = 10 (numero di colonne per riga)
    ADD     HL, DE              ; HL = HL + DE (salta alla prossima riga)
NextTitleRow:
    DEC     B                   ; Decrementa il contatore righe
    JR      NZ, CheckRowLoop    ; Continua a controllare le righe
    RET                         ; Fine della subroutine
;----------------------------------------------------------------------
;  Constants
;----------------------------------------------------------------------
MATRIX       EQU 8000h         ; Indirizzo base dell’area 20x10 (200 bytes)
ROWS         EQU 20
COLS         EQU 10
SIZE         EQU ROWS * COLS    ; 200
KEYBOARD_MAP:                   db #FE,"#","Z","X","C","V"
                                db #FD,"A","S","D","F","G"
                                db #FB,"Q","W","E","R","T"
                                db #F7,"1","2","3","4","5"
                                db #EF,"0","9","8","7","6"
                                db #DF,"P","O","I","U","Y"
                                db #BF,"#","L","K","J","H"
                                db #7F," ","#","M","N","B"
BLOCK_T:        EQU 2
BLOCK_I:        EQU 0
BLOCK_O:        EQU 1
BLOCK_S:        EQU 3
BLOCK_Z:        EQU 4
BLOCK_L:        EQU 5
BLOCK_J:        EQU 6

ORIENT_NORTH:   EQU 0   ; Matrix as is (20 high, 10 wide)
ORIENT_SOUTH:   EQU 1   ; Upside down (20 high, 10 wide)
ORIENT_EAST:    EQU 2   ; Rotated 90° right (10 high, 20 wide)
ORIENT_WEST:    EQU 3   ; Rotated 90° left  (10 high, 20 wide)
BLOCK_3x3:
    DEFB    0,0
    DEFB    0,1
    DEFB    0,2
    DEFB    1,0
    DEFB    1,1
    DEFB    1,2
    DEFB    2,0
    DEFB    2,1
    DEFB    2,2
    DEFB    0xFF
BLOCK_4x4:
    DEFB    0,0
    DEFB    0,1
    DEFB    0,2
    DEFB    0,3
    DEFB    1,0
    DEFB    1,1
    DEFB    1,2
    DEFB    1,3
    DEFB    2,0
    DEFB    2,1
    DEFB    2,2
    DEFB    2,3
    DEFB    3,0
    DEFB    3,1
    DEFB    3,2 
    DEFB    3,3
    DEFB    0xFF
BLOCK_I_DEF_0:
    DEFB    0, 0   
    DEFB    0, 1   
    DEFB    0, 2   
    DEFB    0, 3   
    DEFB    0xFF   
BLOCK_I_DEF_90:
    DEFB    0,0
    DEFB    1,0
    DEFB    2,0
    DEFB    3,0
    DEFB    0xFF
BLOCK_I_DEF_180:
    DEFB    0,0
    DEFB    0,1
    DEFB    0,2
    DEFB    0,3
    DEFB    0xFF
BLOCK_I_DEF_270:
    DEFB    0,0
    DEFB    1,0
    DEFB    2,0
    DEFB    3,0
    DEFB    0xFF
BLOCK_O_DEF_0:
    DEFB    0,0
    DEFB    0,1
    DEFB    1,0
    DEFB    1,1
    DEFB    0xFF
BLOCK_O_DEF_90:
    DEFB    0,0
    DEFB    0,1
    DEFB    1,0
    DEFB    1,1
    DEFB    0xFF
BLOCK_O_DEF_180:
    DEFB    0,0
    DEFB    0,1
    DEFB    1,0
    DEFB    1,1
    DEFB    0xFF
BLOCK_O_DEF_270:
    DEFB    0,0
    DEFB    0,1
    DEFB    1,0
    DEFB    1,1
    DEFB    0xFF
BLOCK_T_DEF_0:
    DEFB    0, 0   
    DEFB    0, 1   
    DEFB    0, 2   
    DEFB    1, 1   
    DEFB    0xFF   
BLOCK_T_DEF_90:
    DEFB    0,1
    DEFB    1,0
    DEFB    1,1
    DEFB    2,1
    DEFB    0xFF
BLOCK_T_DEF_180:
    DEFB    0,1
    DEFB    1,0
    DEFB    1,1
    DEFB    1,2
    DEFB    0xFF
BLOCK_T_DEF_270:
    DEFB    0,0
    DEFB    1,0
    DEFB    1,1
    DEFB    2,0
    DEFB    0xFF
BLOCK_S_DEF_0:
    DEFB    0, 1   
    DEFB    0, 2   
    DEFB    1, 0   
    DEFB    1, 1   
    DEFB    0xFF   
BLOCK_S_DEF_90:
    DEFB    0,0
    DEFB    1,0
    DEFB    1,1
    DEFB    2,1
    DEFB    0xFF
BLOCK_S_DEF_180:
    DEFB    0,1
    DEFB    0,2
    DEFB    1,0
    DEFB    1,1
    DEFB    0xFF
BLOCK_S_DEF_270:
    DEFB    0,0
    DEFB    1,0
    DEFB    1,1
    DEFB    2,1
    DEFB    0xFF
BLOCK_Z_DEF_0:
    DEFB    0, 0   
    DEFB    0, 1   
    DEFB    1, 1   
    DEFB    1, 2   
    DEFB    0xFF   
BLOCK_Z_DEF_90:
    DEFB    0,1
    DEFB    1,0
    DEFB    1,1
    DEFB    2,0
    DEFB    0xFF
BLOCK_Z_DEF_180:
    DEFB    0,0
    DEFB    0,1
    DEFB    1,1
    DEFB    1,2
    DEFB    0xFF
BLOCK_Z_DEF_270:
    DEFB    0,1
    DEFB    1,0
    DEFB    1,1
    DEFB    2,0
    DEFB    0xFF
BLOCK_J_DEF_0:
    DEFB    0, 0   
    DEFB    0, 1   
    DEFB    0, 2   
    DEFB    1, 0   
    DEFB    0xFF   
BLOCK_J_DEF_90:
    DEFB    0,0
    DEFB    0,1
    DEFB    1,1
    DEFB    2,1
    DEFB    0xFF
BLOCK_J_DEF_180:
    DEFB    0,2
    DEFB    1,0
    DEFB    1,1
    DEFB    1,2
    DEFB    0xFF
BLOCK_J_DEF_270:
    DEFB    0,0
    DEFB    1,0
    DEFB    2,0
    DEFB    2,1
    DEFB    0xFF
BLOCK_L_DEF_0:
    DEFB    0, 0   
    DEFB    0, 1   
    DEFB    0, 2   
    DEFB    1, 2   
    DEFB    0xFF   
BLOCK_L_DEF_90:
    DEFB    0,1
    DEFB    1,1
    DEFB    2,0
    DEFB    2,1
    DEFB    0xFF
BLOCK_L_DEF_180:
    DEFB    0,0
    DEFB    1,0
    DEFB    1,1
    DEFB    1,2
    DEFB    0xFF
BLOCK_L_DEF_270:
    DEFB    0,0
    DEFB    0,1
    DEFB    1,0
    DEFB    2,0
    DEFB    0xFF
BLOCK_COLORS:
    DEFB    COLOR_BLUE      ; ID=0
    DEFB    COLOR_YELLOW    ; ID=1
    DEFB    COLOR_CYAN      ; ID=2
    DEFB    COLOR_GREEN     ; ID=3
    DEFB    COLOR_RED       ; ID=4
    DEFB    COLOR_MAGENTA   ; ID=5
    DEFB    COLOR_WHITE     ; ID=6

BLOCKS_WIDTH:
    DB  4           ; Block I - 0 Degrees
    DB  1           ; Block I - 90 Degrees
    DB  4           ; Block I - 180 Degrees
    DB  1           ; Block I - 270 Degrees
    DB  2           ; Block O - 0 Degrees
    DB  2           ; Block O - 90 Degrees
    DB  2           ; Block O - 180 Degrees
    DB  2           ; Block O - 270 Degrees
    DB  3           ; Block T - 0 Degrees
    DB  2           ; Block T - 90 Degrees
    DB  3           ; Block T - 180 Degrees
    DB  2           ; Block T - 270 Degrees
    DB  3           ; Block S - 0 Degrees
    DB  2           ; Block S - 90 Degrees
    DB  3           ; Block S - 180 Degrees
    DB  2           ; Block S - 270 Degrees
    DB  3           ; Block Z - 0 Degrees
    DB  2           ; Block Z - 90 Degrees
    DB  3           ; Block Z - 180 Degrees
    DB  2           ; Block Z - 270 Degrees
    DB  3           ; Block J - 0 Degrees
    DB  2           ; Block J - 90 Degrees
    DB  3           ; Block J - 180 Degrees
    DB  2           ; Block J - 270 Degrees
    DB  3           ; Block L - 0 Degrees
    DB  2           ; Block L - 90 Degrees
    DB  3           ; Block L - 180 Degrees
    DB  2           ; Block L - 270 Degrees
BLOCKS_HEIGHT:
    DB  1           ; Block I - 0 Degrees
    DB  4           ; Block I - 90 Degrees
    DB  1           ; Block I - 180 Degrees
    DB  4           ; Block I - 270 Degrees
    DB  2           ; Block O - 0 Degrees
    DB  2           ; Block O - 90 Degrees
    DB  2           ; Block O - 180 Degrees
    DB  2           ; Block O - 270 Degrees
    DB  2           ; Block T - 0 Degrees
    DB  3           ; Block T - 90 Degrees
    DB  2           ; Block T - 180 Degrees
    DB  3           ; Block T - 270 Degrees
    DB  2           ; Block S - 0 Degrees
    DB  3           ; Block S - 90 Degrees
    DB  2           ; Block S - 180 Degrees
    DB  3           ; Block S - 270 Degrees
    DB  2           ; Block Z - 0 Degrees
    DB  3           ; Block Z - 90 Degrees
    DB  2           ; Block Z - 180 Degrees
    DB  3           ; Block Z - 270 Degrees
    DB  2           ; Block J - 0 Degrees
    DB  3           ; Block J - 90 Degrees
    DB  2           ; Block J - 180 Degrees
    DB  3           ; Block J - 270 Degrees
    DB  2           ; Block L - 0 Degrees
    DB  3           ; Block L - 90 Degrees
    DB  2           ; Block L - 180 Degrees
    DB  3           ; Block L - 270 Degrees
BLOCKS_TABLE:
        ; ID=0 => pezzo I
    DW      BLOCK_I_DEF_0
    DW      BLOCK_I_DEF_90
    DW      BLOCK_I_DEF_180
    DW      BLOCK_I_DEF_270

    ; ID=1 => pezzo O
    DW      BLOCK_O_DEF_0
    DW      BLOCK_O_DEF_90
    DW      BLOCK_O_DEF_180
    DW      BLOCK_O_DEF_270

    ; ID=2 => pezzo T
    DW      BLOCK_T_DEF_0
    DW      BLOCK_T_DEF_90
    DW      BLOCK_T_DEF_180
    DW      BLOCK_T_DEF_270

    ; ID=3 => pezzo S
    DW      BLOCK_S_DEF_0
    DW      BLOCK_S_DEF_90
    DW      BLOCK_S_DEF_180
    DW      BLOCK_S_DEF_270

    ; ID=4 => pezzo Z
    DW      BLOCK_Z_DEF_0
    DW      BLOCK_Z_DEF_90
    DW      BLOCK_Z_DEF_180
    DW      BLOCK_Z_DEF_270

    ; ID=5 => pezzo J
    DW      BLOCK_J_DEF_0
    DW      BLOCK_J_DEF_90
    DW      BLOCK_J_DEF_180
    DW      BLOCK_J_DEF_270

    ; ID=6 => pezzo L
    DW      BLOCK_L_DEF_0
    DW      BLOCK_L_DEF_90
    DW      BLOCK_L_DEF_180
    DW      BLOCK_L_DEF_270
CHAR_ROW_COMPLETED: 
    DB 0xFF
TEXT_EMPTY5:
    DB "     ",0
TEXT_SCORE:
    DB "SCORE",0
TEXT_HIGH_SCORE_1:
    DB "HIGH ",0
TEXT_HIGH_SCORE_2:
    DB "SCORE",0
TEXT_LEVEL:
    DB "LEVEL",0
TEXT_GAME_OVER_1:
    DB "          ",0
TEXT_GAME_OVER_2:
    DB "   GAME   ",0

TEXT_GAME_OVER_3:
    DB "   OVER   ",0

TEXT_BLOCKS:
    DB "BLKS ",0
TEXT_MAX_BLOCKS:
    DB "MAX  ",0
TEXT_START_LEVEL:
    DB "START LEVEL",0
TEXT_ORIENTATION:
    DB "DIRECTION",0
TEXT_LEVEL_GROWING:
    DB "LEVEL GROWING",0
TEXT_YES:
    DB "YES",0
TEXT_NO:
    DB " NO",0
TEXT_NORTH:
    DB "NORTH",0
TEXT_SOUTH:
    DB "SOUTH",0
TEXT_EAST:
    DB " EAST",0
TEXT_WEST:
    DB " WEST",0
TEXT_CHAOS:
     DB "CHAOS",0
TEXT_SPACE:
     DB " ",0
TEXT_COPYRIGHT:
    DB "2025 FAUSTO PRACEK",0
TEXT_COMMANDS:
    DB "4=LEFT  5=ROTATE  6=RIGHT",0
TEXT_COMMANDS_SPACE:
    DB "SPACE=PULL DOWN",0
TEXT_COMMANDS_SPACE_TO_START:
    DB "PRESS SPACE TO START",0
ZXETRISMATRIX:
    DEFB 255, 255, 255, 0,   255, 0,   255, 0,   255, 255, 255, 0,   255, 255, 255, 0,   255, 255, 255, 0,   255, 0,   255, 255, 255
    DEFB   0,   0, 255, 0,   255, 0,   255, 0,   255,   0,   0,   0,     0, 255,   0,   0,   255,   0,   255,   0,   255,   0,   255,   0,   0
    DEFB   0,   0, 255, 0,   255, 0,   255, 0,   255,   0,   0,   0,     0, 255,   0,   0,   255,   0,   255,   0,   255,   0,   255,   0,   0
    DEFB   0, 255,   0,   0,   0, 255,   0,   0,   255, 255,   0,   0,     0, 255,   0,   0,   255, 255, 255,   0,   255,   0,   0,   255,   0
    DEFB 255,   0,   0,   0, 255,   0, 255,   0,   255,   0,   0,   0,     0, 255,   0,   0,   255, 255,   0,   0,   255,   0,   0,   0, 255
    DEFB 255,   0,   0,   0, 255,   0, 255,   0,   255,   0,   0,   0,     0, 255,   0,   0,   255,   0,   255,   0,   255,   0,   0,   0, 255
    DEFB 255, 255, 255, 0,   255, 0,   255, 0,   255, 255, 255, 0,     0, 255,   0,   0,   255,   0,   255,   0,   255,   0,   255, 255, 255
COLORS_TABLE:
    DEFB COLOR_BLUE
    DEFB COLOR_YELLOW
    DEFB COLOR_CYAN
    DEFB COLOR_GREEN
    DEFB COLOR_RED
    DEFB COLOR_MAGENTA
    DEFB COLOR_WHITE
InGameTuneData:                    
    DB 0x1B,0x24,0x22,0x1E,0x22,0x24,0x28,0x28    ; E5, B4, C5, D5, C5, B4, A4, A4
    DB 0x22,0x1B,0x1E,0x22,0x24,0x22,0x1E,0x1B    ; C5, E5, D5, C5, B4, C5, D5, E5
    DB 0x1B,0x22,0x28,0x24,0x22,0x24,0x22,0x1E    ; E5, C5, A4, A4, C5, E5, D5, C5
    DB 0x24,0x22,0x1B,0x1B,0x22,0x1B,0x22,0x24    ; B4, C5, D5, E5, C5, A4, A4, C5
    DB 0x22,0x1E,0x1B,0x1B                        ; E5, C5, A4, A4  (chiusura/frase finale)

;----------------------------------------------------------------------
;  Variabili
;----------------------------------------------------------------------
Matrix:         
    DEFS 200 ; 200 bytes for the game matrix (20x10)
TempMatrix:         
    DEFS 200 
CurrentMatrixOrientation:
    DEFB ORIENT_NORTH
SavedY:         
    DEFB 0
SavedX:         
    DEFB 0
ColorOfBlock:       
    DEFB  0   ; usato per salvare il colore del blocco
CurrentBlockTable: 
    DW 0,0
CurrentBlockRotation:
    DEFB    0                           ; Rotazione corrente del pezzo (0-3)
CurrentBlock:
    DEFB    0                           ; Pezzo corrente 
BlockCoordinates:
    DEFS    16                          ; Coordinate dei byte occupati dal pezzo
    DEFB    0xFF                        ; Fine delle coordinate
CurrentPosition:
    DEFB    0                           ; Offset corrente nella matrice
PreviousPosition:
    DEFB    0                           ; Offset precedente nella matrice
Seed:           
    DB      0x5A                        ; Seed for random number generation 
FrameCounter:
    DB      0                           ; Contatore di frame per il movimento del pezzo
SecondsCounter:
    DB      0                           ; Contatore di secondi per il movimento del pezzo
MinutesCounter:
    DB      0                           ; Contatore di minuti per il movimento del pezzo
Score:
    DW      0                           ; Current score
HighScore:
    DW      0                           ; High score
Level:
    DB      0                           ; Livello corrente
BlockSpeed:
    DB      0                           ; Velocità del pezzo corrente
BlockSpeedCounter:
    DB      0                           ; Contatore di velocità del pezzo corrente
PrintMatrixMaxRow:
    DB      0                          ; Massimo numero di righe nella matrice
BlockLeftMoving:
    DB      0                          ; Flag per il movimento a sinistra del pezzo corrente, se 1 vuol dire verso sinistra, se 2 verso destra (viene usato solo i movimenti laterali)
TempXBlocPosition:
    DB      0                          ; Posizione temporanea del pezzo corrente (per verfica spostamenti laterali)
LastKeyPressed:
    DB      0                          ; Ultimo tasto premuto
KeyPressed:
    DB      0                          ; Tasto premuto (0=NO, 1=SI)
ForcePrintMatrixRepaintAll:
    DB      0                          ; Forza il ridisegno della matrice (0=NO, 1=SI)
CompletedRows:
    DB      0                          ; Righe completate (0=NO, 1=SI)
Changes:     
    DEFB 0
ChangesOnce: 
    DEFB 0
NextBlock:
    DEFB    0                           ; Pezzo successivo 
NextBlockXPosition:
    DEFB 0
NextBlockYPosition
    DEFB 0
Bug:
    DB 0x00                              ; Used for bug workaround only
TempBlock:
    DEFS 16
ShowNextBlock:
    DB 0                              ; Flag to show next block (0=NO, 1=YES)
NextBlockColor:
    DB 0                              ; Color of the next block
NextBlockTable:
    DW 0,0
LastSamprSprite:
    DB 0                              ; Last sprite used for Sampr
MoveToBottomInAction:
    DB 0                              ; Flag to indicate if the block is moving to the bottom (0=NO, 1=YES)
CurrentWallTile:
    DB 0                              ; Current wall orientation (0=left, 1=right)
PlacedBlocks:
    DW 0                              ; Number of blocks placed
MaxPlacedBlocks:
    DW 0                              ; Amx number of blocks placedù
NumberValue:  DB "12345",0   ;  characters plus null terminator
LevelBlocksCounter:
    DB 0                              ; Number of blocks placed in the current level
SelectedLevel:
    DB 0                              ; Selected level (0-9)
SelectedOrientation:
    DB 0                              ; Selected orientation (0-3)  
SelectedCanGrow:
    DB 0                              ; Selected can grow (0=NO, 1=YES)
SelectedOption:
    DB 0                              ; Selected option (0=NO, 1=YES)
TitleTimer:
    DB 0                              ; Timer for title screen
GameOver:
    DB 0                              ; Game over flag (0=NO, 1=YES)
MusicNoteIndex:
    DB 0                              ; Index for music note
InGame:
    DB 0                              ; In game flag (0=NO, 1=YES)