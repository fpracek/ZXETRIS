; ===================================================================
; SAM.PR World - 2025 Fausto Pracek
; ===================================================================

        DEVICE ZXSPECTRUM48
        SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION
        
        ORG 0x8000               ; Loader address (0x8000)

        INCLUDE "TILES.asm"
        INCLUDE "SCREEN.asm"
        INCLUDE "INPUTS.asm"
        INCLUDE "FP_UTILS.asm"
        INCLUDE "GAMESUBS.asm"

        



;------------------------------------------------------------------------
; START OF PROGRAM
;------------------------------------------------------------------------

InitGame:
        CALL    SetM2RRoutine
        LD      H, 0
        LD      L, 0
        LD      (MaxPlacedBlocks), HL           ; Set the maximum number of blocks to 0
        LD      (HighScore), HL                 ; Set the high score to 0
        LD      A, 1
        LD      (SelectedLevel), A
        LD      (SelectedCanGrow), A            ; Can grow
        LD      A, 4
        LD      (SelectedOrientation), A        ; CHAOS
        
        CALL    Screen_LoadTiles                ; Load tiles into RAM
InitMenu:
        XOR     A
        LD      (KeyPressed), A
        CALL    ShowMenu                        ; Show menu
        LD      A, 1
        LD      (InGame), A
        LD      A, ORIENT_NORTH
        LD      (CurrentMatrixOrientation), A  ; Set the current matrix orientation to north
        XOR     A
        LD      (KeyPressed), A

StartGame:
        LD      A, (SelectedLevel)
        LD      (Level), A
        CALL    SetSpeedByLevel
        CALL    ShowGameField  ; Show game field
        CALL    InitNewGame
        CALL    MatrixInit
        CALL    ChooseRandomBlock       ; Choose a random piece
        DI                                      ; Disable interrupts
        IM      2                               ; Set the interrupt mode
        EI                                      ; Enable interrupts

;------------------------------------------------------------------------
; TETRIS GAME LOOP
;------------------------------------------------------------------------

TetrisLoop:
        LD      A, (GameOver)
        CP      1
        JP      Z, ShowGameOver
        LD      A, (ShowNextBlock)
        CP      0
        JR      Z, TetrisLoopNext

        LD      A, 1
        LD      HL, (PlacedBlocks)
        CALL    Math_AddAToHL
        LD      (PlacedBlocks), HL      ; Incrementa il numero di pezzi piazzati
        LD      BC, (MaxPlacedBlocks)
        CALL    Math_CompareHLtoBC
        CP      1
        JR      NZ, TetrisLoopNext
        LD      (MaxPlacedBlocks), HL  ; Set the maximum number of blocks to the current number of blocks        
TetrisLoopNext:

        CALL    CheckAndMarkCompletedRows
        LD      A, (CompletedRows)
        CP      0
        JR      Z, TetrisLoopContinue
        DI
        CALL    ScoreUpdate
        LD      (ForcePrintMatrixRepaintAll), A
        CALL    PrintMatrix
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        CALL    RemoveCompletedRows
        ;CALL    PrintMatrix
        EI
TetrisLoopContinue:







        DI
        LD      A, (NextBlock)
        LD      (CurrentBlock), A
        CALL    ChooseRandomBlock       ; Choose a random piece
        LD      A, (NextBlock)
        CALL    UpdateNextBlockInfo
        CALL    SetNextBoxPosition
        CALL    ShowNextBlockOnBox
        CALL    PlaceBlockAtTop         ; Place the piece at the top
        EI

MoveBlock:
        LD      D, 0
        LD      E, 0
        CALL    ReadKeyboard

        LD      A, (ShowNextBlock)
        CP      1
        JP      Z, TetrisLoop
        JP      MoveBlock         ; Continue moving down



        SAVESNA "./out/zxetris.sna", InitGame
END
