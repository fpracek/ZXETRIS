        DEVICE ZXSPECTRUM48
        SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION
        
        INCLUDE "constants.asm"
        INCLUDE "tiles.asm"
        INCLUDE "screen.asm"


        ORG 0x8000               ; Loader address (0x8000)



;========================================================================
; START OF PROGRAM
;========================================================================

StartGame:
        LD   SP, 0xFF58                 ; Inizializza lo stack
        CALL Screen_LoadTiles           ; Load tiles into RAM
        CALL Screen_ClearScreen         ; Clear screen
loop:
        jp loop

      
        SAVESNA "./out/samprwrl.sna", StartGame
END
