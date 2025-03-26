; ===================================================================
; Screen subroutines
; ===================================================================

    
    
;------------------------------------------------------------------------
; Load 8x8 tiles into RAM
; INPUT: -
; OUTPUT: -
; MODIFIES: DE, HL, BC
;------------------------------------------------------------------------
Screen_LoadTiles:
        DI                      ; Interrupts disabled
        LD   DE, RAM_CHAR_SET_ADDRESS + (96*8)   
        LD   HL, TILES_DEF   
        LD   BC, 312
        LDIR
        EI                      ; Interrupts enabled
        RET

;------------------------------------------------------------------------
; Clear the screen and set attributes
; INPUT: -
; OUTPUT: -
; MODIFIES: AF, DE, HL, BC
;------------------------------------------------------------------------
Screen_ClearScreen:
        ld hl, 16384        ;pixels 
        ld de, 16385        ;pixels + 1
        ld bc, 6143         ;pixels area length - 1
        ld (hl), 0          ;set first byte to '0'
        ldir                ;copy bytes
        LD a, %000000 <<3 | %000100 | %1000000
        LD hl, ATTR_BASE               ; start at attribute start
        LD de, ATTR_BASE + 1           ; copy to next address in attributes
        LD bc, 768 - 1    ; 'loop' attribute size minus 1 times
        LD (hl), a                      ; initialize the first attribute
        LDIR                            ; fill the attributes
        LD   A, 0       ; A = 0 (bit [2:0] = 0 => Nero)
        OUT  (0xFE), A  ; scrive 0 nella porta 0xFE
        xor a         ; start from row 0
ClearScreenRowLoop:
        cp 24
        ret z
        ld d, a
        push af
        xor a      ; start from column 0
ClearScreenColLoop:
        cp 32
        jp z, ClearScreenColLoopEnd
        ld e, a
        push af
        ld a,32
        call Screen_PrintRamChar
        pop af
        inc a
        jp ClearScreenColLoop
ClearScreenColLoopEnd:        
        pop af
        inc a
        jp ClearScreenRowLoop
        RET

;------------------------------------------------------------------------
; Print a single RAM character out to a screen address
; INPUT:
;   A: Character to print
;   D: Character Y position
;   E: Character X position
; OUTPUT: -
; MODIFIES: -
;------------------------------------------------------------------------
Screen_PrintRamChar:           
        PUSH DE
        EXX                             ; Backup registers BC, DE, HL
        POP DE
        PUSH AF
        LD HL, RAM_CHAR_SET_ADDRESS     ; Character set bitmap data in ROM
        LD B,0                          ; BC = character code
        LD C, A
        SLA C                           ; Multiply by 8 by shifting
        RL B
        SLA C
        RL B
        SLA C
        RL B
        ADD HL, BC                      ; And add to HL to get first byte of character
        CALL Screen_GetCharAddress           ; Get screen position in DE
        LD B,8                          ; Loop counter - 8 bytes per character
PrintRamCharL1:          
        LD A,(HL)                       ; Get the byte from the ROM into A
        LD (DE),A                       ; Stick A onto the screen
        INC HL                          ; Goto next byte of character
        INC D                           ; Goto next line on screen
        DJNZ PrintRamCharL1             ; Loop around whilst it is Not Zero (NZ)
        EXX                             ; Restore registers BC, DE, HL
        POP AF
        RET
;------------------------------------------------------------------------
; Print a single ROM character out to a screen address
; INPUT:
;   A: Character to print
;   D: Character Y position
;   E: Character X position
; OUTPUT: -
; MODIFIES: -
;------------------------------------------------------------------------
Screen_PrintRomChar:          
        PUSH DE
        EXX                             ; Backup registers BC, DE, HL
        POP DE
        PUSH AF
        LD HL, ROM_CHAR_SET_ADDRESS     ; Character set bitmap data in ROM
        LD B,0                          ; BC = character code
        SUB 32                          ; Adjust for the character set
        LD C, A
        SLA C                           ; Multiply by 8 by shifting
        RL B
        SLA C
        RL B
        SLA C
        RL B
        ADD HL, BC                      ; And add to HL to get first byte of character
        CALL Screen_GetCharAddress           ; Get screen position in DE
        LD B,8                          ; Loop counter - 8 bytes per character
PrintRomCharL1:          
        LD A,(HL)                       ; Get the byte from the ROM into A
        LD (DE),A                       ; Stick A onto the screen
        INC HL                          ; Goto next byte of character
        INC D                           ; Goto next line on screen
        DJNZ PrintRomCharL1             ; Loop around whilst it is Not Zero (NZ)
        EXX                             ; Restore registers BC, DE, HL
        POP AF
        RET
;------------------------------------------------------------------------
; Get screen address from a character (X,Y) coordinate
; INPUT:
;   D: Y character position (0-23)
;   E: X character position (0-31)
; OUTPUT:
;   DE: screen address 
; MODIFIES: A
;------------------------------------------------------------------------
Screen_GetCharAddress:       
        LD A,D
        AND %00000111
        RRA
        RRA
        RRA
        RRA
        OR E
        LD E,A
        LD A,D
        AND %00011000
        OR %01000000
        LD D,A
        RET                             