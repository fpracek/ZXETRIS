;------------------------------------------------------------------------
; Set screen charater attribute
; INPUT:
;   A: Value
;   D: Character Y position
;   E: Character X position
; OUTPUT: -
; MODIFIES: HL
;------------------------------------------------------------------------
SetAttributeAtDE:
        PUSH    DE
        PUSH    HL
        PUSH    BC
        PUSH    AF
        LD      A, D    ; Load row in A
        ; Before keeping the bits of the third, we carry out the rotations
        RRCA
        RRCA         ; Passes the bits of the third to bits 0 and 1
        RRCA         ; and those of the row to bits 5, 6 and 7
        LD      L, A    ; Load the result in L
        AND     0x03     ; A = bits of the third
        OR      0x58     ; Adds the fixed bits of the high part of the address
        LD      H, A    ; H = 0101 10TT
        LD      A, L   ; A = row in bits 5, 6 and 7 and third in bits 0 and 1
        AND     0xE0     ; Keeps the bits of the line
        OR      E       ; Adds the bits of the column
        LD      L, A    ; L = RRRC CCCC
        POP     AF
        LD      (HL),A          ; Scrivi l'attributo
        CP      0xFF
        JR      NZ, SetAttributeAtDE_1

        LD      (HL),COLOR_RED
SetAttributeAtDE_1:
        

        
        POP     BC
        POP     HL
        POP     DE
        RET
;------------------------------------------------------------------------
; Print string
; INPUT:
;  HL: String address
;  D: Y position
;  E: X position
;  B: If equals to 1 use yellow character
; OUTPUT: -
; MODIFIES: DE, HL, BC
;------------------------------------------------------------------------
Screen_PrintString:
        LD      C, 0x47
        LD      A, B
        LD      B, C
        CP      1
        JR      NZ, Screen_PrintStringContinue
        LD      B, 0x46
Screen_PrintStringContinue:
        LD      A, (HL)            ; Load address of current string position to A
        OR      A                  ; Flag updating in base of A value
        RET     Z          
        PUSH    AF 
        LD      A, B
        CALL SetAttributeAtDE
        POP     AF
        PUSH    AF
        SUB     27
        CALL Screen_PrintRamChar
        POP     AF
        INC     E
        INC     HL                 ; Current string position address set to next element
        JR   Screen_PrintStringContinue        ; Loop repeat

;-----------------------------------------------------
; Print space character with a specified attribute
; INPUT:
;   A: Attribute
;   D: Y position
;   E: X position
; OUTPUT: -
; MODIFIES: AF
;-----------------------------------------------------
Screen_PrintSpaceChar:
        EXX                      

        ;EX      AF,AF'
        PUSH    DE
        ;EX      AF,AF'      
        CALL    SetAttributeAtDE
        POP     DE
        ;EX      AF,AF'   
        LD      HL, ROM_CHAR_SET_ADDRESS
        LD      B,0
        LD      C,0              
        ADD     HL,BC
        CALL    Screen_GetCharAddress
        LD      B,8
PrintLoop:
        LD      A,(HL)         
        LD      (DE),A         
        INC     HL
        INC     D             
        DJNZ    PrintLoop
        EXX                
        RET
;------------------------------------------------------------------------
; Load 8x8 tiles into RAM
; INPUT: -
; OUTPUT: -
; MODIFIES: DE, HL, BC
;------------------------------------------------------------------------
Screen_LoadTiles:
        DI                      ; Interrupts disabled
        LD      DE, RAM_CHAR_SET_ADDRESS   
        LD      HL, TILES_DEF   
        LD      BC, 552
        LDIR
        EI                      ; Interrupts enabled
        RET

;------------------------------------------------------------------------
; Clear the screen and set attributes
; INPUT: -
; OUTPUT: -
; MODIFIES: AF, DE, HL, BC
;------------------------------------------------------------------------
Screen_Clear:
        LD   A, 0           ; 0 in the lower 3 bits = black
        OUT  (254), A       ; Send A to port 0xFE
        
        ;----------------------------------------------------------
        ; Clear 6144 bytes of screen pixel area (0x4000..0x57FF)
        ;----------------------------------------------------------
        
        LD   HL, 0x4000      ; Start address of pixel area
        LD   DE, 0x4001      ; DE = HL + 1 for LDIR
        LD   BC, 6144        ; Number of bytes to clear
        LD   (HL), 0         ; Store 0 in the first byte
        LDIR                 ; Repeats until BC = 0 (fills with 0)
        
        ;----------------------------------------------------------
        ; Fill 768 bytes of attributes area (0x5800..0x5AFF)
        ; with 0x07 (white on black)
        ;----------------------------------------------------------
        
        LD   HL, 0x5800      ; Start address of attributes area
        LD   DE, 0x5801      ; DE = HL + 1 for LDIR
        LD   BC, 768         ; Number of attribute bytes
        LD   (HL), 0x07      ; Attribute = 0x07 (white on black)
        LDIR                 ; Fill the attribute area with 0x07
        
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
        PUSH    DE
        EXX                                 ; Backup registers BC, DE, HL
        POP     DE
        PUSH    AF
        LD      HL, RAM_CHAR_SET_ADDRESS    ; Character set bitmap data in ROM
        LD      B,0                         ; BC = character code
        LD      C, A
        SLA     C                           ; Multiply by 8 by shifting
        RL      B
        SLA     C
        RL      B
        SLA     C
        RL      B
        ADD     HL, BC                      ; And add to HL to get first byte of character
        CALL    Screen_GetCharAddress       ; Get screen position in DE
        LD      B,8                         ; Loop counter - 8 bytes per character
PrintRamCharL1:          
5       LD      A,(HL)                      ; Get the byte from the ROM into A
        LD      (DE),A                      ; Stick A onto the screen
        INC     HL                          ; Goto next byte of character
        INC     D                           ; Goto next line on screen
        DJNZ    PrintRamCharL1              ; Loop around whilst it is Not Zero (NZ)
        EXX                                 ; Restore registers BC, DE, HL
        POP     AF
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
        PUSH    DE
        EXX                                 ; Backup registers BC, DE, HL
        POP     DE
        PUSH    AF
        LD      HL, ROM_CHAR_SET_ADDRESS    ; Character set bitmap data in ROM
        LD      B,0                         ; BC = character code
        SUB     32                          ; Adjust for the character set
        LD      C, A
        SLA     C                           ; Multiply by 8 by shifting
        RL      B
        SLA     C
        RL      B
        SLA     C
        RL      B
        ADD     HL, BC                      ; And add to HL to get first byte of character
        CALL    Screen_GetCharAddress       ; Get screen position in DE
        LD      B,8                         ; Loop counter - 8 bytes per character
PrintRomCharL1:          
        LD      A,(HL)                      ; Get the byte from the ROM into A
        LD      (DE),A                      ; Stick A onto the screen
        INC     HL                          ; Goto next byte of character
        INC     D                           ; Goto next line on screen
        DJNZ    PrintRomCharL1              ; Loop around whilst it is Not Zero (NZ)
        EXX                                 ; Restore registers BC, DE, HL
        POP     AF
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
        LD      A,D
        AND     %00000111
        RRA
        RRA
        RRA
        RRA
        OR      E
        LD      E,A
        LD      A,D
        AND     %00011000
        OR      %01000000
        LD      D,A
        RET     
;------------------------------------------------------------------------
; Put a 16x16 tile on the screen
; INPUT:
;   A: Tile number (0-255)
;   D: Y position (0-23)
;   E: X position (0-31)
; OUTPUT: -
; MODIFIES: A
;------------------------------------------------------------------------
Screen_Print16x16Tale:
        PUSH    DE
        PUSH    AF
        LD      A, 0x07
        CALL    SetAttributeAtDE
        POP     AF
        CALL    Screen_PrintRamChar        ; Print the first 8x8 tile
        INC     D                       ; Move to the right for the second tile
        INC     A                       ; Increment the tile number
        PUSH    AF
        LD      A, 0x07
        CALL    SetAttributeAtDE
        POP     AF
        CALL    Screen_PrintRamChar        ; Print the second 8x8 tile
        INC     E                       ; Move back to the left for the first tile
        DEC     D                       ; Move down for the next row of tiles
        INC     A                       ; Increment the tile number
        PUSH    AF
        LD      A, 0x07
        CALL    SetAttributeAtDE
        POP     AF
        CALL    Screen_PrintRamChar        ; Print the third 8x8 tile
        INC     D                       ; Move to the right for the second tile
        INC     A                       ; Increment the tile number
        PUSH    AF
        LD      A, 0x07
        CALL    SetAttributeAtDE
        POP     AF
        CALL    Screen_PrintRamChar        ; print the fourth 8x8 tile
        POP     DE
        RET


;------------------------------------------------------------------------
; Constants
;------------------------------------------------------------------------
SCR_BASE                        EQU 0x4000      ; Base address of the Spectrum screen
ATTR_BASE                       EQU 0x5800      ; Start of the attribute area
ROM_CHAR_SET_ADDRESS            EQU 0x3D00      ; Start of the ROM character set
RAM_CHAR_SET_ADDRESS            EQU 0x9C40      ; Start of the RAM character set      
COLOR_BLACK                     EQU 0x00        
COLOR_BLUE                      EQU 0x08        
COLOR_YELLOW                    EQU 0x30        
COLOR_CYAN                      EQU 0x28        
COLOR_GREEN                     EQU 0x20        
COLOR_RED                       EQU 0x10        
COLOR_MAGENTA                   EQU 0x18        
COLOR_WHITE                     EQU 0x38        
COLOR_WALL                      EQU 0x57
;-------------------------------------------------------------------------
; Variables
;-------------------------------------------------------------------------
