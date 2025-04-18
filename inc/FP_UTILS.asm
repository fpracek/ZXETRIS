;**************************************************************************
;* FP UTILITIES                                                           *
;**************************************************************************


;============================================================
; STRING UTILITIES
;============================================================

;--------------------------------------------------------------------------
; *** String_NumberToASCII ***
; Convert a 16-bit unsigned number to a string of ASCII digits.
;---------------------------------------------------------------------------
; INPUT:
;   HL = the 16-bit unsigned number to convert.
; OUTPUT: -
; MODIFY: HL, DE, BC, AF
;--------------------------------------------------------------------------
String_NumberToASCII:
   LD       BC,-10000
   CALL     .Num1
   LD       BC,-1000
   CALL     .Num1
   LD       BC,-100
   CALL     .Num1
   LD       C,-10
   CALL     .Num1
   LD       C,-1
.Num1:
   LD       A,'0'-1
.Num2:
   INC      A
   ADD      HL, BC
   JR       C, .Num2
   SBC      HL, BC
   LD       (DE), A
   INC      DE
   RET

;---------------------------------------------------------------------
; *** String_RemoveLeadingZeros ***
; Remove leading zeros from the string
;----------------------------------------------------------------------
; INPUT:
;   HL: Pointer to the string
; OUTPUT: -
; MODIFY: HL, AF, BC
;---------------------------------------------------------------------
String_RemoveLeadingZeros:
    CALL    String_GetLength  ; Get the length of the string
    LD      B, A            ; Set B to the number of digits 
.Loop:
    ; Check if we are at the last digit; if so, exit.
    LD      A, B
    CP      1
    RET     Z
    ; Load the current character.
    LD      A, (HL)
    CP      '0'
    RET     NZ  ; If the character is not '0', we've reached the first
                          ; nonzero digit; stop replacing.
    ; Replace the '0' with a space.
    LD       A, ' '
    LD      (HL), A
    INC     HL              ; Advance to the next character.
    DEC     B               ; Decrement the digit counter.
    JR      .Loop


;---------------------------------------------------------------------
; *** String_GetLength ***
; Get the length of a string in bytes
;----------------------------------------------------------------------
; INPUT:
;   HL: Pointer to the string
; OUTPUT: -
; MODIFY: HL, AF, BC
;---------------------------------------------------------------------
String_GetLength:
    PUSH    HL          ; save HL
    XOR     A           ; A = 0
    LD      B, A        ; B = 0 (length counter)

.Loop:
    LD      A, (HL)     ; load byte
    OR      A           ; set Z if it’s zero
    JR      Z, .Done
    INC     B           ; increment length
    INC     HL
    JR      .Loop

.Done:
    POP     HL          ; restore HL
    LD      A, B        ; return length in A
    RET

;================================================================
; MATH UTILITIES
;================================================================


;-----------------------------------------------------------------------
; *** Math_AddAToHL ***
; Adds the 8‑bit value in A to HL
;-----------------------------------------------------------------------
; INPUT:
;   A:  Value to add
;   HL: Pointer to the variable
; OUTPUT:
;   HL: HL + A
; MODIFY: A, BC
;----------------------------------------------------------------------- 
Math_AddAToHL:
    LD    B, 0        ; B = 0
    LD    C, A     ; C = A
    ADD   HL, BC   ; HL = HL + BC = HL + A
    RET

;--------------------------------------------------------------------------
; *** Math_CompareHLtoBC ***
; Compare the 16‑bit unsigned value in HL with the 16‑bit unsigned value in BC
;--------------------------------------------------------------------------
; INPUT:
;   HL: first 16‑bit value
;   BC: second 16‑bit value
;
; OUTPUT:
;   A: Comparison result (1->HL>BC, 2->HL<BC, 0->HL=BC)
; MODIFY: AF
;--------------------------------------------------------------------------
Math_CompareHLtoBC:
    LD   A, H               ; A ← high byte of HL
    CP   B                  ; compare with high byte of BC
    JR   Z, .CompareLow      ; if H == B, compare low bytes
    JR   C, .SetLess         ; if H < B, HL < BC
    ; else H > B ⇒ HL > BC
    LD   A, 1
    RET

.CompareLow:
    LD   A, L               ; A ← low byte of HL
    CP   C                  ; compare with low byte of BC
    JR   Z, .SetEqual        ; if L == C, HL == BC
    JR   C, .SetLess         ; if L < C, HL < BC
    ; else L > C ⇒ HL > BC
    LD   A, 1
    RET

.SetLess:
    LD   A, 2
    RET

.SetEqual:
    XOR  A                  ; A = 0
    RET

;------------------------------------------------------------------------------
; *** Math_DivisibleCheck ***
; Checks whether the 8‑bit value in B is exactly divisible by the 8‑bit value in C
;--------------------------------------------------------------------------------
; INPUT:
;   B = dividend
;   C = divisor
; OUTPUT:
;   A: 1 if B is divisible by C else 0
;------------------------------------------------------------------------------
Math_DivisibleCheck:
    ; 1) If divisor C = 0, cannot divide → return A=0
    LD   A, C
    OR   A
    JR   Z, .NotDivisible

    ; 2) If dividend B = 0, 0 is divisible by any nonzero C → return A=1
    LD   A, B
    OR   A
    JR   Z, .Divisible

    ; 3) Main loop: subtract C repeatedly from A until A < C
.Loop:
    CP   C               ; compare A (current remainder) with C
    JR   C, .NotDivisible; if A < C then remainder ≠ 0 → not divisible
    SUB  C               ; A = A - C
    OR   A               ; set Z if A == 0
    JR   Z, .Divisible   ; if exact zero remainder, divisible
    JR   .Loop           ; otherwise repeat

.Divisible:
    LD   A, 1
    RET

.NotDivisible:
    XOR  A               ; A = 0
    RET
