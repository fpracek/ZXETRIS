; ===================================================================
; Inputs
; ===================================================================

; -----------------------------------------------------------------------------
; CheckInput
; -----------------------------------------------------------------------------
; Reads Kempston joystick (port #1F) and the keyboard (Q, A, O, P).
; Returns in A one of these values:
;   0..7 => direction pressed
;   255 => no direction
;
; Kempston bits:
;   bit0=right, bit1=left, bit2=down, bit3=up
; We decode to 8 directions with possible diagonals.
;
; If no joystick direction, we scan the keys:
;   Q => up,    A => down
;   O => left,  P => right
; Combinations => diagonals.
;
; The code is just an example. Adapt it as needed.
; -----------------------------------------------------------------------------

CheckInput:

    ; 1) Read Kempston joystick
    ;IN    A,(#1F)      ; read port #1F into A
    ;AND   #0F          ; we only care about bits 0..3
    ;CP    0
    ;JR    NZ,DecodeJoystick

    ; if A=0 => no joystick direction => check keyboard
    JP    ReadKeyboard

; -----------------------------------------------------------------------------
; DecodeJoystick: bits 3..0 => up,down,left,right => produce a direction
; -----------------------------------------------------------------------------
DecodeJoystick:
    LD    B,A        ; keep a copy in B
    ; bit3=up=8, bit2=down=4, bit1=left=2, bit0=right=1

    ; We want to combine them for diagonals: up+left => 4, up+right => 5, etc.
    ; E.g. if bit3 & bit1 => up-left => direction=4
    ; We'll do a simple check approach:

    LD    A,#FF      ; default = none, we'll override
    ; check if up
    BIT   3,B
    JR    Z,NoUp
    ; up=1
    ; check if left
    BIT   1,B
    JR    NZ,UpLeft
    ; check if right
    BIT   0,B
    JR    NZ,UpRight
    ; otherwise pure up
    LD    A,2
    RET
UpLeft:
    LD    A,4
    RET
UpRight:
    LD    A,5
    RET
NoUp:
    ; check if down
    BIT   2,B
    JR    Z,NoDown
    ; down=1
    ; check if left
    BIT   1,B
    JR    NZ,DownLeft
    ; check if right
    BIT   0,B
    JR    NZ,DownRight
    ; otherwise pure down
    LD    A,3
    RET
DownLeft:
    LD    A,6
    RET
DownRight:
    LD    A,7
    RET
NoDown:
    ; no up, no down => check left or right
    BIT   1,B
    JR    NZ,JustLeft
    BIT   0,B
    JR    NZ,JustRight
    ; if we get here => something else, but we said bits 0..3 => must be 0 => not possible
    LD    A,#FF
    RET
JustLeft:
    LD    A,0
    RET
JustRight:
    LD    A,1
    RET

