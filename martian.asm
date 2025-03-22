        DEVICE ZXSPECTRUM48
        SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION
        
        ORG #8000               ; Loader address (0x8000)

;========================================================================
;  BIOS SUBROUTINES
;========================================================================

;========================================================================
; START OF PROGRAM
;========================================================================

Start:
         
        CALL LoadTiles            ; Load tiles into RAM
        


        LD A, 9
        LD HL, Humans
        LD (HL), A
        LD A, 9 
        LD HL, Martians
        LD (HL), A

;------------------------------------------------------------------------
;  Show game presentation
;------------------------------------------------------------------------
ShowPresentation:
        CALL ClearScreen        ; Clear the screen and set attributes
        LD D,0
        LD E, 10
	ld      HL, TEXT_MARTIAN_WAR
        call PrintString

        LD D,1
        LD E, 10
	ld      HL, TEXT_MARTIAN_WAR_UNDERLINE
        call PrintString

        LD D,3
        LD E, 1
	ld HL, TEXT_PRESENTATION_3_1
        call PrintString

        LD D,4
        LD E, 1
	ld HL, TEXT_PRESENTATION_4_1
        call PrintString

        LD D,6
        LD E, 1
	ld HL, TEXT_PRESENTATION_6_1
        call PrintString

	LD D,7
        LD E, 1
	ld HL, TEXT_PRESENTATION_7_1
        call PrintString

        LD D,8
        LD E, 1
	ld HL, TEXT_PRESENTATION_8_1
        call PrintString

        LD D,9
        LD E, 1
	ld HL, TEXT_PRESENTATION_9_1
        call PrintString

        LD D,10
        LD E, 1
	ld HL, TEXT_PRESENTATION_10_1
        call PrintString

        LD D,11
        LD E, 1
	ld HL, TEXT_PRESENTATION_11_1
        call PrintString

        LD D,12
        LD E, 1
	ld HL, TEXT_PRESENTATION_12_1
        call PrintString

        LD D,13
        LD E, 1
	ld HL, TEXT_PRESENTATION_13_1
        call PrintString

        LD D,14
        LD E, 1
	ld HL, TEXT_PRESENTATION_14_1
        call PrintString

        LD D,15
        LD E, 1
	ld HL, TEXT_PRESENTATION_15_1
        call PrintString

        LD D,16
        LD E, 1
	ld HL, TEXT_PRESENTATION_16_1
        call PrintString

        LD D,17
        LD E, 1
	ld HL, TEXT_PRESENTATION_17_1
        call PrintString

        LD D,18
        LD E, 1
	ld HL, TEXT_PRESENTATION_18_1
        call PrintString

        LD D,19
        LD E, 1
	ld HL, TEXT_PRESENTATION_19_1
        call PrintString

        LD D,21
        LD E, 3
	ld HL, TEXT_PRESENTATION_21_3
        call PrintString

        LD D,23
        LD E, 5
	ld HL, TEXT_FAUSTO_PRACEK
        call PrintString

        CALL WaitUntilEnterIsPressed


;-------------------------------------------------------------------------
; Level selection
;-------------------------------------------------------------------------
LevelSelection:
        
      

        CALL ClearScreen        ; Clear the screen and set attributes

        LD D,0
        LD E, 10
	ld      HL, TEXT_MARTIAN_WAR
        call PrintString

        LD      D,1
        LD      E, 10
	ld      HL, TEXT_MARTIAN_WAR_UNDERLINE
        call    PrintString

	ld	hl, TXT_LEVELS
	ld	e, 0
	ld	d, 7
	call    PrintString

	ld	hl, TXT_LEVEL_1
	ld	e, 5
	ld	d, 9
	call    PrintString

	ld      hl, TXT_LEVEL_2
	ld	e, 5
	ld	d, 10
	call    PrintString

	ld      hl, TXT_LEVEL_3
	ld	e, 5
	ld	d, 11
	call    PrintString

        ld      hl, TXT_KEYS
	ld	e, 0
	ld	d, 15
	call    PrintString

        ld      hl, TXT_KEY_4
	ld	e, 5
	ld	d, 17
	call    PrintString

        ld      hl, TXT_KEY_5
	ld	e, 5
	ld	d, 18
	call    PrintString

        ld      hl, TXT_KEY_6
	ld	e, 5
	ld	d, 19
	call    PrintString

        ld      hl, TXT_KEY_SPACE
	ld	e, 5
	ld	d, 20
	call    PrintString

LevelSelectionLoop:
        call ResetMartianInfo
        CALL ReadKeyboard
        CP 49
        JP Z, LevelSelectionLevel1
        CP 50
        JP Z, LevelSelectionLevel2
        CP 51
        JP Z, LevelSelectionLevel3
        JP LevelSelectionLoop
LevelSelectionLevel1:
        LD A, 1
        LD HL, LevelSelection
        LD (HL), A
        LD A, 12 
        LD HL, Martians
        LD (HL), A
        LD hl, MartianShipsInfo+23              ; Martian ship speed
        ld (hl), 4
        ld hl, MartianShipsInfo
        ld a, 20                                ; Set martian bullet frequency ratio
        ld (hl), a
        ld hl, HumanWeaponHideLevelTimer
        ld a, 15
        ld (hl), a
        JP GameStart
LevelSelectionLevel2:
        LD A, 2
        LD HL, LevelSelection
        LD (HL), A
        LD A, 15 
        LD HL, Martians
        LD (HL), A
        LD hl, MartianShipsInfo+23              ; Martian ship speed
        ld (hl), 3
        ld hl, MartianShipsInfo
        ld a, 8                                 ; Set martian bullet frequency ratio
        ld (hl), a
        ld hl, HumanWeaponHideLevelTimer
        ld a, 15
        ld (hl), a
        JP GameStart
LevelSelectionLevel3:
        LD A, 3
        LD HL, LevelSelection
        LD (HL), A
        LD A, 19 
        LD HL, Martians
        LD (HL), A
        LD hl, MartianShipsInfo+23              ; Martian ship speed
        ld (hl), 1
        ld hl, MartianShipsInfo
        ld a, 20                                ; Set martian bullet frequency ratio     
        ld (hl), a
        ld hl, HumanWeaponHideLevelTimer
        ld a, 30
        ld (hl), a
;------------------------------------------------------------------------
; Game start
;------------------------------------------------------------------------
GameStart:
        LD A, 15
        LD HL, Humans
        LD (HL), A
        CALL ShowGameField
        CALL UpdateInfo

       
        CALL ShowRunningHumans



        LD A, 15
        LD HL, HumanWeaponPosition
        LD (HL), A
        CALL ShowHumanWeapon   
        LD HL, HumanWeaponBulletYPosition
        LD A, 0
        LD (HL), A
        ld hl, MartianShipCollisionIndex
        ld a, 10
        ld (hl), a                                      ; Reset martian index to remove (10 = no martian to remove)
        ld hl, SpriteCollisionStaticHumanPosition
        xor a
        ld (hl), a                                      ; Reset static human position to remove (0 = no human to remove)
        ld	hl, CurrentMartianBullet
	ld	(hl), 0		                        ; Reset martian bullets counter
        call ResetHumanWeaponHideTimer
        call ResetMartianBulletsPositions
;------------------------------------------------------------------------
; Main game loop
;------------------------------------------------------------------------
GameLoop:
        JP CheckGameStatus
        call MartianBulletFireEvaluation
GameLoopAfterStatus:
        CALL RemoveMartianShipAfterCollision
        CALL RemoveStaticHumanAfterCollision
        CALL CheckHumanWeaponHideTimer
        CALL ReadKeyboard
        CP 32   
        JP Z, GameLoopHumanWeaponFire
        CP 52
        JP Z, GameLoopHumanWeaponDirectionLeft
        CP 53
        JP Z, GameLoopHumanWeaponStop
        CP 54
        JP Z, GameLoopHumanWeaponDirectionRight
GameLoopNoFire:
        call NewMartianShipShowingEvaluation
        call MartianShipsMove
GameLoopHumanWeaponMovementManagement:
        CALL GameMainLoopDelay
        LD HL, HumanWeaponPosition
        LD A, (HL)              ; Get current human weapon position
        LD E, A
        LD HL, HumanWeaponDirection
        LD A, (HL)              ; Get current human weapon direction
        CP 1
        JP Z, GameLoopHumanWeaponMoveLeft
        CP 2
        JP Z, GameLoopHumanWeaponMoveRight

GameLoopHumanWeaponBulletManagement:
        LD HL, HumanWeaponBulletYPosition
        LD A, (HL)
        CP 0
        JP NZ, GameLoopHumanBulletMove
GameLoopMartiansBulletsManagement:
        call MartianBulletFireEvaluation
        call MoveMartianBullets
        
        JP GameLoop   
GameLoopHumanBulletMove:
        LD HL, HumanWeaponBulletYPosition
        LD A, (HL)
        DEC A
        CP 1
        JP Z, GameLoopHumanBulletMove1PosDecremented
        DEC A
GameLoopHumanBulletMove1PosDecremented:
        LD (HL), A
        LD D, A
        LD HL, HumansWeaponBulletMaxTop
        CP (HL)
        JP Z, GameLoopHumanRemoveBullet
        INC a
        CP (HL)
        JP Z, GameLoopHumanRemoveBullet
        LD HL, HumanWeaponBulletXPosition
        LD A, (HL)
        LD E, A
        LD A, SPRT_HUMAN_BULLET
        CALL PrintRamChar
        INC A
        INC E
        call PrintRamChar
        LD A, D
        CP 16
        JP Z, GameLoopMartiansBulletsManagement
        
        INC A
        LD D, A
        LD A, 32
        CALL PrintRamChar
        DEC E
        LD A, 32
        CALL PrintRamChar
        INC D
        LD A, 32
        CALL PrintRamChar
        INC E
        LD A, 32
        CALL PrintRamChar
        ld      a, TILE_BORDER_VERTICAL
        ld      e, 20
        ld      d, 1
        call    PrintRamChar
        inc     d
        call    PrintRamChar
        ld      a, TILE_BORDER_BOTTOM_LEFT_CORNER
        inc     d
        call    PrintRamChar
        JP GameLoopMartiansBulletsManagement
GameLoopHumanRemoveBullet:
        LD HL, HumanWeaponBulletYPosition
        LD A, (HL)
        LD D, A
        LD A, 0
        LD (HL), A
        LD HL, HumanWeaponBulletXPosition
        LD A, (HL)
        LD E, A
        INC D
        LD HL, HumansWeaponBulletMaxTop
        LD A, (HL)
        CP 1
        JP Z, GameLoopHumanRemoveBulletContinue
        LD A, 32
        CALL PrintRamChar
        INC E
        call PrintRamChar
GameLoopHumanRemoveBulletContinue:
        INC D
        LD A, 32
        CALL PrintRamChar
        INC E
        LD A, 32
        CALL PrintRamChar
        DEC E
        DEC E
        LD A, 32
        CALL PrintRamChar
        ld      a, TILE_BORDER_VERTICAL
        ld      e, 20
        ld      d, 1
        call    PrintRamChar
        inc     d
        call    PrintRamChar
        ld      a, TILE_BORDER_BOTTOM_LEFT_CORNER
        inc     d
        call    PrintRamChar
        call    UpdateInfo
        call    ShowGameFieldWithoutClear
        
	jp	GameLoopMartiansBulletsManagement


GameLoopHumanWeaponMoveLeft:
        DEC E
        LD A, E
        CP 1
        JP Z, GameLoopHumanWeaponDirectionRightChange
        LD HL, HumanWeaponPosition
        LD (HL), E
        CALL ShowHumanWeapon
        JP GameLoopHumanWeaponBulletManagement
GameLoopHumanWeaponMoveRight:
        INC E
        LD A, E
        CP 30
        JP Z, GameLoopHumanWeaponDirectionLeft
        LD HL, HumanWeaponPosition
        LD (HL), E
        CALL ShowHumanWeapon
        JP GameLoopHumanWeaponBulletManagement
GameLoopHumanWeaponFire:
        
        LD HL, HumanWeaponBulletYPosition
        LD A, (HL)
        CP 0
        JP NZ, GameLoop
        ld hl, HumanWeaponHideTimer
        ld a,(hl)
        cp 0
        JP NZ, GameLoopNoFire
        LD HL, HumansWeaponBulletMaxTop
        LD A, 0
        LD (HL), A
        LD HL, HumanWeaponBulletYPosition
        LD A, 18
        LD (HL), A        ; Set bullet Y position
        LD HL, HumanWeaponPosition
        LD A, (HL)
        LD HL, HumanWeaponBulletXPosition
        LD (HL), A        ; Set bullet X position
        CP 20
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 21
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 22
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 23
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 24
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 25
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 26
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 27
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 28
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 29
        JP Z, GameLoopHumanWeaponFireLowerTop
        CP 30
        JP Z, GameLoopHumanWeaponFireLowerTop
GameLoopHumanWeaponFireContinue:
        JP GameLoopHumanWeaponMovementManagement
GameLoopHumanWeaponFireLowerTop:
        LD HL, HumansWeaponBulletMaxTop
        LD A, 2
        LD (HL), A        ; Set max top bullet limit
        JP GameLoopHumanWeaponFireContinue
GameLoopHumanWeaponDirectionLeft:
        ld HL,HumanWeaponDirection
        LD A,1
        LD (HL),A
        JP GameLoopHumanWeaponMovementManagement

GameLoopHumanWeaponDirectionRightChange:
        LD HL, HumanWeaponPosition
        LD (HL), A
        CALL ShowHumanWeapon
GameLoopHumanWeaponDirectionRight:
        
        ld HL,HumanWeaponDirection
        LD A,2
        LD (HL),A
        JP GameLoopHumanWeaponBulletManagement
GameLoopHumanWeaponStop:
        ld HL,HumanWeaponDirection
        LD A,0
        LD (HL),A
        JP GameLoopHumanWeaponMovementManagement
;========================================================================
;  ROUTINES
;========================================================================

; ==============================================
; Rnd16
; ----------------------------------------------
; Generates a 16-bit pseudo-random number in HL
; using a linear-feedback shift register (LFSR).
; On return:
;   HL = new pseudo-random value
; The new seed is also stored back into RandomSeed.
; ==============================================
Rnd16:
  ; 1) Load current seed from memory into HL
  LD HL,(RandomSeed16)

  ; 2) Compute feedback bit as XOR of bits 15, 13, 12, 10 of the old seed.
  ;    bit 15 = H bit 7
  ;    bit 13 = H bit 5
  ;    bit 12 = H bit 4
  ;    bit 10 = H bit 2

  LD A,0
  LD B,H        ; We'll check specific bits in B

  ; Check bit 7 (old bit15)
  BIT 7,B
  JR NZ,Bit7is1
Bit7is0:
  JR CheckBit5
Bit7is1:
  XOR 1         ; A = A XOR 1
CheckBit5:
  BIT 5,B       ; old bit13
  JR NZ,Bit5is1
Bit5is0:
  JR CheckBit4
Bit5is1:
  XOR 1
CheckBit4:
  BIT 4,B       ; old bit12
  JR NZ,Bit4is1
Bit4is0:
  JR CheckBit2
Bit4is1:
  XOR 1
CheckBit2:
  BIT 2,B       ; old bit10
  JR NZ,Bit2is1
Bit2is0:
  JR FeedbackDone
Bit2is1:
  XOR 1
FeedbackDone:
  ; Now A = 0 or 1, which is the new feedback bit

  ; 3) Shift HL left by 1 bit
  ;    - The leftmost bit (bit15) is lost
  ;    - The carry from L's bit7 goes into H's bit0
  SLA L
  RL H

  ; 4) Force bit 0 of L to the feedback bit in A
  LD C,A        ; Save feedback in C
  LD A,L
  AND 0FEh      ; Clear bit0
  OR  C         ; Set it if feedback=1
  LD L,A

  ; 5) Store updated HL back into memory as the new seed
  LD (RandomSeed16),HL

  ; 6) Return with the new pseudo-random number in HL
  RET

; =================================================
; GetMartianShipPosOnLose
; -------------------------------------------------
; Returns in:
;   A = x (column)
;   B = y (row)
;
; Valid positions:
;   - Top zone:     y=1..3,   x=1..18
;   - Middle zone:  y=4..8,   x=1..29
;   - Final zone:   y=15..20, x=1..29
;
; Exclusions:
;   - row 0, 9..14, 21..23
;   - col 0, 30, 31
;   - in top zone, columns 19..31 esclusi
;   - HL must be < 373 => scartiamo se >= 373
; =================================================

GetMartianShipPosOnLose:
  ; 1) Loop until HL < 373
GetRandomValue:
  CALL Rnd16             ; HL = 16-bit random number
  LD DE,373
  CALL CompareHLwithDE   ; Compare HL with DE (373)
  JR NC,GetRandomValue   ; If HL >= 373, discard and regenerate

  ; Now HL < 373
  ; 2) Check sub-zones

  ; --> Top zone threshold: 0..53 (54 positions)
  LD A,H
  OR L                   ; Combine HL in flags
  LD DE,54
  CALL CompareHLwithDE
  JR C,InTopZone         ; If HL < 54, go top zone

  ; --> Middle zone threshold: next 145 positions => 54..198
  LD DE,199             ; 54 + 145 = 199
  CALL CompareHLwithDE
  JR C,InZoneA          ; If HL < 199, go zone A (rows 4..8)

; --> Otherwise, zone B (rows 15..20)
  LD DE,199
  OR A
  SBC HL,DE             ; HL in [0..(174-1)] = 0..173
  ; rows 15..20 => 6 righe
  ; columns 1..29 => 29 colonne

  CALL Divide16BitBy29  ; B=Quotient, C=Remainder
  ; row
  LD A,B
  ADD A,15
  LD B,A
  ; col
  LD A,C
  INC A
  RET

;==================================================
; InZoneA:
; HL in [54..198]
; => adjust: HL -= 54 => range [0..(145-1)] = [0..144]
; rows 4..8 => 5 righe, columns 1..29 => 29 colonne
;==================================================
InZoneA:
  LD DE,54
  OR A
  SBC HL,DE

  CALL Divide16BitBy29   ; B=Quotient, C=Remainder
  ; row
  LD A,B
  ADD A,4
  LD B,A
  ; col
  LD A,C
  INC A
  RET

;==================================================
; InTopZone:
; HL in [0..53]
; => 3 righe (1..3), 18 colonne (1..18)
;==================================================
InTopZone:
  CALL Divide16BitBy18   ; B=Quotient, C=Remainder
  ; row
  LD A,B
  INC A                  ; => y = B + 1 => 1..3
  LD B,A
  ; col
  LD A,C
  INC A                  ; => x = C + 1 => 1..18
  RET


;==================================================
; CompareHLwithDE
;--------------------------------------------------
; Compares HL with DE:
;  - Carry set if HL < DE
;  - Zero set if HL = DE
;==================================================
CompareHLwithDE:
  LD A,H
  CP D
  JR NZ,CheckHighDone
  LD A,L
  CP E
  RET
CheckHighDone:
  RET


;==================================================
; Divide16BitBy18
;--------------------------------------------------
; HL / 18 -> quotient in B, remainder in C
;==================================================
Divide16BitBy18:
  LD B,0
Div18Loop:
  LD A,H
  OR A
  JR Z,CheckLow18

Sub18:
  LD DE,18
  OR A
  SBC HL,DE
  INC B
  JR Div18Loop

CheckLow18:
  LD A,L
  CP 18
  JR C,EndDiv18
  JR Sub18

EndDiv18:
  LD C,L
  LD A,0
  LD H,A
  LD L,A
  RET


;==================================================
; Divide16BitBy29
;--------------------------------------------------
; HL / 29 -> quotient in B, remainder in C
;==================================================
Divide16BitBy29:
  LD B,0
Div29Loop:
  LD A,H
  OR A
  JR Z,CheckLow29

Sub29:
  LD DE,29
  OR A
  SBC HL,DE
  INC B
  JR Div29Loop

CheckLow29:
  LD A,L
  CP 29
  JR C,EndDiv29
  JR Sub29

EndDiv29:
  LD C,L
  LD A,0
  LD H,A
  LD L,A
  RET








;------------------------------------------------------------------------
; Check game status
;------------------------------------------------------------------------
CheckGameStatus:
        LD HL, Martians
        LD A, (HL)
        CP 0
        JP Z, GameOverWin
        LD HL, Humans
        LD A, (HL)
        CP 0
        JP Z, GameOverLose
        JP GameLoopAfterStatus

;------------------------------------------------------------------------
; Game over win
;------------------------------------------------------------------------
GameOverWin:
        call HideHumanWeapon

        ld      hl, TXT_WIN_1
	ld	e, 8
	ld	d, 10
	call    PrintString

        ld      hl, TXT_WIN_2
	ld	e, 1
	ld	d, 12
	call    PrintString

        ld      hl, TXT_WIN_3
	ld	e, 7
	ld	d, 13
	call    PrintString

        ld      hl, TXT_PLAY_AGAIN
	ld	e, 2
	ld	d, 16
	call    PrintString
        call ShowGameFieldWithoutClear

        LD   E, 1               ; Initialize E to 1

LoopNumbers:
        
        ; Check if E is odd or even
        LD   A, E
        AND  1                  ; If bit 0 is 1 => odd, if 0 => even
        JR   Z, SkipNumber      ; If even, skip the call part

        ; Set D=20 and call GetMatrixValue
        LD   D, 20
        CALL GetMatrixValue     ; Return value in A

        ; If A == SPRT_HUMAN_STATIC, then call ShowJumpingHuman
        CP   SPRT_HUMAN_STATIC
        CALL Z, ShowJumpingHuman

SkipNumber:
        ; Increment E
        INC  E
        CP   30                 ; If E == 30, we're past 29
        JR   NZ, LoopNumbers    ; If not 30, continue the loop

        ; If E reached 30, reset to 1 and restart the loop
        LD   E, 1
        JR   LoopNumbers







;------------------------------------------------------------------------
; Game over lose
;------------------------------------------------------------------------
GameOverLose:
        ld hl, RandomSeed16
        ld b, 10
        ld c, 250
        ld (hl), bc
        call HideHumanWeapon

        ld      hl, TXT_LOSE_1
	ld	e, 11
	ld	d, 10
	call    PrintString

        ld      hl, TXT_LOSE_2
	ld	e, 1
	ld	d, 12
	call    PrintString

        ld      hl, TXT_LOSE_3
	ld	e, 2
	ld	d, 13
	call    PrintString

        ld      hl, TXT_PLAY_AGAIN
	ld	e, 2
	ld	d, 22
	call    PrintString

        ld   d,1
        ld   e, 1
        ld   a, SPRT_MARTIAN_SHIP_1_A
        call Draw16x8Tile
GameOverLoseLoop:
        call ShowGameFieldWithoutClear
        call GameMainLoopDelay
        call GetMartianShipPosOnLose
        ld   d,b
        ld   e,a
        call GetNewRandomNumber
        cp   75
        jp   c, GameOverLoseLoopShip1
        cp   150
        jp   c, GameOverLoseLoopShip2
        
        ld   a, SPRT_MARTIAN_SHIP_3_A
        jp   GameOverLoseLoopContinue
GameOverLoseLoopShip1:
        ld   a, SPRT_MARTIAN_SHIP_1_A
        jp   GameOverLoseLoopContinue
GameOverLoseLoopShip2:
        ld   a, SPRT_MARTIAN_SHIP_2_A
GameOverLoseLoopContinue:
        call Draw16x8Tile
        CALL ReadKeyboard
        CP 35
        JP Z, GameOverLoseLoopEnd
        jp GameOverLoseLoop
GameOverLoseLoopEnd:
        call ClearScreen
        JP LevelSelection

;------------------------------------------------------------------------
;  Check human weapon hide timer
;------------------------------------------------------------------------
CheckHumanWeaponHideTimer:
        LD HL, HumanWeaponHideTimer
        LD a, (HL)
        cp 0
        jp nz, CheckHumanWeaponHideTimerContinue
        ret
CheckHumanWeaponHideTimerContinue:
        DEC a
        LD (HL), a
        cp 0
        ret nz
        call ShowHumanWeapon
        ret


;------------------------------------------------------------------------
; Remove static human after collision
;------------------------------------------------------------------------
RemoveStaticHumanAfterCollision:
        LD HL, SpriteCollisionStaticHumanPosition
        LD A, (HL)
        CP 0
        RET Z
        LD HL, SpriteCollisionStaticHumanPosition
        LD D, 20
        LD E, (HL)
        LD (HL), 0      ; Reset remove static human position
        LD A, 32
        CALL PrintRamChar
        INC E
        LD A, 32
        CALL PrintRamChar
        INC D
        LD A, 32
        CALL PrintRamChar
        DEC E
        LD A, 32
        CALL PrintRamChar
        INC D
        LD A, 32
        CALL PrintRamChar
        INC E
        LD A, 32
        CALL PrintRamChar
        LD HL, Humans
        LD A, (HL)
        DEC A
        LD (HL), A              ; Decrease humans counter
        CALL UpdateInfo
        ret
;------------------------------------------------------------------------
; Remove martian ship after collision
;------------------------------------------------------------------------
RemoveMartianShipAfterCollision:
        
        LD      HL, MartianShipCollisionIndex
        LD      A, (HL)
        CP      10
        ret     z
        ld      b,a
        ld      a, 10
        LD      (HL), A                         ; reset MartianShipCollisionIndex (10=no martian to remove)
        LD      a, b
        LD      HL, MartianShipsInfo+6
        call    AddAToHL                   
        ld      a,(HL)                          ; Get current martian Y position
        ld      d,a
        xor     a
        LD      (HL), A                         ; Reset martian Y position

        ld      a, b
        LD      HL, MartianShipsInfo+2
        call    AddAToHL     
        ld      a,(HL)                          ; Get current martian X position
        ld      e,a              
        xor     a
        LD      (HL), A                         ; Reset martian X position

        
        LD      HL, MartianShipsInfo+22
        LD      A, b
        LD      (HL), A                         ; Set current martian index                        
       
												
	ld	hl, MartianShipsInfo+14
        ld      a, b
	call	AddAToHL
	ld	(hl), 0				; Reset flag change direction
        
        call    MartianShipRemove

        ld      HL, HumanWeaponBulletXPosition
        ld      A, (HL)
        ld      E, A
        xor     A
        ld      (HL), A                          ; Reset bullet X position
        ld      HL, HumanWeaponBulletYPosition
        ld      A, (HL)
        ld      D, A
        xor     A
        ld      (HL), A                          ; Reset bullet Y position
        ld      A, 32
        call    PrintRamChar                     ; Clear bullet 1/2
        inc     E
        ld      a, 32
        call    PrintRamChar                     ; Clear bullet 2/2
        LD      HL, Martians
        LD      A, (HL)
        DEC     A
        LD      (HL), A
        CALL    UpdateInfo

        ret
;-----------------------------------------------
; SetValue
; Set A value at [C][B] location
; 
; Input:
;   E = column (0..31)
;   D = row    (0..23)
;   A = valore da memorizzare
; Output: -
;-----------------------------------------------
SetMatrixValue:
        push af
        push bc 
        push de
        push hl
        ld   b, e
        ld   c, d
        ; Save A value into E
        push af

        ; Offset calculation = (C * 32) + B
        ld   a,c            ; A = row

        ld   h,0
        ld   l,a            ; HL = row
        add  hl,hl          ; HL = row * 2
        add  hl,hl          ; HL = row * 4
        add  hl,hl          ; HL = row * 8
        add  hl,hl          ; HL = row * 16
        add  hl,hl          ; HL = row * 32

        ld   d,0            ; Set DE = B
        ld   e,b
        add  hl,de          ; HL = (riga * 32) + column

        ; Base matrix added
        ld   de,RAM_GAME_MATRIX_ADDRESS
        add  hl,de          ; HL = memory address
        pop af
        ld   e, a           ; New value saved into E
        ld   a, (hl)        ; Old value saved into A
        cp 32
        call nz, SpritesCollision
        ld  a, e
        ld   (hl),a         ; write E value in memory
        pop hl
        pop de
        pop bc
        pop af
        ret
;-----------------------------------------------
; Get martian index by its row position
; A: Row position
; Return martian index in A
;-----------------------------------------------
GetMartianIndexByRow:
        push hl
        push bc
        ld b, a
        ld c, 0
        xor a
GetMartianIndexByRowLoop:
        cp 4
        jp z, GetMartianIndexByRowEnd
        push af
        ld hl, MartianShipsInfo+6
        call AddAToHL
        ld a, (hl)
        cp b
        jp z, GetMartianIndexByRowIndexFound
        pop af
        inc a
        jp GetMartianIndexByRowLoop
GetMartianIndexByRowIndexFound:
        pop af
        ld c, a
GetMartianIndexByRowEnd:
        ld a, c
        pop bc
        pop hl
        ret
;-----------------------------------------------
; Sprites collision
; E: New value
; A: Old value
; C: Collision row
; B: Collision column
;-----------------------------------------------
SpritesCollision:
        push hl
        ld d, a
        ;ld hl, MartianShipCollisionIndex
        ;ld a, 10
        ;ld (hl), a                              ; Reset martian index to remove (10 = no martian to remove)
        ld a, e
        cp SPRT_HUMAN_BULLET
        jp z, SpritesCollisionHumanBullet
        cp SPRT_HUMAN_BULLET+1
        jp z, SpritesCollisionHumanBullet
        cp SPRT_MARTIAN_BULLET
        jp z, SpritesCollisionMartianBullet
        cp SPRT_MARTIAN_BULLET+1
        jp z, SpritesCollisionMartianBullet
        jp SpritesCollisionEnd 
SpritesCollisionHumanBullet:
        ld a, d
        CP   SPRT_MARTIAN_SHIP_1_A
        JR   C, SpritesCollisionHumanBulletNoMartianShip
        CP   (SPRT_MARTIAN_SHIP_3_B + 1)
        JR   NC, SpritesCollisionHumanBulletNoMartianShip
        ld    a,c
        call GetMartianIndexByRow
        ld hl, MartianShipCollisionIndex
        ld (hl), a       ; Save martian index         
        JR   SpritesCollisionEnd
SpritesCollisionMartianBullet:
        ld   a, d
        CP   SPRT_HUMAN_STATIC
        JR   NZ, SpritesCollisionMartianBulletHumanWeapon
        ld   a, b
        ld   HL, SpriteCollisionStaticHumanPosition    ; Save human static position
        ld   (HL), a

        ld  hl, CurrentMartianBullet
        ld  a, (hl)                                     ; Save current martian bullet id
        ld  hl, MartianBullets
        call AddAToHL
        ld (hl),0                                       ; Reset current bullet X position
        ld a, 100
        call AddAToHL
        ld (hl),0                                       ; Reset current bullet Y position

        JR   SpritesCollisionEnd

SpritesCollisionMartianBulletHumanWeapon:
        ld   a, c
        CP   18
        JR   NZ, SpritesCollisionEnd
        call HideHumanWeapon
        ld hl, HumanWeaponHideLevelTimer
        ld a, (hl)
        ld hl, HumanWeaponHideTimer                     ; Set timer to hide human weapon
        ld (hl), a
        ld HL,HumanWeaponDirection
        LD A,0
        LD (HL),A                                       ; Stop human weapon movement
        ld  hl, CurrentMartianBullet
        ld  a, (hl)                                     ; Save current martian bullet id
        ld  hl, MartianBullets
        call AddAToHL
        ld (hl),0                                       ; Reset current bullet X position
        ld a, 100
        call AddAToHL
        ld (hl),0                                       ; Reset current bullet Y position

        JR   SpritesCollisionEnd
SpritesCollisionHumanBulletNoMartianShip:
     
SpritesCollisionEnd:
        pop hl
        ret
;------------------------------------------------------------------------
; Hide human weapon
;------------------------------------------------------------------------
HideHumanWeapon:
        ld   hl, HumanWeaponPosition
        ld   a, (hl)
        ld   e, a
        ld   d, 18
        ld   a, 32
        call PrintRamChar
        inc  e
        call PrintRamChar
        RET
;-----------------------------------------------
; GetMatrixValue
; Get value from (GameMatrix + (C*32 + B)) and put it into A
; 
; Input:
;   E = column (0..31)
;   D = row    (0..23)
; Output:
;   A = read value
;-----------------------------------------------
GetMatrixValue:

        push bc
        push de
        push hl

        ld   b, e
        ld   c, d

        ; Offset calculation= (C * 32) + B
        ld   a,c
        ld   h,0
        ld   l,a
        add  hl,hl
        add  hl,hl
        add  hl,hl
        add  hl,hl
        add  hl,hl          ; HL = row * 32

        ld   a,b
        ld   d,0
        ld   e,a
        add  hl,de         ; HL = (riga * 32) + colonna

        ld   de,RAM_GAME_MATRIX_ADDRESS
        add  hl,de          ; HL = memory address

        ld   a,(hl)         ; Put value into A

        pop hl
        pop de
        pop bc

        ret

;------------------------------------------------------------------------
; Martian ships management
;------------------------------------------------------------------------
MartianShipsMove:
 	ld	hl, MartianShipsInfo+24
	ld	a, (hl) 								; Get martians ships speed counter
	inc	a									; Increment martian speed counter
	ld	(hl), a									; Save new martian speed counter
	ld	hl, MartianShipsInfo+23							; Get martian ship speed
	cp	(hl)
	ret	nz									; No time to move
	ld	hl, MartianShipsInfo+24
	ld	(hl), 0 								; Reset martians ships speed counter
	ld	hl, MartianShipsInfo+22						
	ld	(hl), 0									; Reset martians ship index
	xor	a									; reset A registry
MartianShipsMoveLoop:
	cp	4
	ret	z									; No more ships to move
	ld 	hl, MartianShipsInfo+6						
	call	AddAToHL
	ld	b,(hl)									; Get Y position
	ld	a,b
	cp	0
	jp	z, MartianShipsMoveNext
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld 	hl, MartianShipsInfo+14						
	call	AddAToHL
	ld	a,(hl)									; Get if direction is changhed
	cp	0
	call	z, MartianShipsMoveChangeDirectionVerify
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld 	hl, MartianShipsInfo+2						
	call	AddAToHL
	ld	c,(hl)									; Get X position
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld 	hl, MartianShipsInfo+10						
	call	AddAToHL
	ld	d,(hl)									; Get current direction
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	call	MartianShipsMoveExecute							; move ship
	
	call	MartianShipUpdatePositionInfo						; Put new coordinates into info array

	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld	hl, MartianShipsInfo+18						
	call	AddAToHL
	ld	d, (hl)									; Get martians ship type

	call	MartianShipsMoveChangeType						; change ship type

	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld	hl, MartianShipsInfo+18						
	call	AddAToHL
	ld	(hl), d									; Set martians ship type

	
	call	PutMartianShipOnScreen
MartianShipsMoveNext:
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)								        ; Get martians ship index
	inc 	a
	ld	(hl), a								        ; Save new martians ship index
	jp	MartianShipsMoveLoop
MartianShipsMoveChangeType:
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)								        ; Get martians ship index
	ld	hl, MartianShipsInfo+18									
	call	AddAToHL		
	ld	a, (hl)									; Get martian ship type from array index found
	cp	SPRT_MARTIAN_SHIP_1_A
	jp	z, MartianShipsMoveChangeType_1_B
	cp	SPRT_MARTIAN_SHIP_1_B
	jp	z, MartianShipsMoveChangeType_1_A
	cp	SPRT_MARTIAN_SHIP_2_A
	jp	z, MartianShipsMoveChangeType_2_B
	cp	SPRT_MARTIAN_SHIP_2_B
	jp	z, MartianShipsMoveChangeType_2_A
	cp	SPRT_MARTIAN_SHIP_3_A
	jp	z, MartianShipsMoveChangeType_3_B
	cp	SPRT_MARTIAN_SHIP_3_B
	jp	z, MartianShipsMoveChangeType_3_A

MartianShipsMoveChangeType_1_A:
	ld		d, SPRT_MARTIAN_SHIP_1_A
	ret
MartianShipsMoveChangeType_1_B:
	ld		d, SPRT_MARTIAN_SHIP_1_B
	ret
MartianShipsMoveChangeType_2_A:
	ld		d, SPRT_MARTIAN_SHIP_2_A
	ret
MartianShipsMoveChangeType_2_B:
	ld		d, SPRT_MARTIAN_SHIP_2_B
	ret
MartianShipsMoveChangeType_3_A:
	ld		d, SPRT_MARTIAN_SHIP_3_A
	ret
MartianShipsMoveChangeType_3_B:
	ld		d, SPRT_MARTIAN_SHIP_3_B
	ret
MartianShipsMoveChangeDirectionVerify:
	ld	a, c
	cp	6
	jp	z,MartianShipsMoveChangeDirectionPreEvaluation
	cp	12
	jp	z,MartianShipsMoveChangeDirectionPreEvaluation
	cp	19
	jp	z,MartianShipsMoveChangeDirectionPreEvaluation
	cp	25
	jp	z,MartianShipsMoveChangeDirectionPreEvaluation
	ret
MartianShipsMoveChangeDirectionPreEvaluation:
	ld	a, b
	cp	20
	ret	z
MartianShipsMoveChangeDirectionEvaluation:
	call	GetNewRandomNumber
	cp	50
	ret	nc
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld 	hl, MartianShipsInfo+14						
	call	AddAToHL
	ld	(hl),1									; Update flag direction changed
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld 	hl, MartianShipsInfo+10
	call	AddAToHL							
	ld	a,(hl)									; Get actual direction
	cp	1
	call	z, MartianShipsMoveChangeDirectionLeft
	call	nz, MartianShipsMoveChangeDirectionRight
	ret
MartianShipsMoveChangeDirectionLeft:
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld 	hl, MartianShipsInfo+10
	call	AddAToHL		
	ld	(hl), 2								        ; Update martian ship direction
	ret
MartianShipsMoveChangeDirectionRight:
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)									; Get martians ship index
	ld 	hl, MartianShipsInfo+10
	call	AddAToHL		
	ld	(hl), 1									; Update martian ship direction
	ret

MartianShipsMoveExecute:
	ld	a,d
	cp	2
	jp	z, MartianShipsMoveExecuteToRight
MartianShipsMoveExecuteToLeft:
	dec	c									; Decrement X position
	ld	a,c
	cp	0
	jp	z, MartianShipsMoveExecuteRemove					; Remove ship because it is on left limit
	ret
MartianShipsMoveExecuteToRight:	
	ld	d,29									; Right limit X position to check
	ld	a,b
	cp	1
	call	z, MartianShipsMoveExecuteToRightChange			                ; If Y position is 1 the X right limit become 18
        cp	2
	call	z, MartianShipsMoveExecuteToRightChange			                ; If Y position is 2 the X right limit become 18
	ld	a,c
	cp	d	
	jp	z, MartianShipsMoveExecuteRemove		                        ; Remove ship because it is on right limit
	inc	c								        ; Increase X position
	ret
MartianShipsMoveExecuteToRightChange:
	ld		d, 18
	ret
MartianShipsMoveExecuteRemove:
        push bc
        pop de
	call	MartianShipRemove
	ld		c,0
	ld		b,0
	ret

;------------------------------------------------------------------------
; Print martian ship
; C: X position
; B: Y position
; D: First character code
;------------------------------------------------------------------------
PutMartianShipOnScreen:

	push	hl									; Backup HL registry to stack
	push	de									; Backup DE registry to stack
	push	bc									; Backup BC registry to stack
        push    bc
        push    de
        pop     bc
        pop     de
        ld      a, d
        cp      1
        jp      z, PutMartianShipOnScreenClearLine
        cp      2
        jp      z, PutMartianShipOnScreenClearLine
        jp      PutMartianShipOnScreenGo
PutMartianShipOnScreenClearLine:
        ld      a, e
        cp      18
        jp      z, PutMartianShipOnScreenGo
        

        push    de
        ld      e, 18
        ld      a, 32
        call    PrintRamChar
        pop     de
PutMartianShipOnScreenGo:
        ld      hl,  MartianShipMaxRight
        ld      a, 16
        ld      (HL), a
        ld      a, d
        cp      1
        jp      z, PutMartianShipOnScreenContinue
        cp      2
        jp      z, PutMartianShipOnScreenContinue
        ld      (hl), 29
PutMartianShipOnScreenContinue:        
	ld      c,e

	ld	a, e
	cp	0
	jp	z, PutMartianShipOnScreenEnd								; Invalid X position
	ld	a, d
	cp	0
	jp	z, PutMartianShipOnScreenEnd								; Invalid Y position
        ld      a, e
        cp      1
        jp      z, PutMartianShipOnScreenNoClearBefore
        dec     e
	ld	a, 32											; Clear space before (TOP)
	call	PrintRamChar
        inc     e
PutMartianShipOnScreenNoClearBefore:
        ld      hl, MartianShipMaxRight
        ld      a, (HL)
        ld	a, e
        cp      (HL)
        jp      z, PutMartianShipOnScreenNoClearAfter
        inc     e
        inc     e
        ld	a, 32											; Clear space after (TOP)
	call	PrintRamChar
        dec     e
        dec     e
PutMartianShipOnScreenNoClearAfter:

        ld	a, d
        cp      1
        jp      nz, PutMartianShipOnScreenPrint  
        ld      a, e
        cp      24
        jp      nz, PutMartianShipOnScreenPrint  
        inc     e
        inc     e
        ld	a, 32											; Clear space after on first row (TOP)
	call	PrintRamChar
        dec     e
        dec     e
PutMartianShipOnScreenPrint:
        ld a, b
        call Draw16x8Tile
        ld      a, TILE_BORDER_VERTICAL
        ld      e, 20
        ld      d, 1
        call    PrintRamChar
        inc     d
        call    PrintRamChar
        ld      a, TILE_BORDER_BOTTOM_LEFT_CORNER
        inc     d
        call    PrintRamChar
PutMartianShipOnScreenEnd:

	pop		bc														; Restore HL registry from stack
	pop		de														; Restore DE registry from stack
	pop		hl														; Restore BC registry from stack
	ret
;------------------------------------------------------------------------
; Remove martian ship from screen
; D: Character Y position
; E: Character X position
;------------------------------------------------------------------------
MartianShipRemove:
	push	DE	
        ld      a, e
        cp      29
        jp      z, MartianShipRemoveContinue
        cp      18
        jp      z, MartianShipRemoveContinue
	inc	e
MartianShipRemoveContinue:
	ld	a, 32
	call	PrintRamChar											
	inc	e
	ld	a, 32	
        call	PrintRamChar			
	ld	hl, MartianShipsInfo+1
	ld	a, (hl)
	dec	a
	ld	(hl), a													; Decrement amount of showed ships
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)													; Get martians ship index
	ld	hl, MartianShipsInfo+14
	call	AddAToHL
	ld	(hl), 0													; Reset flag change direction
	pop 	DE	
        call   ClearRowFromMartianShip												
	ret
;------------------------------------------------------------------------
; Clear row from martian ship
; D: Row index
;------------------------------------------------------------------------
ClearRowFromMartianShip:
        cp 18
        ret z
        cp 19
        ret z
        cp 20
        ret z
        cp 21
        ret z
        cp 22
        ret z
        cp 23
        ret z
        push de
        push bc
        push af
        ld b, 18
        ld a, d
        CP 1
        JP Z, ClearRowFromMartianShipContinue
        CP 2
        JP Z, ClearRowFromMartianShipContinue
        CP 3
        JP Z, ClearRowFromMartianShipContinue
        ld b, 31
ClearRowFromMartianShipContinue:
        ld a,1
ClearRowFromMartianShipLoop:
        push af
        ld e,a
        call GetMatrixValue
        cp 32
        jp Z, ClearRowFromMartianShipLoopNext
        cp SPRT_HUMAN_BULLET
        jp Z, ClearRowFromMartianShipLoopNext
        cp SPRT_HUMAN_BULLET+1
        jp Z, ClearRowFromMartianShipLoopNext
        cp SPRT_MARTIAN_BULLET
        jp Z, ClearRowFromMartianShipLoopNext
        cp SPRT_MARTIAN_BULLET+1
        jp Z, ClearRowFromMartianShipLoopNext
        ld a, 32
        call PrintRamChar
ClearRowFromMartianShipLoopNext:
        pop af
        inc a
        cp b
        jp NZ, ClearRowFromMartianShipLoop
        pop af
        pop bc
        pop de
        ret
;------------------------------------------------------------------------
; Update martian ship position info
; A: Array index
; B: Position Y
; C: Position X
;------------------------------------------------------------------------
MartianShipUpdatePositionInfo:
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)					; Get martians ship index										
	ld 	hl, MartianShipsInfo+6						
	call	AddAToHL
	ld	(hl), b
	ld	hl, MartianShipsInfo+22						
	ld	a, (hl)					; Set Y position
	ld 	hl, MartianShipsInfo+2						
	call	AddAToHL
	ld	(hl), c				        ; Set X position
	ret

;------------------------------------------------------------------------
; Get firts free martian ship array index
;------------------------------------------------------------------------
GetFirstFreeMartianShipArrayIndex:
	push	hl														; Backup HL registry to stack
	ld		a,0
GetFirstFreeMartianShipArrayIndexLoop:
	push	af														; Put AF registry to stack for save the counter
	ld	hl, MartianShipsInfo+2
	call	AddAToHL
	ld	a,(hl)
	cp	0
	jp	z, GetFirstFreeMartianShipArrayIndexLoopEnd
	pop	af														; Restore AF for to retrieve the counter
	inc	a
	jp	GetFirstFreeMartianShipArrayIndexLoop
GetFirstFreeMartianShipArrayIndexLoopEnd:
	pop		af														; Restore AF for to retrieve the counter
	pop		hl														; Restore HL registry from stack
	ret
 
 ;------------------------------------------------------------------------
 ; New martian ship showing evaluation
;------------------------------------------------------------------------
NewMartianShipShowingEvaluation:
	ld	hl, MartianShipsInfo+1				        ; Get number of martians ships displayed
	ld	a, (hl)

	ld	hl, Martians
	cp	(hl)
	
	ret	nc						        ; return because remain martians ships are less 
	ld	a,r
	cp	6
	ret	nc					                ; No new martian ship needed to display
	
	call 	GetFirstFreeMartianShipArrayIndex

	ld	c, 29							; New martian ship start position
	ld	d, 1							; New martian ship start direction from right
	call	GetNewRandomNumber
	cp	125
	call	nc, NewMartianShipShowingEvaluationFromLeft		; Change new martian ship start position and direction

	call 	GetFirstFreeMartianShipArrayIndex			; Get free array index
	push	af							; Save array index found
	ld	hl, MartianShipsInfo+10									
	call	AddAToHL
	ld	(hl),d							; Put direction into array index found
	pop	af							; Restore array index found
	push	af							; Save array index found												
	ld	d, 0
	call	GetNewRandomNumber					; Type of martians ship to show determination
	cp	255
	call	c, NewMartianShipShowingEvaluationShip_1
	cp	150
	call	c, NewMartianShipShowingEvaluationShip_2
	cp	80
	call	c, NewMartianShipShowingEvaluationShip_3
	pop	af							; Restore array index found
	push	af							; Save array index found
	ld	hl, MartianShipsInfo+18									
	call	AddAToHL		
	ld	(hl), d						        ; Put martian ship type into array index found

	call	GetNewRandomNumber 					; Y position of martians ship to show determination
	ld	b,1
	cp	200
	call	c, NewMartianShipShowingEvaluationRow_2			; Row 2 selected
	cp	150
	call	c, NewMartianShipShowingEvaluationRow_4			; Row 3 selected
	cp	100
	call	c, NewMartianShipShowingEvaluationRow_5			; Row 4 selected
	cp	50
	call	c, NewMartianShipShowingEvaluationRow_6		        ; Row 6 selected

        ld      a, b
        cp      1
        jp      nz, NewMartianShipShowingEvaluationPosHor
        ld      a, c
        cp      29
        jp      nz, NewMartianShipShowingEvaluationPosHor
        ld      c, 18
NewMartianShipShowingEvaluationPosHor:
	pop	af							; Restore array index found
	push	af							; Save array index found
	ld	hl, MartianShipsInfo+10									
	call	AddAToHL
	ld	d,(hl)							; Get direction from array index found (because if row is busy and new proposed row changed to 2 the start position will be from left)
	pop	af							; Restore array index found
	push	af							; Save array index found

	call	NewMartianShipShowingEvaluationPosVer			; Change Y position if it's busy from another ship
	
	ld	a, b
	cp	1
	jp	z, NewMartianShipShowingValidRow	
	cp	2
	jp	z, NewMartianShipShowingValidRow	
	cp	4
	jp	z, NewMartianShipShowingValidRow	
	cp	5
	jp	z, NewMartianShipShowingValidRow	
	cp	6
	jp	z, NewMartianShipShowingValidRow	
	jp	NewMartianShipShowingNotValidRow			; No valid row for martian ship
NewMartianShipShowingValidRow:
	pop	af							; Restore array index found
        push    af
        ld      a, b
        cp      1
        jp      z, NewMartianShipShowingValidRowContinue
        cp      2
        jp      z, NewMartianShipShowingValidRowContinue
        ld      a, c
        cp      18
        jp      nz, NewMartianShipShowingValidRowContinue
        ld      c, 29

NewMartianShipShowingValidRowContinue:
        pop     af
	push	af							; Save array index found
	ld	hl, MartianShipsInfo+2									
	call	AddAToHL		
	ld	(hl), c						        ; Put X position into array index found
	pop	af						        ; Restore array index found
	push	af							; Save array index found
	ld	hl, MartianShipsInfo+6								
	call	AddAToHL	
	ld	(hl), b							; Put Y position into array index found
	pop	af							; Restore array index found
	push	af							; Save array index found
	ld	hl, MartianShipsInfo+10								
	call	AddAToHL	
        ld      a, c
        cp      18
        jp      nz, NewMartianShipShowingValidRowContinue2
        ld      a, b
        CP      2
        jp      nz, NewMartianShipShowingValidRowContinue2
        ld      d, 1
NewMartianShipShowingValidRowContinue2:
	ld	(hl), d							; Put direction from array index found
	pop	af							; Restore array index found
	push	af							; Save array index found
	ld	hl, MartianShipsInfo+18								
	call	AddAToHL	
	ld	d, (hl)							; Get martian ship type from array index found
	pop	af							; Restore array index found
	call	PutMartianShipOnScreen
	ld	hl, MartianShipsInfo+1
	ld	a, (hl)							; Get number of martians ships on screen
	inc	a							; Increment number of martians ships on screen
	ld	(hl), a							; Save new number of martians ships on screen
	ret							
NewMartianShipShowingEvaluationFromLeft
	ld	c, 1							; New martian ship start position
	ld	d, 2							; New martian ship start direction from left
	ret
NewMartianShipShowingEvaluationShip_1:
	ld d, SPRT_MARTIAN_SHIP_1_A
	ret
NewMartianShipShowingEvaluationShip_2:
	ld d, SPRT_MARTIAN_SHIP_2_A
	ret
NewMartianShipShowingEvaluationShip_3:
	ld d, SPRT_MARTIAN_SHIP_3_A
	ret
NewMartianShipShowingEvaluationRow_2:
	ld b, 2
        ld a, c
        cp 29
        ret nz
        ld c, 18
	ret
NewMartianShipShowingEvaluationRow_4:
	ld		b, 4
	ret
NewMartianShipShowingEvaluationRow_5:
	ld		b, 5
	ret
NewMartianShipShowingEvaluationRow_6:
	ld		b, 6
	ret
NewMartianShipShowingEvaluationPosVer:
	ld		a,	0
NewMartianShipShowingEvaluationPosVerLoop:
	cp	4
	jp	z,NewMartianShipShowingEvaluationPosVerRow2		; Proposed Y position is free
	push	af							; Backup conter to stack
	ld	hl, MartianShipsInfo+6
	call	AddAToHL
	ld	a, (hl)
	cp	b
	jp	z, NewMartianShipShowingEvaluationPosVerBusy		; Proposed Y position is busy
	pop	af							; Restore counter from stack
	inc	a
	jp 	NewMartianShipShowingEvaluationPosVerLoop
NewMartianShipShowingEvaluationPosVerBusy:
	pop		af						; Restore counter from stack
	ld		a, 0					
NewMartianShipShowingEvaluationPosVerBusyLoop:	
	push	af						        ; Backup conter to stack	
	ld	e, a																			
	ld	hl, MartianShipsInfo+6
	call	AddAToHL
	ld	a, (hl)
	cp	0
	jp	z, NewMartianShipShowingEvaluationPosVerBusyEnd	        ; Avaible free Y position found
	pop	af							; Restore counter from stack
	inc	a
	jp 	NewMartianShipShowingEvaluationPosVerBusyLoop
NewMartianShipShowingEvaluationPosVerBusyEnd:
	ld	b, e
	pop	af							; Restore counter from stack
	ld	a, 2
	cp	b
	ret	nz							; if not new proposed Y position is equals 2 then it's valid
	ld	d, 2							; Force direction for row 2 from left
NewMartianShipShowingEvaluationPosVerRow2:
	ld	a, b
        cp	1                                                       
	jp	z, NewMartianShipShowingEvaluationPosVerIs12Rows
	cp	2
        jp	z, NewMartianShipShowingEvaluationPosVerIs12Rows
	ret								; If not Y is 1 or 2 each X position is ok
NewMartianShipShowingEvaluationPosVerIs12Rows:

	ld	a, c
	cp	1
	ret	z							; If not Y is 1 or 2 and X=1 position is ok
	ld	c, 18							; Set X position on row 2 for direction from right
	LD      HL, MartianShipsInfo+22
        LD      A,(HL)                         ; Get current martian index                        
       
												
	ld	hl, MartianShipsInfo+10
        ld      a, b
	call	AddAToHL
	ld	(hl), 2				; Set direction from right
        ret
NewMartianShipShowingNotValidRow:
	pop		af
	ret


;------------------------------------------------------------------------
; Add A registry value to HL registry value
;------------------------------------------------------------------------
AddAToHL:
	add   a, l   
	ld    l, a    
	adc   a, h    
	sub   l       
	ld    h, a    
	ret
;------------------------------------------------------------------------
; Get a new random number
; Return the value in A registry
;------------------------------------------------------------------------
GetNewRandomNumber:
	push	bc
	ld		a,(RandomSeed)
	ld		b, a
	add		a, a
	add		a, a
	add		a, b
	inc		a
	ld		(RandomSeed),a
	pop	bc
	ret
;------------------------------------------------------------------------
; Show human weapon
;------------------------------------------------------------------------
ShowHumanWeapon:
        PUSH HL
         LD HL, HumanWeaponHideTimer
        LD (HL),0
        LD HL, HumanWeaponPosition
        LD A, (HL)
        LD E, A
        LD D, 18
        LD A, SPRT_HUMAN_WEAPON
        CALL Draw16x8Tile  
        LD HL, HumanWeaponPosition
        LD A, (HL)
        CP 1
        JP NZ, ShowHumanWeaponClearLeft
ShowHumanWeaponClearContinue:
        LD HL, HumanWeaponPosition
        LD A, (HL)
        CP 29
        JP NZ, ShowHumanWeaponClearRight
ShowHumanWeaponEnd:        
        POP HL
        RET
ShowHumanWeaponClearLeft:
        LD HL, HumanWeaponPosition
        LD A, (HL)
        DEC A
        LD E,A
        LD D, 18
        LD A, 32
        CALL PrintRamChar
        JP ShowHumanWeaponClearContinue
ShowHumanWeaponClearRight:
        LD HL, HumanWeaponPosition
        LD A, (HL)
        INC A
        INC A
        LD E,A
        LD D, 18
        LD A, 32
        CALL PrintRamChar
        JP ShowHumanWeaponEnd

;------------------------------------------------------------------------
; Read the keyboard
; Returns: A = ASCII code of key pressed
;------------------------------------------------------------------------
ReadKeyboard:          
                LD HL,KEYBOARD_MAP              ; Point HL at the keyboard list
                LD D,8                                  ; This is the number of ports (rows) to check
                LD C,#FE                            ; C is always FEh for reading keyboard ports
ReadKeyboard0:        
                LD B,(HL)                               ; Get the keyboard port address from table
                INC HL                                  ; Increment to list of keys
                IN A,(C)                                ; Read the row of keys in
                AND #1F                                     ; We are only interested in the first five bits
                LD E,5                                  ; This is the number of keys in the row
ReadKeyboard1:        
                SRL A                                   ; Shift A right; bit 0 sets carry bit
                JR NC,ReadKeyboard2   ; If the bit is 0, we've found our key
                INC HL                                  ; Go to next table address
                DEC E                                   ; Decrement key loop counter
                JR NZ,ReadKeyboard1   ; Loop around until this row finished
                DEC D                                   ; Decrement row loop counter
                JR NZ,ReadKeyboard0   ; Loop around until we are done
                AND A                                   ; Clear A (no key found)
                RET
ReadKeyboard2:        
                LD A,(HL)                               ; We've found a key at this point; fetch the character code!
                RET
 



;------------------------------------------------------------------------
; Wait until ENTER is pressed
;------------------------------------------------------------------------
WaitUntilEnterIsPressed:
        CALL ReadKeyboard
        CP 35
        RET Z
        JR WaitUntilEnterIsPressed

; -----------------------------------------------------
; Labels for the detected keys
; -----------------------------------------------------
CheckButtonPressedKey1:
    LD   A, 1
    LD   A, #F8
    OUT  (C), A
    RET

CheckButtonPressedKey2:
    LD   A, 2
    LD   A, #F8
    OUT  (C), A
    RET
CheckButtonPressedKey3:
    LD   A, 3
    LD   A, #F8
    OUT  (C), A
    RET

CheckButtonPressedKey4:
    LD   A, 4
    LD   A, #F8
    OUT  (C), A
    RET

CheckButtonPressedKey5:
    LD   A, 5
    LD   A, #F8
    OUT  (C), A
    RET

CheckButtonPressedKey6:
    LD   A, 6
    LD   A, #F8
    OUT  (C), A
    RET

CheckButtonPressedKeyEnter:
    LD   A, 7
    LD   A, #F8
    OUT  (C), A
    RET

CheckButtonPressedKeySpace:
    LD   A, 0
    LD   A, #F8
    OUT  (C), A
    RET



;------------------------------------------------------------------------
; Update info
;------------------------------------------------------------------------
UpdateInfo:
        CALL RefreshHumansCounter
        CALL RefreshMartiansCounter
        RET

;------------------------------------------------------------------------
; Refresh martians counter
;------------------------------------------------------------------------
RefreshMartiansCounter:
	push	hl														; Backup HL registry to stack
	push	de														; Backup DE registry to stack
	push	bc														; Backup BC registry to stack
	push	af														; Backup AF registry to stack
	ld	hl, Martians
	push	hl
	ld	a,(hl)
	cp	10
        jp      c, RefreshMartiansCounterLess10
	cp	20
	jp      c, RefreshMartiansCounterLess20
	pop	hl
	ld	a,(hl)
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a	
	add	e, 48
	ld    	d, 2
	ld    	c, 31
	call    PrintRomChar
	ld    	d, 2
	ld    	e, 30
	ld    	a, 50
	call    PrintRomChar
	jp	RefreshMartiansCounterEnd
RefreshMartiansCounterLess20:
	pop	hl
	ld	a,(hl)
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	dec	a
	add	a, 48
	ld    	d, 2
	ld    	e, 31
	call    PrintRomChar
	ld    	d, 2
	ld    	e, 30
	ld    	a, 49
	call    PrintRomChar
	jp	RefreshMartiansCounterEnd
RefreshMartiansCounterLess10:
	ld    	d, 2
	ld    	e, 30
	ld    	a, 32
	call  	PrintRomChar
	pop 	hl
	ld	a,(hl)
	add	a, 48
	ld	d, 2
	ld    	e, 31
	call    PrintRomChar
RefreshMartiansCounterEnd:
	pop		af														; Restore AF registry from stack
	pop		bc														; Restore BC registry from stack
	pop		de														; Restore DE registry from stack
	pop		hl														; Restore HL registry from stack
	ret

;------------------------------------------------------------------------
; Refresh humans counter
;------------------------------------------------------------------------
RefreshHumansCounter:
	push	hl														; Backup HL registry to stack
	push	de														; Backup DE registry to stack
	push	bc														; Backup BC registry to stack
	push	af														; Backup AF registry to stack
	ld	hl, Humans
	push	hl
	ld	a,(hl)
	cp	10
        jp      c, RefreshHumansCounterLess10
	pop	hl
	ld	a,(hl)
	sub 	10
	add	a, 48
	ld    	d, 1
	ld    	e, 31
	call  	PrintRomChar
	ld    	d, 1
	ld    	e, 30
	ld    	a, 49
	call    PrintRomChar
	jp	RefreshHumansCounterEnd
RefreshHumansCounterLess10:
	ld    	d, 1
	ld    	e, 30
	ld    	a, 32
	call    PrintRomChar
	pop 	hl
	ld	a,(hl)
	add	a, 48
	ld	e, 31
	ld    	d, 1
	call    PrintRomChar
RefreshHumansCounterEnd:
	pop		af														; Restore AF registry from stack
	pop		bc														; Restore BC registry from stack
	pop		de														; Restore DE registry from stack
	pop		hl														; Restore HL registry from stack
	ret
ShowRunningHumans:
        
        LD HL, HumansLastStaticPosLeft
        LD (HL), 15                     ; Target left running human position
        LD HL, HumansLastStaticPosRight
        LD (HL), 17                     ; Target right running human position
        LD HL, HumansToDisplay
        LD (HL), 16                     ; Amount of humans to display
ShowRunningHumansLoop:
        LD HL, HumansToDisplay
        LD A, (HL)
        DEC A
        CP 0
        RET Z
        LD (HL), A
        BIT 0, A 
        JP NZ, ShowRunningHumansLoopLeft
        CALL ShowRightRunningHumans
        LD HL, HumansLastStaticPosRight
        LD A, (HL)
        INC A
        INC A
        LD (HL), A
        JP ShowRunningHumansLoop
ShowRunningHumansLoopLeft
        CALL ShowLeftRunningHumans
        LD HL, HumansLastStaticPosLeft
        LD A, (HL)
        DEC A
        DEC A
        LD (HL), A
        JP ShowRunningHumansLoop
;------------------------------------------------------------------------
; Clear the screen and set attributes
;------------------------------------------------------------------------
ClearScreen:
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
        call ClearGameMatrix
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
        call PrintRamChar
        pop af
        inc a
        jp ClearScreenColLoop
ClearScreenColLoopEnd:        
        pop af
        inc a
        jp ClearScreenRowLoop
        RET
;------------------------------------------------------------------------
; Show game field
;------------------------------------------------------------------------
ShowGameField:
	call	ClearScreen     
ShowGameFieldWithoutClear:
	ld	e, 19
	ld    	d, 0
ShowGameFieldUpperLineLoop				
	ld    	a, TILE_BORDER_HORIZONTAL
	call  	PrintRamChar
        dec     e
	ld	a, e
	cp	0
	jp	z, ShowGameFieldBottomLine
	jp	ShowGameFieldUpperLineLoop
ShowGameFieldBottomLine:
	ld      e, 30
	ld	d, 23
ShowGameFieldBottomLineLoop:
	ld    	a, TILE_BORDER_HORIZONTAL
	call  	PrintRamChar
	dec	e
	ld	a, e
	cp	0
	jp	z, ShowGameFieldRightLine
	jp	ShowGameFieldBottomLineLoop
ShowGameFieldRightLine:
        ld	e, 31
        ld    	d, 22
ShowGameFieldRightLineLoop
        ld    	a, TILE_BORDER_VERTICAL
        call  	PrintRamChar
        dec     d
        ld	a, d
        cp	3
        jp	z, ShowGameFieldLeftLine
        jp	ShowGameFieldRightLineLoop
ShowGameFieldLeftLine:
        ld      e, 0
        ld	d, 22
ShowGameFieldLeftLineLoop:
        ld    	a, TILE_BORDER_VERTICAL
        call  	PrintRamChar
        dec	d
        ld	a, d
        cp	0
        jp	z, ShowGameFieldCorners
        jp	ShowGameFieldLeftLineLoop
ShowGameFieldCorners:
        ld      a, TILE_BORDER_TOP_LEFT_CORNER
        ld      e, 0
        ld      d, 0
        call    PrintRamChar
        ld      a, TILE_BORDER_TOP_RIGHT_CORNER
        ld      e, 20
        ld      d, 0
        call    PrintRamChar
        ld      a, TILE_BORDER_BOTTOM_LEFT_CORNER
        ld      e, 0
        ld      d, 23
        call    PrintRamChar
        ld      a, TILE_BORDER_BOTTOM_RIGHT_CORNER
        ld      e, 31
        ld      d, 23
        call    PrintRamChar
        ld      a, TILE_BORDER_VERTICAL
        ld      e, 20
        ld      d, 1
        call    PrintRamChar
        inc     d
        call    PrintRamChar
        ld      a, TILE_BORDER_BOTTOM_LEFT_CORNER
        inc     d
        call    PrintRamChar
        inc     e
        ld      a, TILE_BORDER_HORIZONTAL
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        call    PrintRamChar
        inc     e
        ld      a, TILE_BORDER_TOP_RIGHT_CORNER
        call    PrintRamChar
        ld      a, TILE_BORDER_VERTICAL

        ld      e, 21
        ld      d, 0
        ld      HL, TEXT_MARTIAN_WAR
        call PrintString

        ld      e, 21
        ld      d, 1
        ld     HL, TEXT_HUMANS
        call PrintString
        
        ld      e, 21
        ld      d, 2
        ld     HL, TEXT_MARTIANS
        call PrintString

        ret
;------------------------------------------------------------------------
; Load tiles into RAM
;------------------------------------------------------------------------
LoadTiles:
        DI                      ; Interrupts disabled
        LD   DE, RAM_CHAR_SET_ADDRESS + (96*8)   
        LD   HL, TilesDef   
        LD   BC, 792
        LDIR
        EI                      ; Interrupts enabled
        RET

;------------------------------------------------------------------------
; Clear GameMatrix
;------------------------------------------------------------------------
ClearGameMatrix:
        LD   HL, RAM_GAME_MATRIX_ADDRESS
        LD   BC, 768
        ld   a, 32

ClearGameMatrixLoop:
        LD   (HL), A
        INC  HL
        DEC  BC
        LD   A, B
        OR   C
        JR   NZ, ClearGameMatrixLoop
        ret
;------------------------------------------------------------------------
; Draw a 16x32 tile
; D: Y position
; E: X position
; A: First tile number
;------------------------------------------------------------------------
Draw16x32Tile:
        PUSH DE
        CALL PrintRamChar         ; Print top-left tile
        INC A
        INC E
        CALL PrintRamChar         ; Print top-right tile
        INC A
        INC D
        DEC E
        CALL PrintRamChar         ; Print middle-left tile
        INC A
        INC E
        CALL PrintRamChar         ; Print middle-right tile
        INC A
        INC D
        DEC E
        CALL PrintRamChar         ; Print bottom-left tile
        INC A
        INC E
        CALL PrintRamChar         ; Print bottom-right tile
        POP DE
        RET
;------------------------------------------------------------------------
; Draw a 16x8 tile
; D: Y position
; E: X position
; A: First tile number
;------------------------------------------------------------------------
Draw16x8Tile:
        PUSH DE
        CALL PrintRamChar         ; Print top-left tile
        INC A
        INC E
        CALL PrintRamChar         ; Print top-right tile
        POP DE
        RET
;------------------------------------------------------------------------
; Print a single RAM character out to a screen address
;  A: Character to print
;  D: Character Y position
;  E: Character X position
;------------------------------------------------------------------------
PrintRamChar:     
        PUSH BC        
        PUSH DE                 ; Save the character position
        PUSH AF                 ; Save the character code
        PUSH HL                 ; Save the character set address


        call SetMatrixValue

        LD HL, RAM_CHAR_SET_ADDRESS       ; Character set bitmap data in ROM
        LD B,0                  ; BC = character code
        LD C, A
        SLA C                   ; Multiply by 8 by shifting
        RL B
        SLA C
        RL B
        SLA C
        RL B
        ADD HL, BC              ; And add to HL to get first byte of character
        CALL Get_Char_Address   ; Get screen position in DE
        LD B,8                  ; Loop counter - 8 bytes per character
PrintRamCharL1:          
        LD A,(HL)               ; Get the byte from the ROM into A
        LD (DE),A               ; Stick A onto the screen
        INC HL                  ; Goto next byte of character
        INC D                   ; Goto next line on screen
        DJNZ PrintRamCharL1      ; Loop around whilst it is Not Zero (NZ)
        POP HL                  ; Restore the character set address
        POP AF                  ; Restore the character code
        POP DE                  ; Restore the character position
        POP BC                
        RET
;------------------------------------------------------------------------
; Print a single ROM character out to a screen address
;  A: Character to print
;  D: Character Y position
;  E: Character X position
;------------------------------------------------------------------------
PrintRomChar:             
        PUSH DE                 ; Save the character position
        PUSH AF                 ; Save the character code
        PUSH HL                 ; Save the character set address
        LD HL, ROM_CHAR_SET_ADDRESS       ; Character set bitmap data in ROM
        LD B,0                  ; BC = character code
        SUB 32                  ; Adjust for the character set
        LD C, A
        SLA C                   ; Multiply by 8 by shifting
        RL B
        SLA C
        RL B
        SLA C
        RL B
        ADD HL, BC              ; And add to HL to get first byte of character
        CALL Get_Char_Address   ; Get screen position in DE
        LD B,8                  ; Loop counter - 8 bytes per character
PrintRomCharL1:          
        LD A,(HL)               ; Get the byte from the ROM into A
        LD (DE),A               ; Stick A onto the screen
        INC HL                  ; Goto next byte of character
        INC D                   ; Goto next line on screen
        DJNZ PrintRomCharL1      ; Loop around whilst it is Not Zero (NZ)
        POP HL                  ; Restore the character set address
        POP AF                  ; Restore the character code
        POP DE                  ; Restore the character position
        RET
;------------------------------------------------------------------------
; Print string
;  HL: String address
;  D: Y position
;  E: X position
;------------------------------------------------------------------------
PrintString:
        LD   A, (HL)            ; Load address of current string position to A
        OR   A                  ; Flag updating in base of A value
        RET  Z
        CALL PrintRomChar
        inc  E
        INC  HL                 ; Current string position address set to next element
        JR   PrintString        ; Loop repeat
;------------------------------------------------------------------------
; Get screen address from a character (X,Y) coordinate
; D = Y character position (0-23)
; E = X character position (0-31)
; Returns screen address in DE
;------------------------------------------------------------------------
Get_Char_Address:       
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
        RET                             ; Returns screen address in DE






;------------------------------------------------------------------------
; Show left running humans
;------------------------------------------------------------------------
ShowLeftRunningHumans:
        ; Initialize positions
        LD E, 1                 ; X position for left runner

        ; Loop until the row is filled with static humans
ShowLeftRunningHumansLoop:
        ; Draw left runner
        LD A, SPRT_HUMAN_RUNNING_LEFT_POS1
        LD D, 20                ; Y position (row 20)
        CALL Draw16x32Tile
        CALL Delay
        
        ; Move left runner closer to the center
        INC E
        ; Check if left runner has reached the center or a static human
        LD HL, HumansLastStaticPosLeft
        LD A, (HL)
        CP 1
        JP Z, ShowLeftRunningHumansEnd
        LD A, E
        CP (HL)
        JP Z, ShowLeftRunningHumansEnd
        LD A, SPRT_HUMAN_RUNNING_LEFT_POS2
        LD D, 20                ; Y position (row 20)
        CALL Draw16x32Tile
        CALL ClearLeftRunningHuman
        CALL Delay
        
        CALL ClearLeftRunningHuman
        ; Move left runner closer to the center
        INC E
        ; Check if left runner has reached the center or a static human
        LD HL, HumansLastStaticPosLeft
        LD A, E
        CP (HL)
        JP Z, ShowLeftRunningHumansEnd
        LD A, SPRT_HUMAN_RUNNING_LEFT_POS3
        LD D, 20                ; Y position (row 20)
        CALL Draw16x32Tile
        CALL ClearLeftRunningHuman
        CALL Delay
        
        ; Move left runner closer to the center
        INC E
        ; Check if left runner has reached the center or a static human
        LD HL, HumansLastStaticPosLeft
        LD A, E
        CP (HL)
        JP Z, ShowLeftRunningHumansEnd
        CALL ClearLeftRunningHuman
        JP ShowLeftRunningHumansLoop

ShowLeftRunningHumansEnd:
        ; Make left runner static
        LD HL, HumansLastStaticPosLeft
        LD A, (HL)
        LD E, A
        LD A, SPRT_HUMAN_STATIC
        CALL Draw16x32Tile
        LD HL, HumansLastStaticPosLeft
        LD A, (HL)
        CP 1
        RET Z
        CALL ClearLeftRunningHuman
        RET


;------------------------------------------------------------------------
; Show right running humans
;------------------------------------------------------------------------
ShowRightRunningHumans:
        ; Initialize positions
        LD E, 29                 ; X position for right runner


        ; Loop until the row is filled with static humans
ShowRightRunningHumansLoop:
        ; Draw right runner
        LD HL, HumansLastStaticPosRight
        LD A, E
        CP (HL)
        JP Z, ShowRightRunningHumansEnd
        LD A, SPRT_HUMAN_RUNNING_RIGHT_POS1
        LD D, 20                ; Y position (row 20)
        CALL Draw16x32Tile
        CALL Delay
        CALL ClearRightRunningHuman
        
        ; Move right runner closer to the center
        DEC E
        ; Check if right runner has reached the center or a static human
        LD HL, HumansLastStaticPosRight
        LD A, E
        CP (HL)
        JP Z, ShowRightRunningHumansEnd
        LD A, SPRT_HUMAN_RUNNING_RIGHT_POS2
        LD D, 20                ; Y position (row 20)
        CALL Draw16x32Tile
        CALL Delay
        CALL ClearRightRunningHuman
        ; Move right runner closer to the center
        DEC E
        ; Check if right runner has reached the center or a static human
        LD HL, HumansLastStaticPosRight
        LD A, E
        CP (HL)
        JP Z, ShowRightRunningHumansEnd
        LD A, SPRT_HUMAN_RUNNING_RIGHT_POS3
        LD D, 20                ; Y position (row 20)
        CALL Draw16x32Tile
        CALL Delay
        CALL ClearRightRunningHuman
        ; Move right runner closer to the center
        DEC E
        ; Check if right runner has reached the center or a static human
        LD A, E
        CP 15
        JP Z, ShowRightRunningHumansEnd

        JP ShowRightRunningHumansLoop

ShowRightRunningHumansEnd:
        ; Make right runner static
        LD HL, HumansLastStaticPosRight
        LD A, (HL)
        LD E, A
        LD A, SPRT_HUMAN_STATIC
        CALL Draw16x32Tile

        RET



;------------------------------------------------------------------------
; Delay routine
;------------------------------------------------------------------------
Delay:
        PUSH BC
        LD BC, 0x1000
DelayLoop:
        
        DEC BC
        LD A, B
        OR C
        JP NZ, DelayLoop
        POP BC
        RET
;------------------------------------------------------------------------
; Game main loop delay routine
;------------------------------------------------------------------------
GameMainLoopDelay:
        PUSH BC
        LD BC, 0x2000
GameMainLoopDelayLoop:
        
        DEC BC
        LD A, B
        OR C
        JP NZ, GameMainLoopDelayLoop
        POP BC
        RET
 ; --------------------------------------------------------------------
; Subroutine: ShowJumpingHuman
; --------------------------------------------------------------------
; Description:
;   - Simulates a character (human) jumping four times.
;   - Each jump is composed of a sequence of 6 frames displayed one
;     after another. The frames are as follows:
;        1) SPRT_JUMPING_HUMAN_1 at D=19
;        2) SPRT_JUMPING_HUMAN_2 at D=18
;        3) SPRT_JUMPING_HUMAN_3 at D=17
;        4) SPRT_JUMPING_HUMAN_2 at D=18
;        5) SPRT_JUMPING_HUMAN_1 at D=19
;        6) SPRT_HUMAN_STATIC    at D=20
;   - Between each frame, the subroutine calls GameMainLoopDelay to
;     slow down the animation. This routine destroys the value in A.
;   - Register E (horizontal coordinate) must be preserved.
;   - The routine loops 4 times (for 4 jumps) and then returns.
;
; Input:
;   - Register E: horizontal position (not modified here).
;
; Registers used:
;   - B: used as a counter for the four jumps.
;   - A: loaded with the sprite ID for each frame.
;   - D: loaded with the vertical position for each frame.
;
; Output:
;   - None. Returns after all jumps are completed.
; --------------------------------------------------------------------

ShowJumpingHuman:
        PUSH BC             ; Preserve BC if needed by the main program
        PUSH HL             ; Preserve HL if needed by the main program
        LD   B, 4           ; Set up the loop for 4 jumps

JumpLoop:
        ; Frame 1
        LD   A, SPRT_JUMPING_HUMAN_1
        LD   D, 19
        CALL Draw16x32Tile
        LD   A, 32
        inc  D
        inc  D
        inc  D
        CALL PrintRamChar
        inc e
        CALL PrintRamChar
        dec e
        dec d
        dec d
        dec d
        CALL GameMainLoopDelay   ; Slows down the animation (destroys A)
        push bc
        push de
        CALL ReadKeyboard
        pop de
        pop bc
        CP 35
        JP Z, JumpLoopEnd

        ; Frame 2
        LD   A, SPRT_JUMPING_HUMAN_2
        LD   D, 18
        CALL Draw16x32Tile
        LD   A, 32
        inc  D
        inc  D
        inc  D
        CALL PrintRamChar
        inc e
        CALL PrintRamChar
        dec e
        dec d
        dec d
        dec d
        CALL GameMainLoopDelay

        push bc
        push de
        CALL ReadKeyboard
        pop de
        pop bc
        CP 35
        JP Z, JumpLoopEnd

        ; Frame 3
        LD   A, SPRT_JUMPING_HUMAN_3
        LD   D, 17
        CALL Draw16x32Tile
        LD   A, 32
        inc  D
        inc  D
        inc  D
        CALL PrintRamChar
        inc e
        CALL PrintRamChar
        dec e
        dec d
        dec d
        dec d
        CALL GameMainLoopDelay

        push bc
        push de
        CALL ReadKeyboard
        pop de
        pop bc
        CP 35
        JP Z, JumpLoopEnd

        ; Frame 4
        LD   A, SPRT_JUMPING_HUMAN_2
        LD   D, 18
        CALL Draw16x32Tile
        LD   A, 32
        dec  D
        CALL PrintRamChar
        inc e
        CALL PrintRamChar
        inc d
        dec e
        CALL GameMainLoopDelay

        push bc
        push de
        CALL ReadKeyboard
        pop de
        pop bc
        CP 35
        JP Z, JumpLoopEnd

        ; Frame 5
        LD   A, SPRT_JUMPING_HUMAN_1
        LD   D, 19
        CALL Draw16x32Tile
        LD   A, 32
        dec  D
        CALL PrintRamChar
        inc e
        CALL PrintRamChar
        inc d
        dec e
        CALL GameMainLoopDelay

        push bc
        push de
        CALL ReadKeyboard
        pop de
        pop bc
        CP 35
        JP Z, JumpLoopEnd

        ; Return to the static sprite
        LD   A, SPRT_HUMAN_STATIC
        LD   D, 20
        CALL Draw16x32Tile
        LD   A, 32
        dec  D
        CALL PrintRamChar
        inc e
        CALL PrintRamChar
        inc d
        dec e
        CALL GameMainLoopDelay

        push bc
        push de
        CALL ReadKeyboard
        pop de
        pop bc
        CP 35
        JP Z, JumpLoopEnd
        
        ; Decrement the jump counter and loop if not zero
        dec b
        ld a, b
        cp 0
        jp nz, JumpLoop

        POP  HL
        POP  BC
        RET       
JumpLoopEnd:
        POP  HL
        POP  BC
        call ClearScreen
        jp LevelSelection
;------------------------------------------------------------------------
; Clear left running human
;------------------------------------------------------------------------
ClearLeftRunningHuman:
        DEC E
        LD D, 20
        LD A, 32
        CALL PrintRomChar
        INC D
        LD A, 32
        CALL PrintRomChar
        INC D
        LD A, 32
        CALL PrintRomChar
        DEC D
        DEC D
        INC E
        RET
;------------------------------------------------------------------------
; Clear right running human
;------------------------------------------------------------------------
ClearRightRunningHuman:
        INC E
        LD D, 20
        LD A, 32
        CALL PrintRomChar
        INC D
        LD A, 32
        CALL PrintRomChar
        INC D
        LD A, 32
        CALL PrintRomChar
        DEC D
        DEC D
        DEC E
        RET






;------------------------------------------------------------------------
; Move martian Bullets on screen
;------------------------------------------------------------------------
MoveMartianBullets:
					

MoveMartianBulletsLoops:
	ld	hl, CurrentMartianBullet
	ld	a, (hl)
	cp	15
	jp      nz, MoveMartianBulletsLoopsContinue	
        ld	hl, CurrentMartianBullet
	ld	(hl), 0		                                        ; Reset martian bullets counter
        ret                                                             ; All martian bullets processed
MoveMartianBulletsLoopsContinue:						
	ld	hl, MartianBullets					; Get first X element address of martian bullets array
	call	AddAToHL 						; Get current X element address of martian bullets array
        ld	c, (hl)							; Get X martian bullet position
	ld	a, 100							; Add 100 positions to X element address for to find Y element address
	call	AddAToHL
        ld	b, (hl)							; Get Y martian bullet position
	ld	a, (hl)							; Get current Bullet Y position
	cp	0
	call	nz, MoveMartianBulletsOk				; Move martians Bullet
        ld	hl, CurrentMartianBullet
	ld	a, (hl)							; Get current Bullet array index
	inc	a							; Increment array index
	ld	(hl), a							; Save new counter of array index position
	jp 	MoveMartianBulletsLoops
MoveMartianBulletsOk:

	call	RemoveMartianShipBullets				; Remove Bullet from current position
	ld	a, b
	inc	a							; Increment Y position
;       cp      21
;       jp      nz, MoveMartianBulletsOkContinue
;       inc     a
MoveMartianBulletsOkContinue:
	ld	(hl), a							; Save new Y position in array
	cp	22	
	jp	z, MoveMartianBulletsClearArray				; Clear array for no printed Bullet
	ld	hl, CurrentMartianBullet
	ld	a, (hl)
	call	PrintMartianShipBullet
	ret

MoveMartianBulletsClearArray:
	ld	hl, CurrentMartianBullet	
	ld	a, (hl)							; Get current array index
	ld	hl, MartianBullets					; Get first X element address of Bullets array
	call	AddAToHL						; Get current X element address of Bullets array
        ld      a, (hl)
        ld      e, a
	ld	(hl), 0						        ; Reset X position
	ld	a, 100						        ; Add 100 positions to X element address for to find Y element address
	call	AddAToHL
        ld      a, (hl)
	ld	(hl), 0							; Reset Y position
        ;D      B, 21
        ;D      C,E
        ;ALL RemoveMartianShipBullets
        ld      d, 21
        ld      a, 32                                                   ; remove martian bullet from screen
        call    PrintRamChar
        inc     e
        ld      a, 32
        call    PrintRamChar
	ret
MartianBulletFireEvaluation:
	ld	hl, MartianShipsInfo+22
	ld	(hl), -1					        ; Reset martian ship counter	
MartianBulletFireEvaluationLoop:
	ld	hl, MartianShipsInfo+22
	ld	a, (hl)							; Get martian ship counter	
	inc	a							; Increase counter
	cp	4
	ret	z 							; Return after loop of all potential martians ships on screen
	ld	hl, MartianShipsInfo+22
	ld	(hl), a							; Set martian ship counter	
	ld	hl, MartianShipsInfo+2
	call	AddAToHL
	ld	a, (hl)
	cp	0
	jp	z, MartianBulletFireEvaluationLoop		        ; No martian ship present on screen with currend index
	call	GenerateNewRandomMartianBulletNumber		        ; Get random number
	ld	hl, MartianShipsInfo				        ; Get Bullet fire ratio
	cp	(hl)
	call	c, MartianBulletFireEvaluationOk			; Fire Bullet
	jp	MartianBulletFireEvaluationLoop
MartianBulletFireEvaluationOk:
	ld	hl, MartianShipsInfo+22
	ld	a, (hl)							; Get martian ship id	
	ld	hl, MartianShipsInfo+2
	call	AddAToHL
	ld	c, (hl)							; Get X martian ship position												
	ld	hl, MartianShipsInfo+22
	ld	a, (hl)							; Get martian ship id
	ld	hl, MartianShipsInfo+6
	call	AddAToHL
	ld	b, (hl)							; Get Y martian ship position
	call	MartianBulletFireEvaluationFirstIndexAvaible
	cp	15
	ret	z 							; No free Bullet array index found
	inc	b							; Increase Y pos 2 times for display Bullet under the ship
	inc	b
        ld      d, a
        ld      a, c
        and     1
        jr      nz, MartianBulletFireEvaluationOkEven
	inc 	c							; Increase X pos for display Bullet to have an even x position
MartianBulletFireEvaluationOkEven:  
        ld      a, d
	call	UpdateMartianBulletPosition
	call	PrintMartianShipBullet
	ret
;------------------------------------------------------------------------
; Get first available index in martian Bullets array
; Return A=First available index
;------------------------------------------------------------------------
MartianBulletFireEvaluationFirstIndexAvaible:
	push	bc
	xor     a
MartianBulletFireEvaluationFirstIndexAvaibleLoop:
	cp	15
	jp	z, MartianBulletFireEvaluationFirstIndexAvaibleNoFound
	ld	c, a
	ld	hl, MartianBullets
	call	AddAToHL
	ld	a,(hl)
	cp	0
	jp	z, MartianBulletFireEvaluationFirstIndexAvaibleLoopEnd
	ld	a, c 
	inc	a
	jp	MartianBulletFireEvaluationFirstIndexAvaibleLoop
MartianBulletFireEvaluationFirstIndexAvaibleLoopEnd:
	ld	a,c
	pop	bc
	ret
MartianBulletFireEvaluationFirstIndexAvaibleNoFound:
	ld	c, 15
	jp 	MartianBulletFireEvaluationFirstIndexAvaibleLoopEnd

;------------------------------------------------------------------------
; Reset martian Bullets positions
;------------------------------------------------------------------------
ResetMartianBulletsPositions:
	xor	a			; Reset counter
	ld	hl, MartianBullets
ResetMartianBulletsPositionsLoop:
	cp	15
	ret	z 		        ; All positions reset
	push	af	
	push	hl
	ld	(hl), 0
	ld	a, 100
	call	AddAToHL
	ld	(hl), 0
	pop 	hl	
	pop 	af
	inc	hl
	inc    	a
	jp	ResetMartianBulletsPositionsLoop

;------------------------------------------------------------------------
; Reset martian ship info
;------------------------------------------------------------------------
ResetMartianInfo:
	ld	hl, MartianShipsInfo
	ld	a, 1
ResetMartianInfoLoop:
	cp	25
	ret	z
	ld	(hl), 0
	inc	hl
	inc 	a
	jp	ResetMartianInfoLoop

;------------------------------------------------------------------------
; Reset human weapon hide timer
;------------------------------------------------------------------------
ResetHumanWeaponHideTimer:
        push hl
        ld hl, HumanWeaponHideTimer
        ld a, 0
        ld (hl), a
        pop hl
        ret

;------------------------------------------------------------------------
; Generate new random number for martian ship Bullet
; Return A=Generated random number (0<=a<=255)
;------------------------------------------------------------------------
GenerateNewRandomMartianBulletNumber:
	push	bc
	ld	a, r
	ld	b, a
	add	a, a
	add	a, a
	add	a, b
	inc	a
	pop	bc
	ret



;------------------------------------------------------------------------
; Remove martian ship Bullets
; C: X position, 
; B: Y position
;------------------------------------------------------------------------
RemoveMartianShipBullets:				
	ld	a, b
	cp	0
	ret	z			; No valid Y position
	push	bc	
        push    de			
	ld	a, 32
        ld      d, b
        ld      e, c
	call	PrintRamChar	        ; Print space at Bullet position
        				
	inc	e
	ld	a, 32			; Print space at Bullet position (X+1)
	call	PrintRamChar
        pop     de
	pop	bc			
	ret
;------------------------------------------------------------------------
; Print martian ship Bullet
; A: Array position
;------------------------------------------------------------------------
PrintMartianShipBullet:
	push	bc				
	push	af			
        ld      hl, TempValue
        ld      (hl), a	
	ld	hl, MartianBullets
	call	AddAToHL
	ld	c,(hl)				; Get X position
	pop 	af				
	add	a, 100
	ld	hl, MartianBullets
	call	AddAToHL
	ld	b,(hl)				; Get Y position
	ld	a, b
	cp	0
	jp	z, PrintMartianShipBulletEnd      ; No valid Y position
        push    de	
        
        ld      d,b
        ld      e,c	
        dec     d
        ld      a, 32
        call   PrintRamChar
        inc     e
        ld      a, 32
        call   PrintRamChar
        dec e
        inc d

	ld	a, SPRT_MARTIAN_BULLET
	call	PrintRamChar
	inc	e
	ld	a, SPRT_MARTIAN_BULLET+1
	call	PrintRamChar
        
        ld      hl, TempValue
        ld      a, (hl)
        ld	hl, MartianBullets
	call	AddAToHL
	ld	a,(hl)				; Get X position
        cp      0
        jp      nz, PrintMartianShipBulletContinue
        ld      a, 32
        call    PrintRamChar
        dec     e
        call    PrintRamChar
PrintMartianShipBulletContinue:        
        pop     de
PrintMartianShipBulletEnd:
	pop		bc	
	ret
;------------------------------------------------------------------------
; Update martian Bullet position
; C: X position
; B: Y position
; A: Array index
;------------------------------------------------------------------------
UpdateMartianBulletPosition:
	push	af			
	ld	hl, MartianBullets
	call	AddAToHL
	ld	(hl), c			; Save X position
	pop 	af			
	push	af			
	add	a, 100	
	ld	hl, MartianBullets
	call	AddAToHL
	ld	(hl), b		        ; Save Y position
	pop 	af			
	ret



;========================================================================
; TILE DEFINITIONS
;========================================================================
TilesDef:
        db 0x00,0x00,0x00,0x1F,0x10,0x10,0x10,0x10      ; TileBorderTopLeft - 96
        db 0x10,0x10,0x10,0x10,0x10,0x10,0x10,0x10      ; TileBorderVerical - 97
        db 0x00,0x00,0x00,0xFF,0x00,0x00,0x00,0x00      ; TileBorderHorizontal - 98
        db 0x00,0x00,0x00,0xF0,0x10,0x10,0x10,0x10      ; TileBorderTopRight - 99
        db 0x10,0x10,0x10,0x1F,0x00,0x00,0x00,0x00      ; TileBorderBottomLeft - 100
        db 0x10,0x10,0x10,0xF0,0x00,0x00,0x00,0x00      ; TileBorderBottomRight - 101
        db 0x88,0xD8,0xA9,0xA8,0x89,0x8A,0x89,0x00      ; TileMartianLabel1 - 102
        db 0x00,0x00,0xCB,0x2C,0xE8,0x28,0xE8,0x00      ; TileMartianLabel2 - 103
        db 0x10,0x10,0x38,0x90,0x10,0x12,0x0C,0x00      ; TileMartianLabel3 - 104
        db 0x40,0x00,0xC7,0x40,0x47,0x48,0xE7,0x00      ; TileMartianLabel4 - 105
        db 0x00,0x00,0x2C,0xB2,0xA2,0xA2,0xA2,0x00      ; TileMartianLabel5 - 106
        db 0x00,0x00,0x78,0x80,0xF0,0x08,0xF0,0x00      ; TileMartianLabel6 - 107
        db 0x88,0x88,0x8A,0xFA,0x8A,0x8A,0x89,0x00      ; TileHumansLabel1 - 108
        db 0x00,0x00,0x4D,0x4A,0x4A,0x4A,0xAA,0x00      ; TileHumansLabel2 - 109
        db 0x00,0x00,0x1C,0x82,0x9E,0xA2,0x9E,0x00      ; TileHumansLabel3 - 110
        db 0x00,0x00,0xB1,0xCA,0x8B,0x88,0x8B,0x00      ; TileHumansLabel4 - 111
        db 0x00,0x00,0xE0,0x00,0xC0,0x20,0xC0,0x00      ; TileHumansLabel5 - 112
        db 0x03,0x03,0x03,0x03,0x03,0x3F,0x3F,0x3F      ; TileHumanStatic1_1 - 113
        db 0xC0,0xC0,0xC0,0xC0,0xC0,0xFC,0xFC,0xFC      ; TileHumanStatic1_2 - 114
        db 0x33,0x33,0x33,0x33,0x33,0x33,0x33,0x33      ; TileHumanStatic2_1 - 115
        db 0xCC,0xCC,0xCC,0xCC,0xCC,0xCC,0xCC,0xCC      ; TileHumanStatic2_2 - 116
        db 0x03,0x03,0x03,0x03,0x03,0x1F,0x1F,0x1F      ; TileHumanStatic3_1 - 117
        db 0xC0,0xC0,0xC0,0xC0,0xC0,0xF8,0xF8,0xF8      ; TileHumanStatic3_2 - 118
        db 0x0E,0x0E,0x0E,0x0E,0x06,0x06,0x01,0x19      ; TileHumanRightRunning1_1_1 - 119
        db 0x00,0x00,0x00,0x00,0x00,0x00,0x80,0x80      ; TileHumanRightRunning1_1_2 - 120
        db 0x19,0x07,0x07,0x01,0x01,0x01,0x01,0x01      ; TileHumanRightRunning1_2_1 - 121
        db 0x80,0x80,0x80,0xE0,0xE0,0x98,0x98,0x80      ; TileHumanRightRunning1_2_2 - 122
        db 0x01,0x01,0x06,0x06,0x06,0x18,0x18,0x18      ; TileHumanRightRunning1_3_1 - 123
        db 0x80,0x80,0x60,0x60,0x18,0x18,0x06,0x06      ; TileHumanRightRunning1_3_2 - 124
        db 0x03,0x03,0x03,0x03,0x01,0x01,0x01,0x01      ; TileHumanRightRunning2_1_1 - 125
        db 0x00,0x00,0x00,0x00,0x00,0x98,0x98,0xE0      ; TileHumanRightRunning2_1_2 - 126
        db 0x01,0x07,0x07,0x19,0x19,0x01,0x01,0x01      ; TileHumanRightRunning2_2_1 - 127
        db 0xE0,0x80,0x80,0x80,0x80,0x80,0x80,0x80      ; TileHumanRightRunning2_2_2 - 128
        db 0x03,0x03,0x0C,0x0C,0x03,0x03,0x00,0x00      ; TileHumanRightRunning2_3_1 - 129
        db 0x60,0x60,0x00,0x00,0x00,0x00,0xC0,0xC0      ; TileHumanRightRunning2_3_2 - 130
        db 0x01,0x01,0x01,0x01,0x00,0x00,0x33,0x33      ; TileHumanRightRunning3_1_1 - 131
        db 0xC0,0xC0,0xC0,0xC0,0xC0,0xC0,0x00,0x00      ; TileHumanRightRunning3_1_2 - 132
        db 0x0F,0x0F,0x01,0x01,0x01,0x01,0x01,0x01      ; TileHumanRightRunning3_2_1 - 133
        db 0x00,0x00,0xC0,0xC0,0x30,0x30,0x00,0x00      ; TileHumanRightRunning3_2_2 - 134
        db 0x01,0x01,0x0C,0x0C,0x0C,0x0C,0x03,0x03      ; TileHumanRightRunning3_3_1 - 135
        db 0x00,0x00,0xC0,0xC0,0xC0,0x30,0x30,0x30      ; TileHumanRightRunning3_3_2 - 136
        db 0x00,0x00,0x00,0x00,0x00,0x00,0x01,0x01      ; TileHumanLeftRunning1_1_1 - 137
        db 0x70,0x70,0x70,0x70,0x60,0x60,0x80,0x98      ; TileHumanLeftRunning1_1_2 - 138
        db 0x01,0x01,0x01,0x07,0x07,0x19,0x19,0x01      ; TileHumanLeftRunning1_2_1 - 139
        db 0x98,0xE0,0xE0,0x80,0x80,0x80,0x80,0x80      ; TileHumanLeftRunning1_2_2 - 140
        db 0x01,0x01,0x06,0x06,0x18,0x18,0x60,0x60      ; TileHumanLeftRunning1_3_1 - 141
        db 0x80,0x80,0x60,0x60,0x60,0x18,0x18,0x18      ; TileHumanLeftRunning1_3_2 - 142
        db 0x00,0x00,0x00,0x00,0x00,0x19,0x19,0x07      ; TileHumanLeftRunning2_1_1 - 143
        db 0xC0,0xC0,0xC0,0xC0,0x80,0x80,0x80,0x80      ; TileHumanLeftRunning2_1_2 - 144
        db 0x07,0x01,0x01,0x01,0x01,0x01,0x01,0x01      ; TileHumanLeftRunning2_2_1 - 145
        db 0x80,0xE0,0xE0,0x98,0x98,0x80,0x80,0x80      ; TileHumanLeftRunning2_2_2 - 146
        db 0x06,0x06,0x00,0x00,0x00,0x00,0x03,0x03      ; TileHumanLeftRunning2_3_1 - 147
        db 0xC0,0xC0,0x30,0x30,0xC0,0xC0,0x00,0x00      ; TileHumanLeftRunning2_3_2 - 148
        db 0x03,0x03,0x03,0x03,0x03,0x03,0x00,0x00      ; TileHumanLeftRunning3_1_1 - 149
        db 0x80,0x80,0x80,0x80,0x00,0x00,0xCC,0xCC      ; TileHumanLeftRunning3_1_2 - 150
        db 0x00,0x00,0x03,0x03,0x0C,0x0C,0x00,0x00      ; TileHumanLeftRunning3_2_1 - 151
        db 0xF0,0xF0,0x80,0x80,0x80,0x80,0x80,0x80      ; TileHumanLeftRunning3_2_2 - 152
        db 0x00,0x00,0x03,0x03,0x03,0x0C,0x0C,0x0C      ; TileHumanLeftRunning3_3_1 - 153
        db 0x80,0x80,0x30,0x30,0x30,0x30,0xC0,0xC0      ; TileHumanLeftRunning3_3_2 - 154
        db 0x00,0x00,0x00,0x00,0x00,0x03,0x03,0x03      ; TileHumanJumping1_1_1 - 155
        db 0x00,0x00,0x00,0x00,0x00,0xC0,0xC0,0xC0      ; TileHumanJumping1_1_2 - 156
        db 0x03,0x03,0x3F,0x3F,0x3F,0x33,0x33,0x33      ; TileHumanJumping1_2_1 - 157
        db 0xC0,0xC0,0xFC,0xFC,0xFC,0xCC,0xCC,0xCC      ; TileHumanJumping1_2_2 - 158
        db 0x03,0x03,0x03,0x1C,0x1C,0x1C,0x1C,0x1C      ; TileHumanJumping1_3_1 - 159
        db 0xC0,0xC0,0xC0,0x38,0x38,0x38,0x38,0x38      ; TileHumanJumping1_3_2 - 160
        db 0x00,0x00,0x00,0x03,0x03,0x03,0x03,0x03      ; TileHumanJumping2_1_1 - 161
        db 0x00,0x00,0x00,0xC0,0xC0,0xC0,0xC0,0xC0      ; TileHumanJumping2_1_2 - 162
        db 0xFF,0xFF,0xFF,0x03,0x03,0x03,0x03,0x03      ; TileHumanJumping2_2_1 - 163
        db 0xFF,0xFF,0xFF,0xC0,0xC0,0xC0,0xC0,0xC0      ; TileHumanJumping2_2_2 - 164
        db 0x03,0x03,0xFF,0xFF,0xFF,0x00,0x00,0x00      ; TileHumanJumping2_3_1 - 165
        db 0xC0,0xC0,0xFF,0xFF,0xFF,0x00,0x00,0x00      ; TileHumanJumping2_3_2 - 166
        db 0x33,0x33,0x33,0x33,0x33,0x3F,0x3F,0x3F      ; TileHumanJumping3_1_1 - 167
        db 0xCC,0xCC,0xCC,0xCC,0xCC,0xFC,0xFC,0xFC      ; TileHumanJumping3_1_2 - 168
        db 0x03,0x03,0x03,0x03,0x03,0xC3,0xC3,0x3F      ; TileHumanJumping3_2_1 - 169
        db 0xC0,0xC0,0xC0,0xC0,0xC0,0xC3,0xC3,0xFC      ; TileHumanJumping3_2_2 - 170
        db 0x3F,0x3F,0x00,0x00,0x00,0x00,0x00,0x00      ; TileHumanJumping3_3_1 - 171
        db 0xFC,0xFC,0x00,0x00,0x00,0x00,0x00,0x00      ; TileHumanJumping3_3_2 - 172
        db 0xDF,0xDF,0xC1,0xC1,0xC1,0xC1,0xC1,0xC1      ; Tileartian1_A_1_1 - 173
        db 0xFB,0xFB,0x83,0x83,0x83,0x83,0x83,0x83      ; Tileartian1_A_1_2 - 174
        db 0xC1,0xC1,0xC1,0xC1,0xC1,0xC1,0xDF,0xDF      ; Tileartian1_B_1_1 - 175
        db 0x83,0x83,0x83,0x83,0x83,0x83,0xFB,0xFB      ; Tileartian1_B_1_2 - 176
        db 0xE1,0xE1,0xE1,0x1F,0x1F,0xE1,0xE1,0xE1      ; Tileartian2_A_1_1 - 177
        db 0xC7,0xC7,0xC7,0xF8,0xF8,0xC7,0xC7,0xC7      ; Tileartian2_A_1_2 - 178
        db 0x01,0x01,0x01,0xFF,0xFF,0x01,0x01,0x01      ; Tileartian2_B_1_1 - 179
        db 0xC0,0xC0,0xC0,0xFF,0xFF,0xC0,0xC0,0xC0      ; Tileartian2_B_1_2 - 180
        db 0xC1,0xE1,0xF1,0xD9,0xCD,0xC7,0xC3,0xC1      ; Tileartian3_A_1_1 - 181
        db 0x83,0x87,0x8F,0x9B,0xB3,0xE3,0xC3,0x83      ; Tileartian3_A_1_2 - 182
        db 0xC1,0xC3,0xC7,0xCD,0xD9,0xF1,0xE1,0xC1      ; Tileartian3_B_1_1 - 183
        db 0x83,0xC3,0xE3,0xB3,0x9B,0x8F,0x87,0x83      ; Tileartian3_B_2_2 - 184
        db 0x00,0x00,0x00,0x00,0x01,0x08,0x00,0x02      ; TileExplosion1 - 185
	db 0x08,0x00,0x04,0x01,0x10,0x04,0x00,0x24      ; TileExplosion2 - 186
	db 0x00,0x00,0x00,0x00,0x00,0x00,0x20,0x00      ; TileExplosion3 - 187
	db 0x80,0x00,0x40,0x00,0x20,0x08,0x80,0x20      ; TileExplosion4 - 188
	db 0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03      ; TileHumanBullet1 - 189
	db 0xC0,0xC0,0xC0,0xC0,0xC0,0xC0,0xC0,0xC0      ; TileHumanBullet2 - 190
	db 0x00,0x03,0x03,0x03,0x1F,0x1F,0x1F,0xFF      ; TileHumanWeapon1 - 191
	db 0x00,0xC0,0xC0,0xC0,0xF8,0xF8,0xF8,0xFF      ; TileHumanWeapon2 - 192
        db 0x03,0x03,0x03,0x03,0x03,0x03,0x03,0x03      ; TileMartianBullet1 - 193
	db 0xC0,0xC0,0xC0,0xC0,0xC0,0xC0,0xC0,0xC0      ; TileMartianBullet2 - 194

;========================================================================
; CONSTANTS
;========================================================================
SCR_BASE                        EQU 16384       ; Base address of the Spectrum screen
ATTR_BASE                       EQU 0x5800      ; Start of the attribute area
ROM_CHAR_SET_ADDRESS            EQU 0x3D00      ; Start of the ROM character set
RAM_CHAR_SET_ADDRESS            EQU 40000       ; Start of the RAM character set
RAM_GAME_MATRIX_ADDRESS         EQU 42000       ; Start of the RAM character set
SPRT_HUMAN_STATIC               EQU 113         ; ASCII code for the static human sprite
SPRT_JUMPING_HUMAN_1            EQU 155         ; ASCII code for the jumping human sprite (1)
SPRT_JUMPING_HUMAN_2            EQU 161         ; ASCII code for the jumping human sprite (2)
SPRT_JUMPING_HUMAN_3            EQU 167         ; ASCII code for the jumping human sprite (3)
SPRT_HUMAN_RUNNING_LEFT_POS1    EQU 137         ; ASCII code for the running human sprite (left 1)
SPRT_HUMAN_RUNNING_LEFT_POS2    EQU 143         ; ASCII code for the running human sprite (left 2)
SPRT_HUMAN_RUNNING_LEFT_POS3    EQU 149         ; ASCII code for the running human sprite (left 3)
SPRT_HUMAN_RUNNING_RIGHT_POS1   EQU 119         ; ASCII code for the running human sprite (right 1)
SPRT_HUMAN_RUNNING_RIGHT_POS2   EQU 125         ; ASCII code for the running human sprite (right 2)
SPRT_HUMAN_RUNNING_RIGHT_POS3   EQU 131         ; ASCII code for the running human sprite (right 3)
SPRT_HUMAN_WEAPON               EQU 191         ; ASCII code for the human weapon sprite
SPRT_MARTIAN_SHIP_1_A:	        EQU 173		; First martian ship nr. 1a character
SPRT_MARTIAN_SHIP_1_B:	        EQU 175		; First martian ship nr. 1b character
SPRT_MARTIAN_SHIP_2_A:	        EQU 177		; First martian ship nr. 2a character					
SPRT_MARTIAN_SHIP_2_B:	        EQU 179	        ; First martian ship nr. 2b character
SPRT_MARTIAN_SHIP_3_A:	        EQU 181		; First martian ship nr. 3a character								
SPRT_MARTIAN_SHIP_3_B:	        EQU 183		; First martian ship nr. 3b character
SPRT_HUMAN_BULLET               EQU 189         ; ASCII code for the human bullet sprite
SPRT_MARTIAN_BULLET             EQU 193         ; ASCII code for the martian bullet sprite
SPRT_EXPLOSION                  EQU 185         ; ASCII code for the explosion sprite
TILE_BORDER_TOP_LEFT_CORNER     EQU 96          ; ASCII code for the top-left corner of the border
TILE_BORDER_VERTICAL            EQU 97          ; ASCII code for the vertical border
TILE_BORDER_HORIZONTAL          EQU 98          ; ASCII code for the horizontal border
TILE_BORDER_TOP_RIGHT_CORNER    EQU 99          ; ASCII code for the top-right corner of the border
TILE_BORDER_BOTTOM_LEFT_CORNER  EQU 100         ; ASCII code for the bottom-left corner of the border
TILE_BORDER_BOTTOM_RIGHT_CORNER EQU 101         ; ASCII code for the bottom-right corner of the border     
TEXT_MARTIAN_WAR                db "MARTIAN WAR",0
TEXT_MARTIAN_WAR_UNDERLINE      db "===========",0
TEXT_PRESENTATION_3_1           db "There is one weapon remaining",0
TEXT_PRESENTATION_4_1           db "on Earth.",0
TEXT_PRESENTATION_6_1           db "There will be martian ships",0
TEXT_PRESENTATION_7_1           db "flying all over the place and",0
TEXT_PRESENTATION_8_1           db "dropping Bullets down on you",0
TEXT_PRESENTATION_9_1           db "and the human population.",0
TEXT_PRESENTATION_10_1           db "Your mission is quite simple.",0
TEXT_PRESENTATION_11_1           db "There are a limited number of",0
TEXT_PRESENTATION_12_1           db "martian ships and if you",0
TEXT_PRESENTATION_13_1           db "destroy all of them before",0
TEXT_PRESENTATION_14_1           db "they destroy the whole",0
TEXT_PRESENTATION_15_1           db "population of Earth you win.",0
TEXT_PRESENTATION_16_1           db "If your weapon is hit it will",0
TEXT_PRESENTATION_17_1           db "have to be repaired and the",0
TEXT_PRESENTATION_18_1           db "refore will be unusable for a",0
TEXT_PRESENTATION_19_1           db "period of time.",0
TEXT_PRESENTATION_21_3           db "HIT \'RETURN\' TO CONTINUE",0
TEXT_FAUSTO_PRACEK               db "2025 - Fausto Pracek",0
TXT_LEVELS:			db 'WHAT LEVEL WOULD YOU LIKE (1-3)?',0
TXT_LEVEL_1:			db '1  -  Beginner',0
TXT_LEVEL_2:			db '2  -  Intermediate',0
TXT_LEVEL_3:			db '3  -  Advanced',0
TXT_KEYS:			db 'The control weapon keys are:',0
TXT_KEY_4:			db '4       Move to left',0
TXT_KEY_5:			db '5       Stop',0
TXT_KEY_6:			db '6       Move to right',0
TXT_KEY_SPACE:			db 'SPACE   Fire a missile',0
TXT_LOSE_1:			db 'SORRY GUY',0
TXT_LOSE_2:			db 'THE MARTIANS HAVE SUCCESSFULLY',0
TXT_LOSE_3:			db 'DESTROYED ALL LIFE ON EARTH!',0
TXT_PLAY_AGAIN:		  	db "HIT  \'RETURN\'  TO PLAY AGAIN",0
TXT_WIN_1: 			db 'CONGRATULATIONS',0
TXT_WIN_2: 			db 'YOU HAVE SAVED  EARTH FROM THE',0
TXT_WIN_3: 			db 'MARTIAN  ATTACK!!',0
KEYBOARD_MAP:                   db #FE,"#","Z","X","C","V"
                                db #FD,"A","S","D","F","G"
                                db #FB,"Q","W","E","R","T"
                                db #F7,"1","2","3","4","5"
                                db #EF,"0","9","8","7","6"
                                db #DF,"P","O","I","U","Y"
                                db #BF,"#","L","K","J","H"
                                db #7F," ","#","M","N","B"




TEXT_HUMANS                     db "Humans",0
TEXT_MARTIANS                   db "Martians",0

;========================================================================
; VARIABLES
;========================================================================

HumansToDisplay                 db 0
HumansLastStaticPosLeft         db 0
HumansLastStaticPosRight        db 0
Humans                          db 0
Martians                        db 0
HumansDigits                    .byte 0, 0
MartiansDigits                  .byte 0, 0
SelectedLevel                   db 0
HumanWeaponPosition             db 0
HumanWeaponDirection            db 0
HumanWeaponBulletXPosition      db 0
HumanWeaponHideTimer            db 0
HumanWeaponHideLevelTimer       db 0
HumanWeaponBulletYPosition      db 0
HumansWeaponBulletMaxTop        db 0
MartianShipMaxRight             db 0
RandomSeed			db 0
MartianShipCollisionIndex       db 0			
MartianShipsInfo		.byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0		; Bullet frequency ratio, number of displayd ships, XPos1, Xpos2, XPos3, XPos4, YPos1, YPos2, YPos3, YPos4, Direction 1, Direction 2, Direction 3, Direction 4, Direction changed 1, Direction changed 2, Direction changed 3, Direction changed 4, Type 1, Type 2, Type 3, Type 4, Refreshing ship, Ship speed timer, Ship speed timer counter
MartianBullets:														; 15 X positions, 15 Y positions, speed, speed counter
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0  
	.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
CurrentMartianBullet               db 0	
SpriteCollisionStaticHumanPosition db 0			
TempValue                        db 0
CurrentJumpingHuman             db 0
RandomSeed16                     DEFW 0xBEEF
        SAVESNA "martian.sna", Start

