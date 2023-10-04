;===============================================================
; NSMBWii-styled sprite status bar
; With star coin compatibility
; By Erik557
;===============================================================
; A status bar which was intended as a replica of NSMBWii's HUD,
; however I ended up making some modifications to it.
;===============================================================
; NOTE: It's reccomended to install the No More Sprite Tile
; limits patch!
;===============================================================


; RAM addreses (you can change them if you want, however
; it's not really needed.)

       !LivesCounterXPos     = $0EF9
       !LivesCounterYPos     = $0EFA
       !CoinCounterXPos      = $0EFB
       !CoinCounterYPos      = $0EFC
       !SCCounterXPos        = $0EFD
       !SCCounterYPos        = $0EFE
       !TimerXPos            = $0EFF
       !TimerYPos            = $0F00
       !ScoreCounterXPos     = $0F01
       !ScoreCounterYPos     = $0F02
       !StatusBarDisable     = $0F03
       !SCFreeRAM            = $7FB408

; Customizable values

       !CoinsFor1Up    = 100       ; How many coins are needed for a 1-Up
                                   ; Decimal, maximum of 100.

       !TimerSpeed     = $28       ; Timer speed.
                                   ; $3C means 1 SMW Second = 1 second.
                                   ; Default: 3 SMW Seconds = 2 seconds.

       !Style          = 1         ; 0 is ultra-compact (1 row), 1 is 2 rows.

; Tilemaps (GFX00/01)

       !PlayerHeadTile  = $66
       !FullStatusCoin  = $4A
       !CrossTile       = $0D
       !ClockTile       = $0C
       !Number0         = $20
       !Number1         = $21
       !Number2         = $22
       !Number3         = $23
       !Number4         = $30
       !Number5         = $31
       !Number6         = $32
       !Number7         = $33
       !Number8         = $7E
       !Number9         = $1B
       !BlankStatusCoin = $1A

; Nuke that old ugly bar
; (hijacks by Lui37)

org $0081F4
       BRA + : NOP : +
org $0082E8
       BRA + : NOP : +
org $008C81
       pad $009045
org $009051
       pad $0090D1
org $00985A
       BRA $01 : NOP
org $00A2D5
       BRA $01 : NOP
org $00A5A8
       BRA $01 : NOP
org $00A5D5
       BRA $01 : NOP
if read1($00F5F3) != $22    ; powerdown check
org $00F5F8
       BRA $02 : NOP #2
endif
org $01C540
       BRA + : NOP #11 : +

; Remap some tiles

org $00FBA4
       db $64,$64,$62,$60,$E8,$EA,$EC,$EA
org $01E985
       db $64,$64,$62,$60
org $028C6A
       db $64,$64,$62,$60
org $028D42
       db $68,$68,$6A,$6A,$6A,$62,$62,$62
       db $64,$64,$64,$64,$64
org $0296D8
       db $64,$62,$64,$62,$60,$62,$60
org $029922
       db $64,$62,$64,$62,$62
org $029922
       db $64,$62,$64,$62,$62
org $029C33
       db $64,$64,$62,$60,$60,$60,$60,$60
org $02A347
       db $64,$64,$60,$62
org $028ECC
       db $69
;============================================

; SA-1 check

       !dp = $0000
       !addr = $0000
       !sa1 = 0
       !gsu = 0

if read1($00FFD6) == $15
       sfxrom
       !dp = $6000
       !addr = !dp
       !gsu = 1
elseif read1($00FFD5) == $23
       sa1rom
       !dp = $3000
       !addr = $6000
       !sa1 = 1
endif

;============================================

; Hijacks

org $008292
       autoclean JML IRQHack

org $00A5AB
       autoclean JML StatusBarInit

org $00A2E6
       autoclean JML StatusBarMain

freecode

print "Inserted at $",pc

;============================================

; Actual code

;==================
; IRQ Hack
;==================

IRQHack:
       PHB : PHK : PLB
       LDX $0100|!addr
       LDA .allowedModes,x
       BNE .disableIRQ

.defaultIRQ
       PLB
       LDY #$00
       LDA $4211
       JML $008297

.disableIRQ
       PLB
       LDY #$E0
       LDA $4211
       STY $4209
       STZ $420A
       LDA $0DAE|!addr
       STA $2100
       LDA $0D9F|!addr
       STA $420C
       JML $008394

.allowedModes
       db $00,$00,$00,$00,$00,$00,$00,$00
       db $00,$00,$00,$01,$00,$00,$00,$01
       db $00,$00,$01,$01,$01,$01,$00,$00
       db $00,$00,$00,$00,$00

;==================
; INIT Code
;==================

StatusBarInit:
        JSL $05809E                       ; restore hijacked code
        PHA                               ; preserve accumulator

; Set the position of every timer.

       LDA #10
       STA !LivesCounterXPos|!addr
       if !Style == 1
              STA !TimerXPos|!addr
       endif
       LDA #50
       STA !CoinCounterXPos|!addr
       if !Style == 1
              INC #3
       else
              LDA #140
       endif
       STA !ScoreCounterXPos|!addr
       if !Style == 0
              LDA #150
              STA !TimerXPos|!addr
       endif
       LDA #90
       STA !SCCounterXPos|!addr
       LDA #10
       STA !LivesCounterYPos|!addr
       STA !CoinCounterYPos|!addr
       STA !SCCounterYPos|!addr
       if !Style == 1
             LDA #30
       endif
              INC #3
       STA !TimerYPos|!addr
       if !Style == 0
              LDA #5
       endif
       STA !ScoreCounterYPos|!addr

       LDA #!TimerSpeed                  ;\ initialize timer
       STA $0F30|!addr                   ;/
       PLA                               ; restore accumulator
       JML $00A5AF                       ; return

;==================
; MAIN Code
;==================

StatusBarMain:
       JSL $028AB1                 ; restore hijacked code

       PHP : SEP #$30              ;\ Preserve p.flags/8-bit AXY
       PHB : PHK : PLB             ;/ set data bank

; Rewrite the functions of the counters which were removed by hijacks above.

; coins
       LDA $13CC|!addr
       BEQ .NoCoinIncrease
       DEC $13CC|!addr
       INC $0DBF|!addr
       LDA $0DBF|!addr
       CMP #!CoinsFor1Up
       BCC .NoCoinIncrease
       SEC : SBC #!CoinsFor1Up
       STA $0DBF|!addr
       INC $18E4|!addr

; timer
.NoCoinIncrease
       LDA $1493|!addr
       ORA $9D
       BNE .NoTimerDecrease
       LDA $0D9B|!addr
       CMP #$C1
       BEQ .NoTimerDecrease
       DEC $0F30|!addr
       BPL .NoTimerDecrease
       LDA #!TimerSpeed
       STA $0F30|!addr
       LDA $0F31|!addr
       ORA $0F32|!addr
       ORA $0F33|!addr
       BEQ .NoTimerDecrease
       LDX #$02
.TimerLoop
       DEC $0F31|!addr,x
       BPL .zero
       LDA #$09
       STA $0F31|!addr,x
       DEX
       BPL .TimerLoop
.zero
       LDA $0F31|!addr
       BNE .NoSpeed
       LDA $0F32|!addr
       AND $0F33|!addr
       CMP #$09
       BNE .NoSpeed
       LDA #$FF
       STA $1DF9|!addr
.NoSpeed
       LDA $0F31|!addr
       ORA $0F32|!addr
       ORA $0F33|!addr
       BNE .NoTimerDecrease
       JSL $00F606

; This is the actual start of the status bar.

.NoTimerDecrease
       LDA !StatusBarDisable|!addr ;\ if status bar disabled, return
       BNE .return                 ;/
       LDA $0100|!addr             ;\ if on the title screen, return
       CMP #$07                    ; |
       BEQ .return                 ;/

       JSR DrawLives               ;\
       JSR DrawCoins               ; | Draw timers.
       JSR DrawYCoins              ; |
       JSR DrawTimer               ; |
       JSR DrawScore               ;/

.return
       PLB                         ;\ restore
       PLP                         ;/
       JML $00A2EA                 ; return

numberTable:
        db !Number0,!Number1,!Number2,!Number3,!Number4
        db !Number5,!Number6,!Number7,!Number8,!Number9

;========================
; Draw the lives counter
;========================

; this is the only counter I'll comment. Most of the others work the same way, anyway.
DrawLives:
       REP #$10                    ; 16 bit XY
       LDX #$0020                  ; start from 9th slot as others are reserved

       LDY #$0004                  ;\  reserve Y+1 free slots
       STY $00                     ; | returns if not enough
       JSR findFreeSlots           ;/

; "x" tile
       JSR findSlot                ; get slot
       LDA !LivesCounterXPos|!addr ;\
       CLC : ADC #10               ; | Tile X position
       STA $0200|!addr,x           ;/
       LDA !LivesCounterYPos|!addr ;\
       CLC : ADC #11               ; | Tile Y position
       STA $0201|!addr,x           ;/
       LDA #!CrossTile             ;\ Tile number
       STA $0202|!addr,x           ;/
       LDA #$3E                    ;\ Properties of the tile
       STA $0203|!addr,x           ;/
       STZ $00                     ;\ Drew one 8x8 tile, so $00 = 00
       JSR setOAMInfo              ;/

; the rest is similar.
; mario's head
       JSR findSlot
       LDA !LivesCounterXPos|!addr
       STA $0200|!addr,x
       LDA !LivesCounterYPos|!addr
       STA $0201|!addr,x
       LDA #!PlayerHeadTile
       STA $0202|!addr,x
       LDA #$70
       STA $0203|!addr,x
       LDA #$02                    ;\  Drew one 16x16 tile, so $00 = 02
       STA $00                     ; |
       JSR setOAMInfo              ;/

       LDA $0DBE|!addr             ;\
       BMI +                       ; |
       CMP #98                     ; |
       BCC +                       ; | hotfix:
       LDA #98                     ; | prevent the player from getting more than 99 lives
       STA $0DBE|!addr             ;/

+      INC                         ;\
       CMP #$0A                    ; | Determine number of digits to show
       BCS .twoDigits              ;/

.oneDigit
       TAY
       JSR findSlot
       LDA !LivesCounterXPos|!addr
       CLC : ADC #23
       STA $0200|!addr,x
       LDA !LivesCounterYPos|!addr
       CLC : ADC #11
       STA $0201|!addr,x
       LDA numberTable,y
       STA $0202|!addr,x
       LDA #$3E
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo
       JMP +

.twoDigits
       JSR hexToDec
       JSR findSlot
       LDA !LivesCounterXPos|!addr
       CLC : ADC #20
       STA $0200|!addr,x
       LDA !LivesCounterYPos|!addr
       CLC : ADC #11
       STA $0201|!addr,x
       LDA numberTable,y
       STA $0202|!addr,x
       LDA #$3E
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo

       LDA $0DBE|!addr : INC
       JSR hexToDec
       TAY
       JSR findSlot
       LDA !LivesCounterXPos|!addr
       CLC : ADC #28
       STA $0200|!addr,x
       LDA !LivesCounterYPos|!addr
       CLC : ADC #11
       STA $0201|!addr,x
       LDA numberTable,y
       STA $0202|!addr,x
       LDA #$3E
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo

+      SEP #$10                    ; 8-bit XY
       RTS                         ; return

;=======================
; Draw the coin counter
;=======================

DrawCoins:
       REP #$10
       LDX #$0020

       LDY #$0004
       STY $00
       JSR findFreeSlots

       JSR findSlot
       LDA !CoinCounterXPos|!addr
       CLC : ADC #10
       STA $0200|!addr,x
       LDA !CoinCounterYPos|!addr
       CLC
       ADC #11
       STA $0201|!addr,x
       LDA #!CrossTile
       STA $0202|!addr,x
       LDA #$3C
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo

       JSR findSlot
       LDA !CoinCounterXPos|!addr
       STA $0200|!addr,x
       LDA !CoinCounterYPos|!addr
       STA $0201|!addr,x
       LDA #$E8
       STA $0202|!addr,x
       LDA #$34
       STA $0203|!addr,x
       LDA #$02
       STA $00
       JSR setOAMInfo

       LDA $0DBF|!addr
       CMP #$0A
       BCS .twoDigits
.oneDigit
       TAY
       JSR findSlot
       LDA !CoinCounterXPos|!addr
       CLC : ADC #23
       STA $0200|!addr,x
       LDA !CoinCounterYPos|!addr
       CLC : ADC #11
       STA $0201|!addr,x
       LDA numberTable,y
       STA $0202|!addr,x
       LDA #$3C
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo
       JMP +

.twoDigits
       JSR hexToDec
       JSR findSlot
       LDA !CoinCounterXPos|!addr
       CLC : ADC #20
       STA $0200|!addr,x
       LDA !CoinCounterYPos|!addr
       CLC : ADC #11
       STA $0201|!addr,x
       LDA numberTable,y
       STA $0202|!addr,x
       LDA #$3C
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo

       LDA $0DBF|!addr
       JSR hexToDec
       TAY
       JSR findSlot
       LDA !CoinCounterXPos|!addr
       CLC : ADC #28
       STA $0200|!addr,x
       LDA !CoinCounterYPos|!addr
       CLC : ADC #11
       STA $0201|!addr,x
       LDA numberTable,y
       STA $0202|!addr,x
       LDA #$3C
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo

+      SEP #$10
       RTS

;=============================
; Draw the yoshi coin counter
;=============================

DrawYCoins:
       REP #$10
       LDX #$0020

       LDY #$0004
       STY $00
       JSR findFreeSlots

       SEP #$10
       LDY #$02
.LoopCoins
       LDA !SCFreeRAM
       AND .MaskBits,y
       BEQ .NotCollected

       REP #$10
       JSR findSlot
       SEP #$10
       LDA !SCCounterXPos|!addr
       CLC : ADC .collectedPositions,y
       STA $0200|!addr,x
       LDA !SCCounterYPos|!addr
       STA $0201|!addr,x
       LDA #!FullStatusCoin
       STA $0202|!addr,x
       LDA #$34
       STA $0203|!addr,x
       LDA #$02
       STA $00
       REP #$10
       JSR setOAMInfo
       SEP #$10
       JMP .finishDraw

.NotCollected
       REP #$10
       JSR findSlot
       SEP #$10
       LDA !SCCounterXPos|!addr
       CLC : ADC .collectedPositions,y
       STA $0200|!addr,x
       LDA !SCCounterYPos|!addr
       STA $0201|!addr,x
       LDA #!BlankStatusCoin
       STA $0202|!addr,x
       LDA #$34
       STA $0203|!addr,x
       STZ $00
       REP #$10
       JSR setOAMInfo
       SEP #$10

       REP #$10
       JSR findSlot
       SEP #$10
       LDA !SCCounterXPos|!addr
       CLC : ADC .collectedPositions,y
       ADC #8
       STA $0200|!addr,x
       LDA !SCCounterYPos|!addr
       STA $0201|!addr,x
       LDA #!BlankStatusCoin
       STA $0202|!addr,x
       LDA #$74
       STA $0203|!addr,x
       STZ $00
       REP #$10
       JSR setOAMInfo
       SEP #$10

       REP #$10
       JSR findSlot
       SEP #$10
       LDA !SCCounterXPos|!addr
       CLC : ADC .collectedPositions,y
       STA $0200|!addr,x
       LDA !SCCounterYPos|!addr
       CLC : ADC #8
       STA $0201|!addr,x
       LDA #!BlankStatusCoin
       STA $0202|!addr,x
       LDA #$B4
       STA $0203|!addr,x
       STZ $00
       REP #$10
       JSR setOAMInfo
       SEP #$10

       REP #$10
       JSR findSlot
       SEP #$10
       LDA !SCCounterXPos|!addr
       CLC : ADC .collectedPositions,y
       ADC #8
       STA $0200|!addr,x
       LDA !SCCounterYPos|!addr
       CLC : ADC #8
       STA $0201|!addr,x
       LDA #!BlankStatusCoin
       STA $0202|!addr,x
       LDA #$F4
       STA $0203|!addr,x
       STZ $00
       REP #$10
       JSR setOAMInfo
       SEP #$10

.finishDraw
       DEY
       BMI +
       JMP .LoopCoins
+      SEP #$10
       RTS

.MaskBits
       db $01,$02,$04
.collectedPositions
       db 0,16,32

;================
; Draw the timer
;================

DrawTimer:
       REP #$10
       LDX #$0020

       LDY #$0003
       STY $00
       JSR findFreeSlots

       JSR findSlot
       LDA !TimerXPos|!addr
       INC : INC
       STA $0200|!addr,x
       LDA !TimerYPos|!addr
       CLC : ADC #9
       STA $0201|!addr,x
       LDA #!ClockTile
       STA $0202|!addr,x
       LDA #$3E
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo

       SEP #$10
       LDY #$02
.loop
       PHY
       LDA $0F31|!addr,y
       TAY
       STY $00
       REP #$10
       JSR findSlot
       SEP #$10
       LDA !TimerXPos|!addr
       PLY
       CLC : ADC .offsets,y
       STA $0200|!addr,x
       PHY
       LDY $00
       LDA !TimerYPos|!addr
       CLC : ADC #8
       STA $0201|!addr,x
       LDA numberTable,y
       STA $0202|!addr,x
       LDA #$3E
       STA $0203|!addr,x
       REP #$10
       STZ $00
       JSR setOAMInfo
       SEP #$10

       PLY
       DEY
       BPL .loop

       SEP #$10
       RTS

.offsets
       db 12,20,28

;================
; Draw the score
;================

DrawScore:
       SEP #$10
       JSR HandleScore             ; score is handled by separate
       REP #$10
       LDX #$0020

       LDY #$0006
       STY $00
       JSR findFreeSlots

       JSR findSlot
       LDA !ScoreCounterXPos|!addr
       CLC : ADC #60
       STA $0200|!addr,x
       LDA !ScoreCounterYPos|!addr
       CLC : ADC #8
       STA $0201|!addr,x
       LDA #!Number0
       STA $0202|!addr,x
       LDA #$3E
       STA $0203|!addr,x
       STZ $00
       JSR setOAMInfo

       SEP #$10
       LDY #$05
.loop
       PHY
       REP #$10
       JSR findSlot
       SEP #$10
       LDA $0F29|!addr,y
       TAY
       LDA numberTable,y
       STA $0202|!addr,x
       PLY
       LDA !ScoreCounterXPos|!addr
       CLC : ADC .offsets,y
       STA $0200|!addr,x
       LDA !ScoreCounterYPos|!addr
       CLC : ADC #8
       STA $0201|!addr,x
       LDA #$3E
       STA $0203|!addr,x
       REP #$10
       STZ $00
       JSR setOAMInfo
       SEP #$10
       DEY
       BPL .loop

       SEP #$10
       RTS

.offsets
       db 12,20,28,36,44,52


;===================
; Handles the score
;===================

HandleScore:
       REP #$20
       LDX #$03
CODE_008E97:
       LDA $0F36|!addr,X
       STA $00              ;$008E9A
       STZ $01              ;$008E9C
       REP #$20             ;$008E9E
       LDA $0F34|!addr,X    ;$008EA0
       SEC                  ;$008EA3
       SBC #$423F           ;$008EA4
       LDA $00              ;$008EA7
       SBC #$000F           ;$008EA9
       BCC CODE_008EBF      ;$008EAC
       SEP #$20             ;$008EAE
       LDA #$0F             ;$008EB0
       STA $0F36|!addr,X    ;$008EB2
       LDA #$42             ;$008EB5
       STA $0F35|!addr,X    ;$008EB7
       LDA #$3F             ;$008EBA
       STA $0F34|!addr,X    ;$008EBC
CODE_008EBF:
       SEP #$20
       DEX                  ;$008EC1
       DEX                  ;$008EC2
       DEX                  ;$008EC3
       BPL CODE_008E97      ;$008EC4

       LDA $0F36|!addr      ;$008EC6
       STA $00              ;$008EC9
       STZ $01              ;$008ECB
       LDA $0F35|!addr      ;$008ECD
       STA $03              ;$008ED0
       LDA $0F34|!addr      ;$008ED2
       STA $02              ;$008ED5
       LDX #$14             ;$008ED7
       LDY #$00             ;$008ED9
CODE_009012:
       SEP #$20
       STZ $0F15|!addr,X    ;$009014
CODE_009017:
       REP #$20
       LDA $02              ;$009019
       SEC                  ;$00901B
       SBC DATA_008FFC,Y    ;$00901C
       STA $06              ;$00901F
       LDA $00              ;$009021
       SBC DATA_008FFA,Y    ;$009023
       STA $04              ;$009026
       BCC CODE_009039      ;$009028
       LDA $06              ;$00902A
       STA $02              ;$00902C
       LDA $04              ;$00902E
       STA $00              ;$009030
       SEP #$20             ;$009032
       INC $0F15|!addr,X    ;$009034
       BRA CODE_009017      ;$009037

CODE_009039:
       INX
       INY                  ;$00903A
       INY                  ;$00903B
       INY                  ;$00903C
       INY                  ;$00903D
       CPY #$18             ;$00903E
       BNE CODE_009012      ;$009040
       SEP #$20             ;$009042
       RTS                  ;$009044

DATA_008FFA:
       db $01,$00

DATA_008FFC:
       db $A0,$86,$00,$00,$10,$27,$00,$00
       db $E8,$03,$00,$00,$64,$00,$00,$00
       db $0A,$00,$00,$00,$01,$00

;========================
; Other misc. routines
;========================

; Looks for the next free sprite slot in $0200-$03FC
; (16-bit X)

findSlot:
-      CPX #$0200
       BEQ .break
       LDA $0201|!addr,x
       CMP #$F0
       BEQ .break
       INX #4
       BRA -
.break
       RTS

; Finds n free slots
; Entry: $00 for number of slots (16 bit)
; (returns from the "calling" graphics routine if less than n found)

findFreeSlots:
       PHY : LDY #$0000
       LDX #$0020
.loop
       CPX #$0200
       BEQ .notEnoughFound

       LDA $0201|!addr,x
       CMP #$F0
       BNE .notFree
       INY
       CPY.w $00|!dp
       BEQ .enoughFound
.notFree
       INX #4
       BRA .loop
.notEnoughFound
       PLY
       PLA
       PLA
       RTS
.enoughFound
       PLY
       RTS

; Sets OAM info in $0420+
; Entry: $00 with the size (#$00 is 8x8, #$02 is 16x16)

setOAMInfo:
       PHX
       LDA $0200|!addr,x
       REP #$20
       TXA : LSR #2 : TAX
       SEP #$20
       LDA $00
.store
       STA $0420|!addr,x
       PLX
       RTS


; Converts Hex to decimal (16 bit indexes)
; Entry: A with the number

hexToDec:
       LDY #$0000
.loop
       CMP #$0A
       BCC .break
       SBC #$0A
       INY
       BRA .loop
.break
       RTS

print bytes

;==============================================
; Install the Mario GFX DMA patch if necessary
;==============================================

if read1($00A300) != $5C

       print " The Mario GFX DMAer patch has not been applied yet. Applying it now..."


       ; vvv CODE BELOW BY LADIDA vvv

       !Tile = $0A   ;Tile where the extended tiles will be loaded to. Takes up 2 8x8's
                     ;located in SP1

       if !sa1 == 1

              org $00A300
                     autoclean JML BEGINDMA


              org $00DF1A
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00

              db $00,$00,$00,$00,$00,$00,$28,$00
              db $00

              db $00,$00,$00,$00,$04,$04,$04,$00
              db $00,$00,$00,$00,$04,$00,$00,$00
              db $00,$04,$04,$04,$00,$00,$04,$04
              db $04,$04,$04,$04,$00,$00,$04,$00
              db $00,$00,$00,$04,$00,$00,$00,$00
              db $04,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00

              db $00,$00,$00,$00,$04,$04,$04,$00
              db $00,$00,$00,$00,$04,$00,$00,$00
              db $00,$04,$04,$04,$00,$00,$04,$04
              db $04,$04,$04,$04,$00,$00,$04,$00
              db $00,$00,$00,$04,$00,$00,$00,$00
              db $04,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00,$00,$00,$00
              db $00,$00,$00,$00,$00

              org $00DFDA
              db $00,$02,$80,$80          ;[00-03]
              db $00,$02,!Tile,!Tile+$1   ;[04-07]
              db $00,$00,$00,$00          ;[08-0B]
              db $00,$00,$00,$00          ;[0C-0F]
              db $00,$00,$00,$00          ;[10-13]
              db $00,$00,$00,$00          ;[14-17]
              db $00,$00,$00,$00          ;[18-1B]
              db $00,$00,$00,$00          ;[1C-1F]
              db $00,$00,$00,$00          ;[20-23]
              db $00,$00,$00,$00          ;[24-27]
              db $00,$02,$02,$80          ;[28-2B]      Balloon Mario
              db $04                      ;[2C]         Cape
              db $7F                      ;[2D]         Random Gliding tile
              db $4A,$5B,$4B,$5A          ;[2E-31]      Random Gliding frames


              freecode

              BEGINDMA:
                     PHB : PHK : PLB
                     REP #$20
                     LDX #$02
                     LDY $6D84
                     BEQ CODE_00A328

                     ;;
                     ;Mario's Palette
                     ;;

                     LDY #$86
                     STY $2121
                     LDA #$2200
                     STA $4310
                     LDA $6D82
                     STA $4312
                     LDY #$00
                     STY $4314
                     LDA #$0014
                     STA $4315
                     STX $420B

              CODE_00A328:
                     LDY #$80
                     STY $2115
                     LDA #$1801
                     STA $4310

                     LDY $6100
                     CPY #$14
                     BEQ ContinUpload
                     CPY #$07
                     BEQ ContinUpload
                     JMP StopUpload

              ContinUpload:
                     ;;
                     ;Random Cape Tile
                     ;;

                     LDA #$67F0
                     STA $2116
                     LDA $6D99
                     STA $4312
                     LDY #$7E
                     STY $4314
                     LDA #$0020
                     STA $4315
                     STX $420B

                     ;;
                     ;Mario's 8x8 tiles
                     ;;

                     LDA.w #!Tile<<4|$6000
                     STA $2116

                     LDA #$0040
                     STA $4315
                     SEP #$20
                     LDY $19
                     LDA $73E0
                     CLC : ADC DMAGFXtable,y
                     TAY
                     LDA DMAGFXbigtables,y
                     REP #$20
                     ASL #2
                     CLC : ADC.w #BEGINXTND
                     STA $4312
                     LDY.b #BEGINXTND>>16
                     STY $4314
                     STX $420B


              StopUpload:
                     ;;
                     ;Mario's top tiles
                     ;;

                     LDA #$6000
                     STA $2116
                     LDX #$00
              .loop
                     LDA $6D85,x
                     STA $4312
                     LDY #$7E
                     STY $4314
                     LDA #$0040
                     STA $4315
                     LDY #$02
                     STY $420B
                     INX #2
                     CPX $6D84
                     BCC .loop

                     ;;
                     ;Mario's bottom tiles
                     ;;

                     LDA #$6100
                     STA $2116
                     LDX #$00
              .loop2
                     LDA $6D8F,x
                     STA $4312
                     LDY #$7E
                     STY $4314
                     LDA #$0040
                     STA $4315
                     LDY #$02
                     STY $420B
                     INX #2
                     CPX $6D84
                     BCC .loop2
                     SEP #$20

                     PLB
                     JML $00A38F


              DMAGFXtable:                     ;Don't mess with this unless you know what you're doing
                     db $00,$46,$8C,$46

              DMAGFXbigtables:
                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                     ;Basically, each tile takes up $20 bytes. Since we are uploading 2 tiles, it's $40 bytes
                     ;This table shows which frames use which tiles (The tiles are in ExtendGFX.bin)
                     ;
                     ;except in this table, each 8x8 tile is $08 bytes, so 2 of them are $10 bytes

                     ;Small Mario

                     db $00,$00,$00,$00,$00,$00,$00,$00       ;1
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;2
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;3
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;4
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;5
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;6
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;7
                     db $00,$00,$00,$00,$00                     ;8
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;9
                     db $00                                   ;10

                     ;Big/Fire Mario:

                     db $00,$00,$00,$00,$10,$10,$10,$00       ;1
                     db $00,$00,$00,$00,$20,$00,$00,$00       ;2
                     db $00,$30,$30,$30,$00,$00,$40,$40       ;3
                     db $50,$50,$60,$60,$00,$00,$80,$00       ;4
                     db $00,$00,000,$70,$00,$00,$00,$00       ;5
                     db $90,$00,$00,$00,$00,$00,$00,$00       ;6
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;7
                     db $00,$00,$00,$00,$00                     ;8
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;9
                     db $00                                   ;10

                     ;Cape Mario:

                     db $00,$00,$00,$00,$10,$10,$10,$00       ;1
                     db $00,$00,$00,$00,$20,$00,$00,$00       ;2
                     db $00,$30,$30,$30,$00,$00,$40,$40       ;3
                     db $50,$50,$60,$60,$00,$00,$80,$00       ;4
                     db $00,$00,000,$70,$00,$00,$00,$00       ;5
                     db $90,$00,$00,$00,$00,$00,$00,$00       ;6
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;7
                     db $00,$00,$00,$00,$00                     ;8
                     db $00,$00,$00,$00,$00,$00,$00,$00       ;9
                     db $00                                   ;10

              BEGINXTND:
                     ; incbin extendgfx.bin
										 db 00, 00

              else

              org $00A300
                     autoclean JML BEGINDMA

              org $00F691
                     ADC #BEGINXTND

              org $00E1D4+$2B
                     db $00,$8C,$14,$14,$2E
                     db $00,$CA,$16,$16,$2E
                     db $00,$8E,$18,$18,$2E
                     db $00,$EB,$1A,$1A,$2E
                     db $04,$ED,$1C,$1C

              org $00DF1A
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00

                     db $00,$00,$00,$00,$00,$00,$28,$00
                     db $00

                     db $00,$00,$00,$00,$82,$82,$82,$00
                     db $00,$00,$00,$00,$84,$00,$00,$00
                     db $00,$86,$86,$86,$00,$00,$88,$88
                     db $8A,$8A,$8C,$8C,$00,$00,$90,$00
                     db $00,$00,$00,$8E,$00,$00,$00,$00
                     db $92,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00

                     db $00,$00,$00,$00,$82,$82,$82,$00
                     db $00,$00,$00,$00,$84,$00,$00,$00
                     db $00,$86,$86,$86,$00,$00,$88,$88
                     db $8A,$8A,$8C,$8C,$00,$00,$90,$00
                     db $00,$00,$00,$8E,$00,$00,$00,$00
                     db $92,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00,$00,$00,$00
                     db $00,$00,$00,$00,$00

              org $00E3B1
              JSR chartilehijack

              org $00E40D
              JSR capetilehijack

              org $00DFDA
                     db $00,$02,$80,$80          ;[00-03]
                     db $00,$02,!Tile,!Tile+$1   ;[04-07]
              chartilehijack:
                     LDA $DF1A,y
                     BPL +
                     AND #$7F
                     STA $0D
                     LDA #$04
              +      RTS
              capetilehijack:
                     LDA $0D
                     CPX #$2B
                     BCC +
                     CPX #$40
                     BCS +
                     LDA $E1D7,x
              +      RTS

                     db $FF,$FF           ;[22-23]
                     db $FF,$FF,$FF,$FF   ;[24-27]
                     db $00,$02,$02,$80   ;[28-2B]    Balloon Mario
                     db $04               ;[2C]       Cape
                     db !Tile,!Tile+$1    ;[2D-2E]    Random Gliding tiles
                     db $FF,$FF,$FF       ;[2F-31]

              freedata

              BEGINDMA:
                     REP #$20
                     LDX #$02
                     LDY $0D84
                     BNE +
                     JMP .skipall
              +

              ;;
              ;Mario's Palette
              ;;

                     LDY #$86
                     STY $2121
                     LDA #$2200
                     STA $4310
                     TAY
                     LDA $0D82
                     STA $4312
                     STY $4314
                     LDA #$0014
                     STA $4315
                     STX $420B

                     LDY #$80
                     STY $2115
                     LDA #$1801
                     STA $4310
                     LDY #$7E
                     STY $4314

              ;;
              ;Misc top tiles (mario, cape, yoshi, podoboo)
              ;;

                     LDA #$6000
                     STA $2116
                     TAY
                     -
                     LDA $0D85,y
                     STA $4312
                     LDA #$0040
                     STA $4315
                     STX $420B
                     INY #2
                     CPY $0D84
                     BCC -

              ;;
              ;Misc bottom tiles (mario, cape, yoshi, podoboo)
              ;;

                     LDA #$6100
                     STA $2116
                     TAY
                     -
                     LDA $0D8F,y
                     STA $4312
                     LDA #$0040
                     STA $4315
                     STX $420B
                     INY #2
                     CPY $0D84
                     BCC -

              ;;
              ;Mario's 8x8 tiles
              ;;

                     LDY $0D9B
                     CPY #$02
                     BEQ .skipall

                     LDA #!Tile<<4|$6000
                     STA $2116
                     LDA $0D99
                     STA $4312
                     LDY #BEGINXTND>>16
                     STY $4314
                     LDA #$0040
                     STA $4315
                     STX $420B

              .skipall
                     SEP #$20
                     JML $00A38F


              BEGINXTND:
                     ;incbin extendgfx.bin
										 dd 0, 0

              endif

       print " ",bytes," bytes used by the DMAer patch."

endif

