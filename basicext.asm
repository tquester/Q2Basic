

LABTK           equ 23560
REPDEL          equ 23561
REPPER          equ 23562
DEFAD           EQU 23563
STRM6           equ 23574
PIP             equ 23609
ERRNR           equ 23610
FLAGS           equ 23611
ERRSP           equ 23613
NEWPPC          equ 23618
NSPPC           equ 23620
PPC             equ 23621
SUBPPC          equ 23623
BORDCR          equ 23624
VARS            equ 23627
PROG            equ 23635
NXTLIN          equ 23637
ELIN            equ 23641
CHADD           equ 23645
XPTR            equ 23647
FLAGS2          equ 23658
UDGST           equ 23675
COORDS          equ 23677
SPOSN           equ 23688
ATTRP           equ 23693
ATTRT           equ 23693
; Other symbols
DISPST          equ 16384
ATTRST          equ 22528


BASICEXT_START:

; Activate new BASIC Commands
enable:       ld hl , (ZX_ERR_SP)             ; Make Error Return Address
              
              if DEBUG=1
              call printHex4Hl
              ld a,32
              call printA
              endif

              ld  e,(hl)
              inc hl
              ld  d,(hl)
              ld  hl, OLD_ERRVEC          ; Save old Error Vector
              ld  a,(hl)
              cp  $FF
              jr  nz, enable2
              ld  (hl),e
              inc hl
              ld (hl),d

              if DEBUG=1
                ex  hl,de
                call printHex4Hl
              endif

enable2:      ld hl , (ZX_ERR_SP)
              ld de, BASIC_PARSER         ; that of the Main Parser.
              ld (hl) ,e
              inc hl
              ld (hl ) ,d
              ret

OLD_ERRVEC:     dw  $FFFF                

; Pass the control to the previous error function
BASIC_ERROR:    if DEBUG=1 
                call printf
                db " BASIC_ERROR called",0
                endif
               
                ld hl ,BASIC_PARSER
                push hl                     ; Replace Error Return Address.
                ld hl , (ZX_CH_ADD)
                ld (ZX_X_PTR) ,hl               ; Position Error Marker.
                if DEBUG=1
                call printf
                db " Jump back to ",0
                ld  hl,(OLD_ERRVEC)
                call printHex4Hl
                endif

; the book simply performs a jp $12b7 (dez 4791) here. However this disables other extensions like Interface 1
; The following code is copied from the complete ZX ROM disassembly

                ld a, (ZX_ERR_NR)
                bit 7,a                   ; Jump forward if the syntax is
                jp NZ,$12CF               ; ,MAIN-3 correct.
                ld  a,(ZX_FLAGS2)
                bit 4,a                   ; Jump forward if other than
                jr  z,BASIC_JUMPHL        ; MAIN-4 channel 'K' is being used.
                ld hl,(ZX_E_LINE)         ; Point to the start of the line with the error.
                call $11A7                ; REMOVE-FP Remove the floating-point forms from this line.
                ld a,$ff
                ld (ZX_ERR_NR),a          ; Reset ERR-NR and jump back
                if DEBUG=1
                call printf
                db "Jumping to MAIN-2",0
                endif
                jp $12AC                  ; MAIN-2                
BASIC_JUMPHL:   if DEBUG=1
                push hl
                call printf
                db "jumping to ",0
                pop hl  
                call printHex4Hl
                endif

                jp (hl)
SymbolAtErr:    db 0
BASIC_PARSER:   
; -----------------------------------------------------------------                
; --- Debug. Lösche die oberen 3 Zeilen und gebe einen Status aus
                if DEBUG=1
                ld a,0 
                call clearTextLine
                ld a,1
                call clearTextLine
                ld a,(ZX_ERR_NR)
                call printf
                db '%@0000Parser Called ERRNO=',0
                ld  a, (ZX_ERR_NR)
                call printHex2
                endif
; -----------------------------------------------------------------                

; Verlasse die Routine, wenn es nicht Nonsense in BASIC ist
                ld  a, (ZX_ERR_NR)
                cp  11                       ; Is Error Report "Nonsense in BASIC"?
                jp nz, OLDVECEXIT

; Debug ------------------------------------------------------------
                if DEBUG=1
                push hl
                call printf
                db 'sym=$',0
                pop hl

                ld hl , (ZX_CH_ADD)
                dec hl                      ; Fetch the character in
                ld  a, (hl)                 ; the BASIC line which caused the error.
                call printHex2
                inc hl
                ld  a,(hl)
                call printHex2
                endif
; ------------------------------------------------------------------

; Check whether it is an extended command '*' or a label '!

                ld hl , (ZX_CH_ADD)
                dec  hl
                ld   a,(hl)
                cp  33
; -------------------- !LABEL                
                jp  z, BASIC_LABEL              
                cp  42
; -------------------- * Command starting with star                
                jr  z, BASIC_STAR

                inc  hl
                ld   a,(hl)
                if DEBUG
                call              printA
                endif
                cp  42                      ; compare to '*'
                jp nz , BASIC_ERROR         ; Jump if it is not an asterisk.

; We found "*" in an error statement. Lets see if we extended any BASIC commands like CIRCLE *x,y,r
             
                ld      hl , (ZX_CH_ADD)
                dec     hl
                ld      a, (hl)                 ; the BASIC line which caused the error.
                if      DEBUG=1
                ld      a,':'
                call    printA
                ld      a,(hl)
                call    printHex2
                endif
; ------------------- CIRCLE *                  Fast circle
                cp      a,zxb_circle
                jp      z, BASIC_NEWCIRCLE
; ------------------- DRAW *                    Fill region
                cp      a,zxb_draw
                jp      z, BASIC_FILL
; ------------------- SCREEN$ *                 Scroll and Region Copy                
                cp      a,zxb_screen
                jp      z, BASIC_SCREEN
; ------------------- PLOT *                    Draw a Sprite
                cp      a,zxb_plot             
                jp      z, BASIC_PLOT

                if DEBUG=1
                push hl
                call printf
                db 'nothing detected',0
                pop hl
                endif
                jp BASIC_ERROR              ; nothing is implemented here       
; -----------------------------------------------------------
; - Parse command beginning with *                         --
;   *c        - Dectivate extension                        --
;   *DATA     - Define UDG oder sprite                     --
;   *STOP     - Wait for Vertical interrupt                --
; -----------------------------------------------------------
;   
BASIC_STAR:     
                inc       hl
                ld        a,(hl)
                DEBUG_PRINT "start dectected next char="
                if DEBUG=1
                ld        a,(hl)
                call      printHex2
                and       a,$DF
                call      printA
                endif
                ld a,(hl)
                cp        a, zxb_data
                jp        z, BASIC_DATA                   ; *DATA
                cp        zxb_stop                        ; *STOP
                jr        z, BASIC_STOP
                cp        zxb_move
                jr        z, BASIC_MOVE                   ; *MOVE
                and a,$DF ; Make upper case
                cp 'C'                                  ; *C
                jp z, BASIC_CLEAR
                jp BASIC_ERROR

; =================================================================
; *STOP             
; ================================================================= 
BASIC_STOP:
    DEBUG_PRINT "stop called"

    rst $20
    call BASIC_SYNEND
    ;ei
    halt
    jp   BASIC_ENDCMD

; =================================================================
; *MOVE sprite, x,y, dx,dy
;         moves a sprite. The sprite has been drawn at x,y with xor
;         move will delete the sprite by re-drawing and draw a new
;         one at x+dx, y+dy
; =================================================================
BASIC_MOVE:   DEBUG_PRINT "/nmove called"
              rst $20
              DEBUG_PRINT "/nexpecting sprite number"
              call ROM_EXPTNUM          ; sprite
              cp    44
              jp    nz,ERROR
              rst   $20
              DEBUG_PRINT "/nexpecting x"
              call ROM_EXPTNUM          ; x coordinate
              cp    44
              jp    nz,ERROR
              rst   $20
              DEBUG_PRINT "/nexpecting y"
              call  ROM_EXPTNUM          ; y coordiante
              cp    44
              jp    nz,ERROR
              rst   $20
              DEBUG_PRINT "/nexpecting dx"
              call  ROM_EXPTNUM          ; new x coordiante
              cp    44
              jp    nz,ERROR
              rst   $20
              DEBUG_PRINT "/nexpecting dy"
              call ROM_EXPTNUM          ; new y coordiante
              call BASIC_SYNEND
              if      DEBUG=1
              call     GetKey
              endif
              STACKTOA "new y="
              push af
              STACKTOA "new x="
              push af
              STACKTOA "y="
              push af
              STACKTOA "x="
              push af
              STACKTOA "sprite="
              ld      e,a
              if      DEBUG=1
              push     hl
              call     GetKey
              pop      hl
              endif
              pop     af
              ld      b,a
              pop     af
              ld      c,a
              pop     af
              ld      h,a
              pop     af 
              ld      l,a
              PRINTREG "b=",b
              PRINTREG "c=",c
              PRINTREG "h=",h
              PRINTREG "l=",l
              PRINTREG "d=",e
              if       DEBUG=1
              call     GetKey
              endif

              call    drawSpriteMoveXor
              jp      BASIC_ENDCMD
; =================================================================

; =================================================================
; PLOT * sprite, x,y  
;         draw a sprite with OR   
;         sets Collision flag if collision
; PLOT * OR sprite, x,y
;         draw a sprite with OR
;         sets Collision flag if collision
; =================================================================





BASIC_PLOT_MODE: db 0
BASIC_PLOT: DEBUG_PRINT "plot called"
            rst $20
;            cp   zxb_orx 
;            jr   z, BASIC_PLOT_OR
;            cp   zxb_at 
;            jr   z, BASIC_PLOT_AT
;            ld   a,0
;            ld   (BASIC_PLOT_MODE),a
            jr   BASIC_PLOT_1
BASIC_PLOT_AT
            ld   a,2
            ld   (BASIC_PLOT_MODE),a
            rst $20
            jr  BASIC_PLOT_1
BASIC_PLOT_OR:
            ld   a,1
            ld   (BASIC_PLOT_MODE),a
            rst $20
BASIC_PLOT_1:            
           
            DEBUG_PRINT "/nexpect num 1 "
            call ROM_EXPTNUM          ; Sprite number
            cp   44
            jp   nz,ERROR
             rst $20
           DEBUG_PRINT "/nexpect num 2 "
            call ROM_EXPTNUM          ; X Coordinate
            cp   44
            jp   nz,ERROR
            rst $20
            DEBUG_PRINT "/nexpect num 3 "
            call ROM_EXPTNUM          ; Y Coordinate            

            call BASIC_SYNEND
            call ROM_STKTOA                   ; y
            push af
            call ROM_STKTOA                   ; x
            push af
            call ROM_STKTOA                   ; nr
            ld   e,a
            pop  af
            ld   b,a
            pop  af
            ld   c,a
            ld   a,(BASIC_PLOT_MODE)
            ld   d,a
            PRINTREG "b=x=",b
            PRINTREG "c=y=",c
            PRINTREG "d=modus=",d
            PRINTREG "e=nummer",e
            call drawsprite
            jp   BASIC_ENDCMD

; =================================================================
; BASIC_LABEL: Simply eat all characters until the end of the line or ':'
; =================================================================
BASIC_LABEL:
BASIC_LABEL_LOOP:
            rst $20
             cp ','                     ; Check for a comma.	
             jr z, BASIC_LABELEXIT      ; Exit if found.
             cp 13                      ; Check for end of line.
             jr z, BASIC_LABELEXIT      ; Exit if found.
             jr BASIC_LABEL_LOOP              ; Otherwise continue.
BASIC_LABELEXIT:
          call BASIC_SYNEND               ; End of parameter parsing
         jp   BASIC_ENDCMD

BASIC_CLEAR:  if DEBUG=1
                call printf
                db "clear called next char=",0
                endif
                rst  $20
                if DEBUG=1
                call printA
                endif
                call BASIC_SYNEND               ; End of parameter parsing


                ld hl , (ZX_ERR_SP)
                ld de, $1303                ; that of the Main Parser.
                ld (hl) ,e
                inc hl
                ld (hl ) ,d
                push de
                
                ld (iy+0) ,255              ; Reset ERRNR.
                jp ROM_STMTRET

BASIC_SCREEN: if DEBUG=1
              call printf
              db "screen$ called",0

              endif
              rst  $20
              call ROM_EXPTNUM
              cp   zxb_to
              jp   z,BASIC_SCREEN_ADR_TO_SCREEN
              cp   44      ; ,
              jp   nz,BASIC_SCREN_SCROLL

; SCREEN$ x,y,w,h TO adr x is parsed
              rst  $20
              call ROM_EXPTNUM          ; y 
              cp   44      ; ,
              jp   nz,ERROR
              rst  $20
              call ROM_EXPTNUM          ; w
              cp   44      ; ,
              jp   nz,ERROR
              rst  $20
              call ROM_EXPTNUM          ; h
              cp   zxb_to      ; ,
              jp   nz,ERROR
              rst  $20
              call ROM_EXPTNUM          ; h
              call BASIC_SYNEND
              call ROM_STKTOBC           ; adr 
              push bc
              call ROM_STKTOA            ; h
              push af
              call ROM_STKTOA            ; w
              push af
              call ROM_STKTOA            ; y  
              push af
              ld   c,a
              pop  af
              ld   b,a
              pop  af 
              ld  e,a
              pop  af 
              ld  d,a
              pop  hl
; now b/c holds w/h
; d/e holds x/y
; hl holds adr
              call BASIC_ENDCMD
; $SCREEN$ adr TO x,y   adr is parsed, TO is parsed              
BASIC_SCREEN_ADR_TO_SCREEN:
              if DEBUG
              call printf
              db "screen$ adr to screen called",0
              endif
              rst  $20
              call ROM_EXPTNUM          ; w
              cp   44      ; ,
              jp   nz,ERROR
              if DEBUG=1
              call printf
              db "expect num",0
              endif
              rst  $20
              call ROM_EXPTNUM          ; h
              call BASIC_SYNEND
              call ROM_STKTOA            ; h
              push af
              call ROM_STKTOA            ; w
              push af
              call ROM_STKTOBC
              push bc
              pop  hl
              pop  af
              ld   e,a
              pop  af
              ld   d,a
              call BASIC_ENDCMD
              jp   ERROR

BASIC_SCREN_SCROLL:
          call BASIC_SYNEND
          call ROM_STKTOA
          cp   1
          jr   z, BASIC_SCROLL_RIGHT
          cp   2
          jr   z, BASIC_SCROLL_LEFT
          cp   3
          jr   z, BASIC_SCROLL_UP
          cp   4
          jr   z, BASIC_SCROLL_DOWN
          jp  BASIC_ERROR
BASIC_SCROLL_RIGHT:
          call ScrollRight
          jp   BASIC_ENDCMD
BASIC_SCROLL_LEFT:
          call ScrollLeft
          jp   BASIC_ENDCMD
BASIC_SCROLL_UP:
          call ScrollUp
          jp   BASIC_ENDCMD

BASIC_SCROLL_DOWN:
          call ScrollDown
          jp   BASIC_ENDCMD

; =================================================================
; BASIC_DATA:
; DATA *CHR$ USR "a",1,2,3,4,5,6,7,8 
; defines an UDG character
; DATA *sprite, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32
; defines a sprite


; =================================================================
BASIC_DATA: if DEBUG=1
            call printf
            db "/ndata called code=",0
            endif
            rst $20 
            if DEBUG=1
            call printHex2
            endif
            cp   zxb_chr
            jr   z, BASIC_DATA_CHR
            ; now check for 9 numbers, the first is the address of the udg character, followed by 8 bytes
            call BASIC_EXPECT_16BIT
            ld   b, 32
            call BASIC_EXPECT_NUMBERS
            call BASIC_SYNEND
            ld   hl, (BASIC_ADR_BUF)
            ld   a, l
            ld   hl, BASIC_CHAR_BUF
            PRINTREG "a=",a
            PRINTREG16 "hl=",hl
            call prepare_sprite
            jp   BASIC_ENDCMD

BASIC_DATA_CHR:
            rst  $20
;            if DEBUG=1
;            call printf
;            db   "/n*DATA CHR$ -> Parsing adr ",0
;            endif
            call BASIC_EXPECT_16BIT
            ld   b, 8
            call BASIC_EXPECT_NUMBERS
            call BASIC_SYNEND
;            if DEBUG=1
;            call printf
;            db "/n/npoking..",0
;            ld   hl, (BASIC_ADR_BUF )
;            call printDezHL
;            endif

            ld   de,(BASIC_ADR_BUF)
            ld   hl,BASIC_CHAR_BUF
            ld   b,8
BASCI_DATA_CHR_POKE_LOOP
            ld  a,(hl)
;            if DEBUG=1
;            call printHex2
;            endif
            ld (de),a
            inc hl
            inc de
            ld a,32
;            if DEBUG=1
;            call printA
;            endif
            djnz BASCI_DATA_CHR_POKE_LOOP



            ;ldir
            jp   BASIC_ENDCMD

BASIC_EXPECT_16BIT:
           call ROM_EXPTNUM
           cp   44
           jp   nz, ERROR 
           rst $20
           bit 7, (iy+1)
           ret z
           call ROM_STKTOBC
           ld   hl,BASIC_ADR_BUF
           ld   (hl),bc
           push bc
           pop  hl
           ret
BASIC_EXPECT_NUMBERS:
           ld   hl, BASIC_CHAR_BUF
BASIC_EXPECT_NUMBERS_LOOP:           
           push hl      
           push bc
           call ROM_EXPTNUM
           cp  $2c
           jr  nz, BASIC_EXPECT_NUMBERS_LOOP3
           rst $20
BASIC_EXPECT_NUMBERS_LOOP3:                      
           bit 7, (iy+1)
           jr   z, BASIC_EXPECT_NUMBERS_LOOP2
           call ROM_STKTOA
           pop  bc
           pop  hl
           ld (hl),a
           inc hl
           push hl
           push bc
           
BASIC_EXPECT_NUMBERS_LOOP2:           
           pop  bc
           pop  hl
           djnz BASIC_EXPECT_NUMBERS_LOOP
           ret

BASIC_GET_DATA_BYTES:
           push hl
           push bc
           call ROM_STKTOA
           pop bc
           pop hl
           ld (hl),a
           dec hl
           djnz BASIC_GET_DATA_BYTES
            ret



BASIC_CHAR_BUF: defs 33
BASIC_ADR_BUF: dw 0


; =================================================================
; DRAW *x,y  fills the aerea with the current color
; =================================================================
BASIC_FILL:                         
              if DEBUG=1 
              call printf
              db "draw* called",0
              endif
                rst  $20
                call ROM_EXPTNUM
                cp   $2C
                jp   nz,ERROR
                rst  $20
                call ROM_EXPTNUM
                call BASIC_SYNEND               ; End of parameter parsing
                call ROM_STKTOA                 ; y
                push af
                call ROM_STKTOA                 ; x
                ld   d,a
                pop  af
                ld   e,a
                call sfill
                jr   BASIC_ENDCMD

BASIC_NEWCIRCLE:
                if DEBUG=1
                push hl
                call printf
                db 'circle * detected ',0
                pop hl
                endif
                rst  $20
                call ROM_EXPTNUM
                cp   $2C
                jp   nz,ERROR
                rst  $20
                call ROM_EXPTNUM
                cp   $2C
                jp   nz,ERROR
                rst  $20
                call ROM_EXPTNUM
               ; rst  $20
               ; call printHex2
                call BASIC_SYNEND               ; End of parameter parsing
                if DEBUG=1
                push hl
                call printf
                db 'running circle ',0
                pop hl
                endif

                call ROM_STKTOA
                push af
                call ROM_STKTOA
                push af
                call ROM_STKTOA
                ld   b,a
                pop  af
                ld   c,a
                pop  af
                ld  l,a
                call Circle

BASIC_ENDCMD:
                ld hl , (ZX_ERR_SP)
                ld de, BASIC_PARSER         ; that of the Main Parser.
                push de
                ld (hl) ,e
                inc hl
                ld (hl ) ,d
                ld  hl, (OLD_ERRVEC)
                
                ld (iy+0) ,255              ; Reset ERRNR.
                jp ROM_STMTRET

BASIC_SYNEND:   bit 7, (iy+1)
                jr z,BASIC_SYNEXIT         ; Exit without error if in Syntax-time.
                ret                         ; continue
BASIC_SYNEXIT:  if DEBUG=1
                call printf
                db " exit because syntax checking",0
                endif
                pop hl                      ; Remove return address.    
                ld (iy+0) ,255              ; Reset ERRNR.
                jp ERROR                    ; return to ROM

SYNEND:         
                pop hl                      ; Remove return address.
                bit 7, (iy+1)
                jr z,EXIT                   ; Leave if in Syntax-time,
                jp (hl )                    ; otherwise return to calling routine.
; return to ROM
EXIT:          // ld (iy+0) ,255 
                ld hl ,BASIC_PARSER
                push hl                     ; Replace Error Return Address.
                ld hl , (ZX_CH_ADD)
                ld (ZX_X_PTR) ,hl               ; Position Error Marker.
                jp  $12B7                    ; Back into ROM.

OLDVECEXIT:     if DEBUG=1
              call printf
                db "OLDVECEXIT",0
                endif
                ld (iy+0),255
                ld hl ,BASIC_PARSER
                push hl                     ; Replace Error Return Address.
                ld hl, (OLD_ERRVEC)
                jp (hl)

BASIC_EXIT:     ld (iy+0),255
                ld hl ,BASIC_PARSER
                push hl                     ; Replace Error Return Address.
                //jp $1B28
                jp ROM_STMTRET


       ld (iy+0) ,255              ; Reset ERRNR.
                ld hl,BASIC_PARSER          ; Replace new Error Return Address.
                push hl
                jp ERROR
                jp ROM_STMTRET                  ; Return to ROM.

; Error Handling
ERROR:        DEBUG_PRINT "\nexit with error"  
              bit 7, (iy+1)
       ;       jr nz ,RUNER                ; Jump forward if in run-time.
SYNER:          ld hl ,BASIC_PARSER
                push hl                     ; Replace Error Return Address.
                ld hl , (ZX_CH_ADD)
                ld (ZX_X_PTR) ,hl               ; Position Error Marker.
                jp  4791                    ; Back into ROM.
RUNER:          res 5, (iy+1)               ; Signal ready for next
                bit 1, (iy+48)              ; keypress,and empty printer
                call nz,ROM_COPYBUF             ; buffer if used.
                ld a, (ZX_ERR_NR)
                inc a
                push af                     ; Store actual Report code.
                ld hl,0
                ld (ZX_DEFADD) ,hl              ; Reset DEFADD
                ld (iy+38) ,h               ; XPTR-hi and
                ld (iy+55) ,h               ; FLAGX.
                inc hl                      ; Make stream O point

                ld (ZX_STRMS) ,hl              ; to channel K.
                call ROM_SETMIN                 ; Clear calculator stack etc.
                call ROM_CLSLWR                 ; Clear lower screen area,but signal

                set 5,(iy+2)                ; it will need to be cleared again.

                pop af                      ; Fetch Report Code.
                ld  b ,a                    ; Save it temporarily.                
                cp 10                       ; Adjust Report codes
                jr c, Numcod                ; greater than 10 to give
                add a,7                     ; relevant Hex character.
Numcod:         call ROM_OUTCODE                ; Print Report code.
                ld   a,32                   ; (space)
                rst  16                     ; PRINT
                ld a,b                      ; Fetch Report Code again,and use it to
                ld de, 5009                 ; address the relevant message.
                call ROM_POMSGE             ; Print the message.
                xor 43
                ld de ,5430
                call ROM_POMSGE             ; Print a comma and a space.
                ld bc, (ZX_PPC_Line)

                call ROM_OUTNUM             ; Print current line no.
                ld a,58                     ; (Colon)
                rst  16                     ; PRINT
                ld   b,0

                ld   c, (iy+13)
                call ROM_OUTNUM                 ; Print current statement no.
                call ROM_CLEARSP
                ld a, (ERRNR)               ; Store Error No. in a,before

                ld  (iy+0) , 255            ; clearing the system variable.

                ld hl ,BASIC_PARSER

                push hl                     ; Replace Error Return Address.
                jp   4988                   ; Back into ROM.

                  DISPLAY "Basic Extension size = ", /D, $-BASICEXT_START,  " bytes"