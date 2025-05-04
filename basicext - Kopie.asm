
ROM_PRINT       equ $10     ; Prints the character in a
ROM_GETCHAR     equ $18     ; Fetches a character currently adresses by CHAADD into the a Register
                            ; A return is made only if the char is printable and not space (CHRR32)
                            ; otherwise CHADD is incremented and the fetch repeated
ROM_NXTCHAR     equ $20     ; CHAD is incremented before jumping to GETCHAR
ROM_INCCHAD     equ $74     ; CHADD is incremented an dthe contents of the new address 
                            ; returned to the A-Register where printable or not
ROM_EXPTNUM     equ $1C82   ; Evaluates in part the numerical expression currently pointed to by 
                            ; CHADD. During Syntax checking the routine confirms the presence of
                            ; a valuid numerical expression; in run tim in places the evaluated
                            ; expressed on the top of the calculator stack; to be fetched
                            ; by STKTOA or STKTOBC                           
ROM_EXPT2NM     equ $1C7A   ; As EXPTNUM but searches for two numbers separated by comma
ROM_NEXT2NM     equ $1C79   ; CHADD is incremented by for jumping to EXPTT2NM
ROM_EXPTSTG     equ $1C8C   ; Like EXPTNUM but for string 
ROM_STKTOA      equ $1E94   ; Fetches the last number from calculator stack and places it in A
ROM_STKTOBC     equ $1E99   ; Fetches the last number from calculator stack and places it in BC
ROM_STKFTCH     equ $2BF1   ; Fetches the last String, BC holds length, DE points to String
ROM_LINADDR     equ $196E   ; On Entry HL holds BASIC line number, On Exit address in HL, previous in DE
ROM_RECLAIM     equ $1eE5   ; On Entry the first to reclaimed in DE, the first to be left alone in HL
ROM_RECLAIM2    equ $1eE8   ; On Entry the first to reclaimed in HL, BC the length
ROM_BORDER      equ $2294   ; Fetches number from calculator stack and sets the border
ROM_BORDER2     equ $229B   ; A register -> border
ROM_SCROLL      equ $E00   ; B=number of top line 
ROM_CLRPRB      equ $eDF   ; Clears the print buffer
ROM_CPLINES     equ $EDF   ; If line number in HL < BC then carry is set
ROM_SETMIN      equ $16b0  ; Effectivly clears the editing and subsequent araes
ROM_BRKKEY      equ $1F54  ; Carry is set if break is pressed
ROM_CLSLWR      equ $ED6E  ; Clears lower part of the screen
ROM_OUTCODE     equ $15EF  ; Prints the digit in A (0..9)
ROM_POMSGE      equ $C0A   ;  On Netry DE holds base add of table, A number of th emessage to be printed at lower part
ROM_OUTNUM      equ $1A1B  ; Prints numer in BC if lower than 10000d
ROM_CLEARSP     equ $1097  ; HL signals whether the editing area of the workspace is to be cleared
ROM_STMTRET     equ $1B76  ; The return point after correctly executed statment
ROM_CHECKEND    equ $1BEE  ; Error is reported if CHADD is not addressing the end of a BASIC command or line during syntax check
ROM_MAK1SPC     equ $1652  ; A space is opened-up immediatly before the location in HL
ROM_DRAWLIN     equ $24ba  ; B/C holds x/y displacement, D/E holds sign. Line is drawn from last PLOT positition
ROM_COPYBUF     equ $ECD

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

; Activate new BASIC Commands
enable:       call calcLines
              ld hl , (ERRSP)             ; Make Error Return Address
              call printHex4Hl
                 ld a,32
                 call printA
                ld  e,(hl)
                inc hl
                ld  d,(hl)
                ld  hl, OLD_ERRVEC          ; Save old Error Vector
                ld  (hl),e
                inc hl
                ld (hl),d
                ex  hl,de
                call printHex4Hl
                
                ld hl , (ERRSP)
                ld de, BASIC_PARSER         ; that of the Main Parser.
                ld (hl) ,e
                inc hl
                ld (hl ) ,d
          
              

                ret

OLD_ERRVEC:     dw  0                

; Pass the control to the previous error function
BASIC_ERROR:    
              call printf
              db " BASIC_ERROR called",0
               ld hl ,BASIC_PARSER
                push hl                     ; Replace Error Return Address.
                ld hl , (CHADD)
                ld (XPTR) ,hl               ; Position Error Marker.
                ld  hl,(OLD_ERRVEC)
                ;jp $12CF
                jp  4791 

                ld hl , (ERRSP)
                ld de, BASIC_PARSER         ; that of the Main Parser.
                ld (hl) ,e
                inc hl
                ld (hl ) ,d

                ld  hl,(OLD_ERRVEC)
                jp  (hl)

SymbolAtErr:    db 0
BASIC_PARSER:   ld a,0 
                call clearTextLine
                ld a,1
                call clearTextLine
                ld a,(ERRNR)
                call printf
                db '%@0000Parser Called ERRNO=',0
                
                ld  a, (ERRNR)
                call printHex2
                cp  11                       ; Is Error Report "Nonsense in BASIC"?
                ;jp nz , BASIC_ERROR               ; Jump if not.
                jp nz, OLDVECEXIT

                push hl
                call printf
                db 'sym=$',0
                pop hl

                ld hl , (CHADD)
                dec hl                      ; Fetch the character in
                ld  a, (hl)                 ; the BASIC line which caused the error.
                call printHex2
                inc hl
                ld  a,(hl)
                call printHex2
                cp  42                      ; compare to '*'
                jp nz , BASIC_ERROR         ; Jump if it is not an asterisk.

// We found "*" in an error statement. Lets see if we extended any BASIC commands like CIRCLE *x,y,r
             
                ld      hl , (CHADD)
                dec     hl
                ld      a, (hl)                 ; the BASIC line which caused the error.
                call    printHex2
                cp      a,zxb_circle
                jp      z, BASIC_NEWCIRCLE
                push hl
                call printf
                db 'nothing detected',0
                pop hl


// Not an e

                jp BASIC_ERROR              ; nothing is implemented here                
BASIC_NEWCIRCLE:
                push hl
                call printf
                db 'circle * detected ',0
                pop hl
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
                push hl
                call printf
                db 'running circle ',0
                pop hl



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

                ld hl , (ERRSP)
                ld de, BASIC_PARSER         ; that of the Main Parser.
                push de
                ld (hl) ,e
                inc hl
                ld (hl ) ,d
                ld  hl, (OLD_ERRVEC)
                
                ld (iy+0) ,255              ; Reset ERRNR.
                jp ROM_STMTRET
             
               

          
                ;jp BASIC_EXIT                    ; return to ROM



                       ; return to ROM
             



BASIC_SYNEND:   bit 7, (iy+1)
                jr z,BASIC_SYNEXIT         ; Exit without error if in Syntax-time.
                ret                         ; continue
BASIC_SYNEXIT:  call printf
                db " exit because syntax checking",0
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
                ld hl , (CHADD)
                ld (XPTR) ,hl               ; Position Error Marker.
                jp  $12B7                    ; Back into ROM.

OLDVECEXIT:    ld (iy+0),255
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
ERROR:          bit 7, (iy+1)
                jr nz ,RUNER                ; Jump forward if in run-time.
SYNER:          ld hl ,BASIC_PARSER
                push hl                     ; Replace Error Return Address.
                ld hl , (CHADD)
                ld (XPTR) ,hl               ; Position Error Marker.
                jp  4791                    ; Back into ROM.
RUNER:          res 5, (iy+1)               ; Signal ready for next
                bit 1, (iy+48)              ; keypress,and empty printer
                call nz,ROM_COPYBUF             ; buffer if used.
                ld a, (ERRNR)
                inc a
                push af                     ; Store actual Report code.
                ld hl,0
                ld (DEFAD) ,hl              ; Reset DEFADD
                ld (iy+38) ,h               ; XPTR-hi and
                ld (iy+55) ,h               ; FLAGX.
                inc hl                      ; Make stream O point

                ld (STRM6) ,hl              ; to channel K.
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
                ld bc , (PPC)

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