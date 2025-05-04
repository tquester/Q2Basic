g;===========================================================================
; main.asm
;===========================================================================
DEBUG                       equ 0
; Prepared Sprites werden in 8 Shift-Positionen gespeichert. Für 16bit Sprites braucht es 
; 6 Characters also 8*6*32 = 384 Bytes
; Wenn das Sprite mit *DATA angelegt wird, wird das 16x16bit Image einfach um 1, 2, ,..7 
; pixel nach rechts verschoben. Wenn man mehr Bewegung haben möchte kann man die Daten
; auch überschreiben und dann z.B. mit Save Code abspeichern
PREPARED_SPRITE_COUNT       equ 5
; Slow sprites are not shifted, they occupy 32 Bytes (4 Characters). A slow sprite gets
; shifted right before it is displayed
SLOW_SPRITE_COUNT           equ 5



    include "macros.asm"
    ORG 0x8000

    SLDOPT COMMENT WPMEM, LOGPOINT, ASSERTION

max_x:  equ 200
max_y:  equ 100

NEX:    equ 1   ;  1=Create nex file, 0=create sna file

    IF NEX == 0
        ;DEVICE ZXSPECTRUM128
        DEVICE ZXSPECTRUM48
        ;DEVICE NOSLOT64K
    ELSE
        DEVICE ZXSPECTRUMNEXT
    ENDIF

    

START:
        RELOCATE_START 
main:  
        jp debugDemo
        jp enable
 ;       jp CompiledBasic
;        jp relocator_code
scroll: ret
;===========================================================================
; Persistent watchpoint.
; Change WPMEMx (remove the 'x' from WPMEMx) below to activate.
; If you do so the program will hit a breakpoint when it tries to
; write to the first byte of the 3rd line.
; When program breaks in the fill_memory sub routine please hover over hl
; to see that it contains 0x5804 or COLOR_SCREEN+64.
;===========================================================================

; WPMEMx 0x5840, 1, w


;===========================================================================
; Include modules
;===========================================================================
    include "ZXLibrary/zxspectrum.asm"
    include "ZXLibrary/graphics.asm"
    if DEBUG=1
    include "ZXLibrary/print.asm"
    endif
    include "ZXLibrary/math.asm"
    include "basicext.asm"
    
    

    ; Normally you would assemble the unit tests in a separate target
    ; in the makefile.
    ; As this is a very short program and for simplicity the
    ; unit tests and the main program are assembled in the same binary.
    include "unit_tests.asm"

relocator_code:
; start of relocator
    ASSERT 0 < relocate_count   ; (for zero relocation_count the relocator is not needed!)
    ; BASIC sets BC to the address of start (after "RANDOMIZE USR x" BC=x upon entry)
        di
    ; preserve current SP into IX
        ld      ix,0
        add     ix,sp
    ; set SP to the relocation table data
        ld      hl,relocator_table-relocator_code   ; offset from start to the table
        add     hl,bc                               ; absolute address of table
        ld      sp,hl
    ; process the full table of relocation data (A + A' is counter of relocation values)
        ld      a,1+high relocate_count
        ex      af,af
        ld      a,1+low relocate_count
        jr      .relocate_loop_entry
.relocate_loop_outer:
        ex      af,af
.relocate_loop:
    ; relocate single record from the relocate table
        pop     hl
        add     hl,bc       ; HL = address of machine code to modify
        ld      e,(hl)
        inc     hl
        ld      d,(hl)      ; DE = value to modify
        ex      de,hl
        add     hl,bc       ; relocate the value
        ex      de,hl
        ld      (hl),d      ; patch the machine code in memory
        dec     hl
        ld      (hl),e
.relocate_loop_entry:
    ; loop until all "relocate_count" records were processed
        dec     a
        jr      nz,.relocate_loop
        ex      af,af
        dec     a
        jr      nz,.relocate_loop_outer
    ; restore SP
        ld      sp,ix
; end of relocator
        ret



debugDemo:
    if DEBUG=1
    ; Disable interrupts
    di
    ld sp,stack_top

        ld a, ZX_YELLOW*PAPER+ZX_BLUE
        ld hl, ZX_ATTR_P
        ld (hl),a

    ; CLS
    call        clear_screen
    ld          a, ZX_WHITE*PAPER+ZX_BLACK
    ld          (ZX_ATTR_P),a
    call        fill_backg    
    ld          b,9
    ld          c,2
    call        locate
    ld          h,1
    ld          l,2
  ;  call        CompiledBasic
    push        hl
    push        hl
    call    printf
    db          "Hello World! h=%H l=%L",0
    ld		a, ZX_WHITE*PAPER+ZX_BLACK

    ld          hl, sprite1
    ld          a,0
    call        prepare_sprite
    ld          hl, sprite2
    ld          a,5
    call        prepare_sprite

    ld          b,0
    ld          c,0
    ld          a,255-16
loopSprite2:
    push        af
    ld          b,20
    ld          c,140    
    ld          d,2
    ld          e,0
    call        drawsprite
    ld          b,20
    ld          c,140    
    ld          d,2
    ld          e,1
    call        drawsprite
    ld          b,20
    ld          c,140    
    ld          d,2
    ld          e,2
    call        drawsprite



    push        bc
    push        de
    call        drawsprite
    pop         de
    pop         bc   
    ld          h,-2
    ld          l,1
loopSprite3:
    push        hl
    push        de
    push        bc
    call        drawSpriteMoveXor
    pop         bc  
    pop         de
    pop         hl
    ld          a,b
    cp          0
    call        z,reverseH
    cp          240
    call        z,reverseH
    add         h
    ld          b,a

    ld          a,c
    cp          0
    call        z,reverseL
    cp          160
    call        z,reverseL
    add         l
    ld          c,a
    jr          loopSprite3



    ld          a,192-16
loopsprite:
    push        af
    push        bc
    push        de
    call        drawsprite
    ei
    halt
    di
    pop         de
    pop         bc
    push        bc
    push        de
    call        drawsprite
    pop         de
    pop         bc
    inc         b
    ;inc         c
    pop         af
    dec         a
    jr          nz, loopsprite
    pop         af
    inc         c
    dec         a
    jr          nz ,loopSprite2

circletest:

        ld      b,$70
        ld      c,$60
        ld      l,95
        call    Circle
        ld      b,150
        ld      c,80
        ld      l,50
        call    Circle
        ld      b,80
        ld      c,20
        ld      l,15
        call    Circle
        ld      d,150
        ld      e,100
        ld      a,ZX_WHITE*PAPER+ZX_RED
        ld      (ZX_ATTR_P),a
        call    sfill
        ld      d,1
        ld      e,1
        ld      a,ZX_WHITE*PAPER+ZX_BLUE
        ld      (ZX_ATTR_P),a
        ;call    sfill
        ld      a,0
        ld      bc,768 
        ld     hl, $4000+6144
        ld     e,0
FillAttr:ld a,e
 ;        ld (hl),a
         inc hl
         inc e    
         dec bc
         ld  a,c
         cp  0
         jr  nz, FillAttr
         ld  a,b
         cp  0
         jr  nz, FillAttr

        call    ScrollLeft
        call    ScrollRight
        call    ScrollDown
        call    ScrollUp
x3:      jr x3
ret
reverseH:   push af
            ld   a,h
            neg
            ld   h,a
            pop  af
            ret

reverseL:   push af
            ld   a,l
            neg
            ld   l,a
            pop  af
            ret            
clear_screen: 
            ld hl,$4000
            ld bc,6144
clearScreenLoop1:
            ld (hl),0
            inc hl
            dec bc
            ld a,b
            or c
            jr nz,clearScreenLoop1
            ret
fill_backg: ld hl,$4000+6144
            ld bc,768
            ld d,a
fillBackgLoop:
            ld (hl),d
            inc hl
            dec bc
            ld a,b
            or c
            jr nz,fillBackgLoop
            ret       

ZXBASIC_VAR_x:  dw 0
ZXBASIC_VAR_y:  dw 0
ZXBASIC_VAR_dx:  dw 0
ZXBASIC_VAR_dy:  dw 0

CompiledBasic:
ZX_LINE_1000:
;1000  LET x=1{00 00 01 00 00 }: LET y=1{00 00 01 00 00 }: LET dx=1{00 00 01 00 00 }: LET dy=1{00 00 01 00 00 }

	LD HL, 1
	LD (ZXBASIC_VAR_x),HL
	LD HL, 159
	LD (ZXBASIC_VAR_y),HL
	LD HL, 1
	LD (ZXBASIC_VAR_dx),HL
	LD HL, 1
	LD (ZXBASIC_VAR_dy),HL

    LD HL,(IX+1)
ZX_LINE_1010:
;1010  PLOT x,y

	LD DE, (ZXBASIC_VAR_x)
	LD HL, (ZXBASIC_VAR_y)
	call RuntimePlot
ZX_LINE_1020:
;1020  LET x=x+dx

	LD DE, (ZXBASIC_VAR_x)
	LD HL, (ZXBASIC_VAR_dx)
	ADD HL, DE
	LD (ZXBASIC_VAR_x),HL
ZX_LINE_1030:
;1030  LET y=y+dy

	LD DE, (ZXBASIC_VAR_y)
	LD HL, (ZXBASIC_VAR_dy)
	ADD HL, DE
	LD (ZXBASIC_VAR_y),HL
ZX_LINE_1040:
;1040  IF x <= 1{00 00 01 00 00 } ORX x >= 250{00 00 fa 00 00 } THEN  LET dx=0{00 00 00 00 00 }-dx

	LD HL, (ZXBASIC_VAR_x)
	PUSH HL
	LD HL, 1
	POP DE
	SUB HL, DE
	LD  HL,0
	call nc, HL1
	call z, HL1
	PUSH HL
	LD HL, (ZXBASIC_VAR_x)
	PUSH HL
	LD HL, 250
	POP DE
	SUB HL, DE
	LD  HL,0
	call c, HL1
	call z, HL1
	POP DE
	LD  A,L
	OR  E
	LD  L,A
	LD  H,0
	LD  A,L
	CP 0
	JR Z,ZXB_LABEL_1
	LD HL, 0
	LD DE, (ZXBASIC_VAR_dx)
	SUB HL, DE
	LD (ZXBASIC_VAR_dx),HL
ZXB_LABEL_1:
ZX_LINE_1050:
;1050  IF y <= 1{00 00 01 00 00 } ORX y >= 160{00 00 a0 00 00 } THEN  LET dy=0{00 00 00 00 00 }-dy

	LD HL, (ZXBASIC_VAR_y)
	PUSH HL
	LD HL, 1
	POP DE
	SUB HL, DE
	LD  HL,0
	call nc, HL1
	call z, HL1
	PUSH HL
	LD HL, (ZXBASIC_VAR_y)
	PUSH HL
	LD HL, 160
	POP DE
	SUB HL, DE
	LD  HL,0
	call c, HL1
	call z, HL1
	POP DE
	LD  A,L
	OR  E
	LD  L,A
	LD  H,0
	LD  A,L
	CP  0
	JR  Z,ZXB_LABEL_2
	LD HL, 0
	LD DE, (ZXBASIC_VAR_dy)
	SUB HL, DE
	LD (ZXBASIC_VAR_dy),HL
ZXB_LABEL_2:
ZX_LINE_1060:
;1060  GOTO 1010{00 00 f2 03 00 }

	JP ZX_LINE_1010
HL1: LD HL,1
     ret

RuntimePlot:
    ld  b,e
    ld  c,l
    ld  h,0
    call plot
    ret     
sprite1:
                db 0, 0, 0, 0, 15, 248, 1, 128, 65, 128, 255, 248, 255, 252, 255, 228 
                db 95, 252, 15, 252, 7, 248, 1, 32, 1, 32, 15, 252, 0,0, 0, 0             
sprite2:        db 0,0,0,0,0,0,0,0,15,240,15,248,28,204,28,204 
                db 254,127,254,127,254,63,252,24,24,60,60,60,60,24,24 

        endif


;===========================================================================
; Stack.
;===========================================================================

    if DEBUG=1

; Stack: this area is reserved for the stack
STACK_SIZE: equ 400    ; in words


; Reserve stack space
    defw 0  ; WPMEM, 2
stack_bottom:
    defs    STACK_SIZE*2, 0
stack_top:
    ;defw 0
    defw 0  ; WPMEM, 2
    endif
main_end:
relocator_table:
    RELOCATE_TABLE


code_size   EQU     $ - main
    DISPLAY "Code size = ", /D, code_size,  " bytes"
	MakeTape "circles.tap", "zx circles", START, code_size
        RELOCATE_END
relocate_count equ (code_size-relocator_table)/2
    IF NEX == 0
        SAVESNA "z80-sample-program.sna", main
    ELSE
        SAVENEX OPEN "z80-sample-program.nex", main, main_end
        SAVENEX CORE 3, 1, 5
        SAVENEX CFG 7   ; Border color
        SAVENEX AUTO
        SAVENEX CLOSE
    ENDIF


