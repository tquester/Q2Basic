; Pixel screen address, see https://www.youtube.com/watch?v=WXWvXssGAs4&t=1031s
; 7 6 5 4 3 2 1 0 | 7 6 5 4 3 2 1 0
; 0 1 0 y y y y y | y y y x x x x x
;       7 6 2 1 0 | 5 4 3 7 6 5 4 3

OPAND equ 1
OPOR  equ 2
OPPOKE equ 4
MAXSPRITE equ 1
USE_GRAPHICS EQU 1
GRAPH_START: 
	IF USE_GRAPHICS
calcScreenAddressDE:
; d = y
; e = x
; returns hl = Screen address
	push af
	ld	a,d
	and %11000000 
	srl a
	srl a
	srl a
	ld h,a
	ld a,d
	and %00000111
	or  h
	or %01000000
	ld h,a
	ld a,d 
	and %00111000
	sla a
	sla a
	add e
	ld l,a
	pop af
	ret

NextScreenLine:
; see https://www.reddit.com/r/zxspectrum/comments/1jny2ru/challenge_z80_assembly_fastest_next_row_program/
	inc h; move down 1 line
	ld a, h
	and 7; test if still in char block
	ret nz; just return if so

	ld a, l; if not...
	add a, 32; get next char block
	ld l, a
	ret c; return if in new third

	ld a, h; if not....
	sub 8; go back a third
	ld h, a
	ret	
	ENDIF




; ------------------------------------------------
; calc line on screen
; requires calclines to be called
; calcline: A = Line, Returns HL = Pointer to line
; ------------------------------------------------ 
calcLine:
calcLineFast:
	
	PUSH	DE	
	PUSH	BC
	ld		D,0
	ld		E,a
	call 	CalcScreenAddress
	POP		BC
	POP		DE
	ret


	IF USE_GRAPHICS
;----------------------------------------------
;draws a pixel at b=x,c=y with operation h
;----------------------------------------------
plot:		push	hl
			push	bc
			push	de
			push	af
			ld		d,h
			push	de
			ld 		d,b
			ld 		e,c
			push    bc
			call    CalcScreenAddress
			pop     bc
			push	hl
			ld		hl,plotdata
			ld		a,b
			and		7
			ld		e,a
			ld		d,0
			add		hl,de
			ld		c,(hl)
			pop		hl

plot2:		pop		de
			ld		b,(hl)			; byte on screen
			ld		a,d
			cp		OPAND
			jr		z, plotand
			CP		OPOR
			jr		z, plotor
			cp		OPPOKE
			jr		z, plotpoke
			ld		a,c
			xor		b
			ld		(hl),a
			jr		plotend
plotor:		ld		a,c
			or		b
			ld		(hl),a
			jr		plotend
plotand:	ld		a,c
			and		b
			ld		(hl),a
			jr		plotend
plotpoke:	
			ld		(hl),a

plotend:	pop		af
			pop		de
			pop		bc
			pop		hl
			ret


VARX1		EQU 0
VARY1		EQU 1
VARX2		EQU 2
VARY2		EQU 3
VARDIFFX	EQU 4
VARDIFFY	EQU 6
VARABSX		EQU 8
VARABSY		EQU 7
VARSGNX		EQU 8
VARSGNY		EQU 9
VARER		EQU 10
VARSYTLE	EQU 12
; https://www.cpcwiki.eu/forum/programming/fast-line-draw-in-assembly-(breseham-algorithm)/
	; 10 REM === DRAW a LINE. Bresenham algorithm from (x1,y1) to (x2,y2)
	; 20 DX = ABS(X2 - X1) :SX = -1 :IF X1 - x2 < 0 THEN SX = 1
	; 30 DY = ABS(Y2 - Y1) :SY = -1 :IF Y1 - y2 < 0 THEN SY = 1
	; 40 ER = -DY : IF DX - dy > 0 THEN ER = DX
	; 50 ER = INT(ER / 2)
	; 60 PLOT X1,Y1
	; 70 IF X1 = X2 AND Y1 = Y2 THEN RETURN
	; 80 E2 = ER
	; 90 IF E2 +dx > 0 THEN ER = ER - DY:X1 = X1 + SX
	; 100 IF E2 -dy < 0 THEN ER = ER + DX:Y1 = Y1 + SY
	; 110 GOTO 60

	; 10 REM === DRAW a LINE. Bresenham algorithm from (x1,y1) to (x2,y2)
	; 20 DiffX = ABS(X2 - X1) :SgnX = SGN(DiffX)
	; 30 DiffY = ABS(Y2 - Y1) :SgnY = SGN(DiffY)
	; 40 ER = -DiffY : IF DiffX - DiffY > 0 THEN ER = DiffX
	; 50 ER = INT(ER / 2)
	; 60 PLOT X1,Y1
	; 70 IF X1 = X2 AND Y1 = Y2 THEN RETURN
	; 80 E2 = ER
	; 90 IF E2 +DiffX > 0 THEN ER = ER - DiffY:X1 = X1 + SgnX
	; 100 IF E2 -DiffY < 0 THEN ER = ER + DiffX:Y1 = Y1 + SgnY
	; 110 GOTO 60
line: 	PUSHA
		push	ix

		call 	plot		; draw first point
		push	bc

		push	de
		pop		bc
		call    plot		; draw last point
		pop		bc
		LAlloc	20
		
		ld		(ix+VARX1),b
		ld		(ix+VARY1),c
		ld		(ix+VARX2),d
		ld		(ix+VARY2),e
		ld		(ix+VARSYTLE),h

	; 20 DX = ABS(X2 - X1) :SX = -1 :IF X1 - x2 < 0 THEN SX = 1

		ld		l,(ix+VARX2)
		ld		h,0
		ld		e,(ix+VARX1)
		ld		d,0
		sbc		hl,de
		ld		a,h
		call	sgnHL
		ld		(ix+VARSGNX),a
		call	absHL
		ld		(ix+VARDIFFX),hl

	; 30 DiffY = ABS(Y2 - Y1) :SgnY = SGN(DiffY)
		ld		l,(ix+VARY2)
		ld		h,0
		ld		e,(ix+VARY1)
		ld		d,0
		sbc		hl,de
		call	sgnHL
		ld		(ix+VARSGNY),a
		call	absHL
		ld		(ix+VARDIFFY),hl

; 40 ER = -DiffY : IF DiffX - DiffY > 0 THEN ER = DiffX

		ld		hl,0
		ld		de,(ix+VARDIFFY)
		sbc		hl,de
		ld		(ix+VARER),hl

		ld		hl,(ix+VARDIFFX)
		ld		de,(ix+VARDIFFY)
		sbc		hl,de
		jr		NC, line_1
		ld		hl,(ix+VARDIFFX)
		ld		(ix+VARER),hl
		
line_1:	
		ld	hl,(ix+VARER)
	; 50 ER = INT(ER / 2)
		SRA H
  		RR L
		ld		(ix+VARER),hl
		

	
		
line_loop:
	;	call	debugvars
; 60 PLOT X1,Y1		
		ld		h,(ix+VARSYTLE)
		ld		b,(ix+VARX1)
		ld		c,(ix+VARY1)
		call	plot

; 70 IF X1 = X2 AND Y1 = Y2 THEN RETURN
		ld		a,b
		sub		(ix+VARX2)
		call	absA
	
		cp		2
		jr		NC, line_2
		cp		0
		jr		nz, line_2
		ld		a,c
		sub		(ix+VARY2)
		call	absA
;		cp		0
;		jr		z, line_end

		cp		2
		jr		c,line_end
line_2:
	; 80 E2 = ER
		ld		bc,(ix+VARER)

	; 90 IF E2 +DiffX > 0 THEN ER = ER - DiffY:X1 = X1 + SgnX

		LD		hl,bc
		ld		de,(ix+VARDIFFX)
		add		hl,de
		ld		a,h
		and		$80
		cp		0
		jr		nz, line_3
		ld		hl,(ix+VARER)
		ld		de,(ix+VARDIFFY)
		sbc		hl,de
		ld		(ix+VARER),hl
	

		ld		a,(ix+VARX1)
		add		a,(ix+VARSGNX)
		ld		(ix+VARX1),a
line_3:
	; 100 IF E2 -DiffY < 0 THEN ER = ER + DiffX:Y1 = Y1 + SgnY
		ld		hl,bc
		ld		de,(ix+VARDIFFY)
		sbc		hl,de
		ld		a,h
		and		$80
		cp		0
		jr		z,line_4

		ld		hl,(ix+VARER)
		ld		de,(ix+VARDIFFX)
		add		hl,de
		ld		(ix+VARER),hl

		ld		a,(ix+VARY1)
		add		a,(ix+VARSGNY)
		ld		(ix+VARY1),a

line_4:	jp		line_loop



line_end:
		LRelease 20	
		pop		ix
		POPA
		ret



CircMultipoint:
        ld      d,(ix+circ_xc-circ_base)
        ld      e,(ix+circ_yc-circ_base)
        ld      b,(ix+circ_x-circ_base)
        ld      c,(ix+circ_y-circ_base)


CirclePoint:
        ;       d = xc
        ;       e = yc
        ;       b = x
        ;       c = y
;drawCircle(int xc, int yc, int x, int y)
;{
;    putpixel(xc+x, yc+y, RED);
;    putpixel(xc-x, yc+y, RED);

;    putpixel(xc-y, yc+x, RED);
;    putpixel(xc+y, yc+x, RED);

;    putpixel(xc+x, yc-y, RED);
;    putpixel(xc-x, yc-y, RED);

;    putpixel(xc-y, yc-x, RED);
;    putpixel(xc+y, yc-x, RED);
;}

        push    hl
        push    de
        ld      hl,de

;       h = xc
;       l = yc
;       b = x
;       c = y

;    putpixel(xc+x, yc+y, red);
;    putpixel(xc-x, yc+y, RED);
        ld      a,h     ;       xc
        add     b       ;       + x
        ld      d,a     ;       x = xc + x
        
; unten - nach rechts
        ld      a,l     ;       yc
        add     c       ;       + y
        ld      e,a     ;       e = yc+y
;    putpixel(xc+x, yc+y, RED);        
        call    PLOT    ;       plot xc+x, yc+y
; unten - nach links
        ld      a,h
        sub     b
        ld      d,a
;    putpixel(xc-x, yc+y, RED);
        call    PLOT    ;       plot xc-x, yc+y

;    putpixel(xc-y, yc+x, RED);
;    putpixel(xc+y, yc+x, RED);
        ld      a,l     ;       yc
        add     b       ;       + x
        ld      e,a     ;       e = yc+x

        ld      a,h     ;       xc
        sub     c       ;       - y
        ld      d,a     ;       d = xc-x
; links nach unten
;    putpixel(xc-y, yc+x, RED);
        call    PLOT    ;       plot xc-y, yc+x
        ld      a,h     ;       xc
        add     c       ;       + y
        ld      d,a     ;       d = xc+x
; rechts nach unten        
;    putpixel(xc+y, yc+x, RED);
        call    PLOT    ;       plot xc+y, yc+x

;    putpixel(xc+x, yc-y, RED);
;    putpixel(xc-x, yc-y, RED);

        ld      a,l     ;       yc
        sub     c       ;       - y
        ld      e,a     ;       e = yc-y

        ld      a,h     ;       xc
        add     b       ;       + x
        ld      d,a     ;       d = xc+x
; oben - nach rechts 
;    putpixel(xc+x, yc-y, RED);
        call    PLOT    ;       plot xc+x, yc-y
        ld      a,h     ;       xc
        sub     b       ;       - x
        ld      d,a     ;       d = xc-x
; rechts nach unten        
;    putpixel(xc-x, yc-y, RED);
        call    PLOT    ;       plot xc-x, yc-y

;    putpixel(xc-y, yc-x, RED);
;    putpixel(xc+y, yc-x, RED);

        ld      a,l     ;       yc
        sub     b       ;       - x
        ld      e,a     ;       e = yc-x
        ld      a,h     ;       xc
        sub     c;      ;       - y
        ld      d,a     ;       d = xc-y

;    putpixel(xc-y, yc-x, RED);
        call    PLOT    ;       plot xc-x, yc+y
        ld      a,h     ;       xc
        add     c       ;       - y
        ld      d,a     ;       d = xc-x
        call    PLOT    ;       plot xc+x, yc+y

        pop     de
        pop     hl
        ret

circ_base:
circ_x:         defb    0,0
circ_y:         defb    0,0
circ_xc:        defb    0,0
circ_yc:        defb    0,0
circ_r:         defb    0,0
circ_d:         defb    0,0
Circle:
;         b     =       xc      ; input x center
;         c     =       yc      ; input y center
;         l     =       r       ; input radius
;void circleBres(int xc, int yc, int r){
;    int x = 0, y = r;
;   int d = 3 - 2 * r;
;    drawCircle(xc, yc, x, y);
;    while (y >= x){
;
;        // check for decision parameter
;        // and correspondingly
;        // update d, y
;        if (d > 0) {
;            y--;
;            d = d + 4 * (x - y) + 10;
;        }
;        else
;            d = d + 4 * x + 6;
;
;        // Increment x after updating decision parameter
;        x++;
;
;        // Draw the circle using the new coordinates
;        drawCircle(xc, yc, x, y);
;        delay(50);
;    }
;}



;    int y = r;

        ld      ix, circ_base
        ld      h, 0
        ld      (ix + circ_y-circ_base), hl             // y = r
        ld      (ix + circ_r-circ_base), hl             // r
; x = 0
        ld      hl,     0
        ld      (ix + circ_x-circ_base), hl             // x =
        ld      l,b
        ld      (ix + circ_xc-circ_base),hl             // xc = yc
        ld      l,c
        ld      (ix + circ_yc-circ_base),hl             // yc = yc


;   int d = 3 - 2 * r;

        ld      hl,(ix+circ_r - circ_base)
        add     hl,hl
        ld      de,3
        ex      de, hl
        sbc     hl, de           // hl = 3 - 2 * r
        ld      (ix+circ_d-circ_base), hl

;    drawCircle(xc, yc, x, y);
        call    CircMultipoint

;    while (y >= x){
circ_loop:
        ld      hl,(ix+circ_y-circ_base)
        ld      de,(ix+circ_x-circ_base)
        sbc     hl,de
        jr      c, circ_exit
;        // check for decision parameter
;        // and correspondingly
;        // update d, y
;        if (d > 0) {
;            y--;
;            d = d + 4 * (x - y) + 10;
;        }
        ld      hl,(ix+circ_d-circ_base)
        ld      a,h
        and     $80
        jr      nz, circ_d1
        ld      hl, (ix+circ_y-circ_base)
        dec     hl
        ld      (ix+circ_y-circ_base),hl        ; y = y -1
        ld      hl,(ix+circ_x-circ_base)
        ld      de,(ix+circ_y-circ_base)
        sbc     hl, de                          ; hl = x-y
        add     hl,hl
        add     hl,hl                           ; hl = 4 * (x-y)
        ld      de,10
        add     hl, de                          ; hl = 4 * (x-y)+10
        ld      de,(ix+circ_d-circ_base)
        add     hl,de
        ld      (ix+circ_d-circ_base),hl        ; d = d + 4 * (x-y) + 10
        jr      circ_d2


;        else
circ_d1:
;            d = d + 4 * x + 6;
        ld      hl,(ix+circ_x-circ_base)
        add     hl,hl
        add     hl,hl
        ld      de,6
        add     hl,de
        ld      de,(ix+circ_d-circ_base)
        add     hl,de
        ld      (ix+circ_d-circ_base),hl           ; d = d + 4 * x + 6

circ_d2:
;        x++;
        inc     (ix+circ_x-circ_base)
;        drawCircle(xc, yc, x, y);
        call    CircMultipoint
        jr      circ_loop

circ_exit:
        ret

; ------------------------------------------------
; Draws sprite. 
; E = Number
; D = Modus			0 = xor
; B=x, C=Y, 
; ------------------------------------------------
drawsprite:

; Set IY to the start of line
	push    bc
	PUSH	DE
; Set IX to the start of the sprite, each sprite is 24x16 pixel = 48 Bytes 

; Sprite are pre-shifted in 8 positions
; 8 sprites with each one 8 lines and each line is two bytes so we reserve 256 bytes per sprite
 
	ld      c,d
	PRINTREG "e=",e
    call    getShiftedSprite	
	PRINTREG16 "sprite base hl=",hl
	push	hl
	pop		ix
; IX points to the sprite data
; IY points to the line data
; BC is the X-Offset in characters
	ld	    a,c
 	POP		BC
	pop     de			; d = x; e=y
	ld 		b,e
	ld 		e,d
	ld		d,b			; d/e vertauschen, d=y, e=x
	srl     e			; x / 2
	srl     e			; x / 4
	srl     e			; x / 8
	LD		B,16
	CP		0
	JR		Z, drawspritexor
	cp      2
	jr      z, drawspritePoke


	call 	calcScreenAddressDE
drawsprite1:
	
	
	LD		A,(IX)
	OR      (hl)
	LD		(HL),A
	INC		HL
	INC		IX
	LD		A,(IX)
	OR      (hl)
	LD		(HL),A
	INC		HL
	INC		IX
	LD		A,(IX)
	or      (hl)
	LD		(HL),A
	INC		IX
	DEC		HL
	DEC		HL

	call    NextScreenLine
	DJNZ	drawsprite1
	ret

drawspritePoke:
	call 	calcScreenAddressDE
	
drawspritePokeLoop:	
	LD		A,(IX)
	LD		(HL),A
	INC		HL
	INC		IX
	LD		A,(IX)
	LD		(HL),A
	INC		HL
	INC		IX
	LD		A,(IX)
	LD		(HL),A
	INC		IX
	DEC		HL
	DEC		HL

	call    NextScreenLine
	DJNZ	drawspritePokeLoop:	

	ret

drawspritexor:	
	call 	calcScreenAddressDE		; d = y, e=x
drawspritexorLoop:
	LD		A,(IX)
	INC		IX
	XOR		(HL)
	ld		(HL),A
	INC		HL

	LD		A,(IX)
	INC		IX
	XOR		(HL)
	ld		(HL),A
	INC		HL

	LD		A,(IX)
	INC		IX
	XOR		(HL)
	ld		(HL),A

	dec 	hl
	dec		hl
	call 	NextScreenLine
	DJNZ	drawspritexorLoop
	ret	

; e = Sprite Number
; b = x				; we use x and 7 so that the sprite is shifted to the right 0,1,..7 times 
; c = y				; y is ignored
; the sprites 0..PREPARED_SPRITE_COUNT-1 are fast sprites, they are pre-shifted we simply return address 
; bigger sprites are slow sprites, we copy to a temp bitmap and shift it, alsways the same address is returned
; returns hl = address
; destroys a
getShiftedSprite:
    push 	ix
	push    de
	LD		A,E
	cp		PREPARED_SPRITE_COUNT 

	jr      nc,shiftSlowSprite
; calculate the base of the sprite: Sprite Number * 384
	LD		D,0				; e contains the sprite number
	ADD		DE,DE			; *2
	ADD		DE,DE			; *4
	ADD		DE,DE			; *8
	ADD		DE,DE			; *16
	ADD		DE,DE			; *32
	ADD		DE,DE			; *65
	ADD		DE,DE			; *128
	LD		HL, de
	ADD		DE,DE			; *256
	add     de,hl		    ; *384
	LD 		HL, spriteprepdata
	add     hl,de
; calc the address of the shifted sprite. (x coord and 7) * 48
	ld 		a,b
	and	7
	ld		e,a
	ld		d,0
	push    hl
	add	    de,de		; * 2
	add		de,de		; * 4
	add		de,de		; * 8
	add		de,de		; * 16
	ld		hl,de		;
	add     de,de	    ; * 32
	add		de,hl	    ; * 48
	pop     hl
	add     hl,de
	pop		de
	pop		ix
	ret
shiftSlowSprite:
	push    bc
	ld		a,b
	and 	7	
	ld		c,a
	ld		a,e
	sub		PREPARED_SPRITE_COUNT
	ld		e,a
	ld		d,0
	
; Calculate the position of the 16x16 sprite
	add     de,de 		; Sprite Nr * 2
	add     de,de		; * 4
	add     de,de		; * 8
	add     de,de		; * 16
	add     de,de		; * 32
	ld		hl, slowsprites
	ld		ix, tempsprite
	ld		b,16
shiftSlowSpriteLoop1:
	push    bc

; Copy one row of the 16x16 sprite to the 24x16 sprite
	ld      a,(hl)
	inc		hl
	ld		(ix+0),a
	ld	    a,(hl)
	inc		hl
	ld		(ix+1),a
	ld	    a,0
	ld		(ix+2),a


  	
	ld		a,c
	cp		0
	jr		z, shiftSlowSpriteShiftDone
	ld		b,a

shiftSlowSpriteLoop:

	srl		(ix+0)
	rr		(ix+1)
	rr		(ix+2)
	djnz	shiftSlowSpriteLoop
shiftSlowSpriteShiftDone:	
	inc     ix
	inc     ix
	inc     ix
	pop     bc
	djnz	shiftSlowSpriteLoop1
	ld		hl,tempsprite
	pop     bc
	pop	de
	pop ix

	ret

; draws a sprite with double xor.
; First we draw the sprite on the original position, then we xor the moved sprite, which is 1,2,3 or 4 pixels to
; the left/top/right/bottom of the original sprite.
; Then we xor the resulting bitmap to the screen. The result is that the old sprite gets deleted at the same time
; the new sprite is drawn.
drawSpriteMoveXorData:
drawSpriteX:				db 0
drawSpriteX8:				db 0		// x/8
drawSpriteY8:				db 0		// x/8
drawSpriteY:				db 0
drawSpriteWidth:			db 5
drawSpriteHeight:			db 32
drawSpriteNum:				db 0
drawSpriteMoveX:			db 0
drawSpriteMoveY:			db 0

; the spite is 16x16 pixel but due to shifting we need 24x16 pixel
; the bitmap adds 8 pixel to each side, so wie have 48x23 pixel or 5x4 characters
drawSpriteBitmap:			defs 5*32		; 3x2 Characters = 24x16 pixel

DEBUG_BITMAP				macro
							if DEBUG=1
							call debugPrintBitmap
							endif
							endm
debugPrintBitmap:			if DEBUG=1
							push af
							push bc
							push de
							push hl
							ld   e,0
							ld   d,0
							call calcScreenAddressDE
							ld   de, drawSpriteBitmap
							ld   b,31
debugPrintBitmap1:			push bc
							push hl
							ld   b,5
debugPrintBitmap2:			ld   a,(hl)
							ld   a,(de)
							ld   (hl),a
							inc  hl
							inc  de
							djnz debugPrintBitmap2
							pop  hl
							pop  bc
							call NextScreenLine
							djnz debugPrintBitmap1
							pop  hl 
							pop  de
							pop  bc
							pop  af
							ret
							endif	
; 5*32 = 160		

; x = b
; y = c
; newx = h
; newy = l
; spriteNum = e
drawSpriteMoveXor:			push bc
							push de
							push hl
							ld   d,0
							call drawsprite
							pop  bc
							pop  de
							call drawsprite 
							pop  bc	
							ret
							





							ld ix, drawSpriteMoveXorData
                            ld (ix+drawSpriteX-drawSpriteMoveXorData),b
							ld  a,b
							srl a		; a = x / 2
							srl a		; a = x / 4
							srl a		; a = x / 8
							ld (ix+drawSpriteX8-drawSpriteMoveXorData),a
							ld a,c
							srl a		; a = x / 2
							srl a		; a = x / 4
							srl a		; a = x / 8
							ld (ix+drawSpriteY8-drawSpriteMoveXorData),a

							ld (ix+drawSpriteY-drawSpriteMoveXorData),c
							ld (ix+drawSpriteNum-drawSpriteMoveXorData),e
							ld (ix+drawSpriteMoveX-drawSpriteMoveXorData),h
							ld (ix+drawSpriteMoveY-drawSpriteMoveXorData),l
							ld  b,5*32
							ld  a,0   
							ld  hl,drawSpriteBitmap
drawSpriteXorClear:			ld (hl),a
							inc hl
							djnz drawSpriteXorClear
							ld a,255
							DEBUG_BITMAP

							push bc
							ld   a,(ix+drawSpriteX-drawSpriteMoveXorData)
							and 7
							add  8
							ld   b,a
							ld   a,(ix+drawSpriteY-drawSpriteMoveXorData)
							and 7
							add  8
							ld   c,a
							call getShiftedSprite								; the original sprite is now in hl
							call drawSpriteXorPlot
							DEBUG_BITMAP
							
							ld   a,(ix+drawSpriteX-drawSpriteMoveXorData)
							add  a,(ix+drawSpriteMoveX-drawSpriteMoveXorData)
							and 7
							add  8
							ld   b,a
							ld   a,(ix+drawSpriteY-drawSpriteMoveXorData)
							add  a,(ix+drawSpriteMoveY-drawSpriteMoveXorData)
							and 7
							add  8
							ld   c,a

							call getShiftedSprite
							call drawSpriteXorPlot
							DEBUG_BITMAP
							pop  bc

; the bitmap now is prepared and contains the sprite in both positions
; now draw the sprite at x-8/y-8 which is (X/8)-1 in char position 

; the sprite is drawn in x-1..x+4, x = char pos 0..31
							ld  hl,drawSpriteBitmap	; hl = bitmap
							ld  d,5				; width in bytes
							ld  e,32			; height in lines
							ld   a,(ix+drawSpriteX8-drawSpriteMoveXorData)
							sub 1
							jr  nc, drawSpriteXorClip1
							dec d
							;dec  (ix+drawSpriteX8-drawSpriteMoveXorData)
							inc hl				
drawSpriteXorClip1:         ld  b,a				; b = x-1 in char position
							cp  31
							jr  nc,drawSpriteXorClip2
							sub a,31
							push hl
							ld  h,a
							ld  a,d
							sub h
							ld  d,a
							ld  a,(ix+drawSpriteY-drawSpriteMoveXorData)
							sub h
							ld  (ix+drawSpriteY-drawSpriteMoveXorData),a
							pop  hl
drawSpriteXorClip2:			ld  (ix+drawSpriteWidth-drawSpriteMoveXorData),d
							ld  a,c
							sub 8 
							jr  nc, drawSpriteXorClip3
; sprite is above the screen. create a new top line
                            push de
							push hl
							ld  h,a
							ld  a,e
							add h
;							add e,a
							ld  (ix+drawSpriteHeight-drawSpriteMoveXorData),a
; calculate the new first row in the bitmap							
 							ld  a,0
							sub h
							ld  l,a
							ld  h,0
							push bc
							ld  bc,hl
							add hl,hl				; * 2
							add hl,hl				; * 4
							add hl,bc
							ld  de,hl
							pop bc
							pop hl
							add hl,de
							pop de
; now HL points to the first byte in the bitmap
drawSpriteXorClip3:

							push hl
							ld   d,(ix+drawSpriteY-drawSpriteMoveXorData)
							ld   e,0
							call calcScreenAddressDE
							ld   e,(ix+drawSpriteX8-drawSpriteMoveXorData)
							ld   d,0
							add  hl,de
							ex   de,hl
							pop  hl
; hl points to the bitmap
; de points the byte in the screen 
 							ld b,(ix+drawSpriteHeight-drawSpriteMoveXorData)
							ld c,(ix+drawSpriteWidth-drawSpriteMoveXorData)
							
drawSpriteXorToScreen1:		push bc
							ld   b,c
							push hl 
							push de
drawSpriteXorToScreen2:		ld   a,(de)
							xor (hl)
							ld   (de),a
							inc  de
							inc  hl
							djnz drawSpriteXorToScreen2
							pop  hl
							call NextScreenLine
							push hl
							pop  de
							pop  hl
							ld   bc,5
							add  hl,bc
							pop  bc
							djnz drawSpriteXorToScreen1
							ret


; now copy the prepared, clipped bitmap to the screen
; hl contains the correct byte in the screen
; de contains the bitmap
; zx Screen goes from $4000 to $5800



; Now draw the sprite to the screen at the coordinates b/c							
							

; b = x
; c = y
; sprite size 8 bytes * 16 pixel lines
; hl = Sprite Address
drawSpriteXorPlot:
	ld  	e,c
	ld  	d,0
	push  	hl
	push  	bc
	ld  	bc,de
	add  	de,de		; * 2
	add  	de,de		; * 4
	add  	de,bc		; * 5
	pop  	bc
	ld      hl, drawSpriteBitmap
	add		hl,de
	ex 		hl,de
	pop		hl
	ld      c,b
	ld      b,0
	ex      hl,de
	add		hl,bc		; hl = source + byte offset
	ex      hl,de
	ld		b,16			; 16 rows
drawSpriteXorPlotLoop1:
	push    bc
	ld		b,3
drawSpriteXorPlotLoop2:
	ld		a,(de)
	xor		(HL)
	ld 		(de),a
	inc		hl
	inc		de
	djnz	drawSpriteXorPlotLoop2
	inc		de
	inc		de
	pop		bc
	djnz	drawSpriteXorPlotLoop1
	ret   
													

							
prepare_sprite:
	; a = Sprite Number
	; hl = Sprite Data
	cp 		a, PREPARED_SPRITE_COUNT
	jr		nc, prepare_sprite_slow
	push    hl
	ld 		l,a
	ld		h,0
	add		hl,hl		; * 2
	add		hl,hl		; * 4
	add		hl,hl		; * 8
	add		hl,hl		; * 16
	add		hl,hl		; * 32
	add 	hl,hl		; * 64
	add		hl,hl		; * 128
	ld		de,hl
	add		hl,hl		; * 256
	add     hl,de		; * 384
	ld		de, spriteprepdata
	add		hl,de		; hl = base of sprite a

; copy the sprite to the area, the sprite data ist 16x16 pixel
; the shifted sprite id 24x16 pixel

	pop     de				; DE contains the 16x16 sprite 
	push	hl				; hl will hold the 24x16 sprite
	ld		b,16			; 8 sprites
	ex      hl,de
; copy the 16x16 bit sprite to position 0 as 24x16 sprite
prepare_sprite_copy_loop:
	ld		a,(hl)
	inc 	hl
	ld		(de),a
	inc		de
	ld		a,(hl)
	inc 	hl
	ld		(de),a
	inc		de
	ld		a,0
	ld		(de),a	
	inc		de
	djnz	prepare_sprite_copy_loop
	pop		hl


	ld		b,8		; 8 position
prepare_sprite1:
	push 	bc
	ld		de,hl
	ld		bc,48		; 48 = 6*8
	add		hl,bc
	ex		hl,de		; hl = Sprite; de = sprite+32
	ld		b,16		; 16 rows
prepare_sprite2:
    push    bc
	ld		a,(hl)
	inc 	hl
	ld		b,(hl)
	inc		hl
	ld      c,(hl)
	inc     hl
	srl		a
	rr		b
	rr      c
	ld		(de),a
	inc     de
	ld      a,b
	ld		(de),a
	inc		de
	ld      a,c
	ld      (de),a
	inc     de
	pop	    bc
	djnz    prepare_sprite2
	pop     bc
	djnz	prepare_sprite1
	ret	

spritecount:
	LD		hl, spritecount
	ret

; Slow sprites are not pre-shifted, simply store it in
; the slow sprite aerea
prepare_sprite_slow:
	push    hl
	ld		e,a
	ld		d,0
	add		de,de			; *2
	add		de,de			; *4
	add		de,de			; *8
	add		de,de			; *16
	add		de,de			; *32
	ld 		hl,slowsprites
	pop		de
	ld	    bc,32
	ex		de,hl
	ldir	
	ret



; D = X and E = Y
PLOT:
        push    af
        push    bc
        push    hl
        call    CalcScreenAddress

        or (hl)
        ld (hl),a
        call    setColor
        pop     hl
        pop     bc
        pop     af
        ret

// d = x; e=y
setColor:    push de
             push hl
        srl     d
        srl     d
        srl     d
        ld      l,d
        ld      h,$58
        srl     e
        srl     e
        srl     e
        ld      d,0
        add     de,de
        add     de,de
        add     de,de
        add     de,de
        add     de,de
        add     hl,de
;        ld      de,$4000+6144
;        add     hl,de
        ld      a,(ZX_ATTR_P)
        ld      (hl),a
        pop     hl
        pop     de
        ret

; input D = x; e=y
; output a == 0 wenn kein Pixel, a != 0, wenn Pixel
; b = bitmask 
POINT:          push    bc
        push    hl
        call    CalcScreenAddress
        and     (hl)
        pop     hl
        pop     bc
        ret


; input d = x, e = y;
; output hl = Screen Address
;         a = Screen Bit mask
CalcScreenAddress:
        ld      a,7
        and     d                       //      a = x & 7
        ld      b,a
        inc     b                       //      b = (x & 7) + 1
                                        ; b = Shift position innerhalb des Bytes
        ld      a,e
        rra                             ;      a = y / 2
        scf                             ;      cary = 1
        rra                             ;      a = (a / 4) + 128
        or a                            ;      carry = 0
        rra                             ;      a = a / 2
        ld l,a                          ;      l = ((y/4)+128)/2
                                        ;      l = (512+y)/8
                                        ;      l = 64+y/8

; Errechne die Basis der Zeile. Jede Zeile ist 256 Bytes von der nächsten Entfernt
; screen startet bei $4000
; Errechne L=Offset innerhalb der Zeile
; es gibt 3 Blöcke, diese sind 2048 Bytes entfernt ($800, h=h+8)
; In jedem Block gibt es 8 Textzeilen á 8 Zeilen, jeweils 256 Bytes entfernt (h=h+1)
; Die nächste der 8 Textzeilen ist 32 Bytes entfernt

        xor e                           //      a = ((64+y)/2) x y
        and 248                         //      a = a and %1111 1000
        xor e                           //      a = a xor e
        ld h,a                          //      h = a
        ld a,d
        xor l
        and 7
        xor d
        rrca
        rrca
        rrca
        ld l,a
        ld a,1 ; one
CalcScreenAddressBit:
        rrca
        djnz CalcScreenAddressBit
        ret
		
; Calculate the start of the screen line in a.
; a = 0..191
; return hl: Start of line
CalcScreenLineA:
		push de
		push bc
		ld   d,0
		ld   e,a
		call CalcScreenAddress
		pop  bc
		pop  de
		ret		

; --------------------- fill -----------------------------
; scanline fill by John Metcalf
; call with d=x-coord, e=y-coord
; http://www.retroprogramming.com/2017/04/zx-spectrum-scanline-flood-fill.html
; set end marker

sfill:					ld l,255
						push hl

; calculate bit position of pixel

nextrun:				ld a,d
						and 7
						inc a
						ld b,a
						ld a,1
bitpos:					rrca
						djnz bitpos
						ld c,b
						ld b,a

; move left until hitting a set pixel or the screen edge

seekleft:
						ld a,d
						or a
						jr z,goright
						dec d
						rlc b
						call scrpos
						jr nz,seekleft

; move right until hitting a set pixel or the screen edge,
; setting pixels as we go. Check rows above and below and
; save their coordinates to fill later if necessary

seekright:  
						rrc b
						inc d
						jr z,rightedge
goright:				call scrpos
						jr z,rightedge
						ld (hl),a
						call setColor
						inc e
						call checkadj
						dec e
						dec e
						call checkadj
						inc e
						jr seekright

; check to see if there's another row waiting to be filled

rightedge:				pop de
						ld a,e
						inc a
						jr nz,nextrun
						ret  

; calculate the pixel address and whether or not it's set

scrpos:					ld a,e
						and 248
						rra
						scf
						rra
						rra
						ld l,a
						xor e
						and 248
						xor e
						ld h,a
						ld a,l
						xor d
						and 7
						xor d
						rrca
						rrca
						rrca
						ld l,a
						ld a,b
						or (hl)
						cp (hl)
						ret

; check and save the coordinates of an adjacent row
checkadj:				sla c
						ld a,e
						cp 192
						ret nc
						call scrpos+1
						ret z
						inc c
						bit 2,c
						ret nz
						pop hl
						push de
						jp (hl)        

ScrollLeft:				push 	hl
						push	de
						push	bc
						ld	hl, $4001
						ld	de, $4000
						ld	b,192
ScrollLeft1:			push	bc
						ld	bc,31
						ldir
						dec hl
						ld	(hl),0
						inc hl
						inc hl
						inc de
						pop	bc
						djnz ScrollLeft1
						ld	b,24
						ld	a,(ZX_ATTR_P)
ScrollLeftAttr:			push	bc
						ld	bc,31
						ldir
						dec hl
						ld	(hl),a
						inc hl
						inc hl
						inc de
						pop	bc
						djnz ScrollLeftAttr
						pop		bc
						pop		de
						pop		hl	
						ret

ScrollRight:			push 	hl
						push	de
						push	bc
						ld	hl, $4000+6142
						ld	de, $4000+6143
						ld	(hl),255
						ld	b,192
ScrollRight1:			push	bc
						ld	bc,31
						lddr
						dec hl
						ld	(hl),0
						
						dec de
						pop	bc
						djnz ScrollRight1
						ld	hl, $4000+6144+768-2
						ld	de, $4000+6144+768-1
						ld	b,24
						ld	a,(ZX_ATTR_P)
ScrollRightAttr:		push	bc
						ld	bc,31
						lddr
						inc hl
						ld	(hl),a
						dec hl
						dec hl
						dec de
						pop	bc
						djnz ScrollRightAttr
						pop		bc
						pop		de
						pop		hl	
						ret

ScrollDown:				ld  b,23
						ld  c,23*8
ScrollDown1:			push bc
						ld  a,c
						call CalcScreenLineA			; hl = line c
						ex   hl,de						; de = upper line
						ld	 a,c
						sub  8
						call CalcScreenLineA			; hl = lower line
						call CopyScreenLineHLtoDE		; Copy a text line (8 lines á 32 byte)
						pop bc
						ld  a,c
						sub 8
						ld c,a
						djnz ScrollDown1	
						ld	hl, $4000 
						call ClearScreenTextLine
						ld   hl, $4000+6144+22*32		; Start of attributes
						ld   de, $4000+6144+23*32	; Start of attributes second line
						ld   b,23
ScrolllDownAttr:		push bc
						push hl
						push de
						ld bc,32						
						ldir
						pop de	
						pop hl
						ld  bc,32
						sub hl,bc
						sub de,bc
						pop bc
						djnz ScrolllDownAttr
						ld   hl, $4000+6144		; Start of attributes
						ld   b,32
						ld  a,(ZX_ATTR_P)
ScrollDownAttr2: 		ld (hl),a
						inc hl
						djnz ScrollDownAttr2								
		ret

ScrollUp:				ld  b,23						; Number of lines
						ld  c,0							; Line number
ScrollUpLoop:			push bc
						ld  a,c
						call CalcScreenLineA			; hl = line c
						ex   hl,de						; de = upper line
						ld	 a,c
						add  a,8						; hl = line c+8
						call CalcScreenLineA			; hl = lower line
						call CopyScreenLineHLtoDE		; Copy a text line (8 lines á 32 byte)
						pop bc
						ld  a,c
						add 8
						ld c,a
						djnz ScrollUpLoop
						ld   a,23*8
						call CalcScreenLineA			; This is the last line
						call ClearScreenTextLine		; clear it
						ld   de, $4000+6144				; Start of attributes
						ld   hl, $4000+6144+32			; Start of attributes second line
						ld   bc, 23*32					; 23 lines
						ldir							; move attributes one line up
						ex   de,hl
						ld	a,(ZX_ATTR_P)
	
						ld  b,32
ScrollUpLoop2:			ld  (hl),a
						inc hl
						djnz ScrollUpLoop2				

						ret

						ret
ClearScreenTextLine:	push bc	
						ld   b,8
ClearScreenTextLine1:	push bc
						ld   b,32
						push hl
ClearScreenTextLine2:	ld 	(hl),0
						inc hl
						djnz ClearScreenTextLine2
						pop hl
						inc h
						pop bc
						djnz ClearScreenTextLine1
						pop bc
						ret


						ld  a,0	
CopyScreenLineHLtoDE:	ld  b,8

CopyScreenLineHLtoDE1:	push bc
						push hl
						push de
						ld  bc,32	
						ldir
						pop de
						pop hl
						pop bc
						inc h
						inc d 
						djnz CopyScreenLineHLtoDE1
						ret



						




plotdata:	db 		$80,$40,$20,$10,$8,$4,$2,$1
slowsprites:
	defs	32*SLOW_SPRITE_COUNT,255
tempsprite:
	defs	48,255

spriteprepdata:
	DEFS	48*8*PREPARED_SPRITE_COUNT,255

	ENDIF

    DISPLAY "Graph size = ", /D, $-GRAPH_START,  " bytes"