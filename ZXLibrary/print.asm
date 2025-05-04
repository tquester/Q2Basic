PRINT_START:

; ---------------------------------------------------------------------------
; locate, b =x, c = y
; ---------------------------------------------------------------------------

locate:
	ld		a,b
	LD		(charX),a
	ld		a,c
	ld		(charY),a
	ret

; -------------------------------------------------------------------
; print the character in register A at charX, charY. 
; wrap line and scroll
; -------------------------------------------------------------------
printA:
	PUSH	AF
	PUSH	IX
	PUSH	BC
	PUSH	DE
	PUSH	HL
	cp		8
	jr		z,	printTab
	cp		9
	jr		z,	printTab
	cp		10
	jr		z,	printNewline
	cp		13
	jp		z,  printCarriageReturn 
	LD		L,	A
	LD		H,  0
	ADD		HL, HL
	ADD		HL, HL
	ADD		HL, HL
	LD		DE, CHARSET
	ADD		HL, DE
	LD		IX, HL			; ix points to the charmap of ascii A

; calc attribute address
	LD		a, (charY)		
	ld		l,a
	ld		h,0
	add		hl,hl				; * 2
	add		hl,hl				; * 4
	add		hl,hl				; * 8
	add		hl,hl				; * 16
	add		hl,hl				; * 32
	ld		a,(charX)
	ld		e,a
	ld		d,0
	add		hl,de
	ld		de, SCREEN_ATTR
	add		hl,de
	ld		a,(charAttrib);
	ld		(hl),a

	ld		a, (charX)
	add		a,a
	add		a,a
	add		a,a
	ld		d,a

	ld 	    a, (charY)
	add		a,a
	add		a,a
	add		a,a
	ld		e,a

	push	bc
	call 	CalcScreenAddress
	pop		bc
	LD		B,8
	LD		DE, 256
printALoop:
	LD		A,(IX)
	LD		(HL),A
	INC		IX
	ADD		HL, DE
	DJNZ	printALoop

;Advance the pointer, go to next line	
	LD		A, (charX)
	INC		A
	LD		(charX),A
	CP		32
	JR 		C, printAEnd
	call	newline

printAEnd:
	POP		HL
	POP		DE
	POP		BC
	POP		IX
	POP		AF
	ret

printTab:
	ld		a,(charX)
	and		$ff-3
	add		4
	cp		screen_char_maxx 
	jr		nc,printTab1
	ld		(charX),a
	jr		printAEnd
printTab1:
	call	newline
	jr		printAEnd

printNewline:
	ld		a,(charY)
	cp		screen_char_maxy
	jr		c, printNewline2
	inc		a
	ld		(charY),a
	jr		printAEnd
printNewline2:
	call	scroll
	jr		printAEnd


printCarriageReturn:
	call	newline
	jr		printAEnd

; ---------------------------------------------------------------------------
; print 0 terminated text pointed to by hl
; ---------------------------------------------------------------------------

printTextHl:
	 ld		a,(hl)
	 inc	hl
	 cp		0
	 ret	z
	 call	printA
	 jr		printTextHl
; ---------------------------------------------------------------------------
; prints the text after the command
; 		call printtext
; 		db "hello, world",0
;		ld	a,0 ; next assembler command
; ---------------------------------------------------------------------------
printtext:
	pop		hl
printtext1:
	ld		a,(hl)
	inc		hl
	cp		0
	jr		z, printtextend
	call	printA
	jr		printtext1
printtextend:
	push 	hl
	ret
; ---------------------------------------------------------------------------

; ---------------------------------------------------------------------------
; print the text follwing the call, replace parameters
; ld hl, 123
; push hl
; call printf
; db "hl=%lx",0
;
;	Special commands: 	/n = newline, carrage return
;						/axy = Set Attribute x = ink, y = paper
;						/t = tab
;						// = /
;						%d = 8 bit decimal (ld a, 123, push af)
;						%ld = 16 bit decimal (ld hl, 12345, push hl)
;						%x = 8 bit hex
;						%lx = 16 bit hex
;						%c = char (ld a, 'x', push af)
;						%s = text (ld hl, text, push hl)
;						%% = %
; since oder computers like spectrum do not have backslash we use /n instead of \n 
;
; if you have the format string in the data section call printfHL;
;						ld		hl,123
;						push	hl
;						ld		hl,text
;						call	printfHL
;						ret
;		text:			db "Hello, world %ld",0 
; ---------------------------------------------------------------------------

printfBC: dw 0
printf:	
 		ld		hl,0
		add		hl,sp
		pop		hl			; hl points to text following the command
; we can not call the printHL because the parameters are on the stack
; so we set b to 1 as a flag and jump back manually
		ld		(printfBC),bc
		ld		b,1	
		jr		printfLoop
printfExit:		
		ld		bc,(printfBC)
		push	hl
		ret

		
printfHL
		ld		(printfBC),bc
		ld		b,0
printfLoop:
		ld 		a,(hl)
		inc		hl
		cp		0
		jr		z,printfEnd
		cp		'%'
		jr		z, printfpar
		cp		'/'
		jp		z, printfSpecialChar
		call	printA
		jr		printfLoop
printfEnd:
		ld		a,b
		cp		1
		jr		z,printfExit
		ret

printfSpecialChar:
		ld		a,(hl)
		inc		hl
		cp		0
		jr		nz,printfSpecialChar2
		ld		a,'/'
		call 	printA
		jr		printfEnd
printfSpecialChar2:		
		cp		'n'
		jr		z,printfNewLine
		cp		'/'
		jr		z,printfSlash
		cp		't'
		jr		z,printfTab
		cp	    'a'
		jr		z,printfColor
		push	af
		ld		a,'/'
		call	printA
		pop		af
		call	printA 
		jr		printfLoop
		
printfNewLine:
		call	newline
		jr		printfLoop		

printfSlash:
		call	printA
		jr		printfLoop	
printfTab:
		ld		a,8
		call	printA
		jr		printfLoop				
printfColor:
		push 	bc
		ld		a,(hl)
		sub     48
		ld		c,a
		inc		hl
		ld		a,(hl)
		inc	    hl
		sub	 	48
		add		a,a
		add		a,a
		add		a,a	;	*8
		add		a,c
		ld		(charAttrib),a
		pop		bc
		jr		printfLoop		
printfpar:
; Um Werte auszugeben, werden diese auf den Stack gelegt und printf aufgerufen. Die Werte werden in der Funktion gepoppt.
; %@ccll  Print at cc,ll (hex)
; %d	  8 bit signed decimal. ld c, 123, push bc, call printf, db "%d",0
; %ld	  16 bit signed decimal. ld hl, 12345, push hl, call printf, db "%ld",0
; %lx	  16 bit hex. ld hl, 12345, push hl, call printf, db "%lx",0
; %x	  8 bit hex. ld c, 123, push bc, call printf, db "%x",0
; %a      8 bit signed decimal, value in a. ld a, 123, push af, call printf, db "%a",0
; %X      8 bit hex, value in a. ld a, 123, push af, call printf, db "%a",0
; %h      half byte
; %s      String. ld hl, text, push hl, call printf, db "%s",0



		ld 		a,(hl)
		inc		hl
		cp		0
		jr		z,printfEnd
		cp		'@'
		jp		z,printfAt
		cp		'l'
		jr		z,printfLPar
		cp		'x'
		jr		z,printfHex2
		cp		'X'
		jr		z,printfHexa2
		cp		'd'
		jr		z,printfDez2
		cp		'a'
		jr		z,printfDeza2
		cp		'H'
		jr		z,printfDezH2
		cp		'L'
		jr		z,printfDezL2
		cp		'c'
		jr		z,printfC
		cp		'h'
		jr		z,printfHalfByte
		cp		's'
		jr		z,printfString

		push	af
		pop		af
		call	printA
		jp		printfLoop

printfHalfByte:
		pop		de
		ld		a,e
		call	printNibble
		jp		printfLoop
printfC:
		pop		af		
		call	printA
		jp		printfLoop

printfString
		pop		de
		ex		de,hl
		call	printTextHl
		ex		de,hl
		jp		printfLoop
printfHex2:
		pop		de
		ld		a,e
		call	printHex2
		jp		printfLoop
printfHexa2:
		pop		af
		call	printHex2
		jp		printfLoop

printfDez2:
		pop		de
		ld		a,e
		call	printDez2Sgn
		jp		printfLoop
printfDeza2:
		pop		af
		call	printDez2Sgn
		jp		printfLoop
printfDezH2:
		pop		de
		ld 	    a,d
		call	printDez2Sgn
		jp		printfLoop
printfDezL2:
		pop		de
		ld 	    a,e

		call	printDez2Sgn
		jp		printfLoop

printfLPar:
		ld 		a,(hl)
		inc		hl
		cp		0
		jp		z,printfEnd
		cp		'd'
		jr		z,printfDez4
		cp		'x'
		jr		z,printfHex4
		push	af
		ld		a,'%'
		call	printA
		ld		a,'l'
		call	printA
		pop		af
		call	printA
		jp		printfLoop

printfAt:
		push	bc
		call	readHex2HL
		ld		b,a
		call	readHex2HL
		ld		c,a
		call	printSetAt
		pop		bc
		jp	    printfLoop



printfDez4:
		pop		de
		ex		hl,de
		
		call	printDezHlSign
		ex		hl,de
		jp		printfLoop

printfHex4:
		ld		de,bc
		pop		bc
		call	printHex4
		ld		bc,de
		jp		printfLoop
debugvarA:
		ex      af,af
		pop		hl
debugvarA1:
		ld		a,(hl)
		inc		hl
		cp		0
		jr		z, debugvarAEnd
		call	printA
		jr		debugvarA1
debugvarAEnd:
		push 	hl
		ex      af,af
		call     printDez2Sgn
		ret

debugvarHL:
		ld      de,hl
		pop		hl
debugvarHL1:
		ld		a,(hl)
		inc		hl
		cp		0
		jr		z, debugvarHLEnd
		call	printA
		jr		debugvarHL1
debugvarHLEnd:
		push 	hl
		ld      hl,de
		call     printDezHlSign
		ret

printHL:
	PUSH	HL
	PUSH	AF
printHLLoop:
	LD		A,(HL)
	CP		0
	JR		NZ, printH1
	POP		AF
	POP		HL
	RET
printH1:
	call	printA
	INC		HL
	JR		printHLLoop	

newline:
	PUSH	AF
	LD		A,0
	LD		(charX),A
	LD		A,(charY)
	INC		A
	CP		A,24
	JR		C, newline2
	DEC		A
	call	scroll
newline2:
	LD		(charY),A
	POP		AF
	RET

readHex2HL:	
	push	bc
	ld		a,(hl)
	inc		hl
	call	readNibbleA
	add		a,a
	add		a,a	
	add		a,a
	add		a,a
	ld		b,a
	ld		a,(hl)
	inc		hl
	call	readNibbleA
	add		b
	pop		bc
	ret

readNibbleA:
	cp	'A'
	jr	z,readNibbleAA
	jr	nc,readNibbleAA
	sub	'0'
	ret
readNibbleAA:
	sub 'A'-10
	ret	


clearTextLine:
	PUSH	HL
	PUSH	BC
	PUSH	AF
	LD		B,8
	add		a,a
	add		a,a
	add		a,a

clearTextLineLoop:
	PUSH	AF
	PUSH	BC
	call	calcLine
	LD		B,32
	LD		A,0
ckearTextLineLoop1:
	LD		(HL),A
	INC		HL
	DJNZ	ckearTextLineLoop1
	POP		BC
	POP		AF
	INC		A
	DJNZ	clearTextLineLoop
	POP		AF
	POP		BC
	POP		HL
	RET
scrollUp:
	PUSH	AF
	PUSH	BC
	PUSH	HL
	PUSH	DE
	LD		A,	0
	LD		B,	192-8
scrollUpLoop:
	PUSH	AF
	PUSH	BC
	call	calcLine
	PUSH	HL
	ADD		A,8
	call	calcLine
	POP		DE
	LD		BC, 32
	LDIR
	POP		BC
	POP		AF
	INC		A
	DJNZ	scrollUpLoop
	LD		A,192-8
	call	clearTextLine
	POP		DE
	POP		HL
	POP		BC
	POP		AF
	RET

me:			call	printf
			db		"/tI am from Hamburg/n",0
			ret

; b = x; c = y
printSetAt:			push	af
					ld 		a,c
					ld 		(charY),a		
					ld		a,b
					ld		(charX),a
					pop		af
					ret

printNibble:		CP		10						; Emits a Nibble 0..9/A..F in A. 
					JR		C, printNibbleDigit
					ADD		A,55					; A = 65 - 10 = 55. If register A contains 10, we will emit "A"
					JR		printNibble2
printNibbleDigit:	ADD		A,48
printNibble2:		jp		printA					; Insteas of CALL/RET a JR is used

printHex2Sgn:       push    af
                    and     $80
                    jr      z, printHex2Sgn2
                    ld      A,'-'
                    call    printA
                    pop     af
                    neg     
                    jp      printHex2
printHex2Sgn2:      pop     af
                    jp      printHex2

printDez2Sgn:       push    af
                    and     $80
                    jr      z, printDez2Sgn2
                    ld      A,'-'
                    call    printA
                    pop     af
                    neg     
                    jr      printDezA
printDez2Sgn2:      pop     af
                    jr      printDezA


printDezA:       	di
					push    hl
					push    de
					push    bc
					push    af
					push    ix
                    push    iy
					LAlloc	5
                    ld      b,0
					push 	ix
					pop		iy
                    ld      (iy),0
                    inc     iy
printdezLoop:       ld      l,a
                    ld      h,0
                    ld      d,10
                    push    hl
                    call    DivHLxD
                    push    hl
                    pop     bc
                    ld      h,l
                    ld      e,10
                    call    MulHxD
                    pop     de
                    
                    ex      hl,de
                    sub     hl,de
                    ld      a,l
                    add     48
                    ld      (iy),a
                    inc     iy
                    ld      a,c
                    cp      0
                    jr      nz, printdezLoop

                    dec     iy
printdezLoop2:      ld      a,(iy)
                    cp      0
                    jr      z,printdezEnd
                    call    printA
                    dec     iy
                    jr      printdezLoop2
printdezEnd:        LRelease 5	


                    pop     iy
                    pop     ix
					pop     af 
					pop     bc
					pop     de
					pop   	hl
					ei
                    ret

printDezHlSign:     push    af
                    ld      a,h
                    and     $80
                    jr      z,printDezHlSign_1
                    push    hl
                    push    de
                    ld      a,'-'
                    call    printA
                    ld      de,hl
                    ld      hl,0
                    sbc     hl,de
                    call    printDezHL
                    pop     de
                    pop     hl
                    pop     af
                    ret
printDezHlSign_1:   call    printDezHL
                    pop     af
                    ret
printDezHL:       	push    ix
                    push    iy
                    push    hl
                    push    de
                    push    bc
					LAlloc  10



                    ld      (ix),0
                    inc     ix

printdezHLLoop:     
                    ld      d,10
                    push    hl                          ; stack = hl
                    call    DivHLxD
                    
                    push    hl                          
                    pop     bc
                    ld      de,10
                    call    Mul16BCxDE
                    pop     de                          ; de = zahl, z.b. 1234
                                                        ; hl = zahl/10*10 z.b. 1230                    
                    ex      hl,de
                    sub     hl,de
                    ld      a,l
                    add     48
                    ld      (ix),a
                    inc     ix
                    ld      hl,bc
                    ld      a,h
                    cp      0
                    jr      nz, printdezHLLoop
                    ld      a,l
                    cp      0
                    jr      nz, printdezHLLoop

                    dec     ix
printdezHLLoop2:      ld      a,(ix)
                    cp      0
                    jr      z,printdezHLEnd
                    call    printA
                    dec     ix
                    jr      printdezHLLoop2
printdezHLEnd:      LRelease 10	

                    pop     bc
                    pop     de
                    pop     hl
                    pop     iy
                    pop     ix
 
                    ret                    
printHex2:			PUSH	AF
					PUSH	AF						; Writes the HEX number in A to the Output
					SRA		A
					SRA		A
					SRA		A
					SRA		A
					AND		$0F
					call	printNibble
					POP		AF
					AND		15
					CALL	printNibble
					POP		AF
					RET

printHex4Hl			push    hl 
					push    bc
					push    af
					ld		b,h
					ld		c,l
					call	printHex4
					pop		af
					pop		bc
					pop		hl
					ret

printHex4:			LD		A,B
					call	printHex2
					LD		A,C
					JR		printHex2

printPagedHL:		PUSHA
printPagePage:		call	clearScreen 
					ld		bc,0
					ld		de, charX
					call	printSetAt
printPageCharLoop:	ld		a,(charX)
					cp		32
					jr		nz, printPageCharLoop1
					call	newline
printPageCharLoop1:										
					ld		a,(hl)
					inc		hl
					cp		0
					jr		z,printPageHLEndWaitKey
					cp		' '
					call	z,printPageCheckWordWrap
					
					call	printA
					ld		a,(charY)
					cp		22
					jr		nz,printPageCharLoop
					push	hl
					call	printf
					db		"/nq quit, any key next page",0
					pop		hl
					call	GetKey
					cp		'Q'
					jr		z, printPageHLEnd
					call	clearScreen
					ld		bc,0
					call	printSetAt
					jr		printPageCharLoop



printPageCheckWordWrap:
					push	af
					push	bc
					push	hl
					inc		hl
					ld		a,(hl)
					cp		0
					jr		z, printPageCheckWordWrapEnd
					cp		10
					jr		z, printPageCheckWordWrapEnd
					cp		13
					jr		z, printPageCheckWordWrapEnd
					cp		9
					jr		z, printPageCheckWordWrapEnd
// check lenght of word
					call	wordlenHL
					cp		32
					jr		nc, printPageCheckWordWrapEnd
					ld		b,a
					ld		a,(charX)
					add		a,b
					cp		30
					jr		c, printPageCheckWordWrapEnd
					call	newline 					
					pop		hl
					pop		bc
					pop		af
					ld		a,(hl)
					inc		hl
					ret					

printPageCheckWordWrapEnd:
					pop		hl
					pop		bc
					pop		af
					ret		

printPageHLEndWaitKey:
					call	GetKey									

printPageHLEnd:		POPA	
					ret

; wordlen. Input hl = String
; 			output: a = Length of word (until 10, 13, 9, 0, 32)
wordlenHL:			push 	HL	
					push	bc
					ld		b,0
wordlenHLLoop		ld		a,(hl)
					inc		hl
					cp		0
					jr		z, wordLenEnd
					cp		10
					jr		z, wordLenEnd
					cp		13
					jr		z, wordLenEnd
					cp		9
					jr		z, wordLenEnd
					cp		32
					jr		z, wordLenEnd
					inc		b
					jr		wordlenHLLoop
wordLenEnd:			ld		a,b
					pop		bc
					pop		hl
					ret					
														

charX				db		0
charY				db		0


charAttrib			db		WHITE*PAPER+BLACK

	DISPLAY "Print Size =",/D, $-PRINT_START, " bytes"