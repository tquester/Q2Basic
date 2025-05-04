ZX_START: 
screen_maxx             EQU     256
screen_maxy             EQU     192
screen_byte_line        equ     32
screen_size             EQU     32*192
screen_start            EQU     $4000
screen_len				EQU 	6144

SCREEN_ATTR				EQU		$4000+6144
attrib_len				equ		768
screen_char_maxx        EQU     32
screen_char_maxy        EQU     24

CHARSET				    EQU		15360

PAPER					EQU		8

BLACK					equ 	0
BLUE					equ		1
RED						equ		2
PINK					equ		3
GREEN					equ		4
LIGHTBLUE				equ		5
YELLOW					equ		6
WHITE					equ		7

ZX_BLACK					equ 	0
ZX_BLUE					equ		1
ZX_RED						equ		2
ZX_PINK					equ		3
ZX_GREEN					equ		4
ZX_LIGHTBLUE				equ		5
ZX_YELLOW					equ		6
ZX_WHITE					equ		7
;ATTR_P                                  equ             23693
  

ZX_KSTATE				equ	23552; N8	Used in reading the keyboard.
ZX_LAST_K				equ	23560; Nl	Stores newly pressed key.
ZX_REPDEL				equ	23561; 1	Time (in 50ths of a second in 60ths of a second in N. America) that a key must be held down before it repeats. This starts off at 35, but you can POKE in other values.
ZX_REPPER				equ	23562; 1	Delay (in 50ths of a second in 60ths of a second in N. America) between successive repeats of a key held down: initially 5.
ZX_DEFADD				equ	23563; N2	Address of arguments of user defined function if one is being evaluated; otherwise 0.
ZX_K_DATA				equ	23565; Nl	Stores 2nd byte of colour controls entered from keyboard .
ZX_TVDATA				equ	23566; N2	Stores bytes of coiour, AT and TAB controls going to television.
ZX_STRMS				equ	23568; X38	Addresses of channels attached to streams.
ZX_CHARS				equ	23606; 2	256 less than address of character set (which starts with space and carries on to the copyright symbol). Normally in ROM, but you can set up your own in RAM and make CHARS point to it.
ZX_RASP					equ	23608; 1	Length of warning buzz.
ZX_PIP					equ	23609; 1	Length of keyboard click.
ZX_ERR_NR				equ	23610; 1	1 less than the report code. Starts off at 255 (for 1) so PEEK 23610 gives 255.
ZX_FLAGS				equ	23611; X1	Various flags to control the BASIC system.
ZX_TV_FLAG				equ	23612; X1	Flags associated with the television.
ZX_ERR_SP				equ	23613; X2	Address of item on machine stack to be used as error return.
ZX_LIST_SP				equ	23615; N2	Address of return address from automatic listing.
ZX_MODE					equ	23617; N1	Specifies K, L, C. E or G cursor.
ZX_NEWPPC				equ	23618; 2	Line to be jumped to.
ZX_NSPPC				equ	23620; 1	Statement number in line to be jumped to. Poking first NEWPPC and then NSPPC forces a jump to a specified statement in a line.
ZX_PPC_Line 			equ	23621; 2	number of statement currently being executed.
ZX_SUBPPC				equ	23623; 1	Number within line of statement being executed.
ZX_BORDCR				equ	23624; 1	Border colour * 8; also contains the attributes normally used for the lower half of the screen.
ZX_E_PPC				equ	23625; 2	Number of current line (with program cursor).
ZX_VARS					equ	23627; X2	Address of variables.
ZX_DEST					equ	23629; N2	Address of variable in assignment.
ZX_CHANS				equ	23631; X2	Address of channel data.
ZX_CURCHL				equ	23633; X2	Address of information currently being used for input and output.
ZX_PROG					equ	23635; X2	Address of BASIC program.
ZX_NXTLIN				equ	23637; X2	Address of next line in program.
ZX_DATADD				equ	23639; X2	Address of terminator of last DATA item.
ZX_E_LINE				equ	23641; X2	Address of command being typed in.
ZX_K_CUR				equ	23643; 2	Address of cursor.
ZX_CH_ADD				equ	23645; X2	Address of the next character to be interpreted: the character after the argument of PEEK, or the NEWLINE at the end of a POKE statement.
ZX_X_PTR				equ	23647; 2	Address of the character after the ? marker.
ZX_WORKSP				equ	23649; X2	Address of temporary work space.
ZX_STKBOT				equ	23651; X2	Address of bottom of calculator stack.
ZX_STKEND				equ	23653; X2	Address of start of spare space.
ZX_BREG					equ	23655; N1	Calculator's b register.
ZX_MEM					equ	23656; N2	Address of area used for calculator's memory. (Usually MEMBOT, but not always.)
ZX_FLAGS2				equ	23658; 1	More flags.
ZX_DF_SZ				equ	23659; X1	The number of lines (including one blank line) in the lower part of the screen.
ZX_S_TOP				equ	23660; 2	The number of the top program line in automatic listings.
ZX_OLDPPC				equ	23662; 2	Line number to which CONTINUE jumps.
ZX_OSPCC				equ	23664; 1	Number within line of statement to which CONTINUE jumps.
ZX_FLAGX				equ	23665; N1	Various flags.
ZX_STRLEN				equ	23666; N2	Length of string type destination in assignment.
ZX_T_ADDR				equ	23668; N2	Address of next item in syntax table (very unlikely to be useful).
ZX_SEED					equ	23670; 2	The seed for RND. This is the variable that is set by RANDOMIZE.
ZX_FRAMES				equ	23672; 3	3 byte (least significant first), frame counter. Incremented every 20ms. See Chapter 18.
ZX_UDG					equ	23675; 2	Address of 1st user defined graphic You can change this for instance to save space by having fewer user defined graphics.
ZX_COORDSX				equ	23677; 1	x-coordinate of last point plotted.
ZX_COORDSY 				equ	23678; 1	y-coordinate of last point plotted.
ZX_P_POSN				equ	23679; 1	33 column number of printer position
ZX_PR_CC				equ	23680; X2	Full address of next position for LPRINT to print at (in ZX printer buffer). Legal values 5B00 - 5B1F. [Not used in 128K mode or when certain peripherals are attached]
ZX_ECHO_E				equ	23682; 2	33 column number and 24 line number (in lower half) of end of input buffer.
ZX_DF_CC				equ	23684; 2	Address in display file of PRINT position.
ZX_DFCCL				equ	23686; 2	Like DF CC for lower part of screen.
ZX_S_POSN_COL			equ	23688; X1	33 column number for PRINT position
ZX_S_POSN_LINE 			equ	23689; X1	24 line number for PRINT position.
ZX_SPOSNL				equ	23690; X2	Like S POSN for lower part
ZX_SCR_CT				equ	23692; 1	Counts scrolls: it is always 1 more than the number of scrolls that will be done before stopping with scroll? If you keep poking this with a number bigger than 1 (say 255), the screen will scroll on and on without asking you.
ZX_ATTR_P				equ	23693; 1	Permanent current colours, etc (as set up by colour statements).
ZX_MASK_P				equ	23694; 1	Used for transparent colours, etc. Any bit that is 1 shows that the corresponding attribute bit is taken not from ATTR P, but from what is already on the screen.
ZX_ATTR_T				equ	23695; N1	Temporary current colours, etc (as set up by colour items).
ZX_MASK_T				equ	23696; N1	Like MASK P, but temporary.
ZX_P_FLAG				equ	23697; 1	More flags.
ZX_MEMBOT				equ	23698; N30	Calculator's memory area; used to store numbers that cannot conveniently be put on the calculator stack.
ZX_NMIADD				equ	23728; 2	This is the address of a user supplied NMI address which is read by the standard ROM when a peripheral activates the NMI. Probably intentionally disabled so that the effect is to perform a reset if both locations hold zero, but do nothing if the locations hold a non-zero value. Interface 1's with serial number greater than 87315 will initialize these locations to 0 and 80 to allow the RS232 "T" channel to use a variable line width. 23728 is the current print position and 23729 the width - default 80.
ZX_RAMTOP				equ	23730; 2	Address of last byte of BASIC system area.
ZX_P_RAMT				equ	23732; 2	Address of last byte of physical RAM.

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




GetKeyOrJoystick		call	ReadKeyboard
						cp		0
						jr		nz, GetKeyOrJoystick2
						ld 		bc,31
						in 		a,(c)
						and		31
						ld		b,a
;						push	af
;						push	bc
;						ld		c,b
;						push	bc
;						call	printf
;						db		"%@0002K:%x  ",0
;						pop		bc
;						pop		af
						cp		0
						jr		z, GetKeyOrJoystick
						ld		a,$ff
						ret

GetKeyOrJoystick2:		push 	af
GetKeyOrJoystick3:		call    ReadKeyboard
                        cp      0
                        jr      nz,GetKeyOrJoystick3
                        pop     af
                        ret				

GetKey:                 call    ReadKeyboard
                        cp      0
                        jr      z,GetKey

WaitKeyRelease:         push af
WaitKeyRelease1:        call    ReadKeyboard
                        cp      0
                        jr      nz,WaitKeyRelease1
                        pop     af
                        ret
ReadKeyboard:           
                        PUSH    HL
                        PUSH    DE
                        PUSH    BC    
 
                        LD HL,Keyboard_Map                      ; Point HL at the keyboard list
                        LD D,8                                  ; This is the number of ports (rows) to check
                        LD C,$FE                                ; C is always FEh for reading keyboard ports
Read_Keyboard_0:        LD B,(HL)                               ; Get the keyboard port address from table
                        INC HL                                  ; Increment to list of keys
                        IN A,(C)                                ; Read the row of keys in
                        AND $1F                                 ; We are only interested in the first five bits
                        LD E,5                                  ; This is the number of keys in the row
Read_Keyboard_1:        SRL A                                   ; Shift A right; bit 0 sets carry bit
                        JR NC,Read_Keyboard_2                   ; If the bit is 0, we've found our key
                        INC HL                                  ; Go to next table address
                        DEC E                                   ; Decrement key loop counter
                        JR NZ,Read_Keyboard_1                   ; Loop around until this row finished
                        DEC D                                   ; Decrement row loop counter
                        JR NZ,Read_Keyboard_0                   ; Loop around until we are done
                        AND A                                   ; Clear A (no key found)
                        POP     BC
                        POP     DE
                        POP     HL
                        RET
Read_Keyboard_2:		
                        LD A,(HL)                               ; We've found a key at this point; fetch the character code!
                        POP     BC
                        POP     DE
                        POP     HL
                        RET

ReadMKeyboard:           
                        PUSH    HL
                        PUSH    DE
                        PUSH    BC    
						PUSH	IX
;						ld		bc,$0112
;						call	printSetAt  						
;						ld      a,$12*8
;						call    clearTextLine						

						ld		ix,ReadKeyboardPressedKeys
						ld		(ix),0
                        LD HL,Keyboard_Map                      ; Point HL at the keyboard list
                        LD D,8                                  ; This is the number of ports (rows) to check
                        LD C,$FE                                ; C is always FEh for reading keyboard ports
ReadMKeyboard_0:        LD B,(HL)                               ; Get the keyboard port address from table
                        INC HL                                  ; Increment to list of keys
                        IN A,(C)                                ; Read the row of keys in
                        AND $1F                                 ; We are only interested in the first five bits
;						call	printHex2
;						push	af
;						ld		a,32
;						call	printA
;						pop 	af
                        LD E,5                                  ; This is the number of keys in the row
ReadMKeyboard_1:        SRL A                                   ; Shift A right; bit 0 sets carry bit
                        JR C,ReadMKeyboard_2                   ; If the bit is 0, we've found our key
						push	af
						ld		a,(hl)
						ld		(ix),a
						inc		ix
						ld		(ix),0
						pop		af
ReadMKeyboard_2:								
                        INC HL                                  ; Go to next table address
                        DEC E                                   ; Decrement key loop counter
                        JR NZ,ReadMKeyboard_1                   ; Loop around until this row finished
                        DEC D                                   ; Decrement row loop counter
                        JR NZ,ReadMKeyboard_0                   ; Loop around until we are done
                        AND A                                   ; Clear A (no key found)
						ld		hl,ReadKeyboardPressedKeys
;						ld		bc,$0014
;						call	printSetAt
;						call	printHL
						POP		IX
                        POP     BC
                        POP     DE
                        POP     HL
                        RET

		

ReadKeyboardPressedKeys:defs 5*8+1,0						

cls:
                        LD		HL,$4000
                        LD		BC,6144
cls1:
                        LD		A,$00
                        LD		(HL),A
                        INC		HL
                        DEC		BC
                        LD    A,B
                        OR    C
                        JP		NZ, cls1
                        LD		BC,768
cls2:
                        LD		A,BLACK*8+GREEN
                        LD		(HL),A
                        INC		HL
                        DEC		BC
                        LD    A,B
                        OR    C
                        JP		NZ, cls2
                        RET

changeScreenAttrib:
                push    hl
                push    de
                ld      a,(screenCurAttrib)
                inc     a
                cp      screenMaxAttrib
                jr      c,changeScreenAttrib2
                ld      a,0
changeScreenAttrib2:
                ld      (screenCurAttrib),a
                call    setCurrentScreenAttributes
                pop     de
                pop     hl
                ret

setCurrentScreenAttributes:                
                ld      a,(screenCurAttrib);
                ld      e,a
                ld      d,0
                ld      hl,screenAttribs
                add     hl,de
                ld      a,(hl)
                call    setGameScreenAttributes
                ld      hl,SCREEN_ATTR+32*16
                ld      bc,(24-16)*32
                ld      a,LIGHTBLUE*PAPER+BLACK
                call    setGameScreenAttributesHLBC

                ret

; a = attribute
setGameScreenAttributes
                ld      hl,SCREEN_ATTR
                ld      bc,32*16
setGameScreenAttributesHLBC
                push    de
                ld      d,a
setGameScreenAttributesLoop:
                ld      (hl),d
                inc     hl
                dec     bc
                ld      a,c
                or      a
                jr      nz, setGameScreenAttributesLoop
                ld      a,b
                or      a
                jr      nz, setGameScreenAttributesLoop
                pop     bc
                ret

screenAttribs:  db      BLACK*PAPER+WHITE
                db      WHITE*PAPER+BLACK
                db      BLUE*PAPER+WHITE
                db      WHITE*PAPER+BLUE
screenCurAttrib db      0
screenMaxAttrib equ     screenCurAttrib-screenAttribs						

clearScreen:		push	af
					push	bc
					push	hl
					ld		hl, screen_start
					ld		bc, screen_len
clearScreenLoop:	ld		a,0
					ld		(hl),a
					inc		hl
					dec		bc
					ld		a,c
					cp		0
					jr		nz,clearScreenLoop
					ld		a,b
					cp		0
					jr		nz,clearScreenLoop

					ld		hl, SCREEN_ATTR
					ld		bc, attrib_len
					
clearAttribLoop:	ld		a, WHITE*PAPER+BLACK
					ld		(hl),a
					inc		hl
					dec		bc
					ld		a,c
					cp		0
					jr		nz,clearAttribLoop
					ld		a,b
					cp		0
					jr		nz,clearAttribLoop

					pop		hl
					pop		bc
					pop		af
					ret 	


clearLowerScreenWhite:
					ld		a,WHITE*PAPER+BLACK
					jr  	clearLowerScreen
clearLowerScreenBlue:
					ld		a,LIGHTBLUE*PAPER+BLACK


; clears the screen from line 16 to 24 with the color in a
clearLowerScreen:	push	bc
					push	hl
					push	af
					ld		c,129
					ld		b,192-129
clearLowerScreen1:	ld		a,c
					call	calcLine
					push	bc
					ld		b,32
					ld		a,0
clearLowerScreen2:	ld		(hl),a
					inc		hl
					djnz	clearLowerScreen2
					pop		bc
					inc		c
					djnz	clearLowerScreen1

					pop		af
					ld		hl,SCREEN_ATTR+32*16
					ld		b,(24-16)*32
clearLowerScreen3:	ld		(hl),a
					inc		hl
					djnz	clearLowerScreen3
					pop		hl
					pop		bc
					ret																								

markScreen:		push	af
					push	bc
					push	hl
					ld		hl, 0x4000
					ld		bc, 6144
markScreenLoop:		ld		a,(hl)
					cp		0
					jr		nz,markScreenLoop1
					ld		(hl),$aa
markScreenLoop1:			
					inc		hl
					dec		bc
					ld		a,c
					cp		0
					jr		nz,markScreenLoop
					ld		a,b
					cp		0
					jr		nz,markScreenLoop
					pop		hl
					pop		bc
					pop		af
					ret 	

beep:	ld		a,(border)		; fetch border
		or		$08

beeploop2:
		push 	bc
		xor  %00010000
		out	 ($fe),a
beeploop1:
		dec	 c
		jr	nz, beeploop1
		cp	b,0
		jr	z,beeploop1x
		ld	e,a
		ld	 a,b
		cp	 0
		jr	z,beeploop1x
		ld	 a,e
		dec  b
		jr	nz, beeploop1
beeploop1x:		
		ld 	a,e
		pop	bc
		ld	 e,a
		dec  l
		jr   nz,beeploop2
		ld	 a,h
		cp	0
		jr	z,beeploop2x
		ld	a,e
		dec	 h
beeploop2x:		
		jr	 nz,beeploop2
		ret


border:	db	7
                     
; --------------- data section ------------------------- 
KEY_SHIFT:              equ     1
KEY_SYMBOL_SHIFT:		equ		2	
Keyboard_Map:           DB $FE, 1,"Z","X","C","V"
                        DB $FD,"A","S","D","F","G"
                        DB $FB,"Q","W","E","R","T"
                        DB $F7,"1","2","3","4","5"
                        DB $EF,"0","9","8","7","6"
                        DB $DF,"P","O","I","U","Y"
                        DB $BF,13,"L","K","J","H"
                        DB $7F," ",2,"M","N","B"   


	DISPLAY "ZX Size =",/D, $-ZX_START, " bytes"