ZXB_CLEAR       EQU     $FD
ZXB_VAL         EQU     $B0
ZXB_INPUT       EQU     $EE
ZXB_LET			equ		#F1
ZXB_LOAD        EQU     $EF
ZXB_CODE        EQU     $AF
ZXB_RANDOMIZE   EQU     $F9
ZXB_USR         EQU     $C0
ZX_PRINT		equ	#F5
ZX_STOP		    equ	#E2

spectrum	equ	#A3
zxb_play		equ	#A4
zxb_rnd		equ	#A5
zxb_inkey		equ	#A6
zxb_pi		equ	#A7
zxb_fn		equ	#A8
zxb_point		equ	#A9
zxb_screen		equ	#Aa
zxb_attr		equ	#Ab
zxb_at		equ	#Ac
zxb_tab		equ	#Ad
zxb_valS		equ	#Ae
zxb_code		equ	#Af
zxb_val		equ	#B0
zxb_len		equ	#B1
zxb_sin		equ	#B2
zxb_cos		equ	#B3
zxb_tan		equ	#B4
zxb_asn		equ	#B5
zxb_acs		equ	#B6
zxb_atn		equ	#B7
zxb_ln		equ	#B8
zxb_exp		equ	#B9
zxb_int		equ	#Ba
zxb_sqr		equ	#Bb
zxb_sgn		equ	#Bc
zxb_absx		equ	#Bd
zxb_peek		equ	#Be
zxb_in		equ	#Bf
zxb_usr		equ	#C0
zxb_str		equ	#C1
zxb_chr		equ	#C2
zxb_notx		equ	#C3
zxb_bin		equ	#C4
zxb_orx		equ	#C5
zxb_andx		equ	#C6
zxb_line		equ	#Ca
zxb_then		equ	#Cb
zxb_to		equ	#Cc
zxb_step		equ	#Cd
zxb_deffn		equ	#Ce
zxb_cat		equ	#Cf
zxb_format		equ	#D0
zxb_move		equ	#D1
zxb_erase		equ	#D2
zxb_open		equ	#D3
zxb_close		equ	#D4
zxb_merge		equ	#D5
zxb_verify		equ	#D6
zxb_beep		equ	#D7
zxb_circle		equ	#D8
zxb_ink		equ	#D9
zxb_paper		equ	#Da
zxb_flash		equ	#Db
zxb_bright		equ	#Dc
zxb_inverse		equ	#Dd
zxb_over		equ	#De
zxb_out		equ	#Df
zxb_lprint		equ	#E0
zxb_llist		equ	#E1
zxb_stop		equ	#E2
zxb_read		equ	#E3
zxb_data		equ	#E4
zxb_restore		equ	#E5
zxb_new		equ	#E6
zxb_border		equ	#E7
zxb_cont		equ	#E8
zxb_continue	equ	#E8
zxb_dim		equ	#E9
zxb_rem		equ	#Ea
zxb_for		equ	#Eb
zxb_goto		equ	#Ec
zxb_gosub		equ	#Ed
zxb_input		equ	#Ee
zxb_load		equ	#Ef
zxb_list		equ	#F0
zxb_let		equ	#F1
zxb_zxb_pause		equ	#F2
zxb_next		equ	#F3
zxb_poke		equ	#F4
zxb_print		equ	#F5
zxb_plot		equ	#F6
zxb_run		equ	#F7
zxb_save		equ	#F8
zxb_rand		equ	#F9
zxb_randomize	equ	#F9
zxb_if		equ	#Fa
zxb_cls		equ	#Fb
zxb_draw		equ	#Fc
zxb_clear		equ	#Fd
zxb_return		equ	#Fe
zxb_copy		equ	#Ff


ZXB_GOTO		equ	#Ec



line_useval	=	0
line_number	=	10
line_step	=	10

;; Begin of basic line

LINE  MACRO
	ASSERT line_number < #4000 , Line number overflows
	db	high line_number
	db	low line_number
	LUA ALLPASS
	sj.parse_code('dw line_' .. tostring(sj.calc("line_number")) .. '_length')
	sj.parse_line(   'line_' .. tostring(sj.calc("line_number")) .. '_begin')
	ENDLUA
      ENDM

;; End of basic line

LEND  MACRO
	db	#0D
	LUA ALLPASS
	sj.parse_line('line_'
		.. tostring(sj.calc("line_number"))
		.. '_length = $ - line_'
		.. tostring(sj.calc("line_number"))
		.. '_begin')
	ENDLUA
line_number  =	line_number + line_step
      ENDM

SETLINE MACRO	number
line_number = number
	ENDM

LAlloc     		MACRO   value
				push	hl
				push	de
				push	bc
                ld	hl,0
                add	hl,sp
                ld	de,value
                sub	hl,de
                ld	ix,hl
                ld	sp,hl
				ld	bc,(ix+value)
				ld	de,(ix+value+2)
				ld	hl,(ix+value+4)
                ENDM

LRelease	    MACRO   value
                ld		hl,0
                add		hl,sp
                ld		de,value
                add		hl,de
                ld		sp,hl	  
				pop		bc
				pop		de
				pop		hl              
                ENDM

LAllocNoPush	MACRO   value
                ld	hl,0
                add	hl,sp
                ld	de,value
                sub	hl,de
                ld	ix,hl
                ld	sp,hl
				ld	bc,(ix+value)
				ld	de,(ix+value+2)
				ld	hl,(ix+value+4)
                ENDM

LReleaseNoPush  MACRO   value
                ld		hl,0
                add		hl,sp
                ld		de,value
                add		hl,de
                ld		sp,hl	  
                ENDM				

PUSHA           MACRO    
                    PUSH    AF
                    PUSH    BC
                    PUSH    DE
                    PUSH    HL
                ENDM

POPA           MACRO   
                    POP     HL
                    POP     DE
                    POP     BC
                    POP     AF
                ENDM	


DEBUG_PRINT   macro x 
                if DEBUG=1
                push bc
                push de
                push hl
                call printf
                db x,0
                pop hl
                pop de
                pop bc
                endif
                endm
DEBUG_PRINT_REGISTER macro text, reg 
          if DEBUG=1
          push    hl
          push    de
          push    bc
          push    af 
          call printf
          db text,0
          pop     af
          pop     bc
          pop     de
          pop     hl
          ld      a,reg
          push    hl
          push    de
          push    bc
          push    af 
          call   printDezA
          pop     af
          pop     bc
          pop     de
          pop     hl
          endif
          endm

STACKTOA: macro  text
          if DEBUG=1
          call newline
          call printf 
          db text,0
          endif 
          call ROM_STKTOBC
          if DEBUG=1
          push  bc
          call printHex4
          call GetKey
          pop  bc
          endif
          ld   a,c
          endm 

PRINTREG  macro text, reg
          if DEBUG=1
          push hl
          push af
          push bc
          push de
          push hl
          call printf
          db text,0
          pop  hl
          ld  a,reg
          call printDezA
          pop de
          pop bc
          pop af
          pop hl
          endif
          endm

PRINTREG16  macro text, reg
          if DEBUG=1
          push af
          push bc
          push de
          push hl
          call printf
          db text,0
          pop  hl
          pop de
          pop bc
          pop af
          push reg
          push hl
          push reg 
          pop  hl
          call printDezHL
          pop  hl
          pop  reg
          endif
          endm

;; Include number value into basic line

NUM   MACRO	value
	IF line_useval
	  db	val,'"'
	ENDIF
	  LUA ALLPASS
	  sj.parse_code('db	"' .. tostring(sj.calc("value")) .. '"')
	  ENDLUA
	IF line_useval
	  db	'"'
	ELSE
	  db	#0E,0,0
	  dw	value
	  db	#00
	ENDIF
      ENDM


    MACRO        MakeTape tape_file?, prog_name?, code_adr?, code_len?

                ORG     #5C00
.bas_start    
				LINE
				DB      ZXB_LET,"codebase=",ZXB_VAL, '"32768"'
				LEND
				LINE	
                DB      ZXB_LOAD, '"'                           ; LOAD "
.code_name      DB      prog_name?                              ; code name
                ASSERT ($ - .code_name) <= 10                   ; (max 10 chars)
                DB      '"',ZXB_CODE,'codebase:'                       ; " CODE a:
				LEND

				LINE	
				DB      ZXB_CLEAR, ZXB_VAL, '"32768":'
                DB      ZXB_LET,"codebase=",ZXB_VAL, '"32768":'
				DB      ZXB_LET,"enablebasic=codebase+",ZXB_VAL, '"3":'
				LEND

				LINE
				db		ZX_STOP	
				LEND

				LINE
				db      zxb_print, zxb_usr, 'enablebasic:'
				db		ZX_STOP
				LEND

                LINE
                db      zxb_cls
                LEND



				LINE 
				db zxb_cls
				LEND

				LINE 
				db zxb_circle, '*', zxb_val,'("100"),',zxb_val,'("100"),',zxb_val,'("50")'
				LEND


				LINE
				db		ZX_STOP	
				LEND

				LINE 
				db  zxb_load, '*"m";', zxb_val, '("1")', ';"basictest1"' 
				LEND


			;	LINE
			;	db		ZXB_GOTO: NUM 10	:
			;	LEND



.bas_len        EQU     $-.bas_start

            EMPTYTAP tape_file?
            SAVETAP  tape_file?,BASIC,prog_name?,.bas_start,.bas_len,1
            ; make CODE-block load address 0, so it must be overriden by "LOAD CODE" explicitly

            SAVETAP  tape_file?,CODE,prog_name?,code_adr?,code_len?,0
			ENDM

        MACRO        MakeBinTape tape_file?, prog_name?, code_adr?, code_len?
        SAVETAP  tape_file?,CODE,prog_name?,code_adr?,code_len?,0
    	ENDM

