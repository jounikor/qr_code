; This is the size "optimized" version that is forced to 3-L
; and mask0. Only byte mode.
; Compiled using Pasmo (see https://github.com/jounikor/pasmo)
; pasmo -1 --tapbas --alocal qrcoder.asm tap.tap tap.map
;

; If defined then 1 pixel "empty" border is added to left
;QR_ADD_BORDER   equ     1

; Following constants need to be defined or reserved space during
; compilation:
;
; PHR_BUF_PTR   overlaid on QR_DST_SIZE
; ENC_BUF_PTR   no alignment, size ENC_BUF_SIZE octets
; QR_DST_PTR    no alignment, size QR_DST_SIZE <- place qr-code here
; GF_G2E_PTR    256 octet alignment, size 256
; GF_E2G_PTR    256 octet alignment, size 256

QR_DIM          equ     29
QR_CWDS         equ     55            
QR_CWDS_EWDS    equ     QR_CWDS+15
QR_MAX_PHRASE   equ     76
QR_SHIFT		equ		1		; output QR-code is shifted right

;
PHR_BUF_SIZE    equ     QR_CWDS
ENC_BUF_SIZE    equ     QR_CWDS_EWDS
QR_DST_SIZE     equ     QR_DIM*4



; Test driving program starts here..

			DEVICE ZXSPECTRUM128
			SLDOPT  COMMENT WPMEM, LOGPOINT, ASSERTION

            org $8000

	DEFINE	TEST_SPECTRUM	1
MASK		equ		0		; 0 or 1


;
; Some sample code..
;
main:
            di
            exx
            push    hl
            push    de
            push    bc
            exx

			; Call only once..
            call    qr_code_init

			; For each generated QR-code
			ld      ix,test_string2
            ld      a,TEST_STR_LEN2
            call    qr_code_generate
    IFDEF TEST_SPECTRUM    
            ld      b,64
            ld      c,112
            call    qr_code_render_spectrum
	ENDIF
			ld      ix,test_string1
            ld      a,TEST_STR_LEN1
            call    qr_code_generate
    IFDEF TEST_SPECTRUM    
            ld      b,64
            ld      c,176
            call    qr_code_render_spectrum
	ENDIF
            exx
            pop     bc
            pop     de
            pop     hl
            exx
            ei

            ret


;TEST_STR_LEN1	equ	22
test_string1:
;            db      "http://www.scoopex.us/"
TEST_STR_LEN1	equ	34
            db      "https://www.deadcoderssociety.net/"

;TEST_STR_LEN2	equ	29
TEST_STR_LEN2	equ	43-1
test_string2:
			db		"http://www.youtube.com/watch?v=a_RNDL10ERg"
			;db		"https://www.youtube.com/watch"
	IFDEF TEST_SPECTRUM
;
; Render QR-Code (on ZX Spectrum..)
;
; Inputs:
;  B = y
;  C = x
;
; Returns:
;  None.
;
; Trashes:
;  A,BC,DE,HL,IX,B'
;
qr_code_render_spectrum:
            ld      ix,QR_DST_PTR
            exx
            ld      b,QR_DIM
_line_loop: ;
            exx
            call    get_address_BC
            
			REPT	4
			ld		a,(ix)
			ld		(hl),a
			inc		ix
			inc		hl
			ENDR
			
			inc     b
            exx
            djnz    _line_loop
            exx
            ret

;
; Calculate screen coordinates from X and Y positions.
; Made just to work.. no attempt to make it fast.
;
; Inputs:
;  B = Y pos
;  C = X pos
;
; Returns:
;  HL = ptr to screen
;
; Trashes:
;  A
;
get_address_BC:
            ld      a,00111000b
            and     b
            add     a,a
            add     a,a
            ld      l,a
            ld      a,11111000b
            and     c
            rrca
            rrca
            rrca
            or      l
            ld      l,a

            ld      a,00000111b
            and     b
            ld      h,a
            ld      a,11000000b
            and     b
            rrca
            rrca
            rrca
            or      h
            add     a,HIGH($4000)
            ld      h,a
            ret
	ENDIF


;----------------------------------------------------------------------------
; Start of the QR-code generator
;
;
			org		($+255) & 0xff00

; Note! the following tables and variabled must be 256 bytes aligned.
;

qr_sta:
patterns:
            ; Generator polynomial for 3-L
gen_15:     db      8,183,61,91,202,37,51,58,58,237,140,124,5,99,105
qr_stm:     db      0                       ;
qr_y_adder:	dw		0
qr_y:       db      0
qr_x:       db      0
qr_m:		db		0
qr_final_mask:
            dw      0
qr_template_empty_bits:
			;		 +                                 +
			db		00000000b,00111111b,11111100b,00000000b
			db		00000000b,00111111b,11111100b,00000000b
			db		00000000b,00111111b,11111100b,00000000b
			db		00000000b,00111111b,11111100b,00000000b
			db		00000000b,00111111b,11111100b,00000000b
			db		00000000b,00111111b,11111100b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000000b,00111111b,11111100b,00000000b
			db		00000000b,00111111b,11111100b,00000000b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111111b,11111100b
			db		01111110b,11111111b,11111000b,00111100b
			db		00000000b,00111111b,11111000b,00111100b
			db		00000000b,00111111b,11111000b,00111100b
			db		00000000b,00111111b,11111000b,00111100b
			db		00000000b,00111111b,11111000b,00111100b
			db		00000000b,00111111b,11111111b,11111100b
			db		00000000b,00111111b,11111111b,11111100b
			db		00000000b,00111111b,11111111b,11111100b
			db		00000000b,00111111b,11111111b,11111100b
			;		      starting pos for encoding    ^  
qr_template_static_bits:
			;		 +                                 +
	IF MASK=0
			; mask pattern 0: 111011111000100
			db		01111111b,00000000b,00000001b,11111100b
			db		01000001b,00000000b,00000001b,00000100b
			db		01011101b,01000000b,00000001b,01110100b
			db		01011101b,00000000b,00000001b,01110100b
			db		01011101b,00000000b,00000001b,01110100b
			db		01000001b,00000000b,00000001b,00000100b
			db		01111111b,01010101b,01010101b,11111100b
			db		00000000b,01000000b,00000000b,00000000b
			db		01110111b,11000000b,00000011b,00010000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000111b,11000000b
			db		00000000b,01000000b,00000100b,01000000b
			db		01111111b,01000000b,00000101b,01000000b
			db		01000001b,01000000b,00000100b,01000000b
			db		01011101b,01000000b,00000111b,11000000b
			db		01011101b,00000000b,00000000b,00000000b
			db		01011101b,01000000b,00000000b,00000000b
			db		01000001b,01000000b,00000000b,00000000b
			db		01111111b,01000000b,00000000b,00000000b
	ENDIF
	IF MASK=1
			; mask pattern 1: 111001 01 1110011
			db		01111111b,01000000b,00000001b,11111100b	; row % 2 = 0
			db		01000001b,01000000b,00000001b,01110100b	; row % 2 = 1
			db		01011101b,00000000b,00000001b,01110100b
			db		01011101b,00000000b,00000001b,01110100b
			db		01011101b,01000000b,00000001b,01110100b
			db		01000001b,01000000b,00000001b,00000100b
			db		01111111b,01010101b,01010101b,11111100b
			db		00000000b,01000000b,00000000b,00000000b
			db		01110011b,01000000b,00000011b,11001100b	;;
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000000b,00000000b
			db		00000000b,00000000b,00000000b,00000000b
			db		00000001b,00000000b,00000111b,11000000b
			db		00000000b,01000000b,00000100b,01000000b
			db		01111111b,00000000b,00000101b,01000000b
			db		01000001b,01000000b,00000100b,01000000b
			db		01011101b,00000000b,00000111b,11000000b
			db		01011101b,00000000b,00000000b,00000000b
			db		01011101b,01000000b,00000000b,00000000b
			db		01000001b,01000000b,00000000b,00000000b
			db		01111111b,01000000b,00000000b,00000000b	; row % 2 = 0
	ENDIF
qr_mid:

;
;
;
;

qr_code_init:		;		One time init of QR-code generation
			ld      b,0 
            ld      hl,GF_E2G_PTR
            ld      a,1
            jr      gf_not_over_256

;----------------------------------------------------------------------------
;
; Initialize QR-Code generation.
;
; Inputs:
;  None.
; 
; Returns:
;  None.
;
; Trashes:
;  Assume all except IY.
;
;qr_code_init:
            ; gf_init and decode_qr_layout need to be called only
            ; once for any number of calculated QR-Codes.
;
; Generate Galois Field tables
gf_main:      ;
            add     a,a
            jr nc,  gf_not_over_256
            ;
            xor     285-256
gf_not_over_256:
            ld      (hl),a
            inc     l
            djnz    gf_main
            ;

            ld      b,254
gf_copy:      ;
            ld      l,b
            ld      a,(hl)      ; from gf_e2g[n]
            inc     h
            ld      l,a
            ld      (hl),b      ; to gf_g2e[f_e2g[n]] = n
            dec     h
            djnz    gf_copy
			ret


;
; Generate QR-Code (assume only 3-L)
;
; Inputs:
;  IX = ptr to phrase. Max length is 76 characters.
;  A = length of the phrase.
;
; Returns:
;  none. (was C_flag = 0 if phrase is too long)
;
; Trashes:
;  IY is not trashed.
;
qr_code_generate:
            ; Make sure text phrase is not longer than 76 characters.
            ;call    encode_phrase
            ;call    polydiv

            ; Encode codewords, ecc words and padding to qr-code bits layout
            ;jp		encode_layout

;
; Encodes the input phrase. This function has a lot of assumption:
; - only alphanumeric coding supported
; - letters must be uppercase and no invalid characters allowed
; - maximum phrase length after encoding is 55 bytes
; - 
; 
; The function takes also care of:
; - adding mode (0010b) + length (9 bits)
; - termination bits (up to 4)
; - alignment to octet boundary
; - padding (if needed)
; - generating ECC code words
;
; Inputs:
;  IX = prt to string phrase
;  A  = string length
;
; Returns:
;  none. (was C_flag = 0 if error, C_flag = 1 if OK.)
;
; Trashes:
;  A,BC,DE,HL,IX
;
encode_phrase:
            ld      hl,PHR_BUF_PTR
            push    hl

			;
            ; Bits needed by alphanumeric encoder are:
            ; b = 4 + c + 11(d DIV 2) + 6(d MOD 2), where
            ;     where c = 9, d = strlen(phrase)
			;
			; Bits needed by byte encder are:
			; b = 4 + c + d*w
			;    where c = 8, d = phrase len
			;

			; pad  bytes 236 and 17 = 11101100b and  00010001b

            ;
            ; In 3-L QR-Code we have 55 octets for code words, which means
            ; D can be 76 alphanumeric characters or 55 bytes.
            ;
            ; B = alphanumeric string len in octets, 9 bits but only 7 bits can be used 
            ;     i.e., 00 nnnnnnn -> A = 0nn nnnnn 
            ; B = byte string len in octets, 8 bits but only 7 will can be used.
            ;     i.e., 00 nnnnnnn -> A = 0nn nnnnn 
			;

            ld      b,a
            rlca
            rlca
            rlca
            rlca
            
			; mode = 0100b and nnnn0000 of length
			and     00001111b
            or      01000000b
            ld      (hl),a
            inc     hl
            ld      a,b

			; 0000nnnn of length
			rld

.main:
			ld		a,(ix+0)
			ld		c,a
			inc		ix
			rrca
			rrca
			rrca
			rrca
			rld
			inc		hl
			ld		a,c
			rld
			djnz	.main
			
			; Add 0000b stop bot sequence
			xor		a		; Clears C_flag
			rld
			inc		hl


			; Sequence is already byte aligned, so check if we need to pad..
            pop     de
			push	hl
            sbc     hl,de
            ld      a,55
            sub     l
            and     a
			pop		hl
            jr z,   .no_alignment

            ;
            ld      b,a
            ld      a,11101100b
.pad:       ld      (hl),a
            inc     hl
            xor     11111101b
            djnz    .pad

.no_alignment:
			;jr		_no_alignment




; Polynomial division.
;
; Inputs:
;  none
;
; Returns:
;  DE = start of ecc words
;
; Trashes:
;  A,B,C,H,L
;
; Note:
;  This routine does not handle the case where polynomial
;  is shorter than generator.
;  Everything is based on 3-L assumptions.

polydiv:    ;

            ld      hl,PHR_BUF_PTR
            ld      de,ENC_BUF_PTR
            ;
            ; Move polynomial into to ecc_buffer for division 
            push    hl          ; polynomial
            push    de          ; code words
            push    de          ; ecc words
            ld      bc,55
            ldir

            ; Clear the rest of the ecc_buffer
            xor     a
            ld      b,15
.clear:     ld      (de),a
            inc     de
            djnz    .clear

            ; polynomial division main loop..
            ld      c,55            ; "polylen"
            pop     hl
            ;
            ; HL = phrase i.e., polynomial
            ; DE = generator

.div_main:  ;
            ; get 'ex' into A
            ld      a,(hl)
            inc     hl
            ld      d,HIGH(GF_G2E_PTR)
            ld      e,a
            ld      a,(de)

            ld      b,15
            ld      de,gen_15
            push    hl

.div_loop:  ;
            push    af
            
            ; A = ex + gen[m] if ex + gen[m] < 256 else ex + gen[m] - 255
            ex      de,hl
            add     a,(hl)
            adc     a,0
            inc     hl
            ex      de,hl

            ; A = e2g[A]
            push    de
            ld      d,HIGH(GF_E2G_PTR)
            ld      e,a
            ld      a,(de)
            pop     de
            
            ; p[n+m] ^ A
            xor     (hl)
            ld      (hl),a
            inc     hl
            pop     af
            ;
            djnz    .div_loop

            ;
            ;
            pop     hl          ; ecc words
            dec     c
            jr nz,  .div_main

            ; move poly at the front of ECC
            pop     de          ; code words
            pop     hl          ; polynomial
            ld      c,55
            ldir

            ;ret				; flow to next routine

;
; Encode the QR-Code layout into the templete.
; The function also add the remaining 7 padding bits.
;
; Inputs:
;  none
;
; Returns:
;  None
;
; Trashes:
;  A,BC,DE,HL,
;
;
encode_layout:
            ld      hl,qr_template_static_bits
            ld      de,QR_DST_PTR
			ld		bc,QR_DST_SIZE
			ldir
			; init position statemachine
			ld		hl,-4
			ld		(qr_y_adder),hl
			ld      a,QR_DIM-1
			ld      (qr_y),a
			inc		a				; QR-code is shifted left by 1 pixel
			ld      (qr_x),a
			ld		a,00000100b		; pixel position for the last pixel
			ld		(qr_m),a
			xor		a
			ld		(qr_stm),a
			;
			ld      hl,QR_DST_PTR+QR_DST_SIZE-1
			ld		ix,qr_template_empty_bits+QR_DST_SIZE-1
            ld      de,ENC_BUF_PTR
			push	hl
			push	ix

            ld      c,7
            ld      a,10000000b
.main:      ld      b,80            ; 7*80 = 560 = (8*(55+15))
.loop:      add     a,a     
            jr nz,  .skip1
            ld      a,(de)  
            inc     de
            adc     a,a     ; add stop bit and move bit 7 into C_flag

.skip1:     ; if C_flag = 0 then a white pixel, if 1 then a black pixel
			push    bc
			push	af

			ld		c,0
            jr nc,	.white
			dec		c
.white:     ;
            ; Check if need to skip this module/pixel..
			ld		a,(qr_m)
.while:     ; If bit is 0 in the empty bit bitmask -> skip
            and		(ix)
            call z,	pos_next
            jr z,	.while
            
			and		c
			or		(hl)
			ld      (hl),a
			call	pos_next

			pop		af
            pop     bc
            djnz    .loop
            dec     c
            jr nz,  .main

            ;ret

			pop		ix
			pop		hl
;
; Generic mask applying function. This routine calls
; mask kernels pointed by IX.
; 
; Note: The kernel pointed by IX must have the 4 octets preamble
; for next kernel and the 15 bit mask+ecc to be inserted into the
; QR-Code.
;
; Inputs:
;  HL = ptr to QR_DST_PTR+QR_DST_SIZE-1
;  IX = ptr to qr_template_empty_bits+QR_DST_SIZE-1
; 
; Returns:
;  None.
;
; Trashes:
;  A,BC,HL,D
;

apply_mask:
	IF MASK = 0
            ;ld      hl,QR_DST_PTR+QR_DST_SIZE-1
            ;ld		ix,qr_template_empty_bits+QR_DST_SIZE-1
			ld      c,QR_DIM
			ld		d,01010101b
.mask0_kernel:
			ld		b,4
.loop:      
			ld		a,d
			and		(ix)
			xor		(hl)
			ld		(hl),a
			dec		hl
			dec		ix
			djnz	.loop
            rlc		d
			dec     c
            jr nz,	.mask0_kernel
            ret
	ENDIF
	IF MASK = 1
            ;ld      hl,QR_DST_PTR+QR_DST_SIZE-1
            ;ld		ix,qr_template_empty_bits+QR_DST_SIZE-1
			ld      c,QR_DIM
			ld		d,11111111b
.mask1_kernel:
			ld		b,4
.loop:      
			ld		a,d
			and		(ix)
			xor		(hl)
			ld		(hl),a
			dec		hl
			dec		ix
			djnz	.loop
			ld		a,d
			cpl
			ld		d,a
			dec     c
            jr nz,	.mask1_kernel
            ret
	ENDIF


;
; Return the next candidate position in the QR-Code matrix.
; The function takes care of the vertical sync line and can
; skip it.
;
; Inputs:
;  HL = ptr to empty bit bitmask
;  IX = ptr to QR-code dst bitmap
;   C = $00 for transparent pixel, $ff for black pixel
;
; Returns:
;  HL = updated HL
;  IX = updated IX
;
; Trashes:
;  None
;
;  qr_dir:
;   00000000b = up
;   11111111b = down
;
;  qr_stm:
;   00000000b = next right to left move
;   10000000b = next left to right + next line move
;
pos_next:
            push    de
			push	bc
            push    af
			ld		de,(qr_y_adder)
			ld		a,(qr_stm)
			add		a,10000000b
			ld		(qr_stm),a
			jr nc,	.l_shift
			; 
.r_shift_add_y:
			cp		d
			ld		a,(qr_y)
			; C_flag = 0 if D == 0,   increase A
			; C_flag = 1 if D == $ff, decrease A
			; Complement C_flag so that
			;  A = A + $00 + 1
			; or
			;  A = A + $ff + 0
			ccf
			adc		a,d
			;
			cp		QR_DIM
			jr c,	.within_bounds
			ld		a,d
			cpl
			ld		d,a
			ld		a,e
			neg
			ld		e,a
			ld		(qr_y_adder),de
			jr		.l_shift
.within_bounds:
			ld		(qr_y),a
			add		hl,de
			add		ix,de

			ld		a,(qr_m)
			rrca
			jr nc,	.r_check_column6
			inc		hl
			inc		ix
.r_same_byte:
			jr		.r_check_column6

.l_shift:	;
			ld		a,(qr_m)
			ld		c,-1
			rlca
			jr nc,	.l_check_column6
			dec		hl
			dec		ix
			jr		.l_check_column6
.r_check_column6
			ld		c,1
.l_check_column6:
			ld		b,a
			ld		a,(qr_x)
			add		a,c
			cp		$6+1
			jr nz,	.same_byte
			rlc		b
			dec		a
.same_byte:
			ld		(qr_x),a
			pop		af
			ld		a,b
			pop		bc
			ld		(qr_m),a
			pop		de
			ret

qr_end:

;
; Follwing buffers are freely relocatable. Note that both
; GF_E2G_PTR and GF_G2E_PTR must have 256 octet alignment.
; Below code can be commented out and then use defines themselves
; for memory pointers.

ENC_BUF_PTR:
            ds      ENC_BUF_SIZE	; QR_CWDS_EWDS i.e. code words + ecc words
PHR_BUF_PTR:
			;ds		PHR_BUF_SIZE	; overlaid on QR_DST_PTR buffer
QR_DST_PTR: ds      QR_DST_SIZE		;


            org ($+255) & 0xff00

            ; These two table must be 256 bytes each and in 256 bytes alignment.
GF_E2G_PTR: ds      256
GF_G2E_PTR: ds      256


	SAVESNA		"qrcoder.sna",main


