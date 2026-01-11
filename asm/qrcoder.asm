; This is the size "optimized" version that is forced to 3-L
; and mask0.
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

            org $8000

TEST_SPECTRUM	equ		1


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
			ld      ix,test_string1
            ld      a,TEST_STR_LEN1
            call    qr_code_generate
    IF DEFINED TEST_SPECTRUM    
            ld      b,64
            ld      c,112
            call    qr_code_render_spectrum
	ENDIF
			ld      ix,test_string2
            ld      a,TEST_STR_LEN2
            call    qr_code_generate
    IF DEFINED TEST_SPECTRUM    
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
;            db      "HTTP://WWW.SCOOPEX.US/"
TEST_STR_LEN1	equ	33
            db      "HTTP://WWW.DEADCODERSSOCIETY.NET/"

TEST_STR_LEN2	equ	67
test_string2:
			db	"HTTPS://WWW.LIPPU.FI/EVENT/JINJER-HELSINGIN-KULTTUURITALO-20307254/"
	IF DEFINED TEST_SPECTRUM
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
			ENDM
			
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
; Start pof QR code
			org		($+255) & 0xff00
; Note! the following tables and variabled must be 256 bytes aligned.
;

qr_sta:
alnum_map:
patterns:
            ; Generator polynomial for 3-L
gen_15:     db      8,183,61,91,202,37,51,58,58,237,140,124,5,99,105
qr_stm:     db      0                       ;
qr_y_adder:	dw		0
			ds      32-15-3                 ; space for ascii till space
            ;db      ' ',0,0,0,'$','%',0,0,0,0,'*','+',0,'-','.','/'
            db       36,0,0,0, 37, 38,0,0,0,0, 39, 40,0, 41, 42, 43
            ;db      '0','1','2','3','4','5','6','7','8','9',':'
            db        0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  44
qr_y:       db      0
qr_x:       db      0
qr_dir:     db      0
qr_m:		db		0
qr_final_mask:
            dw      0
            ;db      'A','B','C','D','E','F','G','H','I'
            db       10, 11, 12, 13, 14, 15, 16, 17, 18
            ;db      'J','K','L','M','N','O','P','Q','R'
            db       19, 20, 21, 22, 23, 24, 25, 26, 27
            ;db      'S','T','U','V','W','X','Y','Z' 
            db       28, 29, 30, 31, 32, 33, 34, 35
            ; after this we have 256-90 left for this 256 bytes segment

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
			db		01111111b,00000000b,00000001b,11111100b
			db		01000001b,00000000b,00000001b,00000100b
			db		01011101b,01000000b,00000001b,01110100b
			db		01011101b,00000000b,00000001b,01110100b
			db		01011101b,00000000b,00000001b,01110100b
			db		01000001b,00000000b,00000001b,00000100b
			db		01111111b,01010101b,01010101b,11111100b
			db		00000000b,01000000b,00000000b,00000000b
			;db		01010110b,11000000b,00000011b,00010000b
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
qr_mid:

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
qr_code_init:
            ; gf_init and decode_qr_layout need to be called only
            ; once for any number of calculated QR-Codes.
;
; Generate Galois Field tables
_gf_init:
            ld      b,0 
            ld      hl,GF_E2G_PTR
            ld      a,1
            jr      _not_over_256

_main:      ;
            add     a,a
            jr nc,  _not_over_256
            ;
            xor     285-256
_not_over_256:
            ld      (hl),a
            inc     l
            djnz    _main
            ;

            ld      b,254
_copy:      ;
            ld      l,b
            ld      a,(hl)      ; from gf_e2g[n]
            inc     h
            ld      l,a
            ld      (hl),b      ; to gf_g2e[f_e2g[n]] = n
            dec     h
            djnz    _copy
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
            call    encode_phrase
            call    polydiv

            ; Encode codewords, ecc words and padding to qr-code bits layout
            call    encode_layout
			call	apply_mask
			ret


; Polynomial division.
;
; Inputs:
;  HL = ptr to polynomial (size of 55 octets)
;  DE = ptr to encoding buffer (size of 55+15 octets)
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
_clear:     ld      (de),a
            inc     de
            djnz    _clear

            ; polynomial division main loop..
            ld      c,55            ; "polylen"
            pop     hl
            ;
            ; HL = phrase i.e., polynomial
            ; DE = generator

_div_main:  ;
            ; get 'ex' into A
            ld      a,(hl)
            inc     hl
            ld      d,HIGH(GF_G2E_PTR)
            ld      e,a
            ld      a,(de)

            ld      b,15
            ld      de,gen_15
            push    hl

_div_loop:  ;
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
            djnz    _div_loop

            ;
            ;
            pop     hl          ; ecc words
            dec     c
            jr nz,  _div_main

            ; move poly at the front of ECC
            pop     de          ; code words
            pop     hl          ; polynomial
            ld      c,55
            ldir

            ret


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
;  DE = ptr to dst buffer (size of 55 octets)
;  A  = string length
;
; Returns:
;  none. (was C_flag = 0 if error, C_flag = 1 if OK.)
;
; Trashes:
;  A,BC,DE,HL,IX
;
encode_phrase:
            ld      de,PHR_BUF_PTR
            push    de
            ;
            ; Bits needed by alphanumeric encoder are:
            ; b = 4 + c + 11(d DIV 2) + 6(d MOD 2), where
            ; where c = 9, d = strlen(phrase)
            ;
            ; In 3-L QR-Code we have 55 octets for code words, which means
            ; D can be 76 characters. That fits in to 9 bit length.
            ;
            ; B = string len in octets, 9 bits but only 7 bits can be used.. 
            ;     i.e., 00 nnnnnnn -> A = 0nn nnnnn 

            ld      b,a
            rlca
            rlca
            rlca
            and     00000011b
            or      00100000b  ; mode = 0010b and 0 + 0nn of length
            ld      (de),a
            inc     de
            ld      a,b
            and     00011111b   ; remaining 5 bits of length
            or      00100000b   ; stop bit

            ld      c,b
_main:      ;
            ; A  = bit buffer with stop bit. C_flag=1 when A becomess full
            ; C  = number of character pairs
            ; DE = destination
            ; IX = source
            ; C_flag = pushed into stack whether there is odd number of 
            ;          characters.. if even C_flag = 0
            ;
            ; get 11 bits encoded 2x character
            ld      b,4
            ld      h,0

            dec     c
            jr z,   _add_odd_char
            jp m,   _no_odd_char
            dec     c

            push    de
			ld		d,h

			ld      h,HIGH(alnum_map)
            ld      l,(ix+0)
            ld      e,(hl)
            inc     ix
			
			; mul45
			push	hl
			ld		b,8-2
			ld		h,45<<2
			ld		l,d
_mul45loop:
			add		hl,hl
			jr nc,	_no_mul_add
			add		hl,de
_no_mul_add:
			djnz	_mul45loop
			ex		de,hl
			pop		hl
			;

            ld      l,(ix+0)
            ld      l,(hl)
            inc     ix
            ld      h,b
			add     hl,de
            pop     de

            ld      b,3
            sla     h
            sla     h
            sla     h
            sla     h
            sla     h
            ; upper 3 bits of the encoded chars
_bits1:     sla     h
            adc     a,a
            jr nc,  _no_ovl1
            ld      (de),a
            inc     de
            ld      a,1
_no_ovl1:   djnz    _bits1
            ; lower 8 bits of the encoded chars
            ld      b,8
_bits2:     sla     l
            adc     a,a
            jr nc,  _no_ovl2
            ld      (de),a
            inc     de
            ld      a,1
_no_ovl2:   djnz    _bits2
            ; next 2 chars..
            jr      _main

_add_odd_char:
            ; We need to do one more char..
            ld      h,HIGH(alnum_map)
            ld      l,(ix+0)
            ld      h,(hl)
            inc     ix

            ; 6 bits of the encoded chars + terminating 0000b
            ; Since we support only 3-L we know that terminating 0s
            ; are always total 4 not less.
            ld      b,6+4
            sla     h
            sla     h
_no_odd_char:
_bits3      sla     h
            adc     a,a
            jr nc,  _no_ovl3
            ld      (de),a
            inc     de
            ld      a,1
_no_ovl3:   djnz    _bits3

            ; Align to full byte
            jr c,   _aligned
_align8:    add     a,a
            jr nc,  _align8
            ld      (de),a
            inc     de
_aligned:
            ; Check if padding is required..
            pop     hl
            push    de
            ex      de,hl
            ccf
            sbc     hl,de
            ld      a,55
            sub     l
            and     a
            pop     de
            jr z,   _no_alignment

            ;
            ld      b,a
            ld      a,11101100b
_pad:       ld      (de),a
            inc     de
            xor     11111101b
            djnz    _pad

_no_alignment:
            ;scf         ; C_flag = 1
            ret

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

            ld      c,7
            ld      a,10000000b
_main:      ld      b,80            ; 7*80 = 560 = (8*(55+15))
_loop:      add     a,a     
            jr nz,  _skip1
            ld      a,(de)  
            inc     de
            adc     a,a     ; add stop bit and move bit 7 into C_flag

_skip1:     ; if C_flag = 0 then a white pixel, if 1 then a black pixel
			push    bc
			push	af

            jr nc,	_black
            ld		c,$ff
            jr		_skip2
_black:     ld		c,0
_skip2:     ;
            ; Check if need to skip this module/pixel..
			ld		a,(qr_m)
_while:     ; If bit is 0 in the empty bit bitmask -> skip
            and		(ix)
            call z,	pos_next
            jr z,	_while
            
			and		c
			or		(hl)
			ld      (hl),a
			call	pos_next

			pop		af
            pop     bc
            djnz    _loop
            dec     c
            jr nz,  _main

            ret

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
			jr nc,	_l_shift
			; 
_r_shift_add_y:
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
			jr c,	_within_bounds
			ld		a,d
			cpl
			ld		d,a
			ld		a,e
			neg
			ld		e,a
			ld		(qr_y_adder),de
			jr		_l_shift
_within_bounds:
			ld		(qr_y),a
			add		hl,de
			add		ix,de

			ld		a,(qr_m)
			rrca
			jr nc,	_r_check_column6
			inc		hl
			inc		ix
_r_same_byte:
			jr		_r_check_column6

_l_shift:	;
			ld		a,(qr_m)
			ld		c,-1
			rlca
			jr nc,	_l_check_column6
			dec		hl
			dec		ix
			jr		_l_check_column6
_r_check_column6
			ld		c,1
_l_check_column6:
			ld		b,a
			ld		a,(qr_x)
			add		a,c
			cp		$6+1
			jr nz,	_same_byte
			rlc		b
			dec		a
_same_byte:
			ld		(qr_x),a
			pop		af
			ld		a,b
			pop		bc
			ld		(qr_m),a
			pop		de
			ret

;
; Generic mask applying function. This routine calls
; mask kernels pointed by IX.
; 
; Note: The kernel pointed by IX must have the 4 octets preamble
; for next kernel and the 15 bit mask+ecc to be inserted into the
; QR-Code.
;
; Inputs:
;  HL = ptr to qr-code template
;  IX = ptr to mask kernel function
; 
; Returns:
;  None.
;
; Trashes:
;  A,BC,HL
;
apply_mask:
            ld      hl,QR_DST_PTR+QR_DST_SIZE-1
            ld		ix,qr_template_empty_bits+QR_DST_SIZE-1
			ld      c,QR_DIM-1
_main:
			ld		b,QR_DIM
			ld		a,00000100b		; pixel position for the last pixel
			ld		(qr_m),a
_loop:      
_mask0_kernel:
            ld      a,b
			dec		a
            add     a,c
            and     00000001b
			push	bc
			ld		a,(qr_m)
			ld		b,a
			jr nz,  _do_not_flip 
			and		(ix)
            jr z,	_permanent_module
			xor		(hl)
            ld      (hl),a
_permanent_module:
			ld		a,b
_do_not_flip:
			rlca
			jr nc,	_same_byte
			dec		hl
			dec		ix
_same_byte:
			ld		(qr_m),a
			pop		bc
			djnz	_loop
			dec		hl		; this is because that 1 pixel QR-code shift
			dec		ix
            dec     c
            jp p,   _main

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
			ds		PHR_BUF_SIZE

QR_DST_PTR: ds      QR_DST_SIZE		;


            org ($+255) & 0xff00

            ; These two table must be 256 bytes each and in 256 bytes alignment.
GF_E2G_PTR: ds      256
GF_G2E_PTR: ds      256


    END main




