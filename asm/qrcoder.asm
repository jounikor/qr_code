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
; PHR_BUF_PTR   no alignment, size PHR_BUF_SIZE octets
; PHR_BUF_PTR   no alignment, size ENC_BUF_SIZE octets
; QR_DST_PTR    no alignment, size QR_DST_SIZE <- place qr-code here
; QR_TMP_PTR    no alignment, size QR_TMP_SIZE
; GF_G2E_PTR    256 octet alignment, size 256
; GF_E2G_PTR    256 octet alignment, size 256

QR_DIM          equ     29
QR_CWDS_EWDS    equ     (55+15)*8
QR_CWDS         equ     55            
QR_MAX_PHRASE   equ     76

;
PHR_BUF_SIZE    equ     QR_CWDS
ENC_BUF_SIZE    equ     QR_CWDS_EWDS
QR_DST_SIZE     equ     QR_DIM*4
QR_TMP_SIZE     equ     (QR_DIM+1)*QR_DIM
QR_TMP_END_PTR  equ     QR_TMP_PTR+QR_DIM*(QR_DIM+1)-2

; 
QR_WHITE        equ     00000000b   ; bit 0 == 0 -> permanent pixel/module
QR_BLACK        equ     10000000b
QR_WHITE_T      equ     00000001b   ; bit 0 == 1 -> can be overwritten
QR_BLACK_T      equ     10000001b
QR_EMPTY        equ     01000001b

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


            call    qr_code_init

            ld      ix,test_string
            ld      a,TEST_STR_LEN
            call    qr_code_generate
    IF DEFINED TEST_SPECTRUM    
            ld      b,64
            ld      c,112
            call    qr_code_render_spectrum
	ENDIF
            exx
            pop     bc
            pop     de
            pop     hl
            exx
            ei

            ret


;TEST_STR_LEN	equ	22
test_string:        ; 22 chars
            ;db      "HTTP://WWW.SCOOPEX.US/"
TEST_STR_LEN	equ	33
            db      "HTTP://WWW.DEADCODERSSOCIETY.NET/"

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
            ld      ix,QR_TMP_PTR
            ld      b,64
            ld      c,112
            exx
            ld      b,QR_DIM
_line_loop: ;
            exx
            call    get_address_BC
            call    print_1_line
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
            ds      32-15-1                 ; space for ascii till space
            ;db      ' ',0,0,0,'$','%',0,0,0,0,'*','+',0,'-','.','/'
            db       36,0,0,0, 37, 38,0,0,0,0, 39, 40,0, 41, 42, 43
            ;db      '0','1','2','3','4','5','6','7','8','9',':'
            db        0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  44
qr_y:       db      0
qr_x:       db      0
qr_dir:     db      0
qr_final_mask:
            dw      0
            ds      6-5       ; skip ;<=>?@
            ;db      'A','B','C','D','E','F','G','H','I'
            db       10, 11, 12, 13, 14, 15, 16, 17, 18
            ;db      'J','K','L','M','N','O','P','Q','R'
            db       19, 20, 21, 22, 23, 24, 25, 26, 27
            ;db      'S','T','U','V','W','X','Y','Z' 
            db       28, 29, 30, 31, 32, 33, 34, 35
            ; after this we have 256-90 left for this 256 bytes segment


; Format of the RLE:
;  00 nnnnnn -> white (00000000b), 1 < n < 63
;  01 nnnnnn -> empty (01000000b), 1 < n < 63
;  10 nnnnnn -> black (10000000b), 1 < n < 63
;  11 nnnnnn -> black-white alternating, 1 < n < 63
;  00 000000 -> end mark
;
qr_template:
            db      $87,$02,$4c,$01,$86,$c4			; O
            db      $04,$c2,$01,$4c,$01,$c2,$04,$c4	; O
            db      $82,$c5,$4c,$01,$c2,$82,$c6		; X
            db      $82,$c4,$01,$4c,$01,$c2,$82,$c6	; O
            db      $82,$c4,$01,$4c,$01,$c2,$82,$c6	; O
            db      $04,$c2,$01,$4c,$01,$c2,$04,$c2	; O
            db      $86,$d0,$88						;
			db		$08,$81,$4c,$09					; X
            db      $82,$c3,$c3,$81,$4c,$82,$03,$c2,$02	; XXXOXX XX............XXOOOXOO

            db      $46,$01,$5d,$81
            db      $5d,$01,$5d,$81
            db      $5d,$01,$5d,$81
            db      $5d,$01,$5d,$81
            db      $5d,$01,$5d,$81
            db      $5d,$01,$5d,$81

            db      $4d,$85,$45

            db      $08,$81,$4b,$81,$03,$81,$45
            db      $86,$c3,$4b,$c4,$81,$45			; X
            db      $81,$05,$c3,$4b,$81,$03,$81,$45	; X
            db      $c2,$82,$c5,$4b,$85,$45			; X
            db      $c2,$82,$c4,$01,$55				; O
            db      $c2,$82,$c5,$55					; X
            db      $81,$05,$c3,$55					; X
            db      $86,$c3,$55						; X

            db      $00     ; end mark
            
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
            call    gf_init
            call    decode_qr_template
            ret

;
; Generate QR-Code (assume only 3-L)
;
; Inputs:
;  IX = ptr to phrase. Max length is 76 characters.
;  A = length of the phrase.
;
; Returns:
;  C_flag = 0 if phrase is too long
;
; Trashes:
;  IY is not trashed.
;
qr_code_generate:
            ;
            ; Make sure text phrase is not longer than 76 characters.
            call    encode_phrase
            ret nc

            call    polydiv

            ; There is no need to intealeave 3-L QR-Code
            ;call    interleave

            ; Encode codewords, ecc words and padding to qr-code bits layout
            call    encode_layout
            ;ld      ix,QR_MASK_PTR
            call    apply_mask

            ; Note the big endian byte order..
            ;ld      d,(ix-2)
            ;ld      e,(ix-1)
            ;call   insert_mask
           
            scf         ; C_flag = 1 for OK
            ret

;
; Generate Galois Field tables
;
; Trashes:
;  A,B,H,L
;
gf_init:
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
            ;
            ret

;
; Make a pixel representation of the QR-Code (interleaved).
;
; Inputs:
;  HL = ptr to 4*QR_DIM octets buffer.
;
; Returns:
;  None.
;
; Trashes:
;  A,BC,DE,IX
;
qr_code_render_mem:
            ld      ix,QR_TMP_PTR
            ld      b,QR_DIM
_line_loop:
            call    print_1_line
            djnz    _line_loop
            ret
            

;
; Bitwise QR-Code rendering into the RAM.
; If QR_ADD_BORDER is defined the QR-Code is pushed 1 pixel right so
; that it has 1 pixel border or the left and 2 pixel border on the right.
;
; Inputs:
;  IX = ptr to tmp qr-code
;  HL = ptr to dst memory
;
; Returns:
;  IX = ptr to next row in the tmp qr-code.
;  HL = ptr to next mem area on the 'row'.
;
; Trashes:
;  A,DE
;
print_1_line:
            ld      e,QR_DIM
        IF DEFINED QR_ADD_BORDER
            ld      a,2
        ELSE
            ld      a,1
        ENDIF
_bit_loop:
            ld      d,(ix+0)
            inc     ix
            sla     d
            adc     a,a
            jr nc,  _not_full
            ld      (hl),a
            inc     hl
            ld      a,1
_not_full:
            dec     e
            jr nz,  _bit_loop

            ; last byte is 2 or 3 bits short..
        IF !DEFINED QR_ADD_BORDER
            add     a,a
        ENDIF
            add     a,a
            add     a,a
            ld      (hl),a
            inc     hl      ; next octet
            inc     ix      ; skip hidden alignment byte
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
;  C_flag = 0 if error, C_flag = 1 if OK.
;
; Trashes:
;  A,BC,DE,HL,IX
;
encode_phrase:
            cp      QR_MAX_PHRASE+1
            ret nc

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
            ld      h,HIGH(alnum_map)
            ld      l,(ix+0)
            ld      e,(hl)
            inc     ix
            call    mul45

            ld      l,(ix+0)
            ld      l,(hl)
            inc     ix
            ld      h,0
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
            scf         ; C_flag = 1
            ret

;
; Multiply by 45.
; 
; Inputs:
;  E = value (0 <= C < 45) to multiply
;
; Returns:
;  DE = result
;
; Trashes:
;  DE
;
mul45:      push    hl
            ld      h,0
            ld      l,e

            push    hl      ; 1x to stack
            ; 32x
            add     hl,hl
            add     hl,hl
            push    hl      ; 4x to stack
            add     hl,hl
            push    hl      ; 8x to stack
            add     hl,hl
            add     hl,hl
            pop     de
            add     hl,de
            pop     de
            add     hl,de
            pop     de
            add     hl,de
            ex      de,hl

            pop     hl
            ret

;
; Encode the QR-Code layout into the templete.
; The function also add the remaining 7 padding bits.
;
; Inputs:
;  HL = ptr to (interleaved) code words and ecc words
;  DE = ptr to temporary QR-Code last byte
;
; Returns:
;  None
;
; Trashes:
;  A,BC,DE,HL,
;
;
encode_layout:
            ; init position statemachine
            call    init_next

            ; HL = ptr to the last pixel/module in the QR-Code template
            ; DE = ptr to code + ecc words
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
            jr c,   _black
            ld      c,QR_WHITE_T
            jr      _skip2
_black:     ld      c,QR_BLACK_T
_skip2:     ;
            ; Check if need to skip this module/pixel..
_while:     ; If bit 0 is set this is a non-permanent pixel/module -> skip
            bit     0,(hl)
            call z,   pos_next
            jr z,   _while
            ld      (hl),c
            call nz, pos_next
            
            pop     bc
            djnz    _loop
            dec     c
            jr nz,  _main

            ; add 7 pixels/modules of padding..
            ld      b,7
_pad:       ld      (hl),QR_WHITE_T
            call    pos_next
            djnz    _pad

            ret

;
; Initialize QR-Code matrix zig-zag traveling state machine.
;
; Inputs:
;  None
;
; Returns:
;  HL = ptr to last octet in the template QR-Code array
;
; Trashes:
;  A
;
init_next:
            xor     a
            ld      (qr_stm),a
            ld      (qr_dir),a
            ld      a,QR_DIM-1
            ld      (qr_y),a
            ld      (qr_x),a
            ld      hl,QR_TMP_PTR+(QR_DIM+1)*(QR_DIM-1)+(QR_DIM-1)
            ret

;
; Return the next candidate position in the QR-Code matrix.
; The function takes care of the vertical sync line and can
; skip it.
;
; Inputs:
;  HL = ptr to temporary (octet aligned) qr buffer
;
; Returns:
;  HL = ptr to new position in temporary qr buffer
;
; Trashes:
;  None, also keeps flags as when called.
;
pos_next:
            push    af
            push    de

            ld      a,(qr_dir)
            and     a
            jr nz,  _dir_down

_dir_up:    ;
            ld      a,(qr_stm)
            add     a,10000000b
            ld      (qr_stm),a
            jr c,   _up

            ; move left
            ld      a,(qr_x)
            dec     a

            dec     hl
            jr      _next

_up:        ld      a,(qr_y)
            sub     1
            jr c,   _change_dir

            ld      (qr_y),a
            ld      a,(qr_x)
            inc     a

            ld      de,QR_DIM       ; move x one right at the same time
            sbc     hl,de
            jr      _next

_dir_down:  ;
            ld      a,(qr_stm)
            add     a,10000000b
            ld      (qr_stm),a
            jr c,   _down

            ; move left
            ld      a,(qr_x)
            dec     a

            dec     hl
            jr      _next

_down:      ld      a,(qr_y)
            inc     a
            cp      QR_DIM
            jr nc,  _change_dir

            ld      (qr_y),a
            ld      a,(qr_x)
            inc     a

            ld      de,QR_DIM+2     ; move x one right at the same time
            add     hl,de
            jr      _next

_change_dir:
            ld      a,(qr_dir)
            cpl
            ld      (qr_dir),a

            ; move left as at the same time
            ld      a,(qr_x)
            dec     a
            dec     hl

_next:      ; A = qr_x
            ; This check is for skipping the vertical sync line.
            cp      6
            jr nz,  _done
            dec     a
            dec     hl

            ;
_done:      ld      (qr_x),a
            pop     de
            pop     af

            ret


;
; Decode QR-Code basic template. It includes:
;  - alignment patterns
;  - positional patterns
;  - dark module
;  - reserved areas
;  - sync patterns
;
; The template does not include:
;  - mask patterns (inserted with a separate function)
;
; Note that the area used by the template is:
;  - horizontal QR_DIM+1 (i.e. in 3-L case 30)
;  - vertical   QR_DIM (i.e. in 3-L case 29)
; When processing inside the template you MUST ignore the 
; one column outside the QR_DIM.
;
; Encoding on each octet within the template:
;  00000000b = white pixel/module
;  10000000b = black pixel/module
;  01000001b = empty pixel/module available for encoded pixels
;
decode_qr_template:
            ld      de,qr_template
            ld      hl,QR_TMP_PTR

_loop:      ld      a,(de)
            inc     de
            and     a
            ret z

            ld      c,a
            ld      b,00111111b
            and     b
            ld      b,a
            xor     c
            ld      c,0

            jr z,   _white
            jp p,   _empty

            bit     6,a
            jr z,   _black

            ; black-white alternate
            add     a,a
            ld      c,a

            ; White, black and black & white alternate 
_white:     ; all pass through here i.e. they see CP n here..
_black:
            db      $fe         ; Old 'CP n' trich
            ; Empty pixels/modules will have bit 0 set
_empty:     inc     a
_sta:       ld      (hl),a
            inc     hl
            xor     c
            djnz    _sta
            jr      _loop


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
            ld      hl,QR_TMP_END_PTR
            ld      c,QR_DIM-1
_main:

            ld      b,QR_DIM-1
_loop:      
            ;ld      de,_ret
            ;push    de
            ;jp      (ix)

_mask0_kernel:
            ld      a,b
            add     a,c
            and     00000001b

_ret:       jr nz,  _do_not_flip 
            ld      a,(hl)
            bit     0,a
            jr z,   _permanent_module
            xor     10000000b

            ld      (hl),a
_do_not_flip:
_permanent_module:
            dec     hl
            dec     b
            jp p,   _loop
            dec     hl

            dec     c
            jp p,   _main

            ret
qr_end:
            ;db      11101111b,10001001b     ; L = 3, mask 0

;
; Follwing buffers are freely relocatable. Note that both
; GF_E2G_PTR and GF_G2E_PTR must have 256 octet alignment.
; Below code can be commented out and then use defines themselves
; for memory pointers.

PHR_BUF_PTR:						; encode phrase here
            ds      PHR_BUF_SIZE	; QR_CWDS
ENC_BUF_PTR:
            ds      ENC_BUF_SIZE	; QR_CWDS_EWDS i.e. code words + ecc words

QR_DST_PTR: ds      QR_DST_SIZE		; QR_DIM*4
QR_TMP_PTR: ds      QR_TMP_SIZE		; 30*QR_DIM


            org ($+255) & 0xff00

            ; These two table must be 256 bytes each and in 256 bytes alignment.
GF_E2G_PTR: ds      256
GF_G2E_PTR: ds      256


    END main




