;
; qr-codes in Z80 v0.1 (c) Jouni korhonen
; This implementation supports only 3-L without
; quiet zone..
;
; Target is ZX Spectrum
;
; Compiled using Pasmo (see https://github.com/jounikor/pasmo)
; pasmo -1 --tapbas --alocal qrcoder.asm tap.tap tap.map


QR_DST_ADDR equ $c000
QR_TMP_END_PTR  equ     QR_TMP_PTR+QR_DIM*(QR_DIM+1)-2

QR_WHITE        equ     00000000b
QR_BLACK        equ     10000000b
QR_WHITE_T      equ     00000001b
QR_BLACK_T      equ     10000001b
QR_EMPTY        equ     01000000b
QR_DIR_UP       equ     0
QR_DIR_DOWN     equ     1

QR_DIM          equ     29
QR_VER          equ     3
QR_LEV          equ     'L'
QR_CWDS_EWDS    equ     (55+15)*8
QR_CWDS         equ     55            




            org $8000


main:

            exx
            push    hl
            push    de
            push    bc
            exx

            call    gf_init

            ld      ix,test_string
            ld      a,33
            call    encode_phrase
            call    polydiv
            ; There is no need to intealeave 3-L QR-Code
            ;call    interleave

            ; here be the masking 8x and encoding layout..
            ld      b,8         ;
            ;ld     ix,qr_penalties
_mask_loop:
            push    bc
            call    decode_qr_template
            ;ld      de,0000000000000000b
            ld      de,1111111111111111b
            call    insert_mask
            ;call    encode_layout
            ld      ix,mask7_kernel
            call    apply_mask
            ;call    calc_penalties

            pop     bc
            ;djnz    _mask_loop

            ;call   encode_final
            ;ld     de,(qr_mask)
            ;call   insert_mask
            call    print_2

            exx
            pop     bc
            pop     de
            pop     hl
            exx
ll
            halt
    jr ll
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
; Inputs:
;  DE = mask bits (15 bits, bit 0 is bit 7 of B..)
;
; Returns:
;  None
;
; Trashes:
;  HL,DE,BC,A,HL',BC',IX
;
insert_mask:
            ld      ix,mask_v_delta
            ld      hl,QR_TMP_PTR+(8*30)
            exx
            ld      hl,QR_TMP_PTR+8+(28*30)
            exx

_loop:      xor     a
            ld      b,a

            ; shift out next bit and expand
            ; C=0 -> 00000000b
            ; C=1 -> 10000000b
            sla     e
            rl      d
            rra

            ; horizontal pixel
            ld      (hl),a
            
            ; vertical pixel
            exx
            ld      (hl),a
            
            ; got up in qr code raster
            xor     a
            ld      c,(ix+0)
            cp      c           ; if C == A then C_flag = 0 
                                ; if C == A then Z_flag = 1
            jr z,   _done
            
            sla     c
            rla
            ld      b,a
            sbc     hl,bc       ; clears Z_flag and C_flag
_done:      exx

            ; go right in qr raster..
            ld      c,(ix+15)
            inc     ix
            add     hl,bc       ; does not affect Z_flag
            jr nz,  _loop

            ; C_flag = 0.. undo last add
            sbc     hl,bc
            ; C_flag = 0.. go left in qr raster
            sbc     hl,bc
            ; last bit in D has become either 0000000b or 10000000b
            ld      (hl),d
            ;
            ret

            ; Tables for advancing in the QR-Code for placing mask bits
mask_v_delta:
            db      15,15,15,15,15,15, 210,15,30,15,15,15,15,15,0
mask_h_delta: 
            db       1, 1, 1, 1, 1, 2,   1,14, 1, 1, 1, 1, 1, 1,7

;
;
;
;
;
print_2:
            ld      de,QR_TMP_PTR
            ld      hl,$5800

            ld      c,24
_main:

            ld      b,30
_loop:      ld      a,(de)
            inc     de

            and     11111110b
            jr z,   _white

            xor     10000000b
            jr z,   _black

            ;and     00111111b
            ld      a,00001001b

_white:     xor     00111111b
_black:
            ld      (hl),a
            inc     hl
            djnz    _loop
           
            inc     hl
            inc     hl

            dec     c
            jr nz,  _main

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
;  None
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
            and     00011111b   ; remainin 5 bits of length
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

            ; Align to byte
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

            ld      de,ENC_BUF_PTR
            ; Last pixel/module in the QR-Code template
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
            push    af
            ld      a,QR_EMPTY

_while:     ;
            cp      (hl)
            ;ld      (hl),00101000b
            call nz,    pos_next
            jr nz,  _while
            ld      (hl),c
            call z, pos_next
            
            pop     af
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
; Inputs:
;  HL = ptr to temporary (octet aligned) qr buffer
;
; Returns:
;  HL = ptr to new position in temporary qr buffer
;
; Trashes:
;  None, keeps flags as when called.
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
;  01000000b = empty pixel/module available for encoded pixels
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
_white:
_black:
_empty:
_sta:       ld      (hl),a
            inc     hl
            xor     c
            djnz    _sta
            jr      _loop


;
;
;
;
;
;
;
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
            ld      de,_ret
            push    de
            jp      (ix)

_ret:       jr nz,  _do_not_flip 
            ld      a,(hl)
        IF 0
            bit     0,a
            jr z,   _permanent_module
            xor     10000000b
        ELSE
            cp      QR_EMPTY
            jr nz,  _permanent_module
            xor     11000000b
        ENDIF

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


;
; Mask #0
; (row + column) mod 2 == 0
;
; Inputs:
;  B = col
;  C = row
; 
; Returns:
;  Z_flag = 0 if kernel applies
;
; Trashes:
;  A
;
mask0_kernel:
            ld      a,b
            add     a,c
            and     00000001b
            ret

;
; Mask #1
; (row) mod 2 == 0
;
; Inputs:
;  B = col
;  C = row
; 
; Returns:
;  Z_flag = 0 if kernel applies
;
; Trashes:
;  A
;
mask1_kernel:
            ld      a,c
            and     00000001b
            ret

;
; Mask #2
; (column) mod 3 == 0
;
; Inputs:
;  B = col
;  C = row
; 
; Returns:
;  Z_flag = 0 if kernel applies
;
; Trashes:
;  A
;
mask2_kernel:
            ld      a,b
            call    divmod3_A
            and     a
            ret


;
; Mask #3
; (row + column) mod 3 == 0
;
; Inputs:
;  B = col
;  C = row
; 
; Returns:
;  Z_flag = 0 if kernel applies
;
; Trashes:
;  A
;
mask3_kernel:
            ld      a,b
            add     a,c
            call    divmod3_A
            and     a
            ret


;
; Mask #4
; ( floor(row / 2) + floor(column / 3) ) mod 2 == 0
;
; Inputs:
;  B = col
;  C = row
; 
; Returns:
;  Z_flag = 1 if kernel applies
;
; Trashes:
;  A,A'
;
mask4_kernel:
            push    bc

            ; floor(row/2)
            ld      a,c
            rrca
            push    af

            ; floor(col/3)
            ld      a,b
            call    divmod3_A
            ex      af,af'
            pop     bc
            
            ; floor(row/2) + floor(col/3)
            add     a,b
            pop     bc
            
            ; (floor(row/2) + floor(col/3)) mod 2
            and     00000001b
            ret

;
; Mask #5
; ((row * column) mod 2) + ((row * column) mod 3) == 0
;
; Inputs:
;  B = col
;  C = row
; 
; Returns:
;  Z_flag = 0 if kernel applies
;
; Trashes:
;  A,A'
;
mask5_kernel:
            push    bc
            
            ; (row * column) mod 2
            call    mul8_B_C
            and     00000001b
            push    af

            ; (row * column) mod 3 -> (row * (col mod 3)) mod 3 
            ld      a,b
            call    divmod3_A
            ld      b,a
            call    mul8_B_C
            call    divmod3_A
            
            ; ((row * column) mod 2) + ((row * column) mod 3) == 0
            pop     bc
            add     a,b
            
            ; 
            pop     bc
            ret

;
; Mask #6
; ( ((row * column) mod 2) + ((row * column) mod 3) ) mod 2 == 0
;
; Inputs:
;  B = col
;  C = row
; 
; Returns:
;  Z_flag = 0 if kernel applies
;
; Trashes:
;  A,A'
;
mask6_kernel:
            call    mask5_kernel
            and     00000001b
            ret


;
; Mask #7
; ( ((row + column) mod 2) + ((row * column) mod 3) ) mod 2 == 0
;
; Inputs:
;  B = col
;  C = row
; 
; Returns:
;  Z_flag = 0 if kernel applies
;
; Trashes:
;  A,A'
;
mask7_kernel:
            push    bc

            ; (row + column) mod 2
            ld      a,b
            add     a,c
            and     00000001b
            push    af

            ; (row * column) mod 3 -> (row * (col mod 3)) mod 3 
            ld      a,b
            call    divmod3_A
            ld      b,a
            call    mul8_B_C
            call    divmod3_A

            ; ((row + column) mod 2) + ((row * column) mod 3) ) mod 2 == 0
            pop     bc
            add     a,b
            and     00000001b

            ; 
            pop     bc
            ret


;
; Calculate penalty score 1
;
; 5 consequtive pixels of the same color = 3 penalty point.
; After 5 each additional pixel of the same color add 1 penalty point.
; Do check for each row and column.
;
; Inputs:
;  HL = ptr to qr template
;
; Returns:
;  DE = penalty score
;
calc_penalty1:
        ; Horizontal
        ld      c,QR_DIM        ; rows
        ld      de,0


_h_main:
        ld      b,QR_DIM        ; cols
        xor     a
_h_loop:





        djnz    _h_loop

        dec     c
        jr nz,  _h_main


        ; Vertical




        ret




;
; Calculate quotient and modulo 3 of A. The maximum value of A can be 192.
;
;
; Inputs:
;  A = value to take module 3 and divide by 3. 
;
; Returns:
;  A  = reminder (modulo)
;  A' = quotient
;
; Trashes:
;  None
;
divmod3_A:
            push    bc
            ld      bc,$40c0     ; A cannot be larger than 192 i.e. 3<<6
            ex      af,af'
            xor     a
            ex      af,af'

            ;
_div_main:  cp      c
            jr c,   _too_big
            sub     c
            ex      af,af'
            add     a,b
            ex      af,af'
_too_big:   srl     c
            srl     b
            jr nz,  _div_main
_done:      ;
            pop     bc
            ret


;
; Multiply two 8 bit values mod 256.
;
; Inputs:
;  B = value 1
;  C = value 2
;
; Returns:
;  A = result
;
; Trashes:
;  None
;
; Note: if the result does not fit into 8 bits then the
; the result is (B*C) mod 256.
;
;
mul8_B_C: 
            push    bc
            xor     a
_mul_main:  srl     b
            jr c,   _do_add
            jr z,   _done
            db      $fe     ; Old 'CP n' trick..
_do_add:    add     a,c
            sla     c
            jr nz,  _mul_main
_done:      ;
            pop     bc
            ret



            org     ($+255) & 0xff00
alnum_map:
patterns:
            ; Generator polynomial for 3-L
gen_15:     db      8,183,61,91,202,37,51,58,58,237,140,124,5,99,105
qr_stm:     db      0                       ;
mask_pattern:       ;
            ;       01234567  89abcde7      ; Last bit 7 is for horiz cases 
            db      11101111b,10001001b     ; L = 1, mask 0
            db      11100101b,11100111b
            db      11111011b,01010101b
            db      11110001b,00111011b
            db      11001100b,01011110b
            db      11000110b,00110000b
            db      11011000b,10000010b
            db      11010010b,11101100b
            ds      32-15-1-16              ; space for ascii till space
            ;db      ' ',0,0,0,'$','%',0,0,0,0,'*','+',0,'-','.','/'
            db       36,0,0,0, 37, 38,0,0,0,0, 39, 40,0, 41, 42, 43
            ;db      '0','1','2','3','4','5','6','7','8','9',':'
            db        0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  44
qr_y:       db      0
qr_x:       db      0
qr_dir:     db      0
            ds      6-3       ; skip ;<=>?@
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
            db      $87,$01,$4d,$01,$86,$c4
            db      $04,$c2,$4d,$01,$c2,$04,$c4
            db      $82,$c4,$4d,$01,$c2,$82,$c6
            db      $82,$c4,$4d,$01,$c2,$82,$c6
            db      $82,$c4,$4d,$01,$c2,$82,$c6
            db      $04,$c2,$4d,$01,$c2,$04,$c2
            db      $86,$d0,$86,$c2,$08,$4d,$09,$46
            
            db      $81,$5d,$01,$5d,$81,$5d,$01,$5d
            db      $81,$5d,$01,$5d,$81,$5d,$01,$5d
            db      $81,$5d,$01,$5d,$81,$5d,$01,$5d

            db      $81,$4d,$85,$45

            db      $08,$81,$4b,$81,$03,$81,$45
            db      $87,$01,$4c,$c4,$81,$45
            db      $81,$05,$c2,$4c,$81,$03,$81,$45
            db      $c2,$82,$c4,$4c,$85,$45
            db      $c2,$82,$c4,$56
            db      $c2,$82,$c4,$56
            db      $81,$05,$c3,$55
            db      $87,$02,$56

            db      $00     ; end mark
qr_end:



PHR_BUF_PTR:                    ; encode phrase here
            ds  QR_CWDS
ENC_BUF_PTR:
            ds  QR_CWDS_EWDS    ; code words + ecc words



            org ($+255) & 0xff00

            ; These two table must be 256 bytes each and in 256 bytes alignment.
GF_E2G_PTR: ds  256
GF_G2E_PTR: ds  256


QR_DST_PTR: ds      QR_DIM*4

            org ($+255) & 0xff00
QR_TMP_PTR: ds      32*QR_DIM





test_string:        ; 33 chars
            db      "HTTP://WWW.DEADCODERSSOCIETY.NET/"


    END main




