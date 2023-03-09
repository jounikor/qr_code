;
; qr-codes in Z80 v0.1 (c) Jouni korhonen
; This implementation supports only 3-L without
; quiet zone..
;
; Target is ZX Spectrum
;
; Compiled using Pasmo (see https://github.com/jounikor/pasmo)
; pasmo -1 --tapbas --alocal qrcoder.asm tap.tap tap.map
;
; The code is not ROMmable if QR_FORCE_MASK is not used.
;
;
;


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
QR_VER          equ     3
QR_LEV          equ     'L'
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


; Define this if you want to force a specific mask and
; save a lot of computation..
QR_FORCE_MASK   equ     3


            org $8000


main:
            di
            exx
            push    hl
            push    de
            push    bc
            exx


            ; gf_init and decode_qr_layout need to be called only
            ; once for any number of calculated QR-Codes.
            call    gf_init
            call    decode_qr_template

            ;
            ; Make sure text phrase is not longer than 76 characters.
            ld      ix,test_string
            ld      a,33
            call    encode_phrase
            call    polydiv

            ; There is no need to intealeave 3-L QR-Code
            ;call    interleave

            ; here be the masking 8x and encoding layout..
            ld      b,8         ;
            ld      ix,mask0_kernel
_mask_loop:
            push    bc
            
            ; Encode codewords, ecc words and padding to qr-code bits layout
            call    encode_layout
            
            ; Apply mask pattern (0 to 7)
            call    apply_mask
            
            ; And insert the mask information with ecc into the qr-code
            ; Note the unusual ordering.. big endian \o/
            ld      d,(ix-2)
            ld      e,(ix-1)
            call    insert_mask
            
            ; Calculate total sum of all 4 penalties into HL
            ; All penalty functions could be combined but..
            call    calc_penalty1
            ld      hl,0
            add     hl,de
            push    hl
            call    calc_penalty2
            pop     hl
            add     hl,de
            push    hl
            call    calc_penalty3
            pop     hl
            add     hl,de
            push    hl
            call    calc_penalty4
            pop     hl
            add     hl,de

            ; Store penalty for this mask..
            ld      (ix-5),h
            ld      (ix-6),l

            ; Advance to the next mask pattern
            ld      d,(ix-3)
            ld      e,(ix-4)
            ld      ixh,d
            ld      ixl,e

            pop     bc
            djnz    _mask_loop

            ; Find the mask with the lowest penalty.
penalties:
            ld      de,32767
            ld      b,8         ;
            ld      ix,mask0_kernel
            xor     a               ; Clears C_flag
            ld      c,a
_find_smallest:
            ld      h,(ix-5)
            ld      l,(ix-6)
            sbc     hl,de
            jr nc,  _not_bigger     ; 
            add     hl,de           ; Clears C_flag
            ex      de,hl
            ld      (qr_final_mask),ix
_not_bigger:
            ld      a,(ix-3)
            ld      ixh,a
            ld      a,(ix-4)
            ld      ixl,a
            djnz    _find_smallest

            ; Render final qr-code
            call    encode_layout
            
            ; Based on the penalty calculations use this mask..
            ld      ix,(qr_final_mask)
            ;ld      ix,mask2_kernel
            call    apply_mask

            ; Note the big endian byte order..
            ld      d,(ix-2)
            ld      e,(ix-1)
            call   insert_mask
            
            ; Render..
            call    print_1

            exx
            pop     bc
            pop     de
            pop     hl
            exx
            ei
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
;  HL,DE,BC,A,HL',BC'
;
insert_mask:
            push    ix
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
            pop     ix
            ret

            ; Tables for advancing in the QR-Code for placing mask bits
mask_v_delta:
            db      15,15,15,15,15,15, 210,15,30,15,15,15,15,15,0
mask_h_delta: 
            db       1, 1, 1, 1, 1, 2,   1,14, 1, 1, 1, 1, 1, 1,7

;
; Debug QR-Code rendering onto the screen..
;
;
print_1:
            ld      ix,QR_TMP_PTR
            ld      b,64
            ld      c,112
_print_main:
            call    get_address_BC

            ld      e,QR_DIM
            ld      a,1
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

            ; last byte is 3 bits short..
            add     a,a
            add     a,a
            add     a,a
            ld      (hl),a

            inc     ix      ; skip hidden alignment byte
            inc     b
            ld      a,QR_DIM+64
            cp      b
            jr nz,  _print_main

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
            ld      de,_ret
            push    de
            jp      (ix)

_ret:       jr nz,  _do_not_flip 
            ld      a,(hl)
        IF 1
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
            dw      0                       ; penalty
            dw      mask1_kernel            ; next kernel
            ;       D         E
            db      11101111b,10001001b     ; L = 3, mask 0
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
            dw      0                       ; penalty
            dw      mask2_kernel
            db      11100101b,11100111b
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
            dw      0                       ; penalty
            dw      mask3_kernel
            db      11111011b,01010101b
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
            dw      0                       ; penalty
            dw      mask4_kernel
            db      11110001b,00111011b
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
            dw      0                       ; penalty
            dw      mask5_kernel
            db      11001100b,01011110b
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
            dw      0                       ; penalty
            dw      mask6_kernel
            db      11000110b,00110000b
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
            dw      0                       ; penalty
            dw      mask7_kernel
            db      11011000b,10000010b
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
            dw      0
            dw      mask0_kernel
            db      11010010b,11101100b
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
; Trashes:
;  A,BC,HL
;
calc_penalty1:
            ld      hl,QR_TMP_PTR
            push    ix
            ld      c,QR_DIM
            push    hl
            ld      ix,0

_loop_horizontal:
            ld      b,QR_DIM
            call    penalty1_horizontal
            add     ix,de
            dec     c
            jr nz,  _loop_horizontal

            pop     hl
            ld      c,QR_DIM

_loop_vertical:
            ld      b,QR_DIM
            call    penalty1_vertical
            add     ix,de
            dec     c
            jr nz,  _loop_vertical

            push    ix
            pop     de
            pop     ix

            ret

;
; Inputs:
;  HL = ptr to start to the line (assume size QR_DIM+1, where QR_DIM
;       pixels are checked).
;  B = length of the line to check for (i.e., QR_MIN)
;
; Returns;
;  DE = penalty
;  HL = ptr to next line
;
; Trashes:
;  none
;
penalty1_horizontal:
            push    bc
            ld      e,0
            ld      a,(hl)
            inc     hl

            ; C = length of consequtive pixels of the same color
            ; B = length of the line and we need to test one less
            ; E = line total penalty
            ld      c,1
            dec     b

_line_loop: ;
            ld      d,(hl)
            inc     hl
            ; compare to previous.. xor is 0 or 1 if pixels match
            xor     d
            ; Mask away the "modifable bit" of the pixel, since
            ; we want to match only 7 upper bits.
            and     11111110b
            jr z,   _match
            
            ; Reset C to length of 1 consequtive pixels so far 
            ld      c,0
            ;jr      _cont

_match:     inc     c
            ld      a,c
            cp      5
            jr c,   _cont
            jr nz,  _skip
            
            ; If 5 consequtive pixels are found add penalty by 3
            inc     e
            inc     e

            ; If more than 5 consequtive pixels are found add 1
            ; penalty for each additional pixel.
_skip:      inc     e

            ; Restore A for the latest pixel value
_cont:      ld      a,d
            djnz    _line_loop

            ; Skip the 1 "hidden alignment" pixel.
            inc     hl

            ld      d,b
            pop     bc
            ret


;
; Inputs:
;  HL = ptr to start of the next column
;  B = height of the column to check for (i.e., QR_MIN)
;
; Returns;
;  DE = penalty
;  HL = ptr to next column
;
; Trashes:
;  none
;
penalty1_vertical:
            push    hl
            push    bc
            
            ld      de,QR_DIM+1
            add     hl,de
            ld      e,d
            ld      a,(hl)

            ; C = length of consequtive pixels of the same color
            ; B = length of the line and we need to test one less
            ; E = line total penalty
            ld      c,1
            dec     b

_column_loop: ;
            ld      d,(hl)
            ; compare to previous.. xor is 0 or 1 if pixels match
            xor     d
            ; Mask away the "modifable bit" of the pixel, since
            ; we want to match only 7 upper bits.
            and     11111110b
            jr z,   _match
            
            ; Reset C to length of 1 consequtive pixels so far 
            ld      c,0

_match:     inc     c
            ld      a,c
            cp      5
            jr c,   _cont
            jr nz,  _skip
            
            ; If 5 consequtive pixels are found add penalty by 3
            inc     e
            inc     e

            ; If more than 5 consequtive pixels are found add 1
            ; penalty for each additional pixel.
_skip:      inc     e


_cont:      ; Go to next row..
            push    de
            ld      de,QR_DIM+1
            add     hl,de
            pop     de

_no_ovl:    ; Restore A for the latest pixel value
            ld      a,d
            djnz    _column_loop

            ld      d,b
            pop     bc
            pop     hl
            inc     hl

            ret

;
; Calculate Penalty score 2
;
; 3x(m-1 x n-1) blocks.. over the entire qr-c0de
; brute force search using 2x2 blocks
;
; Inputs:
;  HL = ptr to qr template
;
; Returns:
;  DE = penalty score
;
; Trashes:
;  A,BC,HL
;
calc_penalty2:
            push    ix
            ld      ix,QR_TMP_PTR
            ld      de,0
            ; rows
            ld      c,QR_DIM-1

_row_loop:  ; columns
            ld      b,QR_DIM-1
_column_loop:
            ld      a,(ix+0)
            ; pixel/module next right
            cp      (ix+1)
            jr nz,  _fail
            ; pixel/module below.. +1 due the invisible alignment column
            cp      (ix+QR_DIM+1)       
            jr nz,  _fail
            ; pixel/module below right corner..
            cp      (ix+QR_DIM+2)
            jr nz,  _fail

            inc     de
            inc     de
            inc     de

_fail:      inc     ix
            djnz    _column_loop

            inc     ix      ; advance to the next row
            dec     c
            jr nz,  _row_loop

            pop     ix
            ret

;
; Calculate Penalty score 3
;
; look for 10111010000 or 00001011101 patterns, where 0 = white
; slightly optimized search based on "next" jump to advance to the
; next possible seacrh position based on the search so far..
;
; Inputs:
;  HL = ptr to qr template
;
; Returns:
;  DE = penalty score
;
; Trashes:
;  A,BC,HL
;
calc_penalty3:
            push    ix
            ld      c,QR_DIM
            ld      hl,0
            ld      ix,QR_TMP_PTR
            push    ix

            ; Horizantal searchs..
_row_loop:  ;
            ld      de,40
            ld      b,QR_DIM-11
_col_loop:  ; Brute force.. no skipping of impossible next searches..
            call    penalty3_horizontal1
            call    penalty3_horizontal2
            inc     ix
            djnz    _col_loop

            ; advance to the next row
            ld      de,11+1
            add     ix,de
            dec     c
            jr nz,  _row_loop

            ; Vertical search            
            pop     ix
            ld      c,QR_DIM-11
_col_loop2:  ;
            ld      b,QR_DIM
            push    ix
_row_loop2: ; Brute force.. no skipping of impossible next searches..
            ld      e,40
            call    penalty3_vertical1
            call    penalty3_vertical2
            ld      e,QR_DIM+1
            add     ix,de
            djnz    _row_loop2

            ; advance to the next row
            pop     ix
            inc     ix
            dec     c
            jr nz,  _col_loop2
            
            pop     ix

            ; Return total penalty in DE
            ex      de,hl
            ret


;
; Inputs:
;  IX = ptr to current position in a row
;  HL = current penalty
;  DE = penalty add
;
; Returns:
;  HL = running penalty
;
; Trashes:
;  None. 

penalty3_horizontal1:
            ; index    0123456789a
            ; pattern  10111010000 
            ; next tab 11321214321
_col_loop:
            bit     7,(ix+0)    ; 1
            ret z
            bit     7,(ix+1)    ; 0
            ret nz
            bit     7,(ix+2)    ; 1
            ret z
            bit     7,(ix+3)    ; 1
            ret z
            bit     7,(ix+4)    ; 1
            ret z
            bit     7,(ix+5)    ; 0
            ret nz
            bit     7,(ix+6)    ; 1
            ret z
            bit     7,(ix+7)    ; 0
            ret nz
            bit     7,(ix+8)    ; 0
            ret nz
            bit     7,(ix+9)    ; 0
            ret nz
            bit     7,(ix+10)   ; 0
            ret nz

            ; pattern found.. add penalty by 40
            add     hl,de
            ret

penalty3_horizontal2:
            ; index    a9876543210
            ; pattern  10111010000 
            ; next tab 11321214321
_col_loop:
            bit     7,(ix+10)    ; 1
            ret z
            bit     7,(ix+9)    ; 0
            ret nz
            bit     7,(ix+8)    ; 1
            ret z
            bit     7,(ix+7)    ; 1
            ret z
            bit     7,(ix+6)    ; 1
            ret z
            bit     7,(ix+5)    ; 0
            ret nz
            bit     7,(ix+4)    ; 1
            ret z
            bit     7,(ix+3)    ; 0
            ret nz
            bit     7,(ix+2)    ; 0
            ret nz
            bit     7,(ix+1)    ; 0
            ret nz
            bit     7,(ix+0)   ; 0
            ret nz

            ; pattern found.. add penalty by 40
            add     hl,de
            ret

;
; Returns:
;  IX = ptr to current position in a column
;  HL = current running penalty
;  DE = penalty add
;

penalty3_vertical1:
            push    ix
            push    bc
            ld      bc,4*(QR_DIM+1)

            ; index    0123456789a
            ; pattern  10111010000 
            ; next tab 11321214321
_col_loop:
            bit     7,(ix+0)                ; 1
            jr z,   _failed
            bit     7,(ix+1*(QR_DIM+1))     ; 0
            jr nz,  _failed
            bit     7,(ix+2*(QR_DIM+1))     ; 1
            jr z,   _failed
            bit     7,(ix+3*(QR_DIM+1))      ; 1
            jr z,   _failed
            bit     7,(ix+4*(QR_DIM+1))     ; 1
            jr z,   _failed
            add     ix,bc
            bit     7,(ix+0)                ; 0
            jr nz,  _failed
            bit     7,(ix+1*(QR_DIM+1))     ; 1
            jr z,   _failed
            bit     7,(ix+2*(QR_DIM+1))     ; 0
            jr nz,  _failed
            bit     7,(ix+3*(QR_DIM+1))     ; 0
            jr nz,  _failed
            bit     7,(ix+4*(QR_DIM+1))     ; 0
            jr nz,  _failed
            add     ix,bc
            bit     7,(ix+10)               ; 0
            jr nz,  _failed

            ; pattern found.. add penalty by 40
            add     hl,de
_failed:    ;
            pop     bc
            pop     ix
            ret

penalty3_vertical2:
            push    ix
            push    bc
            ld      bc,4*(QR_DIM+1)


            ; index    0123456789a
            ; pattern  10111010000 
            ; next tab 11321214321
_col_loop:
            bit     7,(ix+0)                ; 0
            jr nz,  _failed
            bit     7,(ix+1*(QR_DIM+1))     ; 0
            jr nz,  _failed
            bit     7,(ix+2*(QR_DIM+1))     ; 0
            jr nz,  _failed
            bit     7,(ix+3*(QR_DIM+1))     ; 0
            jr nz,  _failed
            bit     7,(ix+4*(QR_DIM+1))     ; 1
            jr z,   _failed
            add     ix,bc
            bit     7,(ix+0)                ; 0
            jr nz,  _failed
            bit     7,(ix+1*(QR_DIM+1))     ; 1
            jr z,   _failed
            bit     7,(ix+2*(QR_DIM+1))     ; 1
            jr z,   _failed
            bit     7,(ix+3*(QR_DIM+1))     ; 1
            jr z,   _failed
            bit     7,(ix+4*(QR_DIM+1))     ; 0
            jr nz,  _failed
            add     ix,bc
            bit     7,(ix+10)               ; 1
            jr z,   _failed

            ; pattern found.. add penalty by 40
            add     hl,de
_failed:    ;
            pop     bc
            pop     ix
            ret

;
; Calculate Penalty score 4
;
;
; Inputs:
;  HL = ptr to qr template
;
; Returns:
;  DE = penalty score
;
; Trashes:
;  A,BC,HL
;
calc_penalty4:
            ; Calculate amount of black modules/pixels
            ld      c,QR_DIM
            ld      de,0
_row_loop:  ;
            ld      b,QR_DIM
_col_loop:  ;
            bit     7,(hl)
            jr z,   _not_black
            inc     de
_not_black:
            djnz    _col_loop

            inc     hl          ; Skip hidden alignment octet
            dec     c
            jr nz,  _row_loop

            ; Find percentage of all modules/pixes and round down to
            ; neares 5%. This percentage determination is an approximation
            ; and has some % of error due scaling down the full amount of
            ; modules/pixels to fit in 8 bits. The error is ~0.6% but at
            ; maximum has a peak of ~4%..
            
            ; Make total to fit into E and 8 bits; D becomes 0
            srl     d
            rr      e
            srl     d
            rr      e

            ld      hl,pen4_precent_tab
            db      $fe         ; skip first INC D..
_find_loop:
            inc     d
            ld      a,(hl)
            inc     hl
            cp      e
            jr c,   _find_loop

            ; D = index to found 5% percentage bucket. Multiply by 5.. 
            ld      a,d
            add     a,a
            add     a,a
            add     a,d

            ; A = lower bound of percentage in 5% steps..
            ld      d,a
            sub     50
            jr nc,  _not_negative1
            neg
_not_negative1:
            ld      e,a

            ld      a,d
            sub     45      ; abs((percent+5%) - 50%)
            jr nc,  _not_negative2
            neg
_not_negative2:
            ; min()
            cp      e
            jr c,   _A_smaller
            ld      a,e
_A_smaller: 
            ; multiply lower value by 10 but since it was already in steps of 5 
            ; we just multiply by 2
            add     a,a

            ld      e,a
            ld      d,0
            ret

pen4_precent_tab:
            ;       0      5      10      15      20      25      30    
            db      42>>2, 84>>2, 126>>2, 168>>2, 210>>2, 252>>2, 294>>2
            db      336>>2, 378>>2, 420>>2, 462>>2, 504>>2, 546>>2, 588>>2
            db      630>>2, 672>>2, 714>>2, 756>>2, 798>>2, 841>>2
            db      255 ; end mark
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

; Variables and tables.. the start must be 256 octet aligned.
;

            org     ($+255) & 0xff00
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
            db      $86,$c3,$4c,$01,$86,$c4
            db      $04,$c3,$4c,$01,$c2,$04,$c4
            db      $82,$c5,$4c,$01,$c2,$82,$c6
            db      $82,$c5,$4c,$01,$c2,$82,$c6
            db      $82,$c5,$4c,$01,$c2,$82,$c6
            db      $04,$c3,$4c,$01,$c2,$04,$c2
            db      $86,$d0,$88,$09,$4c,$09
            db      $89,$4c,$89

            db      $46,$01,$5d,$81
            db      $5d,$01,$5d,$81
            db      $5d,$01,$5d,$81
            db      $5d,$01,$5d,$81
            db      $5d,$01,$5d,$81
            db      $5d,$01,$5d,$81

            db      $4d,$85,$45

            db      $08,$81,$4b,$81,$03,$81,$45
            db      $86,$c3,$4b,$c4,$81,$45
            db      $81,$05,$c3,$4b,$81,$03,$81,$45
            db      $c2,$82,$c5,$4b,$85,$45
            db      $c2,$82,$c5,$55
            db      $c2,$82,$c5,$55
            db      $81,$05,$c3,$55
            db      $86,$c3,$55

            db      $00     ; end mark
qr_end:

;
; Follwing buffers are freely relocatable. Note that both
; GF_E2G_PTR and GF_G2E_PTR must have 256 octet alignment.
;

PHR_BUF_PTR:                    ; encode phrase here
            ds      PHR_BUF_SIZE    ; QR_CWDS
ENC_BUF_PTR:
            ds      ENC_BUF_SIZE    ; QR_CWDS_EWDS i.e. code words + ecc words

QR_DST_PTR: ds      QR_DST_SIZE ; QR_DIM*4
QR_TMP_PTR: ds      QR_TMP_SIZE ; 30*QR_DIM


            org ($+255) & 0xff00

            ; These two table must be 256 bytes each and in 256 bytes alignment.
GF_E2G_PTR: ds      256
GF_G2E_PTR: ds      256

test_string:        ; 33 chars
            db      "HTTP://WWW.DEADCODERSSOCIETY.NET/"
            ; 33,11,26,166,95,159,215,220,14,222,160,18,172,69,82,52,34,34
            ; 51,138,119,222,84,157,96,236,17,236,17,236,17,236,17,236,17
            ; 236,17,236,17,236,17,236,17,236,17,236,17,236,17,236,17,236
            ; 17,236,17

    END main




