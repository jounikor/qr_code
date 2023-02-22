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

QR_WHITE    equ     0x00
QR_BLACK    equ     0xff
QR_WHITE_TMP    equ 0x01
QR_BLACK_TMP    equ 0x02
QR_EMPTY    equ     0x04


QR_DIM      equ     29
QR_VER      equ     3
QR_LEV      equ     'L'

            org $8000


main:       exx
            push    hl
            push    de
            push    bc
            exx

            call    gf_init
            call    decode_qr_template_2

            ld      de,0000000100000001b
            ;ld      de,1111111111111111b
            call    insert_mask
            call    print_2

            ;ld      hl,test_phrase
            ;ld      c,55
            ;call    polydiv

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
            ;db      16,16,16,16,16,16, 224,16,32,16,16,16,16,16,0
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

            and     a
            jr z,   _white

            xor     10000000b
            jr z,   _black

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
;  C  = length of the polynomial, i.e. the encoded phrase.
;  HL = ptr to polynomial (size len poly)
;
; Returns:
;  HL = ptr to ecc code words (15 of those)
;
; Trashes:
;  A,B,C,D,E,H,L
;

polydiv:    ;
            ; Move polynomial into to ecc_buffer for division 
            ld      a,55+15
            sub     c
            ld      b,0
            ld      de,ECC_BUFFER_PTR

            push    de
            ldir
            
            ; Clear the rest of the ecc_buffer
_clear:     ld      (hl),0
            inc     hl
            dec     a
            jr nz,  _clear

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
            pop     hl
            dec     c
            jr nz,  _div_main
            
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
; - adding extra 7 alignment bits
;
; Inputs:
;  HL = prt to NUL terminated string
;  DE = ptr to destination (must have space for 55+15 octets
;
; Returns:
;  none
;
encode_phrase:


            ld      a,1100000b
            and     b
            sla     b
            sla     b

    IF 0

decode_qr_template:
            ld      de,qr_template
            ld      hl,QR_TMP_PTR
_main:      ld      a,(de)
            cp      11111111b
            ret z   
            ;
            inc     de
            ld      c,a
            ld      b,4
_loop:      ld      a,11000000b
            and     c
            ld      (hl),a
            inc     hl
            sla     c
            sla     c
            djnz    _loop
            jr      _main

qr_template:
            ; 00b = white -> 00000000b
            ; 01b = empty -> 01000000b
            ; 10b = black -> 10000000b
            ; 11b = invalid
            ;
            db      10101010b,10101000b,10010101b,01010101b,01010101b,01001010b,10101010b,10000000b
            db      10000000b,00001000b,10010101b,01010101b,01010101b,01001000b,00000000b,10000000b
            db      10001010b,10001000b,10010101b,01010101b,01010101b,01001000b,10101000b,10000000b
            db      10001010b,10001000b,10010101b,01010101b,01010101b,01001000b,10101000b,10000000b
            db      10001010b,10001000b,10010101b,01010101b,01010101b,01001000b,10101000b,10000000b
            db      10000000b,00001000b,10010101b,01010101b,01010101b,01001000b,00000000b,10000000b
            db      10101010b,10101000b,10001000b,10001000b,10001000b,10001010b,10101010b,10000000b
            db      00000000b,00000000b,10010101b,01010101b,01010101b,01000000b,00000000b,00000000b
            db      10101010b,10101010b,10010101b,01010101b,01010101b,01101010b,10101010b,10000000b
            db      01010101b,01010001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01011001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01010001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01011001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01010001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01011001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01010001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01011001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01010001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01011001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01010001b,01010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      01010101b,01011001b,01010101b,01010101b,01010101b,10101010b,10010101b,01000000b
            db      00000000b,00000000b,10010101b,01010101b,01010101b,10000000b,10010101b,01000000b
            db      10101010b,10101000b,10010101b,01010101b,01010101b,10001000b,10010101b,01000000b
            db      10000000b,00001000b,10010101b,01010101b,01010101b,10000000b,10010101b,01000000b
            db      10001010b,10001000b,10010101b,01010101b,01010101b,10101010b,10010101b,01000000b
            db      10001010b,10001000b,10010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      10001010b,10001000b,10010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      10000000b,00001000b,10010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      10101010b,10101000b,10010101b,01010101b,01010101b,01010101b,01010101b,01000000b
            db      11111111b
    ENDIF

decode_qr_template_2:
            ld      de,qr_template_2
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


qr_template_2:
            db      $87,$01,$4d,$01,$86,$c4
            db      $04,$c2,$4d,$01,$c2,$04,$c4
            db      $82,$c4,$4d,$01,$c2,$82,$c6
            db      $82,$c4,$4d,$01,$c2,$82,$c6
            db      $82,$c4,$4d,$01,$c2,$82,$c6
            db      $04,$c2,$4d,$01,$c2,$04,$c2
            db      $86,$d0,$86,$c2,$08,$4d,$09,$46
            
            db      $81,$5d
            db      $01,$5d
            db      $81,$5d
            db      $01,$5d
            db      $81,$5d
            db      $01,$5d
            db      $81,$5d
            db      $01,$5d
            db      $81,$5d
            db      $01,$5d
            db      $81,$5d
            db      $01,$5d

            db      $81,$4d,$85,$45

            db      $08,$81,$4b,$81,$03,$81,$45
            db      $87,$01,$4c,$c4,$81,$45
            db      $81,$05,$c2,$4c,$81,$03,$81,$45
            db      $c2,$82,$c4,$4c,$85,$45
            db      $c2,$82,$c4,$56
            db      $c2,$82,$c4,$56
            db      $81,$05,$81,$56
            db      $87,$01,$56

            db      $00
qr_ee:




            org     ($+255) & 0xff00
alnum_map:
patterns:
            ; Generator polynomial for 3-L
gen_15:     db      8,183,61,91,202,37,51,58,58,237,140,124,5,99,105
            db      0                       ; make mask_pattern start even
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
            ds      6       ; skip ;<=>?@
            ;db      'A','B','C','D','E','F','G','H','I'
            db       10, 11, 12, 13, 14, 15, 16, 17, 18
            ;db      'J','K','L','M','N','O','P','Q','R'
            db       19, 20, 21, 22, 23, 24, 25, 26, 27
            ;db      'S','T','U','V','W','X','Y','Z' 
            db       28, 29, 30, 31, 32, 33, 34, 35
            ; after this we have 256-90 left for this 256 bytes segment


; '3-L':eccInfo(3,55,15,1,55,0,0,0b01),  # 29x29
;
; 1 group #1
; 1 block in group #1
; 55 code words
; 15 ecc words
; level 0b01
; 7 alignment bits
; 1 alignment patter in (6,22)
; 


ECC_BUFFER_PTR:
            ds  55+15       ; code words + ecc words




            org ($+255) & 0xff00

            ; These two table must be 256 bytes each and in 256 bytes alignment.
GF_E2G_PTR: ds  256
GF_G2E_PTR: ds  256


QR_DST_PTR: ds      QR_DIM*4

            org ($+255) & 0xff00
QR_TMP_PTR: ds      32*QR_DIM


test_phrase:        ; 55
            db      0x20,0xcb,0x1a,0xa6,0x54,0x63,0xdd,0x20,0x73,0xba
            db      0x9c,0xd4,0x95,0xda,0x8a,0x9d,0xea,0x67,0xd6,0x00
            db      0xec,0x11,0xec,0x11,0xec,0x11,0xec,0x11,0xec,0x11
            db      0xec,0x11,0xec,0x11,0xec,0x11,0xec,0x11,0xec,0x11
            db      0xec,0x11,0xec,0x11,0xec,0x11,0xec,0x11,0xec,0x11
            db      0xec,0x11,0xec,0x11,0xec

; should produce ECC
; 0x22,0xa3,0x53,0x01,0xa3,0x34,0xfc,0x98,0x55,0xf7,0x9d,0x8c,0xa0,0xad,0x90


    END main




