IF !DEF(HALF_HEX_DIGITS_INC)
HALF_HEX_DIGITS_INC SET 1

number_of_hex_digits EQU 2

SECTION "Hex Digit Min", ROM0
hex_digit_minified_list:
.first_begin:
.char_0:
;$0
DB %00001110
DB %00001010
DB %00001010
DB %00001010
DB %00001110 
.first_end:

.char_1:
;$1
DB %00000100
DB %00001100
DB %00000100
DB %00000100
DB %00001110

.char_2:
;$2
DB %00001110
DB %00000010
DB %00001110
DB %00001000
DB %00001110

.char_3:
;$3
DB %00001110
DB %00000010
DB %00000110
DB %00000010
DB %00001110

.char_4:
;$4
DB %00001010
DB %00001010
DB %00001110
DB %00000010
DB %00000010

.char_5:
;$5
DB %00001110
DB %00001000
DB %00001110
DB %00000010
DB %00001110

.char_6:
;$6
DB %00001110
DB %00001000
DB %00001110
DB %00001010
DB %00001110 

.char_7:
;$7
DB %00001110
DB %00000010
DB %00000100
DB %00001000
DB %00001000

.char_8:
;$8
DB %00001110
DB %00001010
DB %00001110
DB %00001010
DB %00001110

.char_9:
;$9
DB %00001110
DB %00001010
DB %00001110
DB %00000010
DB %00001110

.char_A:
;$A
DB %00001110
DB %00001010
DB %00001110
DB %00001010
DB %00001010

.char_B:
;$B
DB %00001100
DB %00001010
DB %00001100
DB %00001010
DB %00001100

.char_C:
;$C
DB %00001110
DB %00001000
DB %00001000
DB %00001000
DB %00001110

.char_D:
;$D
DB %00001100
DB %00001010
DB %00001010
DB %00001010
DB %00001100

.char_E:
;$E
DB %00001110
DB %00001000
DB %00001100
DB %00001000
DB %00001110

.char_F:
;$F
DB %00001110
DB %00001000
DB %00001100
DB %00001000
DB %00001000

hex_digit_minified_list_end:

SECTION "Hex digits temp", WRAM0
hex_digit_tmp:
DS (hex_digit_minified_list.first_end - hex_digit_minified_list.first_begin)

SECTION "Get_char_a_tileset_location_hl", ROM0
get_char_a_tileset_offset_hl:
  ld h, 0
  ld l, a
  MULT_HL_16
  ret

SECTION "Get_char_a_tileset_location_hl", ROM0
set_char_b_to_hex_byte_a_at_de:
  PUSH_HL_BC_DE
    ld c, a
    ld a, b
    call get_char_a_tileset_offset_hl
    add hl, de
    ;VBLANK_WAIT
    ld a, c
    call stitch_hex_digit_a_hl
  POP_HL_BC_DE
  ret

SECTION "Stitch hex_digit", ROM0
stitch_hex_digit_a_hl:
  PUSH_HL_BC
    push af
      LD_DE_HL
      REPT number_of_hex_digits
        push de
          push af
            and $0F
            ld h, $0
            ld l, a
            LD_BC_HL
            MULT_HL_4
            add hl, bc
            ld bc, hex_digit_minified_list
            REPT (hex_digit_minified_list.first_end - hex_digit_minified_list.first_begin)
              push hl
                add hl, bc
                ld a, [hl]
                inc bc
              pop hl
              push hl
                ld l, a
                ld a, [de]
                cp $0
                jr z, .noswap_a\@
                ld a, l
                swap a
                jr .swap_a_end\@
.noswap_a\@:
                ld a, l
.swap_a_end\@:
                LD_HL_DE
                or [hl]       
                ld [hl+], a
                ld [hl], a
                INC_DE_W
              pop hl
            ENDR
          pop af
          swap a
        pop de
      ENDR
    pop af
  POP_HL_BC
  ret

;SECTION "Hex Digit High", ROM0
;DB $E0, $E0, $A0, $A0, $A0, $A0, $A0, $A0, $E0, $E0 
;DB $40, $40, $C0, $C0, $40, $40, $40, $40, $E0, $E0
;DB $E0, $E0, $20, $20, $E0, $E0, $80, $80, $E0, $E0
;DB $E0, $E0, $20, $20, $60, $60, $20, $20, $E0, $E0
;DB $A0, $A0, $A0, $A0, $E0, $E0, $20, $20, $20, $20
;DB $E0, $E0, $80, $80, $E0, $E0, $20, $20, $E0, $E0
;DB $E0, $E0, $80, $80, $E0, $E0, $A0, $A0, $E0, $E0 
;DB $E0, $E0, $20, $20, $40, $40, $80, $80, $80, $80
;DB $E0, $E0, $A0, $A0, $E0, $E0, $A0, $A0, $E0, $E0
;DB $E0, $E0, $A0, $A0, $E0, $E0, $20, $20, $E0, $E0
;DB $E0, $E0, $A0, $A0, $E0, $E0, $A0, $A0, $A0, $A0
;DB $C0, $C0, $A0, $A0, $C0, $C0, $A0, $A0, $C0, $C0
;DB $E0, $E0, $80, $80, $80, $80, $80, $80, $E0, $E0
;DB $C0, $C0, $A0, $A0, $A0, $A0, $A0, $A0, $C0, $C0
;DB $E0, $E0, $80, $80, $C0, $C0, $80, $80, $E0, $E0
;DB $E0, $E0, $80, $80, $C0, $C0, $80, $80, $80, $80


;SECTION "Hex Digit Low", ROM0
;DB $E, $E, $A, $A, $A, $A, $A, $A, $E, $E 
;DB $4, $4, $C, $C, $4, $4, $4, $4, $E, $E
;DB $E, $E, $2, $2, $E, $E, $8, $8, $E, $E
;DB $E, $E, $2, $2, $6, $6, $2, $2, $E, $E
;DB $A, $A, $A, $A, $E, $E, $2, $2, $2, $2
;DB $E, $E, $8, $8, $E, $E, $2, $2, $E, $E
;DB $E, $E, $8, $8, $E, $E, $A, $A, $E, $E 
;DB $E, $E, $2, $2, $4, $4, $8, $8, $8, $8
;DB $E, $E, $A, $A, $E, $E, $A, $A, $E, $E
;DB $E, $E, $A, $A, $E, $E, $2, $2, $E, $E
;DB $E, $E, $A, $A, $E, $E, $A, $A, $A, $A
;DB $C, $C, $A, $A, $C, $C, $A, $A, $C, $C
;DB $E, $E, $8, $8, $8, $8, $8, $8, $E, $E
;DB $C, $C, $A, $A, $A, $A, $A, $A, $C, $C
;DB $E, $E, $8, $8, $C, $C, $8, $8, $E, $E
;DB $E, $E, $8, $8, $C, $C, $8, $8, $8, $8



ENDC ; HALF_HEX_DIGITS_INC
