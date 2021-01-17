IF !DEF(PRINT_UTILS_INC)
PRINT_UTILS_INC SET 1

INCLUDE "constants.inc"
INCLUDE "register_utils.inc"
INCLUDE "vram_utils.inc"

escape_char EQU $5C

SECTION "put string hl at bc", ROM0
put_string_hl_at_bc:
  VBLANK_WAIT
  call put_string_hl_at_bc_in_vblank
  ret

SECTION "put string hl at bc in vblank", ROM0
put_string_hl_at_bc_in_vblank:
  push hl
    push de
      push hl
        ld hl, vram_iterator
        call ld_de_ihl
        call ld_ihl_bc
      pop hl
      call print_hl
      ld hl, vram_iterator
      call ld_ihl_de
    pop de
  pop hl
  ret

SECTION "put string hl at bc in vblank", ROM0
print_hl_at_b_row_c_column_vblank:
  call init_vram_iterator
.line_number:
  dec b
  jp c, .set_offset
    call print_newline
    jp .line_number
.set_offset:
  push hl
    ld hl, newline_offset
    call ld_ihl_bc
  pop hl
  call print_hl
  ret

SECTION "Print high low", ROM0
print_hl:
  PUSH_BC_DE
    ld bc, vram_iterator
    call ld_de_ibc
.loop:
    ld a, [hl+]
    cp escape_char
    jp z, .escape
    cp $0
    jp z, .end
      cp "\n" ; $0a ; '\n'
      jp nz, .default
        call ld_ibc_de
        call print_newline
        call ld_de_ibc
        jp .loop
.escape:
    ld a, [hl+]
.default:
    ld [de], a
    inc de
    jp .loop
.end:
    call ld_ibc_de
  POP_BC_DE
  ret

;back_width EQU $20
;width_mask EQU $E0

SECTION "Print newline", ROM0
print_newline:
  PUSH_HL_BC_DE
    ld hl, vram_iterator
    LD_BC_HL
    call ld_hl_ihl
    ld de, back_width
    add hl, de
    ld a, l
    and width_mask
    ld l, a

    ld a, [newline_offset]
    ld e, a
    ld a, [newline_offset+1]
    ld d, a

    add hl, de

    LD_DE_HL
    call ld_ibc_de
  POP_HL_BC_DE
  ret

PRINT_ADDR : MACRO
  CALL_WITH_HL_PARAM print_hl, \1
ENDM

PRINT_ADDR_U : MACRO
  CALL_WITH_HL_PARAM_U print_hl, \1
ENDM

ENDC ; PRINT_UTILS_INC
