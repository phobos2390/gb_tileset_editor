IF !DEF(PRINT_UTILS_INC)
PRINT_UTILS_INC SET 1

INCLUDE "constants.inc"
INCLUDE "register_utils.inc"
INCLUDE "vram_utils.inc"

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

SECTION "Print high low", ROM0
print_hl:
  push de
    push bc
      ld bc, vram_iterator
      call ld_de_ibc
.loop:
      ld a, [hl]
      cp $0
      jp z, .end
        cp "\n" ; $0a ; '\n'
        jp nz, .default
          call ld_ibc_de
          call print_newline
          call ld_de_ibc
          inc hl
          jp .loop
.default:
      ld [de], a
      inc de
      inc hl
      jp .loop
.end:
      call ld_ibc_de
    pop bc
  pop de
  ret

;back_width EQU $20
;width_mask EQU $E0

SECTION "Print newline", ROM0
print_newline:
  push de
    push bc
      push hl
        ld hl, vram_iterator
        LD_BC_HL
        call ld_hl_ihl
        ld de, back_width
        add hl, de
        ld a, l
        and width_mask
        ld l, a
        LD_DE_HL
        call ld_ibc_de
      pop hl
    pop bc
  pop de
  ret

ENDC ; PRINT_UTILS_INC
