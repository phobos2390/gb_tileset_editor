IF !DEF(VBLANK_UTILS_INC)
VBLANK_UTILS_INC SET 1

INCLUDE "mem_utils.inc"
INCLUDE "register_utils.inc"
INCLUDE "struct_utils.inc"
INCLUDE "func_utils.inc"
INCLUDE "vram_utils.inc"

vblank_list_length EQU $A

SECTION "Vblank list", WRAM0
vblank_list_start:
vblank_list_entry:
.begin:
  .callback_f: DS 2
  .context: DS 2
.end:
vblank_list_entry_size EQU vblank_list_entry.end - vblank_list_entry.begin
REPT vblank_list_length - 1
  DS vblank_list_entry_size
ENDR
DS 0
vblank_list_end:

SECTION "Vblank init", ROM0
init_vblank_list:
  PUSH_HL_BC_DE
    MEMSET vblank_list_start, 0, vblank_list_end - vblank_list_start
  POP_HL_BC_DE
  ret

SECTION "add vblank list entry handler", ROM0
add_vblank_list_entry_hl_bc:
  push de
    push hl
      ld de, vblank_list_entry_size
      ld hl, vblank_list_start
      ld a, [vblank_list_entry.callback_f]
.loop:
      cp $0
      jp z, .end
        add hl, de
        ld a, [hl]
        jp .loop
.end:
      LD_DE_HL
    pop hl
    call ld_ide_hl
    INC_DE_W
    call ld_ide_bc
  pop de
  ret

SECTION "Vblank list entry callback", ROM0
vblank_list_entry_handle:
  PUSH_HL_BC_DE
    call ld_de_ihl
    INC_HL_W
    call ld_bc_ihl
    LD_HL_DE
    CALL_HL
  POP_HL_BC_DE
  ret

SECTION "Vblank callback", ROM0
vblank_cb:
  RET_V
  PUSH_HL_BC
    ld bc, vblank_list_entry_size
    ld hl, vblank_list_start
    ld a, [hl]
.loop:
    cp $0
    jp z, .end
      call vblank_list_entry_handle
      add hl, bc
      ld a, [hl]
      jp .loop
.end:
    ; clear list once callback is completed
    MEMSET vblank_list_start, 0, vblank_list_end - vblank_list_start
  POP_HL_BC
  ret

ENDC ; VBLANK_UTILS_INC