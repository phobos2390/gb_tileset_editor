IF !DEF(SRAM_UTILS_INC)
SRAM_UTILS_INC SET 1

INCLUDE "hardware.inc"

SECTION "Read SRAM", ROM0
; memcopy_from_sram()
; Params
;  bc - IN & OUT size
;  de - destination pointer
memcopy_from_sram:
  LD_A_ADDR_VAL rRAMG, CART_RAM_ENABLE ; enable SRAM read
  push hl
    ld hl, _SRAM
    call ld_hl_ihl

    push hl
      ld bc, $1 ; $1 + $FFFF = $0000 
      add hl, bc
      ld a, h
      or l
      jp z, .dest_size_too_small
    pop hl

    INV_BC
    push hl
      add hl, bc ; size to write  
      jp c, .size_correct ; continue if bc >= hl 
      jp .dest_size_too_small ; end if bc < hl
.size_correct:
    pop hl
    LD_BC_HL ; bc size of data to copy
    ; de destination
    ld hl, _SRAM + $2 ; hl source
    PUSH_HL_BC_DE
      call memcopy
    POP_HL_BC_DE
    jp .end
.dest_size_too_small:
    pop hl
    ld bc, 0
.end:
  pop hl
  LD_A_ADDR_VAL rRAMG, CART_RAM_DISABLE ; disable SRAM read when done
  ret

SECTION "copy SRAM fast", ROM0
memcopy_fast_sram:
  LD_A_ADDR_VAL rRAMG, CART_RAM_ENABLE ; enable SRAM read
  call memcopy_fast
  LD_A_ADDR_VAL rRAMG, CART_RAM_DISABLE ; disable SRAM read when done
  ret

SECTION "memset sram", ROM0
memset_sram:
  LD_A_ADDR_VAL rRAMG, CART_RAM_ENABLE ; enable SRAM read
  ld hl, _SRAM
  call ld_ihl_bc
  INC_HL_W
  call memset
  LD_A_ADDR_VAL rRAMG, CART_RAM_DISABLE ; disable SRAM read when done
  ret

SECTION "Write SRAM", ROM0
; memcopy_from_sram()
; Params
;  bc - size
;  hl - source pointer
memcopy_to_sram:
  LD_A_ADDR_VAL rRAMG, CART_RAM_ENABLE ; enable SRAM read
  push de
    ld de, _SRAM
    call ld_ide_bc
    INC_DE_W
    PUSH_HL_BC
      ; de - destination
      ; hl - src
      ; bc - size
      call memcopy
    POP_HL_BC
  pop de
  LD_A_ADDR_VAL rRAMG, CART_RAM_DISABLE ; disable SRAM read when done
  ret

ENDC ; SRAM_UTILS_INC