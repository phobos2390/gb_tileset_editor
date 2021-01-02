IF !DEF(OAM_UTILS_INC)
OAM_UTILS_INC SET 1

INCLUDE "struct_utils.inc"

number_of_sprites EQU $28

WASTE_160MS : MACRO
  ld a, $40
.loop:
  dec a
  jr nz, .loop
ENDM

SECTION "DMA subroutine load", ROM0
dma_subroutine_load:
  push bc
    push hl
      ld b, LOW(dma_subroutine_end - dma_subroutine)
      ld c, LOW(_HRAM) 
      ld hl, dma_subroutine
      call memcopy_hram
    pop hl
  pop bc
  ret

SECTION "DMA subroutine", ROM0
dma_subroutine:
  ld [rDMA], a
  WASTE_160MS
  ret
dma_subroutine_end:

SECTION "OAM DMA move", HRAM
dma_subroutine_hram:
  DS (dma_subroutine_end - dma_subroutine)

SECTION "Sprite table", WRAM0,ALIGN[8]
sprite_table:
sprite_0:
sprite:
.begin:
.first_begin:
  .y_position: DS 1
  .x_position: DS 1
  .character:  DS 1
  .flags:      DS 1
.end:
.first_end:
sizeof_sprite EQU sprite.first_end - sprite.first_begin
REPT (number_of_sprites - 1)
    DS sizeof_sprite
ENDR
sprite_table_end:

;sizeof_sprite EQU sprite.first_end - sprite.first_begin

SECTION "Update sprites", ROM0
dma_update_sprites:
  ld a, HIGH(sprite_table)
  call dma_subroutine_hram
  ret

SECTION "Init sprite table", ROM0
init_sprite_table:
  PUSH_HL_BC
    ld hl, sprite_table
    ld a, $0
    ld b, LOW(sprite_table_end - sprite_table)
    call memset_fast
  POP_HL_BC
  call dma_subroutine_load
  ret

; set_sprite()
;   b - x position
;   c - y position
;   d - character tile 
;   e - flags
;   hl - address
SECTION "Update sprite at hl", ROM0
set_sprite_at_hl:
.update_y:
  ld [hl], c
.update_x:
  inc hl
  ld [hl], b
.update_character:
  inc hl
  ld [hl], d
.update_flags:
  inc hl
  ld [hl], e
  ret  

SECTION "Update sprite", ROM0
; set_sprite()
;   b - x position
;   c - y position
;   d - character tile 
;   e - flags
;   a - sprite number
set_sprite_a:
  push hl
    SET_HL_TO_STRUCT_INDEX_A sprite
    call set_sprite_at_hl
  pop hl
  ret

ENDC