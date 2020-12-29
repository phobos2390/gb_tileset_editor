INCLUDE "hardware.inc"
INCLUDE "vram_utils.inc"
INCLUDE "test_struct_def.inc"
INCLUDE "constants.inc"
INCLUDE "print_utils.inc"
INCLUDE "interrupt_vectors.inc"

SECTION "Game code", ROM0
main:
  call init_display
  call init_font
  call load_tests
  call run_tests
  call final_init
  ei
.lockup
  halt
  jp .lockup
  halt
  halt

SECTION "Initialize display", ROM0
init_display:
  VBLANK_WAIT

  xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
  ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.
  
  call init_vram_iterator
  call clear_screen
  ret

SECTION "Initialize font", ROM0
init_font:
  ld hl, tileset_start
  ld de, FontTiles
  ld bc, FontTilesEnd - FontTiles
.copyFont
  ld a, [de] ; Grab 1 byte from the source
  ld [hli], a ; Place it at the destination, incrementing hl
  inc de ; Move to next byte
  dec bc ; Decrement count
  ld a, b ; Check if count is 0, since `dec bc` doesn't update flags
  or c
  jr nz, .copyFont
  ret

SECTION "Finalize initialization", ROM0
final_init:
  ; Init display registers
  ld a, %11100100
  ld [rBGP], a

  xor a ; ld a, 0
  ld [rSCY], a ; scroll 0
  ld [rSCX], a ; scroll 1

  ; Shut sound down
  ld [rNR52], a

  ; Turn screen on, display background
  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG8000
  ld [rLCDC], a

  ret

SECTION "Font", ROM0
FontTiles:
INCLUDE "font_redone.inc"
FontTilesEnd:


