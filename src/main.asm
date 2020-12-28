; main area
INCLUDE "hardware.inc"
INCLUDE "register_utils.inc"
INCLUDE "print_utils.inc"
INCLUDE "constants.inc"
INCLUDE "interrupt_vectors.inc"
INCLUDE "vram_utils.inc"
INCLUDE "joypad_eval.inc"

SECTION "headers", ROM0[$100]
EntryPoint:
  di
  jp main

REPT $150 - $104
    db 0
ENDR

SECTION "Game code", ROM0
main:
  call init_display
  call init_font
  ld de, vram_start
  ld bc, vram_iterator
  call ld_ibc_de
  ld hl, hello_world
  call print_hl
  call init_joypad_table
  call init_callbacks
  ld de, timer_cb
  call int_set_timer_de
  ld de, read_joypad
  call int_set_joypad_de

;  call init_set_cursor

  ld de, button_a_f
  ld hl, a_cb
  call ld_ide_hl

  ld de, button_b_f
  ld hl, b_cb
  call ld_ide_hl

  ld de, button_select_f
  ld hl, select_cb
  call ld_ide_hl

  ld de, button_start_f
  ld hl, start_cb
  call ld_ide_hl

  ld de, pad_up_f
  ld hl, up_cb
  call ld_ide_hl

  ld de, pad_down_f
  ld hl, down_cb
  call ld_ide_hl

  ld de, pad_left_f
  ld hl, left_cb
  call ld_ide_hl

  ld de, pad_right_f
  ld hl, right_cb
  call ld_ide_hl

  call final_init
  ei
.lockup
  halt
  jp .lockup

  halt
  halt

BG_log_location EQU _SCRN0 + ($20 * $11)



SECTION "up callback", ROM0
up_cb:
  push hl
    push bc
      ld hl, button_up_str
      ld bc, BG_log_location
      call put_string_hl_at_bc
      ld a, [rSCY]
      dec a
      ld [rSCY], a
    pop bc
  pop hl
  ret

SECTION "down callback", ROM0
down_cb:
  push hl
    push bc
      ld hl, button_down_str
      ld bc, BG_log_location
      call put_string_hl_at_bc
      ld a, [rSCY]
      inc a
      ld [rSCY], a
    pop bc
  pop hl
  ret

SECTION "left callback", ROM0
left_cb:
  push hl
    push bc
      ld hl, button_left_str
      ld bc, BG_log_location
      call put_string_hl_at_bc
      ld a, [rSCX]
      dec a
      ld [rSCX], a
    pop bc
  pop hl
  ret

SECTION "right callback", ROM0
right_cb:
  push hl
    push bc
      ld hl, button_right_str
      ld bc, BG_log_location
      call put_string_hl_at_bc
      ld a, [rSCX]
      inc a
      ld [rSCX], a
    pop bc
  pop hl
  ret

SECTION "a callback", ROM0
a_cb:
  push hl
    push bc
      ld hl, button_a_str
      ld bc, BG_log_location
      call put_string_hl_at_bc
    pop bc
  pop hl
  ret

SECTION "b callback", ROM0
b_cb:
  push hl
    push bc
      ld hl, button_b_str
      ld bc, BG_log_location
      call put_string_hl_at_bc
    pop bc
  pop hl
  ret

SECTION "start callback", ROM0
start_cb:
  push hl
    push bc
      ld hl, button_srt_str
      ld bc, BG_log_location
      call put_string_hl_at_bc
    pop bc
  pop hl
  ret

SECTION "select callback", ROM0
select_cb:
  push hl
    push bc
      ld hl, button_sel_str
      ld bc, BG_log_location
      call put_string_hl_at_bc
    pop bc
  pop hl
  ret

SECTION "Timer callback", ROM0
timer_cb:
  call read_joypad
  call eval_joypad
  call increment_timer_cb
  ret

SECTION "Increment Character", ROM0
increment_timer_cb:
  VBLANK_WAIT
  push hl
    push de
      ld hl, amount_cb_invoked
      call ld_de_ihl
      inc de
      call ld_ihl_de
      ld a, e
    pop de
  pop hl
  sub a, $08
  ret c
  push hl
    push de
      ld a, $0
      ld hl, amount_cb_invoked
      ld [hl+], a
      ld [hl], a
    pop de
  pop hl

  push de
    ld de, vram_start
    ld a, [de]
    inc a
    ld [de], a
    inc de
    ld a, [de]
    inc a
    ld [de], a
    sub a, $7F
    jp c, .end
    add a, $7F
    jp .greater_than_x70
.greater_than_x70:
    ld a, "!"
    ld [de], a
.end:
  pop de
  ret

SECTION "init cursor", ROM0
init_set_cursor:
  push hl
    push af
      
      ld hl, _OAMRAM

      ; Sprite 0 - Y position
      ld a, $1 
      ld [hl], a

      ; Sprite 0 - X position
      inc hl
      ld a, $1
      ld [hl], a

      ; Sprite 0 - character
      inc hl
      ld a, $4
      ld [hl], a

      ; Sprite 0 - flags
      inc hl
      ld a, $0
      ld [hl], a
    pop af
  pop hl
  ret

SECTION "Finalize initialization", ROM0
final_init:
  ; Init display registers
  ld a, %11100100
  ld [rBGP], a

  xor a ; ld a, 0
  ld [rSCY], a
  ld [rSCX], a

  ; Shut sound down
  ld [rNR52], a

  ; Turn screen on, display background
  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_OBJON | LCDCF_BG8000
  ld [rLCDC], a

  ld a, [rTAC]
  or a, TACF_START ; enable timer
  ld [rTAC], a

  ld a, [rIE]
  or a, IEF_HILO
  or a, IEF_SERIAL
  or a, IEF_TIMER
  or a, IEF_LCDC
  or a, IEF_VBLANK
  ld [rIE], a

  ret

SECTION "Initialize display", ROM0
init_display:
.waitVBlank
  ld a, [rLY]
  cp $90 ; Check if the LCD is past VBlank
  jr c, .waitVBlank

  xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
  ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.
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

SECTION "Font", ROM0

;FontTiles:
;INCBIN "font.chr"
;FontTilesEnd:

FontTiles:
INCLUDE "font_redone.inc"
INCLUDE "font_other.inc"
FontTilesEnd:

SECTION "strings", ROM0
hello_world:
  db "Hello World!                    "
  db "Hi!                             "
  db "How are you?                    "
  db " X                              "
  db "     X                          "
  db "\n"
  db " AVCD                           "
  db "                                "
  db "Nutritious                      "
  db "Vital sustainence               "
  db "Hold it!                        "
  db "Did you say                     "
  db "stuff?                          "
  db "1234567890abcdefghij            "
  dB "klmnopqrstuvwxyzABCD            "
  db "EFGHIJKLMNOPQRSTUVWX            "
  db "YZ_-                            "
  db "                 END           ", $0

button_a_str: db     "A press!      \n", $0
button_b_str: db     "B press!      \n", $0
button_sel_str: db   "select press! \n", $0
button_srt_str: db   "start press!  \n", $0
button_up_str: db    "up press!     \n", $0
button_down_str: db  "down press!   \n", $0
button_left_str: db  "left press!   \n", $0
button_right_str: db "right press!  \n", $0
