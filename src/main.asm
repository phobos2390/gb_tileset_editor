; main area
INCLUDE "vblank_utils.inc"
INCLUDE "hardware.inc"
INCLUDE "register_utils.inc"
INCLUDE "print_utils.inc"
INCLUDE "constants.inc"
INCLUDE "interrupt_vectors.inc"
INCLUDE "vram_utils.inc"
INCLUDE "joypad_eval.inc"
INCLUDE "oam_utils.inc"
INCLUDE "timing_utils.inc"

window_layer_start EQU _SCRN1
character_edit_bg EQU window_layer_start + $21
bg_log_location EQU window_layer_start + $2B ;_SCRN0 + ($20 * $10)
mode_log_location EQU window_layer_start + $B + ($20 * $2) ; _SCRN0 + ($20 * $11)
selected_character_window_position EQU window_layer_start + $8B

character_list_vram_start EQU vram_start + $22
selected_character_position EQU vram_start + $20 + $12

SECTION "Game code", ROM0
main:
  call init_display
  call init_font
  call init_window
  call init_vblank_list
  call init_timing_table

  ld de, vram_start
  ld bc, vram_iterator
  call ld_ibc_de

  PRINT_ADDR_U character_list_box

;  ld de, character_list_vram_start
;  ld bc, vram_iterator
;  call ld_ibc_de

;  PRINT_ADDR_U whole_tileset

  ld hl, window_background_str
  ld bc, window_layer_start
  call put_string_hl_at_bc_in_vblank

  call init_joypad_table
  call init_callbacks

  ld de, timing_table_cb
  call int_set_timer_de
  
;  ld hl, vblank_cb
;  ld bc, $0 ; null context
;  ld de, 1 ; update every tick
;  call add_timing_table_entry_callback

  ld hl, timer_cb
  ld bc, $0 ; null context
  ld de, 1 ; update every tick
  call add_timing_table_entry_callback

  ld hl, update_cursor_position
  ld bc, $0 ; null context
  ld de, 1 ; update every tick
  call add_timing_table_entry_callback

  ld hl, update_cursor_character
  ld bc, $0
  ld de, $8
  call add_timing_table_entry_callback;add_vblank_enabled_timing_table_entry_callback  

  ld hl, dma_update_sprites
  ld bc, $0 ; null context
  ld de, 1 ; update every tick
  call add_timing_table_entry_callback

  ld de, read_joypad
  call int_set_joypad_de

  call init_sprite_table
  call init_set_cursor
  
  call default_dpad_callback_init
  call default_button_callback_init
  call char_select_mode

  ld hl, edit_mode_str
  ld bc, mode_log_location
  call put_string_hl_at_bc_in_vblank

  call final_init
  ei
.lockup
  halt
  jp .lockup

  halt
  halt


SECTION "default dpad callback init", ROM0
default_dpad_callback_init:
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
  ret

SECTION "default button callback init", ROM0
default_button_callback_init:
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
  ret

SECTION "up callback", ROM0
up_cb:
  push hl
    push bc
      ld hl, button_up_str
      ld bc, bg_log_location
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
      ld bc, bg_log_location
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
      ld bc, bg_log_location
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
      ld bc, bg_log_location
      call put_string_hl_at_bc
      ld a, [rSCX]
      inc a
      ld [rSCX], a
    pop bc
  pop hl
  ret

SECTION "a callback", ROM0
a_cb:
;  push hl
;    push bc
;      ld hl, button_a_str
;      ld bc, bg_log_location
;      call put_string_hl_at_bc
;    pop bc
;  pop hl
  ret

SECTION "b callback", ROM0
b_cb:
;  push hl
;    push bc
;      ld hl, button_b_str
;      ld bc, bg_log_location
;      call put_string_hl_at_bc
;    pop bc
;  pop hl
  ret

SECTION "start callback", ROM0
start_cb:
  push hl
    push bc
;      ld hl, button_srt_str
;      ld bc, bg_log_location
;      call put_string_hl_at_bc

;      ld hl, edit_mode_str
;      ld bc, mode_log_location
;      call put_string_hl_at_bc_in_vblank

      call char_select_mode
    pop bc
  pop hl
  ret

SECTION "select callback", ROM0
select_cb:
;  push hl
;    push bc
;      ld hl, button_sel_str
;      ld bc, bg_log_location
;      call put_string_hl_at_bc
;    pop bc
;  pop hl
  ret

SECTION "Timer callback", ROM0
timer_cb:
  call joypad_cb
  ;call increment_timer_cb
  ret

SECTION "Joypad callback stats", WRAM0
joypad_cb_amount_invoked: DS 2

SECTION "Joypad callback", ROM0
joypad_cb:
  push hl
    push de
      ld hl, joypad_cb_amount_invoked
      call ld_de_ihl
      inc de
      call ld_ihl_de
      ld a, e
    pop de
  pop hl
  cp $2
  ret c
  ld a, 0
  ld [joypad_cb_amount_invoked], a
  ld [joypad_cb_amount_invoked+1], a
  
  call read_joypad
  call eval_joypad
  ret

SECTION "Sprite character", WRAM0
cursor_character: DS 1
cursor_x: DS 1
cursor_y: DS 1
char_select_cursor_x: DS 1
char_select_cursor_y: DS 1
char_select_character: DS 1
mode_minimum_x: DS 1
mode_maximum_x: DS 1
mode_minimum_y: DS 1
mode_maximum_y: DS 1
selected_character: DS 1
selected_character_data: DS 2

min_x       EQU $8 + $8
min_y       EQU $10 + $8
max_x       EQU $A0 - $18
max_x_comp  EQU $A8 - $18
max_y       EQU $98 - $8
max_y_comp  EQU $A0 - $8
step_size   EQU $8

char_column EQU $10

SECTION "Sprite up dpad", ROM0
char_select_up_cb:
  push bc
    ld a, [mode_minimum_y]
    ld b, a
    ld a, [mode_maximum_y]
    ld c, a

    ld a, [selected_character]
    sub char_column

    ld [selected_character], a
    ld a, [cursor_y]
    sub step_size
    cp b
    jp nc, .store_a
      ld a, c
.store_a:
    ld [cursor_y], a
  pop bc
  ret

SECTION "Sprite down dpad", ROM0
char_select_down_cb:
  push bc
    ld a, [mode_minimum_y]
    ld b, a
    ld a, [mode_maximum_y]
    add (max_y_comp - max_y)
    ld c, a

    ld a, [selected_character]
    add char_column
    ld [selected_character], a
    ld a, [cursor_y]
    add step_size
    cp c
    jp c, .store_a
      ld a, b
.store_a:
    ld [cursor_y], a
  pop bc
  ret

SECTION "Sprite left dpad", ROM0
char_select_left_cb:
  push bc
    ld a, [mode_minimum_x]
    ld b, a
    ld a, [mode_maximum_x]
    ld c, a

    ld a, [selected_character]
    dec a
    ld [selected_character], a
    ld a, [cursor_x]
    sub step_size
    cp b
    jp nc, .store_a
      ld a, [selected_character]
      add char_column
      ld [selected_character], a
      ld a, c
.store_a:
    ld [cursor_x], a
  pop bc
  ret

SECTION "Sprite right dpad", ROM0
char_select_right_cb:
  push bc
    ld a, [mode_minimum_x]
    ld b, a
    ld a, [mode_maximum_x]
    add  (max_x_comp - max_x)
    ld c, a

    ld a, [selected_character]
    inc a
    ld [selected_character], a
    ld a, [cursor_x]
    add step_size
    cp c
    jp c, .store_a
      ld a, [selected_character]
      sub char_column
      ld [selected_character], a
      ld a, b
.store_a:
    ld [cursor_x], a
  pop bc
  ret

SECTION "Cursor select mode a", ROM0
char_select_a_cb:
  ld a, [cursor_x]
  ld [char_select_cursor_x], a

  ld a, [cursor_y]
  ld [char_select_cursor_y], a

  call char_edit_mode
  ret

SECTION "Sprite move start callback", ROM0
sprite_move_start_cb:
;  push hl
;    push bc
      ;ld hl, button_srt_str
      ;ld bc, bg_log_location
      ;call put_string_hl_at_bc

      ;ld hl, default_mode_str
      ;ld bc, mode_log_location
      ;call put_string_hl_at_bc_in_vblank

      ;call default_dpad_callback_init
      ;call default_button_callback_init
;    pop bc
;  pop hl
  ret


SECTION "Set sprite move mode", ROM0
char_select_mode:
  LD_A_ADDR_VAL rWX,$7
  LD_A_ADDR_VAL rWY,$90
  LD_A_ADDR_VAL mode_minimum_x, min_x
  LD_A_ADDR_VAL mode_maximum_x, max_x
  LD_A_ADDR_VAL mode_minimum_y, min_y
  LD_A_ADDR_VAL mode_maximum_y, max_y

  ld de, pad_up_f
  ld hl, char_select_up_cb
  call ld_ide_hl

  ld de, pad_down_f
  ld hl, char_select_down_cb
  call ld_ide_hl

  ld de, pad_left_f
  ld hl, char_select_left_cb
  call ld_ide_hl

  ld de, pad_right_f
  ld hl, char_select_right_cb
  call ld_ide_hl

  ld de, button_start_f
  ld hl, sprite_move_start_cb
  call ld_ide_hl

  ld de, button_a_f
  ld hl, char_select_a_cb
  call ld_ide_hl

  ld de, button_b_f
  ld hl, b_cb
  call ld_ide_hl

  ret

SECTION "Set char edit mode", ROM0
char_edit_mode:
  LD_A_ADDR_VAL rWX,$7
  LD_A_ADDR_VAL rWY,0
  LD_A_ADDR_VAL cursor_x, $10
  LD_A_ADDR_VAL cursor_y, $18

  LD_A_ADDR_VAL mode_minimum_x, min_x
  LD_A_ADDR_VAL mode_maximum_x, (step_size * $7) + min_x
  LD_A_ADDR_VAL mode_minimum_y, min_y
  LD_A_ADDR_VAL mode_maximum_y, (step_size * $7) + min_y

  ld a, [selected_character]
  ld [char_select_character], a
  ld [selected_character_window_position], a
  PUSH_HL_BC
    ld h, 0
    ld l, a
    MULT_HL_16
    ld bc, tileset_start
    add hl, bc
    ld bc, selected_character_data
    call ld_ibc_hl
  POP_HL_BC

  ld de, button_a_f
  ld hl, a_cb
  call ld_ide_hl

  ld de, button_b_f
  ld hl, char_edit_b_cb
  call ld_ide_hl
  
  ld hl, edit_mode_str
  ld bc, mode_log_location
  call put_string_hl_at_bc
  ret

SECTION "Cursor edit mode b", ROM0
char_edit_b_cb:
  ld a, [char_select_cursor_x]
  ld [cursor_x], a

  ld a, [char_select_cursor_y]
  ld [cursor_y], a
  
  ld a, [char_select_character]
  ld [selected_character], a
  
  call char_select_mode
  ret


SECTION "Put char data in window", ROM0
put_selected_character_data_into_window:
  ret

SECTION "Update sprite character", ROM0
update_cursor_character:
  ld a, [selected_character]
  ld [selected_character_position], a
  ld a, [cursor_character]
  inc a
  cp $9
  jp c, .store_a
    ld a, $7
.store_a:
  ld [cursor_character], a
  push hl
    LD_HL_struct_K_field sprite,character,0
    ld [hl], a
  pop hl
  ret

SECTION "Update sprite position", ROM0
update_cursor_position:
  push hl
    LD_HL_struct_K_field sprite,x_position,0
    ld a, [cursor_x]
    ld [hl], a
    LD_HL_struct_K_field sprite,y_position,0
    ld a, [cursor_y]
    ld [hl], a
  pop hl
  ret

bg_char_0 EQU vram_start
bg_char_1 EQU vram_start + $1
SECTION "first character timer", ROM0
first_character_timer_cb:
  ld a, [bg_char_0]
  inc a
  ld [bg_char_0], a
  ret

SECTION "second character timer", ROM0
second_character_timer_cb:
  ld a, [bg_char_1]
  inc a
  cp $9F
  jp nz, .end
    ld a, "!"
.end:
    ld [bg_char_1], a
  ret

SECTION "Increment Character", ROM0
increment_timer_cb:
;  VBLANK_WAIT
  push hl
    push de
      ld hl, amount_cb_invoked
      call ld_de_ihl
      inc de
      call ld_ihl_de
      ld a, e
    pop de
  pop hl
  cp $08
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
  pop de
  ret

SECTION "init cursor", ROM0
init_set_cursor:
  PUSH_HL_BC_DE
    LD_HL_struct_K sprite, 0
    ld bc, $2020  ; x - 1
                  ; y - 1
    ld de, $0700 ; character - 0x4
                 ; flag - %00000000
    call set_sprite_at_hl
    call dma_update_sprites
    LD_A_ADDR_VAL selected_character, 0
    LD_A_ADDR_VAL cursor_character, $07
    LD_A_ADDR_VAL cursor_x, min_x
    LD_A_ADDR_VAL cursor_y, min_y
  POP_HL_BC_DE
  ret

SECTION "Initialize display", ROM0
init_display:
  VBLANK_WAIT

  xor a ; ld a, 0 ; We only need to reset a value with bit 7 reset, but 0 does the job
  ld [rLCDC], a ; We will have to write to LCDC again later, so it's not a bother, really.
  ret

SECTION "Initialize font", ROM0
init_font:
  MEMCPY tileset_start, FontTiles, FontTilesEnd - FontTiles
;  ld hl, tileset_start
;  ld de, FontTiles
;  ld bc, FontTilesEnd - FontTiles
;.copyFont
;  ld a, [de] ; Grab 1 byte from the source
;  ld [hli], a ; Place it at the destination, incrementing hl
;  inc de ; Move to next byte
;  dec bc ; Decrement count
;  ld a, b ; Check if count is 0, since `dec bc` doesn't update flags
;  or c
;  jr nz, .copyFont
  ret

SECTION "Finalize initialization", ROM0
final_init:
  ; Init display registers
  ld a, %11100100
  ld [rBGP], a
  ld [rOBP0], a
  ld [rOBP1], a

  xor a ; ld a, 0
  ld [rSCY], a
  ld [rSCX], a

  ; Shut sound down
  ld [rNR52], a

  ; Turn on screen, background,  bg data       window data, enable window, and sprite, tileset_select
  ld a, LCDCF_ON | LCDCF_BGON | LCDCF_BG9800 | LCDCF_WIN9C00| LCDCF_WINON | LCDCF_OBJON | LCDCF_BG8000
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

SECTION "Font", ROM0
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

character_list_box:
CHAR = $0
  REPT $12
    db $0E
  ENDR
  REPT $10
    db "\n"
    db $0E
    REPT $10
      IF CHAR == 0
        db $20
      ELIF CHAR == "\n"
        db $20
      ELSE
        db LOW(CHAR)
      ENDC
CHAR = CHAR + 1
    ENDR
    db $0E
  ENDR
  db "\n"
  REPT $12
    db $0E
  ENDR
  db "\n"
  db $0

window_background_str:
  REPT $A
    db $0E
  ENDR
  REPT $8
    db "\n"
    db $0E
    REPT $8
      db $20
    ENDR
    db $0E
  ENDR
  db "\n"
  REPT $A
    db $0E
  ENDR
  db "\n"
  db $0

button_a_str: db     "A press!      \n", $0
button_b_str: db     "B press!      \n", $0
button_sel_str: db   "select press! \n", $0
button_srt_str: db   "start press!  \n", $0
button_up_str: db    "up press!     \n", $0
button_down_str: db  "down press!   \n", $0
button_left_str: db  "left press!   \n", $0
button_right_str: db "right press!  \n", $0
default_mode_str: db "DEFAULT MODE  \n", $0
edit_mode_str: db    "EDIT          \n", $0
