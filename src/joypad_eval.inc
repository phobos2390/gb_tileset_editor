IF !DEF(JOYPAD_EVAL_INC)
JOYPAD_EVAL_INC SET 1

INCLUDE "func_utils.inc"

SECTION "Joypad callback tables", WRAM0
pad_up_f: DS 2
pad_down_f: DS 2
pad_left_f: DS 2
pad_right_f: DS 2
button_a_f: DS 2
button_b_f: DS 2
button_start_f: DS 2
button_select_f: DS 2
button_data: DS 1

button_a EQU        %00000001
button_b EQU        %00000010
button_select EQU   %00000100
button_start EQU    %00001000

button_right EQU    %00010000
button_left EQU     %00100000
button_up EQU       %01000000
button_down EQU     %10000000

SECTION "joypad table initialize", ROM0
init_joypad_table:
  push bc
    push hl
      ld bc, 0
      ld hl, pad_up_f
      call ld_ihl_bc
      ld hl, pad_down_f
      call ld_ihl_bc
      ld hl, pad_left_f
      call ld_ihl_bc
      ld hl, pad_right_f
      call ld_ihl_bc
      ld hl, button_a_f
      call ld_ihl_bc
      ld hl, button_b_f
      call ld_ihl_bc
      ld hl, button_start_f
      call ld_ihl_bc
      ld hl, button_select_f
      call ld_ihl_bc
    pop hl
  pop bc
  

SECTION "JOYPAD eval", ROM0
eval_joypad:
.eval_a:
  ld a, [button_data]
  ld b, a
  and button_a
  cp $0
  jp z, .eval_b
    push hl
      ld hl, button_a_f
      call ld_hl_ihl
      ld a, h
      or l
      jp z, .button_a_f_not_defined
        CALL_HL
.button_a_f_not_defined:        
    pop hl
.eval_b:
  ld a, b
  and button_b
  jp z, .eval_select
    push hl
      ld hl, button_b_f
      call ld_hl_ihl
      ld a, h
      or l
      jp z, .button_b_f_not_defined
        CALL_HL
.button_b_f_not_defined:
    pop hl
.eval_select:
  ld a, b
  and button_select
  jp z, .eval_start
    push hl
      ld hl, button_select_f
      call ld_hl_ihl
      ld a, h
      or l
      jp z, .button_select_f_not_defined
        CALL_HL
.button_select_f_not_defined:
    pop hl
.eval_start:
  ld a, b
  and button_start
  jp z, .eval_up
    push hl
      ld hl, button_start_f
      call ld_hl_ihl
      ld a, h
      or l
      jp z, .button_start_f_not_defined
        CALL_HL
.button_start_f_not_defined:
    pop hl
.eval_up:
  ld a, b
  and button_up
  jp z, .eval_down
    push hl
      ld hl, pad_up_f
      call ld_hl_ihl
      ld a, h
      or l
      jp z, .pad_up_f_not_defined
        CALL_HL
.pad_up_f_not_defined:
    pop hl
.eval_down:
  ld a, b
  and button_down
  jp z, .eval_right
    push hl
      ld hl, pad_down_f
      call ld_hl_ihl
      ld a, h
      or l
      jp z, .pad_down_f_not_defined
        CALL_HL
.pad_down_f_not_defined:
    pop hl
.eval_right:
  ld a, b
  and button_right
  jp z, .eval_left
    push hl
      ld hl, pad_right_f
      call ld_hl_ihl
      ld a, h
      or l
      jp z, .pad_right_f_not_defined
        CALL_HL
.pad_right_f_not_defined:
    pop hl
.eval_left:
  ld a, b
  and button_left
  jp z, .eval_end
    push hl
      ld hl, pad_left_f
      call ld_hl_ihl
      ld a, h
      or l
      jp z, .pad_left_f_not_defined
        CALL_HL
.pad_left_f_not_defined:
    pop hl
.eval_end:
  ret

SECTION "JOYPAD Read", ROM0
read_joypad:
  push bc
    ld a, ~P1F_4 ; direction
    ld [rP1], a
    REPT 3
      ld a, [rP1] ; Debouncing logic
    ENDR
    and a, P1F_3 | P1F_2 | P1F_1 | P1F_0
    swap a
    ld b, a
    ld a, ~P1F_5 ; other buttons
    ld [rP1], a
    REPT 6
      ld a, [rP1] ; Debouncing logic
    ENDR
    and a, P1F_3 | P1F_2 | P1F_1 | P1F_0
    or a, b
    cpl
    ld [button_data], a
  pop bc
  ret

ENDC ; JOYPAD_EVAL_INC