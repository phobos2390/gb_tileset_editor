IF !DEF(TIMING_UTILS_INC)
TIMING_UTILS_INC SET 1

INCLUDE "struct_utils.inc"
INCLUDE "register_utils.inc"
INCLUDE "mem_utils.inc"
INCLUDE "func_utils.inc"
INCLUDE "vblank_utils.inc"

number_of_timer_table_entries EQU $A

SECTION "Timer table", WRAM0
number_of_table_entries: DS 1
timer_table_start:
timer_table_entry:
  .begin:
    .entry: DS 1 ; Set to 0 for non entry
    .vblank_wait: DS 1 ; If nonzero, add callback to vblank 
    .ticks: DS 2
    .ticks_per_invocation: DS 2
    .callback_f: DS 2 ; callback function pointer (*callback_f)(context) 
    .context: DS 2 ; pointer to context (*callback_f)(context) placed into bc
  .end:
timer_table_entry_size EQU timer_table_entry.end - timer_table_entry.begin
REPT number_of_timer_table_entries - 1
  DS timer_table_entry_size
ENDR
  DS 1
timer_table_end:

SECTION "Timer table entry add to vblank list", ROM0
add_timer_table_entry_hl_to_vblank_list:
  push bc
    push hl
      ADD_HL_FIELD_OFFSET timer_table_entry, context
      LD_BC_HL
    pop hl
    push hl
      ADD_HL_FIELD_OFFSET timer_table_entry, callback_f
      call ld_hl_ihl
      call add_vblank_list_entry_hl_bc
    pop hl
  pop bc
  ret

SECTION "HandLe timer table entry in hl", ROM0
handle_timer_table_entry_hl:
  PUSH_BC_DE
    push hl
      ADD_HL_FIELD_OFFSET timer_table_entry, ticks
      call ld_bc_ihl
      inc bc
      call ld_ihl_bc
    pop hl
    push hl
      ADD_HL_FIELD_OFFSET timer_table_entry, ticks_per_invocation
      call ld_de_ihl
    pop hl
    push hl ; bc == de ?
      LD_HL_BC
      INV_DE
      add hl,de
      ld a, h
      or l
      jp nz, .end_pop_hl
      ld bc,$0      
    pop hl
    push hl
      ADD_HL_FIELD_OFFSET timer_table_entry, ticks
      call ld_ihl_bc
    pop hl
    push hl
      ADD_HL_FIELD_OFFSET timer_table_entry, vblank_wait
      ld a, [hl]
      cp $0
      jp nz, .put_into_vblank_list
    pop hl
    push hl
      ADD_HL_FIELD_OFFSET timer_table_entry, context
      LD_BC_HL
    pop hl
    push hl
      ADD_HL_FIELD_OFFSET timer_table_entry, callback_f
      call ld_hl_ihl
      CALL_HL
    pop hl
    jp .end
.put_into_vblank_list:
    pop hl
    call add_timer_table_entry_hl_to_vblank_list
    jp .end
.end_pop_hl:
    pop hl
.end:
  POP_BC_DE
  ret

SECTION "Timing table callback", ROM0
timing_table_cb:
  PUSH_HL_BC
    ld bc, timer_table_entry_size
    ld hl, timer_table_start
    ld a, [timer_table_entry.entry]
.loop:
    cp $0
    jp z, .end
      call handle_timer_table_entry_hl
      add hl, bc
      ld a, [hl]
      jp .loop
.end:
  POP_HL_BC
  ret

SECTION "Initialize timing table", ROM0
init_timing_table:
  PUSH_HL_BC_DE
    MEMSET number_of_table_entries,0,(timer_table_end-number_of_table_entries)
  POP_HL_BC_DE
  ret

SECTION "Add timing table callback", ROM0
; hl function
; bc context
; de ticks_per_invocation
add_timing_table_entry_callback:
  PUSH_HL_BC_DE
    push bc
      ld bc, timer_table_entry_size
      ld hl, timer_table_start
      ld a, [timer_table_entry.entry]
.loop:
      cp $0
      jp z, .end
        add hl, bc
        ld a, [hl]
        jp .loop
.end:
    pop bc
    ld a, 1     
    ld [hl+], a ; timer_table_entry.entry = 1
    xor a
    ld [hl+], a ; timer_table_entry.vblank zeroed
    ld [hl+], a ; timer_table_entry.ticks lower
    ld [hl+], a ; timer_table_entry.ticks higher
    call ld_ihl_de ; timer_table_entry.ticks_per_invocation
    LD_DE_HL
    INC_DE_W
    ld hl, sp+$04 ; getting callback function from stack (first pushed value)
    call ld_hl_ihl
    call ld_ide_hl ; timer_table_entry.callback_func
    INC_DE_W
    ld hl, sp+$02 ; getting context pointer from stack (second pushed value)
    call ld_hl_ihl ; loading context param on stack into hl
    call ld_ide_hl ; timer_table_entry.context
    ld a, [number_of_table_entries]
    inc a
    ld [number_of_table_entries], a
  POP_HL_BC_DE
  ret

SECTION "Add timing table callback with vblank", ROM0
; hl function
; bc context
; de ticks_per_invocation
add_vblank_enabled_timing_table_entry_callback:
  call add_timing_table_entry_callback
  PUSH_HL_BC
    ld a, [number_of_table_entries]
    dec a
    SET_HL_TO_STRUCT_INDEX_A_U timer_table_entry
    ADD_HL_FIELD_OFFSET timer_table_entry, vblank_wait
    ld a, 1
    ld [hl], a
  POP_HL_BC
  ret

SECTION "Add timing table callback at de", ROM0
; de - pointer to timing table entry
add_timing_table_entry_de:
  PUSH_HL_BC
    ld bc, timer_table_entry_size
    ld hl, timer_table_start
    ld a, [timer_table_entry.entry]
.loop:
    cp $0
    jp z, .end
      add hl, bc
      ld a, [hl]
      jp .loop
.end:
    LD_BC_HL
    LD_HL_DE
    LD_DE_BC
    ld bc, timer_table_entry.end - timer_table_entry.begin
    call memcopy ; bc = size    hl = source address    de = dest address
  POP_HL_BC
  ret

ENDC