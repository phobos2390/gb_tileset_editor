IF !DEF(TEST_STRUCT_DEF_INC)
TEST_STRUCT_DEF_INC SET 1

INCLUDE "test_list.inc"
INCLUDE "register_utils.inc"
INCLUDE "func_utils.inc"

SECTION "test_list", WRAM0
; first test_struct 
test_list:
test_struct:
.begin:
    .name: DS 2 ; pointer to test_name
    .function: DS 2 ; pointer to function
    .ret_code: DS 1 ; $1 - success
                    ; $2 - failure
    DS 3 ; padding to fit 8 bytes
.end:
sizeof_test_struct EQU test_struct.end - test_struct.begin
REPT (number_of_tests - 1)
  DS sizeof_test_struct
ENDR
test_list_end:

SECTION "Copy test hl to de", ROM0
ld_test_de_hl:
  push hl
    push de
      call ld_ide_ihl
      INC_HL_W
      INC_DE_W
      call ld_ide_ihl
    pop de
  pop hl
  ret

SECTION "Next test_def hl", ROM0
inc_test_def_hl:
  push bc
    ld bc, sizeof_single_test_def
    add hl, bc
  pop bc
  ret

SECTION "Next test_struct de", ROM0
inc_test_struct_de:
  push bc
    push hl
      ld bc, sizeof_test_struct
      LD_HL_DE
      add hl, bc
      LD_DE_HL
    pop hl
  pop bc
  ret

SECTION "Load tests", ROM0
load_tests:
  push hl
    push de
      push bc
        ld hl, test_defs.begin
        ld de, test_struct.begin
        ld bc, (~test_defs.end) + 1 ; -test_defs.end
.loop:
        call ld_test_de_hl ; copy from test_def to test_struct
        call inc_test_def_hl ; hl += sizeof(test_def)
        call inc_test_struct_de ; de += sizeof(test_struct)
        push hl ; Save hl for after check
          add hl, bc ; hl - test_defs.end
          ld a, h
          or l
          cp $0
          jp z, .end ; hl - test_defs.end == 0
                     ; hl - test_defs.end != 0
            pop hl   ; restore hl
            jp .loop
.end:
        pop hl
      pop bc
    pop de
  pop hl
  ret

SECTION "Root Test Strings", ROM0
test_succeeded:
  DB "\n[ success ]\n", $0
test_failed: 
  DB "\n[ failed ]\n", $0

SECTION "Run test at hl", ROM0
run_test_hl:
  push bc
    push de
      ld bc, test_struct.name - test_struct.begin
      push hl
        add hl, bc ; go to test_struct.name
        call ld_hl_ihl ; hl = *hl
        call print_hl
      pop hl
      ld bc, test_struct.function - test_struct.begin
      push hl
        add hl, bc ; go to test_struct.function
        call ld_hl_ihl ; hl = *hl
        push af
          push bc
            push de
              CALL_HL
            pop de
          pop bc
        pop af
        ld a, l ; return code in hl
      pop hl
      ld bc, test_struct.ret_code - test_struct.begin
      push hl
        add hl, bc
        ld [hl], a
        cp $0 ; evaluate return code. 0 - failed, 1 - suceeded
        jp z, .test_failed
.test_success:  
          ld hl, test_succeeded
          jp .test_end
.test_failed:
        ld hl, test_failed
.test_end:
        call print_hl
       pop hl
    pop de
  pop bc
  ret

SECTION "Run tests", ROM0
run_tests:
  push hl
    push bc
      push de
        ld hl, test_list
        ld de, test_list_end
        INV_DE
        ld bc, sizeof_test_struct
.loop:
        push hl
          call run_test_hl
        pop hl
        add hl, bc
        push hl
          add hl, de
          ld a, h
          or l
          cp a, $0
          jp z, .end
            pop hl
            jp .loop
.end:
        pop hl
      pop de
    pop bc
  pop hl
  ret


ENDC ; TEST_STRUCT_DEF_INC