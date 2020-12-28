IF !DEF(FUNC_UTILS_INC)
FUNC_UTILS_INC SET 1

CALL_HL : MACRO
  push de
    ld de, .function_end
    push de
    jp hl
.function_end:
  pop de
ENDM

SECTION "Unique call hl", ROM0
unique_call_hl:
  CALL_HL
  ret

ENDC