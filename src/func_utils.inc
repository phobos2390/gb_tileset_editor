IF !DEF(FUNC_UTILS_INC)
FUNC_UTILS_INC SET 1

CALL_HL : MACRO
  rst $08
ENDM

SECTION "Unique call hl", ROM0
unique_call_hl:
  CALL_HL
  ret

ENDC