IF !DEF(STRUCT_UTILS_INC)
STRUCT_UTILS_INC SET 1

; Params
; 1 - struct_label
; 2 - number of structs
STATIC_ALLOC_STRUCT : MACRO
  REPT ((\2) - 1)
   DS (\1.end - \1.begin)
  ENDR
ENDM

; Find offset for the field
; E.G. 
; a_struct:
; .begin:
; .field_0: DS 1
; .field_1: DS 1
; .end:
; FIELD_OFFSET a_struct,field_0 == 0
; FIELD_OFFSET a_struct,field_1 == 1
; Params
; 1 - struct name
; 2 - field name
FIELD_OFFSET : MACRO
  \1.\2 - \1.begin
ENDM

; Add field offset to hl
; E.G. 
; a_struct:
; .begin:
; .field_0: DS 1
; .field_1: DS 1
; .end:
; FIELD_OFFSET a_struct,field_0 == 0
; FIELD_OFFSET a_struct,field_1 == 1
; Params
; 1 - struct name
; 2 - field name
ADD_HL_FIELD_OFFSET : MACRO
  push bc
    ld bc, \1.\2 - \1.begin
    add hl, bc
  pop bc
ENDM

HANDLE_FIELD_HL : MACRO
  IF (FIELD_OFFSET \1,\2) > 0
    PUSH_HL_BC
      ld bc, FIELD_OFFSET \1,\2
      add hl, bc
      call \3
    POP_HL_BC
  ELSE
    call \3
  ENDC
ENDM

SET_HL_TO_STRUCT_INDEX_A_U : MACRO
  ld hl, \1
  ld bc, (\1.end - \1.begin)
.loop\@:
  cp $0
  jp z, .end_loop\@
    add hl, bc
    dec a
    jp .loop\@
.end_loop\@:
ENDM

SET_HL_TO_STRUCT_INDEX_A : MACRO
  push bc
    SET_HL_TO_STRUCT_INDEX_A_U \1
  pop bc
ENDM

; Params
; 1 - struct_name
; 2 - index
LD_HL_struct_K : MACRO
  ld hl, (\1) + (\2 * (\1.end - \1.begin))
ENDM

; Params
; 1 - struct_name
; 2 - field_name
; 3 - index
LD_HL_struct_K_field : MACRO
  ld hl, (\1.\2) + (\3 * (\1.end - \1.begin))
ENDM

ENDC ; STRUCT_UTILS_INC