IF !DEF(REGISTER_UTILS_INC)
REGISTER_UTILS_INC SET 1

;-------------------------------------------------------------------------------

LD_DE_HL : MACRO ; de = hl
    ld      d,h
    ld      e,l
ENDM

LD_BC_HL : MACRO ; bc = hl
    ld      b,h
    ld      c,l
ENDM

LD_HL_DE : MACRO ; hl = de
    ld      h,d
    ld      l,e
ENDM

LD_HL_BC : MACRO ; hl = bc
    ld      h,b
    ld      l,c
ENDM

LD_DE_BC : MACRO ; de = bc
    ld      d,b
    ld      e,c
ENDM

LD_BC_DE : MACRO ; bc = de
    ld      b,d
    ld      c,e
ENDM

;-------------------------------------------------------------------------------

INC_HL_W : MACRO
  inc hl
  inc hl
ENDM

INC_DE_W : MACRO
  inc de
  inc de
ENDM

INC_BC_W : MACRO
  inc bc
  inc bc
ENDM

;-------------------------------------------------------------------------------

CPL_HL : MACRO
  ld a, h
  cpl
  ld h, a
  ld a, l
  cpl
  ld l, a
ENDM

INV_HL : MACRO
  CPL_HL
  inc hl
ENDM

CPL_DE : MACRO
  ld a, d
  cpl
  ld d, a
  ld a, e
  cpl
  ld e, a
ENDM

INV_DE : MACRO
  CPL_DE
  inc de
ENDM

CPL_BC : MACRO
  ld a, b
  cpl
  ld b, a
  ld a, c
  cpl
  ld c, a
ENDM

INV_BC : MACRO
  CPL_BC
  inc bc
ENDM

;-------------------------------------------------------------------------------

SECTION "LD [bc] de", ROM0
; ld [bc], de
ld_ibc_de:
  ld a, e
  ld [bc], a
  inc bc
  ld a, d
  ld [bc], a
  dec bc
  ret

SECTION "LD [bc] hl", ROM0
; ld [bc], hl
ld_ibc_hl:
  ld a, l
  ld [bc], a
  inc bc
  ld a, h
  ld [bc], a
  dec bc
  ret

SECTION "LD hl [bc]", ROM0
; ld hl, [bc]
ld_hl_ibc:
  ld a, [bc]
  ld l, a
  inc bc
  ld a, [bc]
  ld h, a
  dec bc
  ret

SECTION "LD [de] hl", ROM0
; ld [de], hl
ld_ide_hl:
  ld a, l
  ld [de], a
  inc de
  ld a, h
  ld [de], a
  dec de
  ret

SECTION "LD hl [de]", ROM0
; ld hl, [de]
ld_hl_ide:
  ld a, [de]
  ld l, a
  inc de
  ld a, [de]
  ld h, a
  dec de
  ret

SECTION "LD de [bc]", ROM0
; ld de, [bc]
ld_de_ibc:
  ld a, [bc]
  ld e, a
  inc bc
  ld a, [bc]
  ld d, a
  dec bc
  ret

SECTION "LD [de] bc", ROM0
; ld de, [bc]
ld_ide_bc:
  ld a, [de]
  ld c, a
  inc de
  ld a, [de]
  ld b, a
  dec de
  ret

SECTION "LD bc [de]", ROM0
; ld bc, [de]
ld_bc_ide:
  ld a, c
  ld [de], a
  inc de
  ld a, b
  ld [de], a
  dec de
  ret

SECTION "LD bc [hl]", ROM0
; ld bc, [hl]
ld_bc_ihl:
  ld c, [hl]
  inc hl
  ld b, [hl]
  dec hl
  ret

SECTION "LD [hl] bc", ROM0
; ld [hl], bc
ld_ihl_bc:
  ld [hl], c
  inc hl
  ld [hl], b
  dec hl
  ret

SECTION "LD de [hl]", ROM0
; ld de, [hl]
ld_de_ihl:
  ld e, [hl]
  inc hl
  ld d, [hl]
  dec hl
  ret

SECTION "LD [hl] de", ROM0
; ld [hl], de
ld_ihl_de:
  ld [hl], e
  inc hl
  ld [hl], d
  dec hl
  ret

SECTION "LD hl [hl]", ROM0
; ld hl, [hl]
ld_hl_ihl:
  push bc
    call ld_bc_ihl
    LD_HL_BC
  pop bc
  ret

SECTION "LD [de] [hl]", ROM0
; ld [de], [hl]
ld_ide_ihl:
  push af
    ld a, [hl+]
    ld [de], a
    inc de
    ld a, [hl-]
    ld [de], a
    dec de
  pop af
  ret

SECTION "LD [hl] [de]", ROM0
; ld [hl], [de]
ld_ihl_ide:
  push af
    ld a, [de]
    inc de
    ld [hl+], a
    ld a, [de]
    ld [hl-], a
    dec de
  pop af
  ret

SECTION "LD [bc] [hl]", ROM0
; ld [bc], [hl]
ld_ibc_ihl:
  push af
    ld a, [hl+]
    ld [bc], a
    inc bc
    ld a, [hl-]
    ld [bc], a
    dec bc
  pop af
  ret

SECTION "LD [hl] [bc]", ROM0
; ld [hl], [bc]
ld_ihl_ibc:
  push af
    ld a, [bc]
    inc bc
    ld [hl+], a
    ld a, [bc]
    ld [hl-], a
    dec bc
  pop af
  ret

;-------------------------------------------------------------------------------

ENDC ; REGISTER_UTILS