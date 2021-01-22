; Copied from AntonioND/uCity/source/utils.asm

;###############################################################################
;#                                                                             #
;#                             GENERAL FUNCTIONS                               #
;#                                                                             #
;###############################################################################

IF !DEF(MEM_UTILS_INC)
MEM_UTILS_INC SET 1

INCLUDE "vram_utils.inc"

SECTION "Memset",ROM0

;-------------------------------------------------------------------------------
;- memset()    d = value    hl = start address    bc = size                    -
;-------------------------------------------------------------------------------

memset::

    ld      a,d
    ld      [hl+],a
    dec     bc
    ld      a,b
    or      a,c
    jr      nz,memset

    ret

MEMSET : MACRO
  ld hl, \1
  ld d, \2
  ld bc, \3
  call memset
ENDM

SECTION "Memset_fast",ROM0

;-------------------------------------------------------------------------------
;- memset_fast()    a = value    hl = start address    b = size                -
;-------------------------------------------------------------------------------

memset_fast::

    ld      [hl+],a
    dec     b
    jr      nz,memset_fast

    ret

SECTION "Memcpy",ROM0

;-------------------------------------------------------------------------------
;- memcopy()    bc = size    hl = source address    de = dest address          -
;-------------------------------------------------------------------------------

memcopy:: ; hl and de must be incremented at the end of this

    ld      a,[hl+]
    ld      [de],a
    inc     de
    dec     bc
    ld      a,b
    or      a,c
    jr      nz,memcopy

    ret

MEMCPY : MACRO
  ld de, \1
  ld hl, \2
  ld bc, \3
  call memcopy
ENDM
    
SECTION "Memcpy vram verify", ROM0
memcopy_to_vram:
    ld      a, [rLY]
    cp      $90 ; Check if the LCD is past VBlank
    jr      nc,memcopy_to_vram
.loop:
    ld      a,[hl+]
    ld      [de],a
    inc     de
    dec     bc
    ld      a,b
    or      a,c
    jr      nz,memcopy_to_vram.loop
    ret

SECTION "Memcpy_fast", ROM0

;-------------------------------------------------------------------------------
;- memcopy_fast()    b = size    hl = source address    de = dest address      -
;-------------------------------------------------------------------------------

memcopy_fast:: ; hl and de must be incremented at the end of this

    ld      a,[hl+]
    ld      [de],a
    inc     de
    dec     b
    jr      nz,memcopy_fast

    ret

SECTION "Memcpy_hram",ROM0

;-------------------------------------------------------------------------------
;- memcopy_hram()    b = size    hl = source address    c = dest address      -
;-------------------------------------------------------------------------------

memcopy_hram:: ; hl and de must be incremented at the end of this

    ld      a,[hl+]
    ldh     [c],a
    inc     c
    dec     b
    jr      nz,memcopy_hram

    ret

SECTION "Memcpy_inc",ROM0

;-------------------------------------------------------------------------------
;- memcopy_inc()    b = size    c = increase_src    hl = src    de = dst       -
;-------------------------------------------------------------------------------

memcopy_inc:: ; hl and de should be incremented at the end of this

    ld      a,[hl]
    ld      [de],a

    inc     de ; increase dest

    ld      a,b ; save b
    ld      b,$00
    add     hl,bc ; increase source
    ld      b,a ; restore b

    dec     b
    jr      nz,memcopy_inc

    ret

;-------------------------------------------------------------------------------
;- memcmp()    bc = size      hl = first      de = second  a = diff            -
;-------------------------------------------------------------------------------

memcmp:: ; hl and de must be incremented at the end of this
    dec bc
    ld a, b
    or c
    jp z, .finish

    ld a, [de]
    sub [hl]
    ret nz

    inc de
    inc hl
    jp memcmp

.finish:

    ret

;-------------------------------------------------------------------------------
;- memcmp_fast()    b = size      hl = first      de = second  a = diff       -
;-------------------------------------------------------------------------------

memcmp_fast:: ; hl and de must be incremented at the end of this
    dec b
    jp z, .finish

    ld a, [de]
    sub [hl]
    ret nz ; returns the difference

    inc de
    inc hl
    jp memcmp_fast

.finish:
    ld a, $0 
    ret

ENDC ; MEM_UTILS_INC