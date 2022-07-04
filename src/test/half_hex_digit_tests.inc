IF !DEF(HALF_HEX_DIGIT_TESTS_INC)
HALF_HEX_DIGIT_TESTS_INC=1

include "half_hex_digits.inc"

SECTION "hex digit workspace", WRAM0
hex_digit_workspace:
  DS $10
hex_digit_workspace_end:

SECTION "expected hex digit results", ROM0
expected_hex_digit_results_F4:
  DB %11101010
  DB %11101010
  DB %10001010
  DB %10001010
  DB %11001110
  DB %11001110
  DB %10000010
  DB %10000010
  DB %10000010
  DB %10000010
  DB %00000000
  DB %00000000
  DB %00000000
  DB %00000000
  DB %00000000
  DB %00000000

SECTION "stich_hex_digits_test", ROM0
hex_digits_stitch_test:
  MEMSET hex_digit_workspace, 0, (hex_digit_workspace_end - hex_digit_workspace)

  ld a, $F4 ; hex digit 4 to check
  ld b, $FF
  ld de, tileset_start + $2
  call set_char_b_to_hex_byte_a_at_de

  ld hl, hex_digit_workspace  
  call stitch_hex_digit_a_hl
  ld de, expected_hex_digit_results_F4
  ld b, (hex_digit_workspace_end - hex_digit_workspace)
  call memcmp_fast

  cp $0
  jp z, .success
  jp .fail

.success:
  ld hl, $1
  ret
.fail:
  ld hl, $0
  ret

ENDC ; HALF_HEX_DIGIT_TESTS_INC



