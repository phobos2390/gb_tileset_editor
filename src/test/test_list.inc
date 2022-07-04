IF !DEF(TEST_LIST_INC)
TEST_LIST_INC=1

INCLUDE "basic_tests.inc"
INCLUDE "half_hex_digit_tests.inc"

SECTION "test_directory", ROM0
test_defs: 
.begin:
.first_begin:
  DW basic_test_name, basic_test
.first_end:
  DW failing_test_name, failing_test
  ; -------------------------------------------------------------------
  ;     ADD any additional tests below
  ; -------------------------------------------------------------------
  DW half_hex_digit_test_name, hex_digits_stitch_test
.end:

sizeof_test_defs EQU (test_defs.end - test_defs.begin)
sizeof_single_test_def EQU (test_defs.first_end - test_defs.first_begin)
number_of_tests EQU sizeof_test_defs / sizeof_single_test_def

SECTION "test_names", ROM0
basic_test_name: DB "basic_test",$0
failing_test_name: DB "failing_test",$0
half_hex_digit_test_name: DB "stitch_hex_digit",$FF,$0
; -------------------------------------------------------------------
;     ADD any additional test names below
; -------------------------------------------------------------------

ENDC ; TEST_LIST_INC
