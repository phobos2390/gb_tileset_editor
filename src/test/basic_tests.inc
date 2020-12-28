IF !DEF(BASIC_TESTS_INC)
BASIC_TESTS_INC SET 1

SECTION "basic_tests", ROM0
basic_test:  
  ld hl, $1 ; return 1 (success)
  ret

SECTION "failing_test", ROM0
failing_test:
  ld hl, $0 ; return 0 (failure)
  ret

ENDC ; BASIC_TESTS_INC
