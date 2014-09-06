open Assertions
open Ps1

(* Tests for is_mon_inc *)
TEST_UNIT "is_mon_inc_test1" = assert_true  (is_mon_inc [])
(*are we sure these are true above and below?*)
TEST_UNIT "is_mon_inc_test2" = assert_true (is_mon_inc [1])
TEST_UNIT "is_mon_inc_test3" = assert_true (is_mon_inc [1;1])
TEST_UNIT "is_mon_inc_test4" = assert_false (is_mon_inc [1;0])
TEST_UNIT "is_mon_inc_test5" = assert_true (is_mon_inc [0;1])
TEST_UNIT "is_mon_inc_test6" = assert_true (is_mon_inc [1;2;3])
TEST_UNIT "is_mon_inc_test7" = assert_false (is_mon_inc [3;2;1])
TEST_UNIT "is_mon_inc_test8" = assert_false (is_mon_inc [1;3;2])
TEST_UNIT "is_mon_inc_test9" = assert_true (is_mon_inc [1;1;1])
TEST_UNIT "is_mon_inc_test10" = assert_false (is_mon_inc [1;2;9;0])

(* Tests for is_unimodal*)
TEST_UNIT "is_unimodal_test1" = assert_true  (is_unimodal [])
TEST_UNIT "is_unimodal_test2" = assert_true (is_unimodal [1])
TEST_UNIT "is_unimodal_test3" = assert_true (is_unimodal [1;1])
TEST_UNIT "is_unimodal_test4" = assert_true (is_unimodal [1;0])
TEST_UNIT "is_unimodal_test5" = assert_true (is_unimodal [0;1])
TEST_UNIT "is_unimodal_test6" = assert_true (is_unimodal [1;2;3])
TEST_UNIT "is_unimodal_test7" = assert_true (is_unimodal [3;2;1])
TEST_UNIT "is_unimodal_test8" = assert_true (is_unimodal [1;3;2])
TEST_UNIT "is_unimodal_test9" = assert_true (is_unimodal [1;1;1])
TEST_UNIT "is_unimodal_test10" = assert_true (is_unimodal [1;2;9;0])
(* new tests *)
TEST_UNIT "is_unimodal_test11" = assert_true  (is_unimodal [1;2;3;3;2;0])
TEST_UNIT "is_unimodal_test12" = assert_false (is_unimodal [1;2;1;2])
TEST_UNIT "is_unimodal_test13" = assert_true (is_unimodal [0;1;0])
TEST_UNIT "is_unimodal_test14" = assert_false (is_unimodal [1;2;3;3;2;1;2;2])





let () = Pa_ounit_lib.Runtime.summarize()