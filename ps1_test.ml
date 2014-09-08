open Assertions
open Ps1

(* Tests for is_mon_inc *)
TEST_UNIT "is_mon_inc_test1" = assert_true  (is_mon_inc [])
TEST_UNIT "is_mon_inc_test2" = assert_true (is_mon_inc [1])
TEST_UNIT "is_mon_inc_test3" = assert_true (is_mon_inc [1;1])
TEST_UNIT "is_mon_inc_test4" = assert_false (is_mon_inc [1;0])
TEST_UNIT "is_mon_inc_test5" = assert_true (is_mon_inc [0;1])
TEST_UNIT "is_mon_inc_test6" = assert_true (is_mon_inc [1;2;3])
TEST_UNIT "is_mon_inc_test7" = assert_false (is_mon_inc [3;2;1])
TEST_UNIT "is_mon_inc_test8" = assert_false (is_mon_inc [1;3;2])
TEST_UNIT "is_mon_inc_test9" = assert_true (is_mon_inc [1;1;1])
TEST_UNIT "is_mon_inc_test10" = assert_false (is_mon_inc [1;2;9;0])

(* ---------------------------------------------------- *)

(* Tests for is_unimodal *)
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
TEST_UNIT "is_unimodal_test11" = 
	assert_true  (is_unimodal [1;2;3;3;2;2;0;0;-10])
TEST_UNIT "is_unimodal_test12" = assert_false (is_unimodal [1;2;1;2])
TEST_UNIT "is_unimodal_test13" = assert_true (is_unimodal [0;1;0])
TEST_UNIT "is_unimodal_test14" = 
	assert_false (is_unimodal [1;2;3;3;2;1;2;2;-1])

(* ---------------------------------------------------- *)

(* Tests for rev_int *)
TEST_UNIT "rev_int_test1" = assert_true ((rev_int 1234) = 4321)
TEST_UNIT "rev_int_test2" = assert_true ((rev_int (-1234)) = (-4321))
TEST_UNIT "rev_int_test3" = assert_true ((rev_int 0) = 0)
TEST_UNIT "rev_int_test5" = assert_true ((rev_int (-10)) = (-1))
TEST_UNIT "rev_int_test6" = assert_true ((rev_int 11111) = 11111)
TEST_UNIT "rev_int_test7" = assert_true ((rev_int 4) = 4)

(* ---------------------------------------------------- *)

(* Tests for powerset *)
TEST_UNIT "powerset_test1" = assert_true (powerset[] = [[]])
TEST_UNIT "powerset_test2" = assert_true (powerset[1] = [[];[1]])
TEST_UNIT "powerset_test3" = 
	assert_true (powerset[1;2] = [[]; [2]; [1]; [1;2]])
TEST_UNIT "powerset_test4" = 
	assert_true (powerset[1;2;3] = [[];[3];[2];[2;3];[1];[1;3];[1;2];[1;2;3]])

(* ---------------------------------------------------- *)

(* Tests for unflatten *)
TEST_UNIT "unflatten_test1" = 
	assert_true ((unflatten 2 [1;2;3;4]) = (Some [[1;2];[3;4]]))
TEST_UNIT "unflatten_test2" = 
	assert_true ((unflatten 2 [1;2;3;4;5;6]) = (Some [[1;2];[3;4];[5;6]]))
TEST_UNIT "unflatten_test3" = 
	assert_true ((unflatten 0 [1;2;3;4;5;6]) = None)
TEST_UNIT "unflatten_test4" = 
	assert_true ((unflatten (-5) []) = None)
TEST_UNIT "unflatten_test5" = 
	assert_true ((unflatten (-5) [1;2]) = None)
TEST_UNIT "unflatten_test6" = 
	assert_true ((unflatten 0 []) = None)	
TEST_UNIT "unflatten_test6" = 
	assert_true ((unflatten 4 []) = (Some [[]]))

TEST_UNIT "unflatten_test7" = 
	assert_true ((unflatten 3 [1;2;3;4;5;6;7;8]) = (Some [[1;2;3]; [4;5;6]; [7;8]]))
TEST_UNIT "unflatten_test8" = 
	assert_true ((unflatten 6 [1;2;3;4;5;6]) = (Some [[1;2;3;4;5;6]]))
TEST_UNIT "unflatten_test9" = 
	assert_true ((unflatten 7 [1;2;3;4;5;6]) = (Some [[1;2;3;4;5;6]]))

(* ---------------------------------------------------- *)

(* Tests for int_of_roman *)
TEST_UNIT "int_of_roman_test1" = assert_true ((int_of_roman [I;I;I])=3)
TEST_UNIT "int_of_roman_test1" = assert_true ((int_of_roman [X;L;I;I])=42)
TEST_UNIT "int_of_roman_test1" = 
	assert_true ((int_of_roman [M;C;M;X;C;I;X])=1999)








let () = Pa_ounit_lib.Runtime.summarize()