open Assertions
open ps1

TEST UNIT "is_mon_inc" = 
	assert_true  (is_mon_inc[])
	assert_true (is_mon_inc[1])
	assert_true (is_mon_inc[1;1])
	assert_true (is_mon_inc[1;0])
	assert_false (is_mon_inc[0;1])
	assert_true (is_mon_inc[1;2;3])
	assert_false (is_mon_inc[3;2;1])
	assert_false (is_mon_inc[1;3;2])
	assert_true (is_mon_inc[1;1;1])
