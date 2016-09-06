(* All tests should evaluate to true. *)

(* NOTE: None of the tests in this test suite verify that your solutions *)
(* use pattern matching instead of list / option functions. *)
(* You should check that yourself! *)

(** 38 Cons Cells **)

(* NOTE: These tests do not verify that your solution is tail-recursive. *)
(* You should check that yourself! *)
use "hw2extra.sml";

val test_length_of_a_list_1 = length_of_a_list [1] = 1
val test_length_of_a_list_2 = length_of_a_list [] = 0
val test_length_of_a_list_3 = length_of_a_list [[], [], [1, 2]] = 3
val test_length_of_a_list_4 = length_of_a_list [(1, "hi"), (2, "there")] = 2
val test_length_of_a_list_5 = length_of_a_list ["a", "quick", "brown", "fox"] = 4

(** Pass/Fail **)

(* Pass/Fail -- 1 *)
val test_pass_or_fail_1 = pass_or_fail { id = 1023, grade = SOME 73 } = fail
val test_pass_or_fail_2 = pass_or_fail { id = 1, grade = SOME 48 } = fail
val test_pass_or_fail_3 = pass_or_fail { id = 10231023, grade = SOME 0 } = fail
val test_pass_or_fail_4 = pass_or_fail { id = 1729, grade = NONE } = fail
val test_pass_or_fail_5 = pass_or_fail { id = 432, grade = SOME 74 } = fail
val test_pass_or_fail_6 = pass_or_fail { id = 2, grade = SOME 75 } = pass
val test_pass_or_fail_7 = pass_or_fail { id = 13, grade = SOME 100 } = pass
val test_pass_or_fail_8 = pass_or_fail { id = 15, grade = SOME 86 } = pass

(* Pass/Fail -- 2 *)
val test_has_passed_1 = has_passed { id = 1023, grade = SOME 73 } = false
val test_has_passed_2 = has_passed { id = 1, grade = SOME 48 } = false
val test_has_passed_3 = has_passed { id = 10231023, grade = SOME 0 } = false
val test_has_passed_4 = has_passed { id = 1729, grade = NONE } = false
val test_has_passed_5 = has_passed { id = 432, grade = SOME 74 } = false
val test_has_passed_6 = has_passed { id = 2, grade = SOME 75 } = true
val test_has_passed_7 = has_passed { id = 13, grade = SOME 100 } = true
val test_has_passed_8 = has_passed { id = 15, grade = SOME 86 } = true

(* Pass/Fail -- 3 *)
val test_number_passed_1 = number_passed [{ id = 1, grade = SOME 65 }, { id = 2, grade = SOME 82 },
    { id = 3, grade = NONE }, { id = 5, grade = SOME 96 }] = 2
val test_number_passed_2 = number_passed [] = 0
val test_number_passed_3 = number_passed [{ id = 12, grade = SOME 100 }, { id = 14, grade = SOME 0 },
    { id = 9, grade = NONE }, { id = 2, grade = NONE }] = 1
val test_number_passed_4 = number_passed [{ id = 1, grade = SOME 76 }, { id = 2, grade = SOME 82 },
    { id = 5, grade = SOME 96 }] = 3

(* Pass/Fail -- 4 *)
val test_group_by_outcome_1 = group_by_outcome [{ id = 1025, grade = NONE },
    { id = 4, grade = SOME 99 }] = [(pass, [4]), (fail, [1025])]
val test_group_by_outcome_2 = group_by_outcome [{ id = 1025, grade = SOME 82 },
    { id = 4, grade = SOME 99 }] = [(pass, [1025, 4])]
val test_group_by_outcome_3 = group_by_outcome [{ id = 1025, grade = NONE },
    { id = 4, grade = SOME 34 }] = [(fail, [1025, 4])]
val test_group_by_outcome_4 = group_by_outcome [{ id = 1025, grade = SOME 76 }, { id = 4, grade = SOME 99 },
    { id = 13, grade = NONE }, { id = 34, grade = SOME 74 },
    { id = 1111, grade = SOME 89 }] = [(pass, [1025, 4, 1111]), (fail, [13, 34])]
(* causes polyEqual warning if group_by_outcome has a polymorphic type -- that's ok *)
val test_group_by_outcome_5 = group_by_outcome [] = []

(** Forest For The Trees **)

(* Forest For The Trees -- 1 *)
val test_tree_height_1 = tree_height (node { value = 0, left = node { value = 0,
    left = node { value = 0, left = leaf, right = leaf }, right = leaf },
    right = node { value = 0, left = leaf, right = leaf } }) = 3
val test_tree_height_2 = tree_height leaf = 0
val test_tree_height_3 = tree_height (node { value = "abcde", left = leaf, right = leaf }) = 1
val test_tree_height_4 = tree_height (node { value = true, left = leaf, right = leaf }) = 1
val test_tree_height_5 = tree_height (node { value = 0, left = leaf,
    right = node { value = 0, left = node { value = 0, left = leaf, right = leaf }, right = leaf } }) = 3

(* Forest For The Trees -- 2 *)
val test_sum_tree_1 = sum_tree (node { value = 1, left = node { value = 2,
    left = node { value = 3, left = leaf, right = leaf }, right = leaf },
    right = node { value = 4, left = leaf, right = leaf } }) = 10
val test_sum_tree_2 = sum_tree leaf = 0
val test_sum_tree_3 = sum_tree (node { value = 1729, left = leaf, right = leaf }) = 1729
val test_sum_tree_4 = sum_tree (node { value = 32, left = leaf,
    right = node { value = ~60, left = node { value = 17, left = leaf, right = leaf }, right = leaf } }) = ~11

(* Forest For The Trees -- 3 *)
val test_gardener_1 = gardener (node { value = leave_me_alone,
    left = node { value = prune_me, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
    right = node { value = leave_me_alone, left = leaf, right = leaf } }) =
    node { value = leave_me_alone, left = leaf, right = node { value = leave_me_alone, left = leaf, right = leaf } }
val test_gardener_2 = gardener leaf = leaf
val test_gardener_3 = gardener (node { value = prune_me, left = node { value = prune_me,
    left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
    right = node { value = leave_me_alone, left = leaf, right = leaf } }) = leaf
val test_gardener_4 = gardener (node { value = leave_me_alone,
    left = node { value = leave_me_alone, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
    right = node { value = leave_me_alone, left = leaf, right = leaf } }) =
    node { value = leave_me_alone, left = node { value = leave_me_alone,
    left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf },
    right = node { value = leave_me_alone, left = leaf, right = leaf } }
val test_gardener_5 = gardener (node { value = leave_me_alone, left = node { value = leave_me_alone,
    left = node { value = prune_me, left = leaf, right = leaf }, right = leaf },
    right = node { value = prune_me, left = leaf, right = leaf } }) =
    node { value = leave_me_alone, left = node { value = leave_me_alone, left = leaf, right = leaf }, right = leaf }
val test_gardener_6 = gardener (node { value = prune_me, left = leaf, right = leaf }) = leaf
val test_gardener_7 = gardener (node { value = leave_me_alone, left = leaf, right = leaf }) =
    node { value = leave_me_alone, left = leaf, right = leaf }
val test_gardener_8 = gardener (node { value = leave_me_alone, left = leaf,
    right = node { value = prune_me, left = node { value = prune_me, left = leaf, right = leaf }, right = leaf } }) =
    node { value = leave_me_alone, left = leaf, right = leaf }
