(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE

val test9a = count_wildcards Wildcard = 1

val test9a1 = count_wildcards (TupleP [Wildcard, ConstructorP ("a", Wildcard)]) = 2

val test9b = count_wild_and_variable_lengths (TupleP [Wildcard, ConstructorP ("a", Variable "x")]) = 2

val test9b1 = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", TupleP [Variable "x", ConstructorP ("a", Variable "x")]) = 2

val test9c1 = count_some_var ("x", Variable("x")) = 1

val test10 = check_pat (Variable("x")) = true

val test10a = check_pat (TupleP [Variable "x", Variable "x"]) = false

val test10b = check_pat (TupleP [Variable "x", Variable "x1", TupleP[Variable "x"]]) = false

val test11 = match (Const(1), UnitP) = NONE

val test11a = match (Tuple [Const 1, Const 2, Constructor ("s", Const 1)], TupleP [Variable "a", Variable "b", ConstructorP ("s", Variable "c")])
    = SOME [("a",Const 1),("b",Const 2),("c",Const 1)]

val test11b = match (Tuple [Const 1, Const 2, Constructor ("s", Const 1)], TupleP [Variable "a", Variable "b", ConstructorP ("s1", Variable "c")])
    = NONE

val test12 = first_match Unit [UnitP] = SOME []
