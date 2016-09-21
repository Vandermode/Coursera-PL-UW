(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "code_review3.sml";

val test1 = all_except_option ("string", ["string"]) = SOME []
val test101 = all_except_option ("string", [""]) = NONE
val test102 = all_except_option ("string", ["ok", "string", "go"]) = SOME ["ok", "go"]

val test2 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test201 = get_substitutions1 ([["foo", "bar"], ["bar"]], "foo") = ["bar"]

val test3 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test301 = get_substitutions2 ([["foo", "bar"], ["bar"]], "foo") = ["bar"]

val test4 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test5 = card_color (Clubs, Num 2) = Black

val test6 = card_value (Clubs, Num 2) = 2

val test7 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test701 = (remove_card ([(Hearts, Num 2)], (Hearts, Num 1), IllegalMove) handle IllegalMove => [(Hearts, Num 2)])  = [(Hearts, Num 2)]

val test8 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test801 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, Num 2)] = false

val test9 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test901 = sum_cards [] = 0

val test10 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test1001 = score ([(Hearts, Num 3), (Diamonds, King)], 10) = 4

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             
(*val test14 = score_challenge ([(Hearts,Ace),(Diamonds,Ace),(Spades,Ace)], 30) = 7;

val test15 = officiate_challenge ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        24)
             = 0*)