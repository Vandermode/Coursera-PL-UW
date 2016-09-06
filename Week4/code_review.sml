(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
    | Variable of string
    | UnitP
    | ConstP of int
    | TupleP of pattern list
    | ConstructorP of string * pattern

datatype valu = Const of int
    | Unit
    | Tuple of valu list
    | Constructor of string * valu

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            Wildcard          => f1 ()
            | Variable x        => f2 x
            | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
            | ConstructorP(_,p) => r p
            | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
    | UnitT
    | IntT
    | TupleT of typ list
    | Datatype of string

(**** you can put all your code here ****)
                                     
(* 1 *)
fun only_capitals (sl : string list) =
    List.filter (fn s => Char.isUpper(String.sub(s, 0)))
                    sl

(* 2 *)
fun longest_string1 (sl : string list) = 
    List.foldl (fn (x, re) => if String.size(x) > String.size(re)
                                    then x
                                    else re)
                 ""
                 sl

(* 3 *)
fun longest_string2 (sl : string list) =
    List.foldl (fn (x, re) => if String.size(x) >= String.size(re)
                                    then x
                                    else re)
                 ""
                 sl
(* 4 *)
fun longest_string_helper f =
    List.foldl (fn (x, re) => if f(String.size(x), String.size(re))
                                then x
                                else re)
                 ""
                 
val longest_string3 = longest_string_helper (fn (x, y) => x > y)
                                                
val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

(* 7 *)
fun first_answer f lst =
    case lst of
            [] => raise NoAnswer
        | x::xs => case f x of
                     NONE => first_answer f xs
                 | SOME v => v 
    
(* 8 *)
fun all_answers f lst = 
    let fun loop (acc, []) = SOME acc
        | loop (acc, x::xs) = case f x of
                                    NONE => NONE
                                | SOME l => loop(acc @ l, xs)
    in
            loop([], lst)
    end
        
(* 9a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* 9b *)
val count_wild_and_variable_lengths = g (fn _ => 1) String.size

(* 9c *)
fun count_some_var (s : string, p : pattern) =
    g (fn _ => 0) (fn var => if var = s then 1 else 0) p

(* 10 *)
fun check_pat (p : pattern) =
    let fun pattern_to_list (pat) =
        case pat of
                Variable x        => [x]
            | TupleP ps         => List.foldl (fn (p,i) => (pattern_to_list p) @ i) [] ps
            | ConstructorP (_, p) => pattern_to_list p
            | _                 => []

            fun has_repeats (lst : string list) =
        case lst of
                [] => false 
            | x::xs => (List.exists (fn s => x = s) xs)
                         orelse has_repeats(xs)
    in
            (not o has_repeats o pattern_to_list) p
    end

(* 11 *)
fun match (v : valu, p : pattern) =
    case (v, p) of
            (_, Wildcard) => SOME []
        | (_, Variable s) => SOME [(s, v)]
        | (Unit, UnitP) => SOME []
        | (Const iv, ConstP ip) => if iv = ip then SOME [] else NONE
        | (Tuple tv, TupleP tp) => if List.length(tv) = List.length(tp)
                                                then all_answers match (ListPair.zip(tv, tp))
                                                else NONE
        | (Constructor(s2, cv), ConstructorP(s1, cp)) => if s1 = s2    
                                                                                    then match(cv, cp)
                                                                                    else NONE
        | _ => NONE 

(* 12 *)
fun first_match v pl =
    SOME (first_answer (fn p => match(v, p)) pl) handle NoAnswer => NONE
