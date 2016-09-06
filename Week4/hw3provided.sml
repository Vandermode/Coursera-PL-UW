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
val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s, 0)))

(* 2 *)
val longest_string1 =
    foldl (fn (s, sofar) => if String.size s > String.size sofar then s else sofar) ""

(* 3 *)
val longest_string2 =
    foldl (fn (s, sofar) => if String.size s >= String.size sofar then s else sofar) ""

(* 4 *)
fun longest_string_helper f ss = 
    foldl (fn (s, sofar) => if f (String.size s,  String.size sofar) then s else sofar) "" ss

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode;

(* 7 *)
fun first_answer f [] = raise NoAnswer
    | first_answer f (x :: xs) = 
        case f x of
            SOME v => v
            | NONE => first_answer f xs

(* 8 *)
fun all_answers f lst =
    let fun aux ([], acc) = SOME acc
        | aux (x :: xs, acc) =
            case f x of
                NONE => NONE
                | SOME i => aux (xs, acc @ i)
    in
        aux (lst, [])
    end    

(* 9a *)
val count_wildcards = g (fn () => 1) (fn _ => 0)

(* 9b *)
val count_wild_and_variable_lengths =
    g (fn () => 1) String.size 

(* 9c *)
fun count_some_var (s, p) =
    g (fn () => 0) (fn s' => if s = s' then 1 else 0) p

(* 10 *)
fun check_pat p =
    let fun get_vars (Variable x, done) = done @ [x]
        | get_vars (TupleP ps, done) = foldl get_vars done ps
        | get_vars (ConstructorP (_, p), done) = get_vars (p, done)
        | get_vars (_, done) = done
    
    fun unique [] = true
        | unique (s :: ss) =
            not (List.exists (fn s' => s = s') ss) andalso unique ss
    in
        unique (get_vars (p, []))
    end

(* 11 *)
fun match (v, Variable x) = SOME [(x, v)]
    | match ((_, Wildcard) | (Unit, UnitP)) = SOME []
    | match (Const i, ConstP j) = if i = j then SOME [] else NONE
    | match (Constructor (s, v), ConstructorP (s', p)) = if s = s' then match (v, p) else NONE
    | match (Tuple vs, TupleP ps) = (all_answers match (ListPair.zipEq (vs, ps)) handle UnequalLengths => NONE)
    | match (_, _) = NONE

(* 12 *)
fun first_match v ps = 
    SOME (first_answer (curry match v) ps)
    handle NoAnswer => NONE
