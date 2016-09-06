(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
     string), then you avoid several of the functions in problem 1 having
     polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
        s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str: string, lst: string list) = 
    let fun aux(left: string list, right: string list) =
            case right of
                    []      => NONE
                | h::rest => if same_string(str, h)
                             then SOME(left@rest)
                             else aux(left@[h], rest)
    in  aux([], lst) end
        
fun get_substitutions1(substitutions: string list list, str: string) =
    case substitutions of
            []      => []
        | h::rest => let val r = case all_except_option(str, h) of
                            NONE    => []
                        | SOME(l) => l
                     in r @ get_substitutions1(rest, str) end 
    
fun get_substitutions2(substitutions: string list list, str: string) =
    let fun aux(slst: string list list, ans: string list) =
        case slst of
            [] => ans
            | h::rest => let val r = case all_except_option(str, h) of
                                NONE    => []
                            | SOME(l) => l
                         in aux(rest, ans @ r) end          
    in aux(substitutions, [])  end
    
type name = {first: string, middle: string, last: string}

fun similar_names(substitutions: string list list, namae: name) =
    let val {first = f, middle = m, last = l} = namae
        fun aux(ans: name list, firstnames: string list) =
            case firstnames of
                    []      => ans
                | h::rest => aux({first = h, middle = m, last = l}::ans, rest)
    in namae::aux([],get_substitutions2(substitutions, f)) end
    

(* you may assume that Num is always used with values 2, 3, ..., 10
     though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color((su, _): card) = 
    case su of
            Spades   => Black
        | Clubs    => Black
        | Diamonds => Red
        | Hearts   => Red

fun card_value((_, ra): card) =
    case ra of
            Num(i) => i
        | Ace    => 11
        | _      => 10
        
fun remove_card(cs: card list, c: card, e: exn) =
    let fun aux(clst: card list, ans: card list) = 
            case clst of
                    []       => raise e
                | hd::rest => if hd = c
                                then ans @ rest
                                else aux(rest, ans @ [hd])
    in aux(cs, []) end

fun all_same_color(cs: card list) = 
    case cs of
            []         => true
        | f::[]      => true
        | f::s::rest => if card_color(f) = card_color(s)
                            then all_same_color(s::rest)
                            else false
                        
fun sum_cards(cs: card list) = 
    let fun aux(clst: card list, ans: int) =
            case clst of
                    []       => ans
                | hd::rest => aux(rest, card_value(hd) + ans) 
    in aux(cs, 0) end
    
fun score(held_cards: card list, goal: int) =
    let
        val sum = sum_cards(held_cards)
        val preliminary_score = if sum > goal then 3*(sum - goal) else goal - sum
    in
        if all_same_color(held_cards)
        then preliminary_score div 2
        else preliminary_score
    end

fun officiate(cards: card list, moves: move list, goal: int) = 
    let fun next(held_cards: card list, crds: card list,  mvs: move list) =
        case mvs of
                []               => score(held_cards, goal)
            | Discard(c)::rest => next(remove_card(held_cards, c, IllegalMove), crds, rest)
            | Draw::rest       => case crds of
                                        []        => score(held_cards, goal)
                                    | cd::restc => let
                                                        val held = cd::held_cards
                                                        val s = sum_cards(held)
                                                     in
                                                        if s > goal
                                                        then score(held, goal)
                                                        else next(held, restc, rest)
                                                     end
    in next([], cards, moves) end
    
    
(* Challenge Problems (a) *)

fun count_Ace(held_cards: card list) =
    let fun aux(hc: card list, sum: int) =
            case hc of
                    []           => sum
                | (_, h)::rest => case h of
                                    Ace => aux(rest, sum + 1)
                                    | _ => aux(rest, sum)
    in aux(held_cards, 0) end

fun score_challenge(held_cards: card list, goal: int) =
    let
        fun adjust(sum: int, cnt: int) =
            let val delta = (sum-goal) div 10
            in 
                if sum <= goal then sum
                else if  delta >= cnt then sum - cnt*10
                else if 3*(sum - delta*10 - goal) < goal - (sum - (delta + 1)*10)
                then sum - delta*10
                else sum - (delta + 1)*10
            end
        val sum = adjust(sum_cards(held_cards), count_Ace(held_cards))
        val preliminary_score = if sum > goal then 3*(sum - goal) else goal - sum
    in
        if all_same_color(held_cards)
        then preliminary_score div 2
        else preliminary_score
    end

fun officiate_challenge(cards: card list, moves: move list, goal: int) = 
    let fun next(held_cards: card list, crds: card list,  mvs: move list) =
        case mvs of
                []               => score_challenge(held_cards, goal)
            | Discard(c)::rest => next(remove_card(held_cards, c, IllegalMove), crds, rest)
            | Draw::rest       => case crds of
                                        []        => score_challenge(held_cards, goal)
                                    | cd::restc => let
                                                        val held = cd::held_cards
                                                        val s = sum_cards(held)
                                                     in
                                                        if s - count_Ace(held_cards)*10 > goal
                                                        then score_challenge(held, goal)
                                                        else next(held, restc, rest)
                                                     end
    in next([], cards, moves) end 
    
(* Challenge Problems (b) *)
(*
fun careful_player(cards: card list, goal: int) = 
    let 
        fun next(held_cards: card list, crds: card list, mvs: move list) =
            if score(held_cards, goal) = 0 then mvs
            else case crds of
                    []         => mvs@[Draw]
                    | cd::rest => if goal > sum_cards(held_cards) + 10
                                    then next(held_cards@[cd], rest, mvs@[Draw])
                                    else next(tl held_cards, rest, mvs@[Discard(hd held_cards)])
    in next([], cards, []) end
    *)