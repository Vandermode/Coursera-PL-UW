(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
     string), then you avoid several of the functions in problem 1 having
     polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
        s1 = s2


(* put your solutions for problem 1 here *)
fun all_except_option(element, xs) =
        let fun all_except_helper(xs, acc) =
                        case xs of
                                [] => NONE
                                | x::xs' => if same_string(x, element)
                                                        then SOME (acc @ xs')
                                                        else all_except_helper(xs', x::acc)
        in
                all_except_helper(xs, [])
        end;


fun get_substitutions1([], _) = []
    | get_substitutions1(x::xs, s) =
                case all_except_option(s, x) of
                            NONE => get_substitutions1(xs, s)
                        | SOME ys => ys @ get_substitutions1(xs, s);


fun get_substitutions2(xs, s) =
        let fun get_substitutions2_helper([], acc) = acc
                    | get_substitutions2_helper(x::xs, acc) =
                        case all_except_option(s, x) of
                                NONE => get_substitutions2_helper(xs, acc)
                                | SOME ys => get_substitutions2_helper(xs, acc @ ys)
        in
                get_substitutions2_helper(xs, [])
        end;


fun similar_names(xss, fullname) =
        let val {first:string, last:string, middle:string} = fullname
                val simliar_firstnames = get_substitutions1(xss, first)
                fun similar_names_acc(xs) =
                        case xs of
                                [] => []
                                | x::xs' => {first=x, last=last, middle=middle} ::  similar_names_acc(xs')
        in
                fullname :: similar_names_acc(simliar_firstnames)
        end;


(* you may assume that Num is always used with values 2, 3, ..., 10
     though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color(suit, _) =
        case suit of
                    Spades => Black
                | Clubs => Black
                | Diamonds => Red
                | Hearts => Red;


fun card_value(_, rank) =
        case rank of
                Ace => 11
                | King => 10
                | Queen => 10
                | Jack => 10
                | Num i => i;


fun remove_card(cs, c, e) =
        let fun remove_card_helper(xs, acc) =
                        case xs of
                                [] => raise e
                                | x::xs' => if x = c
                                                        then acc @ xs'
                                                        else remove_card_helper(xs', x ::acc)
        in
                remove_card_helper(cs, [])
        end;


fun all_same_color(cs) =
        case cs of
                [] => true
                | c::[] => true
                | c::cn::cs' => card_color(c) = card_color(cn)
                                                andalso all_same_color(cn::cs');


fun sum_cards(cs) =
        let fun sum_cards_helper(cs, acc) =
                        case cs of
                                [] => acc
                                | c::cs' => sum_cards_helper(cs', card_value(c) + acc)
        in
                sum_cards_helper(cs, 0)
        end;


fun score(cs, goal) =
        let val sumCards = sum_cards(cs)
                val preliminaryScore = if sumCards > goal
                                                             then 3 * (sumCards - goal)
                                                             else goal - sumCards
        in
                if all_same_color(cs)
                then preliminaryScore div 2
                else preliminaryScore
        end;


fun officiate (cs, moves, goal) =
        let fun officiate_helper (cards, moves, cards_held, current_score) =
                        case (cards, moves, sum_cards(cards_held) > goal) of
                                 (_, _, true) => current_score
                             | (_, [], _) => current_score
                             | ([], Draw::_, _) => current_score
                             | (card::cards', Draw::m', _) => officiate_helper(cards', m', card::cards_held, score(card::cards_held, goal))
                             | (cards, Discard c:: m', _) =>
                                             let val cards_held' = remove_card(cards_held, c, IllegalMove)
                                             in
                                                     officiate_helper(cards, m', cards_held', score(cards_held', goal))
                                             end
        in
                officiate_helper(cs, moves, [], score([], goal))
        end;