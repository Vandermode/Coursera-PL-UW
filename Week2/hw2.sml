 (* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option (key, str_lst) =
    let fun aux (current, done) =
        case current of
            [] => NONE
            | s :: rest => if same_string (s, key) then SOME (done @ rest) else aux (rest, done @ [s])
    in
        aux (str_lst, [])
    end

fun get_substitutions1 (s_groups, key) =
    case s_groups of
        [] => []
        | first :: others =>
        case all_except_option (key, first) of
            NONE => get_substitutions1 (others, key)
            | SOME lst => lst @ get_substitutions1 (others, key)

fun get_substitutions2 (s_groups, key) =
    let fun aux (current, done) =
        case current of
            [] => done
            | first :: others => 
            case all_except_option (key, first) of
                NONE => aux (others, done)
                | SOME lst => aux (others, done @ lst)
    in
        aux (s_groups, [])
    end

fun similar_names (s_groups, key as {first=x, middle=y, last=z}) = 
    let fun expand_names (names, done) =
        case names of
            [] => done
            | head :: tail => expand_names (tail, done @ [{first=head, middle=y, last=z}])
    in
        key :: expand_names (get_substitutions2 (s_groups, x), [])
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
fun card_color (suit, rank) =
    case suit of
        (Clubs | Spades) => Black
        | (Hearts | Diamonds) => Red

fun card_value (suit, rank) =
    case rank of
        Num i => i
        | Ace => 11
        | _ => 10

fun remove_card (cards, key_card, ex) =
    case cards of
        [] => raise ex
        | c :: cards' => if c = key_card then cards' else c :: remove_card(cards', key_card, ex)

fun all_same_color (cards) =
    case cards of
         ([] | [_]) => true
         | c1 :: c2 :: rest => card_color c1 = card_color c2 andalso all_same_color (c2 :: rest)

fun sum_cards (cards) =
    let fun aux (cards, scores) =
        case cards of
            [] => scores
            | c :: cards' => aux (cards', card_value c + scores)
    in
        aux (cards, 0)
    end

fun score (cards, goal) =
    let val sum = sum_cards cards
        val pre_scores = if sum > goal then 3 * (sum - goal) else goal - sum
    in
        case all_same_color cards of
            true => pre_scores div 2
            | false => pre_scores
    end

fun officiate (deck, moves, goal) =
    let fun game_aux (deck, moves, hand, sum) =
        if sum > goal then score (hand, goal)
        else
        case moves of
            [] => score (hand, goal)
            | m :: moves' => 
            case (deck, m) of
                (deck, Discard c) => 
                game_aux (deck, moves', remove_card (hand, c, IllegalMove), sum - card_value c)
                | (c :: deck', Draw) => 
                game_aux (deck', moves', c :: hand, sum + card_value c)
                | ([], Draw) => score (hand, goal)
    in
        game_aux (deck, moves, [], 0)
    end

(* challenge problems *)
(* helper function for challenge *)
fun card_value_challenge (suit, rank) =
    case rank of
        Num i => i
        | Ace => 1
        | _ => 10

(* card list -> int * int *)
fun sum_cards_challenge (cards) =
    let fun aux (cards, scores, ace) =
        case cards of
            [] => (scores, ace)
            | c :: cards' => 
            let val value = card_value_challenge c
            in 
                if value = 1 then aux (cards', 1 + scores, ace + 1) 
                else aux (cards', value + scores, ace)
            end
    in
        aux (cards, 0, 0)
    end

fun all_possible_sum (sum, ace) =
    let fun aux (sum, ace, all) =
        case ace of
            0 => all
            | i => aux (sum + 10, ace - 1, (sum + 10) :: all)
    in
        aux (sum, ace, [sum])
    end

fun pre_score (sum, goal) =
    if sum > goal then 3 * (sum - goal) else goal - sum

fun pre_score_challenge (all, goal) =
    case all of
        [] => raise List.Empty
        | first :: [] => pre_score (first, goal)
        | first :: others => Int.min (pre_score (first, goal), pre_score_challenge (others, goal))

fun score_challenge (cards, goal) = 
    let val pre_scores = pre_score_challenge (all_possible_sum (sum_cards_challenge (cards)), goal)
    in
        case all_same_color cards of
            true => pre_scores div 2
            | false => pre_scores
    end

(* simple solution *)
fun score_challenge2 (cards, goal) =
    let fun aux ([], csm, best_score) = best_score
        | aux ((suit, Ace) :: cs', csm, best_score) =
            let val csm = (suit, Num 1) :: csm
                val new_score = score (cs' @ csm, goal)
            in
                aux (cs', csm, Int.min (new_score, best_score))
            end
        | aux (c :: cs', csm, best_score) = aux (cs', c :: csm, best_score)
    in
        aux (cards, [], score (cards, goal))
    end

fun officiate_challenge (deck, moves, goal) =
    let fun game_aux (deck, moves, hand, sum) =
        if sum > goal then score_challenge (hand, goal)
        else
            case moves of
            [] => score_challenge (hand, goal)
            | m :: moves' => 
                case (deck, m) of
                (deck, Discard c) =>
                game_aux (deck, moves', remove_card (hand, c, IllegalMove), sum - card_value_challenge c)
                | (c :: deck', Draw) => 
                game_aux (deck', moves', c :: hand, sum + card_value_challenge c)
                | ([], Draw) => score_challenge (hand, goal)
    in
        game_aux (deck, moves, [], 0)
    end

(* simple solution *)
fun officiate_challenge2 (deck, moves, goal) =
    let fun aux (([], Draw :: _, hand) | (_, [], hand)) = score_challenge2 (hand, goal)
        | aux (cs, Discard c :: ms, hand) = aux (cs, ms, remove_card (hand, c, IllegalMove))
        | aux (c :: cs, Draw :: ms, hand) = 
            let val (sum, _) = sum_cards_challenge (c :: hand)
            in
                if sum > goal then score_challenge2 (c :: hand, goal)  else aux (cs, ms, c :: hand)
            end
    in
        aux (deck, moves, [])
    end

