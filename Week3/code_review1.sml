 (*Dan Grossman, Coursera PL, HW2 Provided Code*)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s, xs) =
    case xs of
        [] => NONE
      | (x::[]) => if same_string (s, x) then SOME [] else NONE
      | (x::xs') => if same_string (s, x) then SOME xs' else
            let val xsoption = all_except_option (s, xs')
            in
                case xsoption of
                    SOME xsoption' => SOME (x::xsoption')
                  | NONE => NONE
            end


fun get_substitutions1(lxs, s) =
    let fun match_option(sopt) =
        case sopt of NONE => [] | SOME sopt' => sopt'
    in 
        case lxs of
            [] => []
          | xs::lxs' => match_option (all_except_option (s, xs)) @ get_substitutions1 (lxs', s)
    end


fun get_substitutions2(lxs, s) =
    let fun match_option sopt = 
        case sopt of NONE => [] | SOME sopt => sopt
        fun aux(lxs, s, acc) = 
            case lxs of
                [] => acc
              | xs::lxs' => aux (lxs', s, acc @ match_option (all_except_option (s, xs)))
    in
        aux(lxs, s, [])
    end


fun similar_names(names, full_name) =
    let
        fun match_name {first=f, last=l, middle=m} = f
        fun make_record (name, {first=f, last=l, middle=m}) =
            {first=name, last=l, middle=m}
        (* extract the original name *)
        val name = match_name full_name
        (* extract all names *)
        val substitutions = match_name full_name::(get_substitutions2 (names, name))
        (* recursively make records out of them *)
        fun make_records substitutions = 
            case substitutions of
                [] => []
              | first::rest => make_record (first, full_name)::make_records rest
    in
        make_records substitutions
    end


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(*put your solutions for problem 2 here*)
fun card_color card =
    case card of
        (Clubs, _) => Black
      |  (Spades, _) => Black
      | _ => Red


fun card_value card =
    case card of
        (_, Num x) => x
      | (_, Ace) => 11
      | _ => 10


fun remove_card(cs, c, e) =
    let
        fun remove_card_local(cs, c, e) =
            case cs of
                [] => []
              | card::rest => if card=c then rest else card::remove_card_local (rest, c, e)
        fun check_for_exception(cs, c, e) =
            case cs of
                [] => raise e
              | card::rest => if card=c then true else check_for_exception (rest, c, e)
        val has_card = check_for_exception (cs, c, e)
        val result = remove_card_local (cs, c, e)
    in
        result
    end


fun all_same_color cs =
    case cs of
        [] => true
      | card::[] => true
      | card1::card2::rest => (card_color card1)=(card_color card2) andalso all_same_color (card2::rest)


fun sum_cards cs =
    let
        fun sum_cards_recursive(cs, acc) =
            case cs of
                [] => acc
              | c::rest => sum_cards_recursive (rest, acc+(card_value c))
        val result = sum_cards_recursive (cs, 0)
    in
        result
    end


fun score(cs, goal) =
    let 
        val sum = sum_cards cs
        val prelim_score = 
            if sum > goal then 3 * (sum - goal)
            else goal - sum
    in
        if all_same_color cs
        then prelim_score div 2
        else prelim_score
    end


fun officiate (cs, moves, goal) =
    let
        fun game_state (cs, moves, goal, held_cs) =
            case moves of
                [] => score (held_cs, goal)
              | Discard c::rest => game_state (cs, rest, goal, remove_card (held_cs, c, IllegalMove))
              | Draw::rest => 
                case cs of
                    [] => score (held_cs, goal)
                  | c::cs' =>
                    let 
                        val held_cs' = (c::held_cs)
                        val sum = sum_cards held_cs'
                    in
                        if sum > goal then score (held_cs', goal)
                        else game_state (cs', rest, goal, held_cs')
                    end
    in
        game_state (cs, moves, goal, [])
    end


val test_score = score ([(Hearts, Ace), (Spades, Num 9)],10)
 

(*helper function to replace n aces with value 1*)
fun replace_n_aces(cs, n) =
    if n=0 then cs
    else
        (case cs of
            (*seems to work without it, but gives non-exhaustive warning*)
            [] => []
          | (s, Ace)::cs' => (s, Num 1)::(replace_n_aces (cs', n - 1))
          | c::cs' => c::(replace_n_aces (cs', n)))

(*helper function to count aces in a hand*)
fun count_aces cs =
    case cs of
        [] => 0
      | (_, Ace)::cs' => 1 + (count_aces cs')
      | _::cs' => count_aces cs'


fun score_challenge (cs, goal) =
    let
        val regular_score = score (cs, goal)
        val aces_count = count_aces cs
        fun try_aces (aces_count, best_score) =
            
            if aces_count = 0 then best_score
            else
                let 
                    val new_cards = replace_n_aces (cs, aces_count)
                    val new_score = score (new_cards, goal)
                in
                    if new_score < best_score then try_aces (aces_count - 1, new_score)
                    else try_aces (aces_count - 1, best_score)
                end
    in
        try_aces (aces_count, regular_score)
    end



fun officiate_challenge(cs, moves, goal) = 
    let
        fun game_state (cs, moves, goal, held_cs) =
            case moves of
                [] => score_challenge (held_cs, goal)
              | Discard c::rest => game_state (cs, rest, goal, remove_card (held_cs, c, IllegalMove))
              | Draw::rest => 
                case cs of
                    [] => score_challenge (held_cs, goal)
                  | c::cs' =>
                    let 
                        val held_cs' = (c::held_cs)
                        val aces_count = count_aces held_cs'
                        val sum = sum_cards (replace_n_aces (held_cs', aces_count))
                    in
                        if sum > goal then score_challenge (held_cs', goal)
                        else game_state (cs', rest, goal, held_cs')
                    end
    in
        game_state (cs, moves, goal, [])
    end


(*helper function to check if we can discard a card to get a zero*)
(*there is probably a better way, append is ugly indeed*)
fun to_discard(drawn_card, cs, goal) =
    let 
        fun to_discard_helper(explored, left) =
            case left of
                [] => NONE
              | c::cs' => 
                    if score (explored @ (drawn_card::cs'), goal)=0
                    then SOME c else to_discard_helper (c::explored, cs')
    in
        to_discard_helper ([], cs)
    end


fun careful_player(cs, goal) =
    let
        fun actions_helper(cs, held_cs, actions) =
            let
                val current_value = sum_cards held_cs
            in
                if score (held_cs, goal) = 0 then actions
                else if abs (current_value - goal) > 10
                then 
                    case cs of
                        c::cs' => actions_helper (cs', c::held_cs, Draw::actions)
                      | [] => actions
                (*less than 11 left to goal*)
                else
                    case cs of
                        c::cs' =>
                            let 
                                val candidate = to_discard (c, held_cs, goal)
                                val sum = sum_cards (c::held_cs)
                            in
                                case candidate of
                                    SOME card => actions_helper (
                                        cs',
                                        card::(remove_card (held_cs, card, IllegalMove)),
                                        Draw::Discard card::actions
                                    )
                                  | _ => if sum <= goal then actions_helper (cs', c::held_cs, Draw::actions) else actions
                            end
                      | _ => actions
            end
    in
        List.rev (actions_helper (cs, [], []))
    end
