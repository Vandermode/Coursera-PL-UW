(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2


(* put your solutions for problem 1 here *)

fun all_except_option(str, [])    = NONE
  | all_except_option(str, s::ss) =
    if same_string(s, str)
    then SOME ss
    else case all_except_option(str, ss) of
              NONE     => NONE
            | SOME ss' => SOME (s::ss')


fun get_substitutions1([], _)      = []
  | get_substitutions1(ss::sss, s) =
    case all_except_option(s, ss) of
        NONE     => get_substitutions1(sss, s)
      | SOME ss' => ss' @ get_substitutions1(sss, s)


fun get_substitutions2(sss, s) =
  let
    fun loop([], acc)      = acc
      | loop(ss::sss, acc) =
        case all_except_option(s, ss) of
             NONE     => loop(sss, acc)
           | SOME ss' => loop(sss, acc @ ss')
  in
    loop(sss, [])
  end


fun similar_names(sss, fullname as {first = first, middle = middle, last = last}) =
  let
    fun loop([])    = []
      | loop(s::ss) = {first = s, middle = middle, last = last}::loop(ss)
  in
    fullname :: loop(get_substitutions2(sss, first))
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

fun card_color((Clubs | Spades), _)    = Black
  | card_color((Diamonds | Hearts), _) = Red


fun card_value(_, (King | Queen | Jack))  = 10
  | card_value(_, Ace)                    = 11
  | card_value(_, Num n)                  = n


fun remove_card([], _, ex)           = raise ex
  | remove_card(c::cs, c': card, ex) = if c = c' then cs else c::remove_card(cs, c', ex)


fun all_same_color([] | [_])            = true
  | all_same_color(c1::(cs' as c2::cs)) =
    card_color(c1) = card_color(c2) andalso all_same_color(cs')


fun sum_cards(cs) =
  let
    fun loop([], acc)    = acc
      | loop(c::cs, acc) = loop(cs, acc + card_value(c))
  in
    loop(cs, 0)
  end


fun score(cs, goal) =
  let
    val score = sum_cards cs
  in
    case (all_same_color(cs), score > goal) of
         (true, true)   => (3 * (score - goal)) div 2
       | (true, false)  => (goal - score) div 2
       | (false, true)  => 3 * (score - goal)
       | (false, false) => goal - score
  end


fun officiate(cs, ms, goal) =
  let
    fun loop((_, [], hcs) | ([], Draw::_, hcs)) = score(hcs, goal)
      | loop (cs, (Discard c)::ms, hcs)         = loop(cs, ms, remove_card(hcs, c, IllegalMove))
      | loop(c::cs, Draw::ms, hcs)              =
        if sum_cards(c::hcs) > goal then score(c::hcs, goal) else loop(cs, ms, c::hcs)
  in
    loop(cs, ms, [])
  end


(* CHALLENGE PROBLEMS *)

fun score_challenge(cs, goal) =
  let
    fun loop([], _, best_score)                 = best_score
      | loop((color, Ace)::cs, csm, best_score) =
        let
          val csm = (color, Num 1)::csm
          val new_score = score(csm @ cs, goal)
        in
          loop(cs, csm, Int.min(new_score, best_score))
        end
      | loop(c::cs, csm, best_score)           = loop(cs, c::csm, best_score)
  in
    loop(cs, [], score(cs, goal))
  end


fun officiate_challenge(cs, ms, goal) =
  let
    fun sum_cards([])           = 0
      | sum_cards((_, Ace)::cs) = 1 + sum_cards(cs)
      | sum_cards(c::cs)        = card_value(c) + sum_cards(cs)

    fun loop((_, [], hcs) | ([], Draw::_, hcs)) = score_challenge(hcs, goal)
      | loop(cs, (Discard c)::ms, hcs)          = loop(cs, ms, remove_card(hcs, c, IllegalMove))
      | loop(c::cs, Draw::ms, hcs)              =
        if sum_cards(c::hcs) > goal then score_challenge(c::hcs, goal) else loop(cs, ms, c::hcs)
  in
    loop(cs, ms, [])
  end


fun careful_player(cs, goal) =
  let

    fun cheat(phs, [], c)    = NONE
      | cheat(phs, h::hs, c) =
        if score(c::(phs @ hs), goal) = 0 then SOME h else cheat(h::phs, hs, c)

    fun next_move(ms, cs) =
      if score(ms, goal) = 0 then [] else pick_move(ms, cs)

    and pick_move(ms, []) = if sum_cards(ms) + 10 < goal then Draw::next_move(ms, cs) else []
      | pick_move(ms, c::cs) =
        case cheat([], ms, c) of
             NONE   => if sum_cards(ms) + 10 < goal orelse sum_cards(c::ms) <= goal
                       then Draw::next_move(c::ms, cs)
                       else []
           | SOME h => [Discard h, Draw]
  in
    next_move([], cs)
  end
