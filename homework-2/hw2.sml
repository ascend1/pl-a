(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

(* a *)
fun all_except_option(s, []) = NONE
  | all_except_option(s, x::xs) =
    let val rem = all_except_option(s, xs)
        val same = same_string(s, x)
    in
      case rem of NONE   => if same then SOME xs else NONE
                | SOME r => if same then rem else SOME (x::r)
    end;

(* b *)
fun get_substitutions1([], s) = []
  | get_substitutions1(x::xs, s) =
    case all_except_option(s, x) of NONE => get_substitutions1(xs, s)
                                  | SOME lst => lst @ get_substitutions1(xs, s);

(* c *)
fun get_substitutions2(xs, s) =
  let fun tail_helper(acc, xs) =
        case xs of [] => acc
                | x::xs' => case all_except_option(s, x) of
                              NONE => tail_helper(acc, xs')
                            | SOME lst => tail_helper(acc @ lst, xs')
  in
    tail_helper([], xs)
  end;

(* d *)
fun similar_names(s_lst, {first=f, middle=m, last=l}) =
  let fun expand_name(replaces, {first=f, middle=m, last=l}) =
        case replaces of
          [] => []
        | x::xs => {first=x, middle=m, last=l} :: expand_name(xs, {first=f, middle=m, last=l});
  in
    case get_substitutions1(s_lst, f) of
      [] => [{first=f, middle=m, last=l}]
    | ys => {first=f, middle=m, last=l} :: expand_name(ys, {first=f, middle=m, last=l})
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

(* a *)
fun card_color(Clubs, _) = Black
  | card_color(Spades, _) = Black
  | card_color(Diamonds, _) = Red
  | card_color(Hearts, _) = Red;

(* b *)
fun card_value(_, Ace) = 11
  | card_value(_, Num x) = x
  | card_value(_, _) = 10;

(* c *)
fun remove_card(cs, c, e) =
  let fun helper(acc, cs, c, e) =
        case cs of
          [] => raise e
        | c'::cs' => if c' = c then acc @ cs' else helper(acc @ [c'], cs', c, e)
  in
    helper([], cs, c, e)
  end;

(* d *)
fun all_same_color(c1::c2::cs') =
    if card_color c1 = card_color c2 then all_same_color(c2::cs') else false
  | all_same_color(cs) = true;

(* e *)
fun sum_cards(cs) =
  let fun helper(acc, cs) =
        case cs of
          [] => acc
        | c::cs' => helper(acc + card_value c, cs');
  in
    helper(0, cs)
  end;

(* f *)
(* make score' version for later problems *)
fun score'(sum, goal, all_same) =
  let fun p_score(sum, goal) =
        if sum > goal then (sum - goal) * 3
        else goal - sum;
  in
    if all_same then p_score(sum, goal) div 2
    else p_score(sum, goal)
  end;

fun score(cs, goal) =
  score'(sum_cards(cs), goal, all_same_color(cs))

(* g *)
fun officiate(deck, moves, goal) =
  let fun process_move(hand, deck, moves, goal) =
        case moves of
          [] => score(hand, goal)
        | m::ms =>
            case m of
              Discard c => process_move(remove_card(hand, c, IllegalMove), deck, ms, goal)
            | Draw =>
                case deck of
                  [] => score(hand, goal)
                | d::ds => let val new_hand = d :: hand
                           in if sum_cards(new_hand) > goal then score(new_hand, goal)
                              else process_move(new_hand, ds, ms, goal)
                           end;
  in
    process_move([], deck, moves, goal)
  end;

(* challenge problems *)

(* utility functions *)
fun count_ace([]) = 0
  | count_ace(c::cs) =
      case c of (_, Ace) => 1 + count_ace(cs)
              | _ => count_ace(cs);

(* a *)
fun score_challenge(cs, goal) =
  let val sum = sum_cards(cs)
      val num_aces = count_ace(cs)
      val all_same = all_same_color(cs)
      fun helper(num_aces, sum, goal, all_same, min_score) =
        if num_aces > 0 then
          let val new_score = score'(sum - 10, goal, all_same)
          in
            if new_score < min_score then
              helper(num_aces - 1, sum - 10, goal, all_same, new_score)
            else
              helper(num_aces - 1, sum - 10, goal, all_same, min_score)
          end
        else
          min_score;
  in
    helper(num_aces, sum, goal, all_same, score'(sum, goal, all_same))
  end;

fun officiate_challenge(deck, moves, goal) =
  let fun process_move(hand, deck, moves, goal) =
        case moves of
          [] => score_challenge(hand, goal)
        | m::ms =>
            case m of
              Discard c => process_move(remove_card(hand, c, IllegalMove), deck, ms, goal)
            | Draw =>
                case deck of
                  [] => score_challenge(hand, goal)
                | d::ds => let val new_hand = d :: hand
                               val sum = sum_cards(new_hand)
                               val num_aces = count_ace(new_hand)
                           in if sum - 10 * num_aces > goal then
                                score_challenge(new_hand, goal)
                              else
                                process_move(new_hand, ds, ms, goal)
                           end;
  in
    process_move([], deck, moves, goal)
  end;

(* b cheating player *)
fun careful_player(deck, goal) =
  let fun closest_card(cs, value) =
        let fun closest_helper(min_c, cs, value) =
              case cs of
                [] => min_c
              | c::cs' =>
                  let val val_diff = card_value c - value
                  in
                    if val_diff > 0 then
                      case min_c of
                        NONE => closest_helper(SOME c, cs', value)
                      | SOME mc =>
                          if val_diff < card_value mc - value then
                            closest_helper(SOME c, cs', value)
                          else
                            closest_helper(min_c, cs', value)
                    else if val_diff = 0 then SOME c
                    else closest_helper(min_c, cs', value)
                  end;
        in
          closest_helper(NONE, cs, value)
        end;
      fun cheating(hand, deck, goal, moves) =
        let val sum = sum_cards(hand)
        in
          case deck of
            [] => if goal - sum > 10 then moves @ [Draw] else moves
          | d::ds =>
              let val space = goal - sum
                  val next_v = card_value d
              in
                if space = next_v then moves @ [Draw]
                else if space > next_v then
                  cheating(d::hand, ds, goal, moves @ [Draw])
                else
                  case closest_card(hand, next_v - space) of
                    NONE => moves
                  | SOME c =>
                      let val new_moves = moves @ [Discard c, Draw]
                          val new_hand = d::remove_card(hand, c, IllegalMove)
                      in
                        if score(new_hand, goal) = 0 then new_moves
                        else cheating(new_hand, ds, goal, new_moves)
                      end
              end
        end;
  in
    cheating([], deck, goal, [])
  end;
