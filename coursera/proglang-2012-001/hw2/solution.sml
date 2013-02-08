(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* problem 1a *)
fun all_except_option(_, []) = NONE
  | all_except_option(s, x::xs') = 
  case all_except_option(s, xs') of
       NONE => if same_string(s, x) then SOME xs' else NONE
     | SOME lst => if same_string(s, x) then SOME lst else SOME(x::lst)

(* problem 1b *)
fun get_substitutions1([], _) = []
  | get_substitutions1(lst::lsts', s) = case all_except_option(s, lst) of
                                             NONE => get_substitutions1(lsts', s) 
                                           | SOME l => l @ get_substitutions1(lsts', s)
(* problem 1c *)
fun get_substitutions2(lsts, s) = 
let fun aux([], _, res) = res
    | aux(lst::lsts', s, res) = case all_except_option(s, lst) of
                                   NONE => aux(lsts', s, res)
                                 | SOME l => aux(lsts', s, res @ l) 
in aux(lsts, s, [])
end

(* problem 1d *)
type fullname = {first:string, middle:string, last:string}
fun similar_names([], _) = []
  | similar_names(lsts, {first=fst, middle=mid, last=las}:fullname) = 
      let fun gen_names([], _) = []
            | gen_names(x::xs', s) = {first=x, middle=mid, last=las}::gen_names(xs', s)
      in {first=fst, middle=mid, last=las}::
        gen_names(get_substitutions2(lsts, fst), {middle=mid, last=las})
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
(* problem 2a *)
fun card_color(c:card) = 
  case c of
       (Spades, _) => Black
     | (Clubs, _) => Black
     | _ => Red

(* problem 2b *)
fun card_value(c:card) = 
  case c of
       (_, Ace) => 11
     | (_, Num x) => x
     | _ => 10

(* problem 2c *)
fun remove_card([], _, e) = raise e
  | remove_card(x::cs', c, e) = if x=c then cs' else x::remove_card(cs', c, e)

(* problem 2d *)
fun all_same_color([]) = true
  | all_same_color(x::[]) = true
  | all_same_color(x1::x2::xs') = 
        card_color(x1)=card_color(x2) andalso all_same_color(x2::xs')

(* problem 2e *)      
fun sum_cards(cs) = 
let fun aux([], sum) = sum
    | aux(x::cs, sum) = aux(cs, sum+card_value(x))
in aux(cs, 0)
end

(* problem 2f *)
fun score(cs, g) =
let 
  val vsum = sum_cards(cs)
  val pscore = if vsum > g then 3*(vsum-g) else g-vsum
in
  if all_same_color(cs) then pscore div 2 else pscore
end

(* problem 2g *)
fun officiate(cards, moves, goal) = 
let 
  fun simulate(hs, _, []) = hs
    | simulate(hs, cs, (Discard m)::ms') = 
        simulate(remove_card(hs, m, IllegalMove), cs, ms')
    | simulate(hs, [], (Draw)::ms') = hs
    | simulate(hs, c::cs', (Draw)::ms') = 
        if sum_cards(c::hs) > goal 
        then c::hs
        else simulate(c::hs, cs', ms')
in score(simulate([], cards, moves), goal)
end
        






