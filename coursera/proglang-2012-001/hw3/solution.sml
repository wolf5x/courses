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

(* problem 1 *)
val only_capitals = List.filter (fn x => Char.isUpper(String.sub(x,0)))

(* problem 2 *)
val longest_string1 = List.foldl (fn(x,acc) => if String.size(x) > String.size(acc) 
                                          then x else acc) ""

(* problem 3 *)
val longest_string2 = List.foldl (fn(x,acc) => if String.size(x) >= String.size(acc)
                                          then x else acc) ""

(* problem 4 *)
fun longest_string_helper f l = List.foldl(fn(x,acc) => if f(String.size(x),
  String.size(acc)) then x else acc) "" l

val longest_string3 = longest_string_helper (fn(x,y) => x > y)

val longest_string4 = longest_string_helper (fn(x,y) => x >= y)

(* problem 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* problem 6 *)
val rev_string = String.implode o List.rev o String.explode

(* problem 7 *)
fun first_answer _ [] = raise NoAnswer
  | first_answer f (x::xs') = 
        case f x of 
             SOME y => y
           | NONE => first_answer f xs'

(* problem 8 *)
fun all_answers f l = List.foldl 
  (fn(x,acc) => case acc of 
                     NONE => NONE 
                   | SOME lacc => (case f x of 
                                        NONE => NONE 
                                      | SOME lst => SOME (lacc@lst))) 
  (SOME []) l

(* problem 9a *)
val count_wildcards = g (fn _ => 1) (fn _ => 0)

(* problem 9b *)
val count_wild_and_variable_lengths = g (fn _ => 1) (String.size)

(* problem 9c *)
fun count_some_var(s, p) = g (fn _ => 0) (fn x => if x=s then 1 else 0) p

(* problem 10 *)
fun check_pat p = 
let 
  fun var_strings p = 
    case p of
         Variable x => [x]
       | TupleP ps => List.foldl (fn(p,lst) => lst @ (var_strings p)) [] ps
       | ConstructorP(_,p) => var_strings p
       | _ => []
  fun check_unique [] = true
    | check_unique (x::xs') = not(List.exists (fn y => x=y) xs') andalso check_unique(xs')
in check_unique(var_strings(p))
end

(* problem 11 *)
fun match (v, p) = 
  case (v, p) of
       (_, Wildcard) => SOME []
     | (v1, Variable p2) => SOME [(p2,v1)]
     | (Unit, UnitP) => SOME []
     | (Const v1, ConstP p2) => if v1=p2 then SOME [] else NONE
     | (Constructor(s1,v1), ConstructorP(s2,p2)) => if s1=s2 
                                                    then match(v1,p2)
                                                    else NONE
     | (Tuple(vs), TupleP(ps)) => if List.length(vs)=List.length(ps) 
                                  then all_answers match (ListPair.zip(vs,ps))
                                  else NONE
     | _ => NONE

(* problem 12 *)
fun first_match v ps = 
  SOME(first_answer (match) (List.foldr (fn(p,lst) => (v,p)::lst) [] ps)) handle NoAnswer => NONE
 
(* challenge problem *)
fun typecheck_patterns (dts, ps) =
let
  fun merge_typ (t1,t2) = 
    if t1=Anything then SOME t2
    else if t2=Anything then SOME t1
    else if t1<>t2 then None
    else SOME t1
    
  (* Map patterns to the most strict typs *)
  fun pat_to_typ p = 
    case p of
         Wildcard => SOME Anything
       | Variable _ => SOME Anything
       | UnitP => SOME UnitT
       | ConstP _ => SOME IntT
       | TupleP xs => List.foldr (fn(p,lst) => (pat_to_typ p)::lst) [] xs
       | ConstructorP (s1,p1) => 
           case (List.find (fn(foo,_,_) => foo=s1) dts) of
                NONE => NONE
              | SOME (foo,bar,ty) => 
                  let 
                    val ok = SOME (Datatype bar)
                  in case (p1,ty) of
                          (Wildcard, Anything) => ok
                        | (Variable, Anything) => ok
                        | (UnitP, UnitT) => ok
                        | (ConstP _, IntT) => ok
                        | (TupleP ps, TupleT ts) => 
                            if List.length(ps)<>List.length(ts)
                            then Anything
                            else if ts<>(List.map merge_typ (List.zip(List.map
                            pat_to_typ ps, ts)))
                                 then Anything
                                 else ts
                        | _ => Anything
                  end
  (* Generate the strict typ that covers all typs generated before. *)
  fun merge_all_typs (ty::tys') =
        SOME(List.foldl (fn(t,acc) => List.map merge_typ (List.zip(t,acc))) ty
        tys')
    | merge_all_typs _ => NONE
in
  merge_all_typs ()






