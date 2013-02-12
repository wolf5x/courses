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
  fun get_dt_by_constructor c =
    List.find (fn(x,_,_) => x=c) dts

  (* check if pattern p matches typ t *)
  fun check_pat_match_typ (p,t) =
    case (p,t) of
         (_, Anything) => true
       (* "Variable" unknown. suppose we have:
       * datatype: MyDt = Con of tupleT([IntT])
       * and pattern: ConstructorP("Con", Variable "x")
       * should it return SOME Datatype "MyDt" or NONE? *)
       (*| (Variable _, _) => true *) 
       | (UnitP, UnitT) => true
       | (ConstP _, IntT) => true
       | (TupleP ps, TupleT ts) => 
           ((List.foldl (fn(x,ans) => ans andalso check_pat_match_typ(x)) 
                true (ListPair.zipEq(ps,ts)))
           handle UnequalLengths => false)
       | (ConstructorP(s1,p1), Datatype s2) =>
           (case get_dt_by_constructor(s1) of
                 NONE => false
               | SOME (c,d,vt) => d=s2 andalso check_pat_match_typ (p1,vt))
       | _ => false
    
  (* Map patterns to the most strict typs *)
  fun pat_to_typ p = 
  let 
    (* map [l1,l2,..] to SOME([f(l1),f(l2),..]), 
    * or NONE if any f(ln) is NONE *)
    fun map_all_or_none f l = 
      List.foldr (fn(x,lst) => 
        case lst of 
             NONE => NONE 
           | SOME lst => case f x of 
                              NONE => NONE 
                            | SOME y => SOME(y::lst)) (SOME []) l
  in
    case p of
         Wildcard => SOME Anything
       | Variable _ => SOME Anything
       | UnitP => SOME UnitT
       | ConstP _ => SOME IntT
       | TupleP ps => (case map_all_or_none pat_to_typ ps of
                           NONE => NONE
                         | SOME ts => SOME (TupleT ts))
       | ConstructorP (s1,p1) =>
           case get_dt_by_constructor s1 of
                NONE => NONE
              | SOME (c,d,vt) => if check_pat_match_typ (p1,vt) 
                                 then SOME (Datatype d)
                                 else NONE
  end
  (* Merge two typs. If can't, return NONE *)
  fun merge_two_typs (t1, t2) =
    case (t1, t2) of
         (NONE, _) => NONE
       | (_, NONE) => NONE
       | (SOME Anything, SOME _) => t2
       | (SOME _, SOME Anything) => t1
       | (SOME(TupleT ts1), SOME(TupleT ts2)) => 
           (* t1 and t2 should have same length *)
           if List.length(ts1)<> List.length(ts2) then NONE
           else 
             let 
               val ts = List.map (fn(t1,t2) => merge_two_typs(SOME t1, SOME t2))
                  (ListPair.zip(ts1,ts2))
             in
               case List.find (fn NONE=>true|_=>false) ts of
                    NONE => SOME(TupleT(List.map valOf ts))
                  | _ => NONE
             end
       | _ => if t1=t2 then t1 else NONE

  (* Generate the strict typ that covers all typs generated before. *)
  fun merge_all_typs [] = NONE
    | merge_all_typs (ty::tys') =
      List.foldl (fn(x,ans) => 
      case ans of 
           NONE => NONE
         | _ => merge_two_typs (x,ans)) ty tys'
in
  merge_all_typs (List.map pat_to_typ ps)
end


