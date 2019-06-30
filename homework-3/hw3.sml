(* Coursera Programming Languages, Homework 3, Provided Code *)

(* 1 *)
val only_capitals = List.filter (fn s => Char.isUpper (String.sub(s, 0)))

(* 2 *)
val longest_string1 =
	List.foldl (fn(s, acc) => if String.size s > String.size acc then s else acc) ""

(* 3 *)
val longest_string2 =
	List.foldl (fn(s, acc) => if String.size s >= String.size acc then s else acc) ""

(* 4 *)
fun longest_string_helper f xs =
	List.foldl (fn(s, acc) => if f(String.size s, String.size acc) then s else acc) "" xs

val longest_string3 = longest_string_helper (fn(a, b) => a > b)
val longest_string4 = longest_string_helper (fn(a, b) => a >= b)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals

(* 6 *)
val rev_string = String.implode o List.rev o String.explode

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

(* 7 *)
fun first_answer _ [] = raise NoAnswer
	| first_answer f (x::xs) =
			case f x of
				SOME v => v
			| NONE => first_answer f xs

(* 8 *)
fun all_answers _ [] = SOME []
	| all_answers f (x::xs) =
			case f x of
				NONE => NONE
			| SOME v => case all_answers f xs of
										NONE => NONE
									| SOME vs => SOME (v @ vs)

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
    end;

(* 9a *)
val count_wildcards = g (fn() => 1) (fn(x) => 0)

(* 9b *)
val count_wild_and_variable_lengths = g (fn() => 1) String.size

(* 9c *)
fun count_some_var(s, p) = g (fn() => 0) (fn(x) => if s = x then 1 else 0) p

(* 10 *)
fun check_pat p =
	let fun collect_var p =
				case p of
					Variable str => [str]
				| TupleP ps => foldl (fn(p, acc) => acc @ (collect_var p)) [] ps
				| ConstructorP(_,p) => collect_var p
				| _ => []
			fun no_duplicate xs =
				case xs of
					[] => true
				| (x::xs') => if List.exists (fn x' : string => x = x') xs' then false else no_duplicate xs'
	in
		(no_duplicate o collect_var) p
	end;

(* 11 *)
fun match(_, Wildcard) = SOME []
	| match(v, Variable str) = SOME [(str, v)]
	| match(Unit, UnitP) = SOME []
	| match(Const v, ConstP pv) = if v = pv then SOME [] else NONE
	| match(Tuple vs, TupleP ps) = (
			all_answers (match) (ListPair.zipEq(vs, ps))
			handle UnequalLengths => NONE)
	| match(Constructor(str_v, v), ConstructorP(str_p, p)) =
			if str_v = str_p then match(v, p) else NONE
	| match(_, _) = NONE;

(* 12 *)
fun first_match v ps =
	SOME (first_answer (match) (List.map (fn p => (v, p)) ps))
	handle NoAnswer => NONE

(* challenge problem *)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

exception UnknownConstructor;

fun type_fit(_, Anything) = true
	| type_fit(UnitT, UnitT) = true
	| type_fit(IntT, IntT) = true
	| type_fit(TupleT src, TupleT dest) = (
			List.all (type_fit) (ListPair.zipEq(src, dest))
			handle UnequalLengths => false)
	| type_fit(Datatype src, Datatype dest) = (src = dest)
	| type_fit(_, _) = false

fun typecheck_pattern _ Wildcard = Anything
  | typecheck_pattern _ (Variable _) = Anything
	| typecheck_pattern _ UnitP = UnitT
	| typecheck_pattern _ (ConstP _) = IntT
	| typecheck_pattern ctors (TupleP ps) =
			TupleT (List.map (typecheck_pattern ctors) ps)
	| typecheck_pattern ctors (ConstructorP(ctor, p)) =
			let
				val in_type = typecheck_pattern ctors p
			  val ctor_sig = List.find (fn(name, _, _) => name = ctor) ctors
			in
				case ctor_sig of
					NONE => raise UnknownConstructor
				| SOME (_, out_str, arg_type) =>
						if type_fit(in_type, arg_type) then Datatype out_str
						else raise UnknownConstructor
			end;

fun typecheck_patterns(ctors, ps) =
	let
	  val pts = List.map (typecheck_pattern ctors) ps
		fun type_reduce(t, ct) =
			case ct of
				NONE => NONE
			| SOME t' =>
					if type_fit(t', t) then ct
				  else if type_fit(t, t') then SOME t
					else NONE
	in
		List.foldl type_reduce (SOME (hd pts)) pts
	end
	handle UnknownConstructor => NONE;
