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
fun only_capitals(ls) = List.filter (fn x => Char.isUpper(String.sub(x,0))) ls

fun longest_string1(ls) = List.foldl (fn (x, init) => if String.size(x) > String.size(init) then x else init) "" ls

fun longest_string2(ls) = List.foldl (fn (x, init) => if String.size(x) > String.size(init) 
	then x else if String.size(x) = String.size(init) then x else init) "" ls

fun longest_string3(ls) = List.foldl (fn (x, init) => if String.size(x) > String.size(init) then x else init) "" ls

fun longest_string4(ls) = List.foldl (fn (x, init) => if String.size(x) > String.size(init)
	then x else if String.size(x) = String.size(init) then x else init) "" ls

fun longest_string_helper f ls = List.foldl (fn (x, init) => if String.size(x) > String.size(init)
	then x else if String.size(x) = String.size(init) then x else init) "" ls

val longest_capitalized = longest_string1  o only_capitals

val rev_string =   String.implode o List.rev o String.explode

fun first_answer f l =
	case l of
		[] => raise NoAnswer
		| x:: ls => case f(x) of
					SOME v => v
					| NONE => first_answer f ls

fun all_answers f l =
	case l of
		[] => SOME []
		| x::ls => case f(x) of
				NONE => NONE
				| SOME v => case all_answers f ls of
						NONE => NONE
						| SOME v1 =>  SOME(v @ v1)

fun count_wildcards p = g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = g (fn () => 1) (fn x => String.size(x)) p