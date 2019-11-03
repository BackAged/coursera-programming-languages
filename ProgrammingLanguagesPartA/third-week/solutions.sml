(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun printList xs = print(String.concatWith ", " (map Int.toString xs));

fun all_except_option(s: string, ls : string list) =
    case ls of
        [] => NONE
        | x::ls' => 
            if same_string(s, x) then SOME ls'
            else case all_except_option(s, ls') of
                    NONE => NONE
                    |SOME z => SOME (x::z)

fun get_substitutions1(s:string, lls: string list list) =
    case lls of 
        [] => []
        | x::lls' =>
            case all_except_option(s, x) of
                NONE => get_substitutions1(s, lls')
                | SOME z => z @ get_substitutions1(s, lls')

fun get_substitutions2(s:string, lls: string list list) =
    let fun get_subs(ll, acc) =
        case ll of 
        [] => acc
        | x::ll' =>
            case all_except_option(s, x) of
                NONE => get_subs(ll', acc)
                | SOME z => get_subs(ll', acc @ z)
    in
        get_subs(lls, [])
    end
    
fun similar_names(lls, fullname:{first:string, middle:string, last:string}) = 
    let 
        val {first:string,middle:string, last:string} = fullname
        val sl = get_substitutions1(first, lls)
        fun fnames ssl = 
            case ssl of
                [] => [fullname]
            |   f::ssl' =>
                    {first=f, middle=middle, last=last}::fnames(ssl')
    in
        fnames(sl)
    end

val a = get_substitutions2("s", [["s", "a"], ["a", "b"], ["s", "b"], ["s", "c"]])
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)