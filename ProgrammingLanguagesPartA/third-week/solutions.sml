(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s: string, ls : string list) =
    case ls of
        [] => NONE
        | x::ls' => 
            if same_string(s, x) then SOME ls'
            else case all_except_option(s, ls') of
                    NONE => NONE
                    |SOME z => SOME (x::z)

fun get_substitutions1(lls: string list list, s:string) =
    case lls of 
        [] => []
        | x::lls' =>
            case all_except_option(s, x) of
                NONE => get_substitutions1(lls', s)
                | SOME z => z @ get_substitutions1(lls',s)

fun get_substitutions2(lls: string list list, s:string) =
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
        val sl = get_substitutions1(lls,first)
        fun fnames ssl = 
            case ssl of
                [] => [fullname]
            |   f::ssl' =>
                    {first=f, middle=middle, last=last}::fnames(ssl')
    in
        fnames(sl)
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
fun card_color(card: card) =
    case card of
        (Clubs, _) => Black
        | (Spades, _) => Black
        | _ => Red
            
fun card_value(card: card) =
    case card of
        (_, Ace) => 11
        |   (_, Num(a)) => a
        |   _ => 10

fun remove_card(cs : card list, c: card, e) =
    case cs of
        [] => raise e
        |  crd::cs' =>
            if crd=c then cs'
            else
                case remove_card(cs', c, e) of
                    z => crd::z
                                    

fun all_same_color(cs : card list) =
    case cs of
        [] => true
        | head::[] => true
        |   head::(neck::rest) =>
            if card_color(head) = card_color(neck) andalso all_same_color(neck::rest) then true
            else false


fun sum_cards(cs : card list) =
    let fun sum(cs, s) =
        case cs of
        [] => s
        |   c::cs' => sum(cs', card_value(c) + s)
    in
        sum(cs, 0)
    end

fun score (cs: card list, goal) =
    let 
        val sum = sum_cards(cs)
        val preliSum = if sum > goal then 3 * (sum-goal) else goal - sum
    in
        case cs of
            [] => goal
            | _ => 
                case all_same_color(cs) of
                    true => preliSum div 2
                    |   false => preliSum
    end
    