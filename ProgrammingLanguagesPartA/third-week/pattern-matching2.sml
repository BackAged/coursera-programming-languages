(* datatype suit = Club | Diamond | Heart | Spade

fun suitToNum s = 
    case s of
        Club => 1
        | Diamond => 2
        | Heart  => 3
        | Spade => 4

suitToNum( 'Club' ) *)

fun full_name {first=x,middle=y,last=z} =
    x ^ " " ^ y ^ " " ^z

val a = {first="shahin", middle="sdfsdf", last="mahmud"}

(* full_name(a) *)