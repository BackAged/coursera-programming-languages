(*
fun filterAndSum(l : int list) = 
    let
      fun filter(l: int list) =
        if null l
        then []
        else 
            if hd l > 0 then hd l :: filter(tl(l)) else 
    in
      body
    end
*)

fun filter(l: int list) =
    if null l
    then []
    else if hd l > 0 then hd l :: filter(tl l) else filter(tl l)