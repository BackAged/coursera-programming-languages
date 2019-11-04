fun append (xs,ys) =
    case xs of
        [] => ys
      | x::xs' => x :: append(xs',ys)

(* append([], 3) *)

fun printList (xlist) = 
    case xlist of
        [] => print("\nfinished\n")
        | x :: xlist' => (
            print(Int.toString(x) ^ ", ");
            printList(xlist')
        )

fun unzip3 lst =
 case lst of 
    [] => ([], [], [])
 | (a, b, c)::tl => 
 case unzip3 tl of        
    (x, y, z) => (a::x, b::y, c::z)
