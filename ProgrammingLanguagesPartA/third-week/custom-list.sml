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

datatype karim = ,