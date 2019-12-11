datatype 'a tree = Empty | Node of 'a * 'a tree list

fun size_forest f = 
    case f of
        [] => 0
        | x::(_,f) =>

fun size_tree Empty = 0
  | size_tree (Node (_, f)) = 1 + size_forest f