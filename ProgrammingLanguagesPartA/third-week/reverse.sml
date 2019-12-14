fun add (ls: int list) =
    case ls of 
        [] => 0 
    |   x :: ls'=>(print(Int.toString(x)); x + add (ls')) 
