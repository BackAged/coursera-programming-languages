fun fold (f, l, acc) =
    case l of
        [] => acc
        | x::ls => (print (Int.toString(x)); fold(f, ls, f(x, acc)) )