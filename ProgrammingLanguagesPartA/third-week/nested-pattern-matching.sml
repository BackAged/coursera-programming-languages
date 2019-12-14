fun zipThreeElementList list_triple =
    case list_triple of
        ([],[],[]) => []
        | (a::xl1, b::xl2, c::xl3) => (a,b,c) :: zipThreeElementList(xl1, xl2, xl3)

fun unzip3 lt =
    case lt of
        [] => ([], [], [])
        | (a,b,c):: tl =>
                let val (x, y, z) = unzip3(tl)
                in
                    (a::x, b::y, c::z)
                end
        

datatype sgn = P | N | Z

fun multsign (x1,x2) =
    let 
        fun sign x = if x=0 then Z else if x>0 then P else N
    in
        case (sign x1,sign x2) of
        (Z,_) => Z
        | (_,Z) => Z
        | (P,P) => P
        | (N,N) => P
        | _ => N (* many say bad style; I am okay with it *)
    end


fun add (a, b) =
    case a, b of
        