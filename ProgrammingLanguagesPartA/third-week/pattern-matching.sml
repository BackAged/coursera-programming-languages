datatype exp = Constant of int
| Negate of exp
| Add of exp * exp
| Multiply of exp * exp

fun eval e =
    case e of
        Constant i => i
        | Negate e2 => ~ (eval e2)
        | Add(e1,e2) => (eval e1) + (eval e2)
        | Multiply(e1,e2) => (eval e1) * (eval e2)


(* eval (Add (Constant 19, Constant 4)) *)

fun append (xs,ys) =
    case xs of
    [] => ys
    | x::xs’ => x :: append(xs’,ys)

(* static environment *)
(* dynamic environment *)
(* type checking rules *)
(* expression evaluation rules *)
(* scope *)
(* recursion *)
(* tail recursion *)
(* custom type *)
(* pattern matching *)
(* type inference => *)
(* polymorphic type => *)

datatype people = {name:string option, id:int}

val b:people = {id=1, name=NONE};
