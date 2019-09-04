fun countdown (x : int) =
    if x=0
    then []
    else x :: countdown(x-1)

val b = countdown(3)