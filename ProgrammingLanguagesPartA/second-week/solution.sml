fun is_older (past : int*int*int, future : int*int*int) = 
    if #1 future > #1 past then true
    else 
        if #1 future < #1 past
        then false
        else if #2 future > #2 past 
             then true
             else if #2 future < #2 past
                then false
                else if #3 future > #3 past
                    then true
                    else false
                    
fun number_in_month(dates : (int*int*int)list, month:int) = 
    if null dates
    then 0
    else if #2 ( hd(dates) ) = month
        then 1 + number_in_month( tl(dates) , month)
        else number_in_month( tl(dates) , month)
        
    
fun number_in_months(dates: (int*int*int)list, months: int list) =
    if null months
    then 0
    else number_in_month(dates,(hd months)) + number_in_months(dates, (tl months))

fun dates_in_month(dates: (int*int*int)list, month:int) = 
    if null dates 
    then []
    else if #2 ( hd(dates) ) = month
        then (hd dates) :: (dates_in_month( tl(dates) , month))
        else dates_in_month( tl(dates) , month)

fun dates_in_months(dates: (int*int*int)list, months:int list) = 
    if null months
    then []
    else dates_in_month(dates,(hd months)) @ dates_in_months(dates, (tl months))

fun  get_nth(w : string list, n : int) =
    if n = 1
    then hd w
    else get_nth((tl w), n-1)

fun date_to_string(date : (int*int*int)) = 
    let
       val months = ["January", "February", "March", "April",
        "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(months, #2 date)  ^ " " ^  Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
    end

fun number_before_reaching_sum (sum : int, num : int list) = 
    let
        fun beforeSum (l : int list, i : int, s : int) = 
            if s < sum
            then beforeSum (tl l, i + 1, s + hd l)
            else i - 1
    in
        beforeSum(num, 0, 0)
    end
    
fun what_month (day : int) =
    let 
       val months = [31,28, 31,30,31,30,31,31,30,31,30,31]
    in
       number_before_reaching_sum(day, months) + 1
    end

fun oldest (days : (int*int*int)list) = 
    if null days
    then NONE 
    else
        let val tl_d = oldest (tl days)
        in
           if isSome tl_d andalso is_older ( valOf tl_d, hd days)
           then tl_d
           else SOME (hd days)
        end