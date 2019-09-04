fun is_older(past : int*int*int, future : int*int*int) = 
    if #1 future > #1 past then true
    else 
        if #1 future < #1 past then false
        else
            if #2 future > #2 past then true
            else
                if #2 future < #2 past then false
                else
                    if #3 future > #3 past then true
                    else false
                    
fun number_in_month(dates: (int*int*int)list, month:int) = 
    if null dates then 0
    else
        if #2 ( hd(dates) ) = month then 1 + number_in_month( tl(dates) , month)
        else number_in_month( tl(dates) , month)
        
        
    
    