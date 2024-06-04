(* date is type int*int*int with year*month*day *)

fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
    let 
        fun is_smaller(x : int, y : int) =
        if x = y 
        then is_older((#2 date1, #3 date1, #1 date1), (#2 date2, #3 date2, #1 date2))
        else x < y
    in
        if (#1 date1) = (#1 date2) andalso (#2 date1) = (#2 date2) andalso (#3 date1) = (#3 date2)
        then false
        else is_smaller((#1 date1), (#1 date2))
    end

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month 
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months (dates : (int*int*int) list, months : int list) =
    let 
        fun month_in_date(date : (int*int*int), months: int list) =
            if null months
            then 0
            else if (#2 date) = (hd months)
                then 1
                else month_in_date(date, tl months)
    in 
        if (null dates) orelse (null months)
        then 0
        else month_in_date(hd dates, months) + number_in_months (tl dates, months)
    end

fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month 
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    let 
        fun month_in_date(date : (int*int*int), months: int list) =
            if null months
            then []
            else if (#2 date) = (hd months)
                then [date]
                else month_in_date(date, tl months)
    in 
        if (null dates) orelse (null months)
        then []
        else month_in_date(hd dates, months) @ dates_in_months (tl dates, months)
    end
    
fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)
