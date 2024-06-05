(* date is type int*int*int with year*month*day *)

fun is_older (date1 : (int * int * int), date2 : (int * int * int)) =
    let 
        val y1 = (#1 date1)
        val y2 = (#1 date2)
        val m1 = (#2 date1)
        val m2 = (#2 date2)
    in
        y1 < y2 orelse (y1 = y2 andalso m1 < m2)
                orelse (y1 = y2 andalso m1 = m2 andalso ((#3 date1) < (#3 date2)))
    end

fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates)) = month 
    then 1 + number_in_month(tl dates, month)
    else number_in_month(tl dates, month)

fun number_in_months (dates : (int*int*int) list, months : int list) = 
    if (null dates) orelse (null months)
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
    
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month 
    then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)

fun dates_in_months (dates : (int*int*int) list, months : int list) =
    if (null dates) orelse (null months)
    then []
    else dates_in_month(dates, hd months) @ dates_in_months (dates, tl months)
    
fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth (tl strings, n - 1)

fun date_to_string (date : (int*int*int)) =
    let
        val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] 
    in
        (get_nth (months, #2 date)) ^ " " ^ (Int.toString(#3 date)) ^ ", " ^ (Int.toString(#1 date))
    end

fun number_before_reaching_sum (sum : int, elements : int list) =
    if (hd elements) >= sum
    then 0
    else 1 + number_before_reaching_sum ((sum - hd elements), tl elements)

fun what_month (day : int) = 
    let
        val months = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
    in
        number_before_reaching_sum (day, months)
    end

fun month_range (day1 : int, day2 : int) = 
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)

fun oldest (dates : (int*int*int) list) = 
    if null dates 
    then NONE
    else if null (tl dates)
        then SOME (hd dates)
        else if is_older(hd dates, hd (tl dates))
            then oldest(hd dates :: tl (tl dates))
            else oldest(tl dates)

fun remove_duplicates (months : int list) = 
    if null months
    then []
    else
        let fun remove_month (month : int, months : int list) = 
            if null months
            then [] 
            else if month = hd months
                then remove_month(month, tl months)
                else hd months :: remove_month(month, tl months)
        in hd months :: remove_duplicates(remove_month(hd months, months))
        end


fun number_in_months_challenge (dates : (int*int*int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))
    
fun dates_in_months_challenge (dates : (int*int*int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))

fun reasonable_date (date : (int * int * int)) = 
    let
        fun is_leap_year (year : int) = 
            if (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0)) 
            then [0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 
            else [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] 

        val months = is_leap_year(#1 date)

        fun sum_days(months : int list, n : int)  =
            if n = 0 orelse null months
            then (#3 date)
            else hd months + sum_days(tl months, n - 1)
    in 
        if (#1 date) <= 0
        then false
        else if (#2 date < 1) orelse (#2 date > 12)
            then false
            else number_before_reaching_sum (sum_days(months, (#2 date)), months) = (#2 date)
    end

    