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

    