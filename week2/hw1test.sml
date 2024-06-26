(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)
use "hw1.sml";


val test1 = is_older ((1,2,3),(2,3,4)) = true
val test1a = is_older ((2000, 9, 30),(2000, 9, 30)) = false
val test1b = is_older ((2000, 9, 29),(2000, 9, 30)) = true
val test1c = is_older ((2000, 9, 31),(2000, 9, 30)) = false

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2a = number_in_month ([],2) = 0
val test2b = number_in_month ([(2012,2,28),(2013,12,1),(2013,2,1)],2) = 2
val test2c = number_in_month ([(2012,12,28),(2013,12,1),(2013,12,1)],2) = 0

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3a = number_in_months ([(2012,1,28),(2013,12,1),(2011,1,31),(2011,5,28)],[2,3,4]) = 0
val test3b = number_in_months ([],[2,3,4]) = 0
val test3c = number_in_months ([(2012,1,28),(2013,12,1),(2011,1,31),(2011,5,28)],[]) = 0

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4a = dates_in_month ([(2012,2,28),(2013,2,2)],2) = [(2012,2,28),(2013,2,2)]
val test4b = dates_in_month ([(2012,4,28),(2013,12,2)],2) = []
val test4c = dates_in_month ([],2) = []
val test4d = dates_in_month ([(2012,2,28),(2000, 4, 30),(2013,2,2)],2) = [(2012,2,28),(2013,2,2)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5a = dates_in_months ([(2012,0,28),(2013,12,1),(2011,0,31),(2011,0,28)],[2,3,4]) = []
val test5b = dates_in_months ([(2013,12,1),(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28),(2013,12,1)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6a = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"
val test6b = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"
val test7a = date_to_string (2013, 1, 20) = "January 20, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8a = number_before_reaching_sum (11, [1,2,3,4,5]) = 4
val test8b = number_before_reaching_sum (9, [1,2,3,4,5]) = 3
val test8c = number_before_reaching_sum (1, [1,2,3,4,5]) = 0

val test9 = what_month 70 = 3
val test9a = what_month 365 = 12
val test9b = what_month 10 = 1
val test9c = what_month 59 = 2
val test9d = what_month 60 = 3

val test10 = month_range (31, 34) = [1,2,2,2]
val test10a = month_range (30, 34) = [1,1,2,2,2]
val test10b = month_range (35, 34) = []
val test10c = month_range (34, 34) = [2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11a = oldest([]) = NONE
val test11b = oldest([(2012,2,28),(2011,4,28)]) = SOME (2011,4,28)
val test11c = oldest([(2011,3,31),(2012,2,28),(2011,4,28)]) = SOME (2011,3,31)

val test12 = number_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2,3,4,4,3]) = 3
val test12a = number_in_months_challenge ([(2012,1,28),(2013,12,1),(2011,1,31),(2011,5,28)],[2,3,4,2]) = 0
val test12b = number_in_months_challenge ([],[2,3,4]) = 0
val test12c = number_in_months_challenge ([(2012,1,28),(2013,12,1),(2011,1,31),(2011,5,28)],[]) = 0
val test12d = number_in_months_challenge ([(2012,1,28),(2013,12,1),(2011,1,31),(2011,5,28)],[2,3,4]) = 0

val test13 = dates_in_months_challenge ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4,2,3]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test13a = dates_in_months_challenge ([(2012,0,28),(2013,12,1),(2011,0,31),(2011,0,28)],[2,3,4,4,4,4,4]) = []
val test13b = dates_in_months_challenge ([(2013,12,1),(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28),(2013,12,1)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test13c = dates_in_months_challenge ([(2013,12,1),(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28),(2013,12,1)],[3,3,4,3,2,2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test14 = reasonable_date(1,2,3) = true
val test14a = reasonable_date(~5,2,23) = false
val test14b = reasonable_date(0,2,23) = false
val test14c = reasonable_date(2000,0,23) = false
val test14d = reasonable_date(2000,13,23) = false
val test14e = reasonable_date(2000,12,23) = true
val test14f = reasonable_date(2000,2,29) = true
val test14g = reasonable_date(2001,2,29) = false
val test14h = reasonable_date(2001,2,28) = true
val test14i = reasonable_date(2001,~2,28) = false
val test14j = reasonable_date(2001,2,0) = false
val test14k = reasonable_date(2001,2,~1) = false
val test14l = reasonable_date(2024,7,5) = true
val test14m = reasonable_date(2024,7,31) = true
val test14n = reasonable_date(2024,6,31) = false