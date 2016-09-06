(* problem 1 *)
fun is_older (date1: int*int*int, date2: int*int*int) =
  if (#1 date1) < (#1 date2)
  then true
  else if (#1 date1) > (#1 date2)
       then false
       else if (#2 date1) < (#2 date2)
            then true
            else if (#2 date1) > (#2 date2)
                  then false
                  else if (#3 date1) < (#3 date2)
                  then true
                  else false
(* problem 2 *)
fun number_in_month1 ( dates : (int*int*int) list, x : int, z : int) = 
    if null dates
    then z
    else if x = #2 (hd dates)
         then number_in_month1 (tl dates, x, z+1)
         else number_in_month1(tl dates, x, z)
         
(* main *)    
fun number_in_month (dates : (int*int*int) list, x :int) = 
    number_in_month1 (dates, x, 0)

(* problem 3 *)
fun number_in_months1 (dates : (int*int*int) list, x : int list, z : int) = 
    if null x
    then z
    else number_in_month1(dates, hd x, z) + number_in_months1(dates, tl x, z)
    
(* main *)    
fun number_in_months (dates : (int*int*int) list, x : int list) = 
    number_in_months1 (dates, x, 0)


(* problem 4 *)
fun dates_in_month1 (dates : (int*int*int) list, month : int, match_dates : (int*int*int) list) = 
    if null dates 
    then match_dates
    else if month = #2 (hd dates)
         then dates_in_month1 (tl dates, month, (hd dates)::match_dates)
         else   dates_in_month1 (tl dates, month, match_dates)
         
fun reverse (x : (int*int*int) list, y : (int*int*int) list) = 
        if null x
        then y
        else reverse (tl x, (hd x)::y)
        
(* main *)
fun dates_in_month (dates : (int*int*int) list, month :int) = 
    reverse (dates_in_month1(dates, month, []), [])
    
(* problem 5 *)   
fun dates_in_months1 (dates : (int*int*int) list, months : int list, match_dates : (int*int*int) list) = 
    if null months
    then match_dates
    else dates_in_months1(dates, tl months, dates_in_month(dates, hd months)@match_dates)
    
(* main *)
fun dates_in_months (dates : (int*int*int) list, months : int list) = 
    reverse (dates_in_months1(dates, months,[]), [])

(* problem 6 *)
fun get_nth (string_list : string list, n : int) = 
    if n = 1
    then hd string_list
    else get_nth (tl string_list, n-1)

(* problem 7 *)
fun date_to_string (date : (int*int*int)) = 
    get_nth ( ["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"], #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)

(* problem 8 *)
fun number_before_reaching_sum1 (sum : int, n : int list, y : int) = 
    if sum-(hd n) <= 0
    then y
    else number_before_reaching_sum1(sum - (hd n), tl n, y+1)

(* main *)
fun number_before_reaching_sum (sum : int, n : int list) = 
    number_before_reaching_sum1(sum, n, 0)

(* problem 9 *)
fun what_month (n : int) = 
    number_before_reaching_sum (n, [31,28,31,30,31,30,31,31,30,31,30,31]) + 1
    
(* problem 10 *)
fun month_range1 (a : int, b : int, c : int list) =
    if b-a < 0
    then c
    else month_range1 (a+1, b, what_month(a)::c)
    
fun reverse1 (x : int list, y : int list) = 
        if null x
        then y
        else reverse1 (tl x, (hd x)::y)

(* main *)    
fun month_range (a : int, b :int) = 
    reverse1 (month_range1(a ,b, []),[])

(* problem 11 *)
fun older (date1: int*int*int, date2: int*int*int) = 
    if is_older (date1, date2)
    then date1
    else date2
    
    
fun oldest1 (dates : (int*int*int) list, date : (int*int*int)) = 
    if null dates
    then SOME (date)
    else if date = (0,0,0)
         then oldest1 (tl dates, older((hd dates), hd dates))
         else oldest1 (tl dates, older((hd dates), date))
         
(* main *)        
fun oldest (dates : (int*int*int) list) = 
    if null dates
    then NONE
    else oldest1 (dates, (0,0,0))
    
    
(* problem 12 *)   
fun is_duplicates (x : int, list1 : int list) = 
    if null list1
    then true
    else if x <> hd list1
         then is_duplicates (x, tl list1)
         else false 
    
fun remove_duplicates (list1 : int list, list2 : int list) =
    if null list1 
    then list2
    else if is_duplicates (hd list1, tl list1)
         then remove_duplicates (tl list1, (hd list1)::list2)
         else remove_duplicates (tl list1, list2)
         
(* main *)
fun number_in_months_challenge (dates : (int*int*int) list, x : int list) = 
    number_in_months (dates, reverse1 (remove_duplicates (x,[]),[]))
    
    
fun dates_in_months_challenge(dates : (int*int*int) list, months :int list) = 
    dates_in_months (dates, reverse1 (remove_duplicates (months,[]),[]))   
    
    
(* problem 13 *)
fun is_leap_year (year : int) = 
    if (year mod 4 = 0 andalso year mod 100 <> 0) orelse (year mod 400 = 0)
    then true 
    else false

fun day_in_month (month : int, day_in_months) = 
    if month = 1
    then hd day_in_months
    else day_in_month(month - 1, tl day_in_months)    
    
(* main *)    
fun reasonable_date (date : (int*int*int)) = 
    if (#1 date) > 0
    then if (#2 date) < 13 andalso (#2 date) > 0
         then if is_leap_year (#1 date)
              then if (#3 date) <= day_in_month ((#2 date), [31,29,31,30,31,30,31,31,30,31,30,31]) andalso (#3 date) > 0
                    then true
                    else false
               else if (#3 date) <= day_in_month ((#2 date), [31,28,31,30,31,30,31,31,30,31,30,31]) andalso (#3 date) > 0
                    then true
                    else false
         else false
    else false
