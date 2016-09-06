
fun is_older (a : int * int * int, b : int * int * int) =
  (* compare year *)
  if (#1 a) < (#1 b)
  then true
  else if (#1 a) > (#1 b)
       then false
       (* compare month *)
       else if (#2 a) < (#2 b)
            then true
            else if (#2 a) > (#2 b)
                 then false
     (* compare day *)
                 else if (#3 a) < (#3 b)
                      then true
                      else false


fun number_in_month (dates: (int * int * int) list, month : int) =
  if null dates
  then 0
  else      
      let
    fun isMonth(date: int*int*int)=
      if (#2 date) = month
      then 1
      else 0
      in
    if null (tl dates)
    then isMonth(hd dates)
    else isMonth(hd dates) + number_in_month(tl dates, month)
                
      end


fun number_in_months (dates: (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)


fun dates_in_month (dates: (int * int * int) list, month : int) =
  if null dates
  then []
  else
      let
    fun hasMonth(date : int * int * int) =
      (#2 date) = month
          
      in
    if null (tl dates)
    then if hasMonth(hd dates)
         then [hd dates]
         else []
      
    else if hasMonth(hd dates)
          then (hd dates) :: dates_in_month(tl dates, month)
    else dates_in_month(tl dates, month)
      end


fun dates_in_months (dates: (int * int * int) list, months : int list) =
  if null months
  then []
  else
      if null dates
      then []
      else    
    let
        val has = dates_in_month(dates, hd months)
        val next = dates_in_months(dates, tl months)
          
    in
        if null has
        then next
        else has@next
    end

fun get_nth (dates : string list, n: int) =
  if n = 1
  then hd dates
  else get_nth(tl dates, n - 1)


fun date_to_string (date:(int * int * int)) =
  let val MONTHS = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(MONTHS, (#2 date)) ^ " " ^ Int.toString((#3 date)) ^ ", " ^ Int.toString((#1 date))
  end


fun number_before_reaching_sum (sum: int, num: int list) =
  let fun nth_before_sum(sum: int, num: int list, nth: int) =
  if sum <= 0
  then nth - 1
  else nth_before_sum(sum - (hd num), tl num, nth + 1)
  in
      nth_before_sum(sum, num, 0)
  end


fun what_month (day: int) =
  let val NUM_DAYS = [31,28,31,30,31,30,31,31,30,31,30,31]
  in
      number_before_reaching_sum(day, NUM_DAYS)+1
  end
      

fun month_range (day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)
      

fun oldest (dates: (int * int * int) list) =
  if null dates
  then NONE
  else
      let
    fun oldest_nonempty (dates: (int * int * int) list) =
      if null (tl dates)
      then hd dates
      else
    let val tl_oldest = oldest_nonempty(tl dates)
    in
        if is_older ( hd dates, tl_oldest)
        then hd dates
        else tl_oldest
    end
      in
    SOME (oldest_nonempty(dates))
      end

    

fun number_in_months_challenge (dates: (int * int * int) list, months : int list) =
  let
      fun notIn(i:int, uni: int list) =
  if null uni
  then true
  else
      if i = (hd uni)
      then false
      else true andalso notIn(i, tl uni)
    
      fun unique(x: int list) =
  if null x
  then []
  else let val uni = unique(tl x)
       in
     if notIn(hd x, uni)
     then (hd x)::uni
     else uni
       end
     
       
  in
      let val unique_month = unique(months)
      in number_in_months(dates, unique_month)
      end          
  end
      
           
         

fun dates_in_months_challenge (dates: (int * int * int) list, months : int list) =
  let
      fun notIn(i:int, uni: int list) =
  if null uni
  then true
  else
      if i = (hd uni)
      then false
      else true andalso notIn(i, tl uni)
    
      fun unique(x: int list) =
  if null x
  then []
  else let val uni = unique(tl x)
       in
     if notIn(hd x, uni)
     then (hd x)::uni
     else uni
       end
     
       
  in
      let val unique_month = unique(months)
      in dates_in_months(dates, unique_month)
      end          
  end



fun reasonable_date(date: (int * int * int)) =
  (* check positive number and month range *)
  if (#1 date) > 0 andalso (#2 date) > 0 andalso (#3 date) > 0 andalso (#2 date) < 13
  then
      let
    fun get_nth_int (days : int list, n: int) =
      if n = 1
      then hd days
      else get_nth_int(tl days, n - 1)

    val NUM_DAYS = [32,29,32,31,32,31,32,32,31,32,31,32]
    val max_days = get_nth_int(NUM_DAYS, (#2 date))
    fun isLeap(year: int) =
      (year mod 400) = 0 orelse ((year mod 4) = 0 andalso not((year mod 100) = 0))
      in
    if (#2 date) = 2
    (* test if day max out for Feb *)
    then
        if isLeap((#1 date))
        then (#3 date) < (max_days + 1) 
        else (#3 date) < max_days
      
    (* test if day max out for other month *) 
    else (#3 date) < max_days
        
      end      
      
  else false
      