(* week 1 homework solution *)

fun is_older(date1 : (int * int * int), date2 : (int * int * int)) = 
    let
        fun fake_count(date : (int * int * int)) =
           (#1 date) * 365 + (#2 date) * 30 + (#3 date)
    in
        fake_count(date1) < fake_count(date2)
    end

fun number_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then 0
    else 
        let val tl_ans = number_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then 1 + tl_ans
            else tl_ans
        end

fun number_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else 
        let val tl_ans = dates_in_month(tl dates, month)
        in
            if #2 (hd dates) = month
            then (hd dates) :: tl_ans
            else tl_ans
        end

fun dates_in_months(dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth(tl strings, n - 1)

fun date_to_string(date : (int * int * int)) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
    end

fun number_before_reaching_sum(sum : int, numbers : int list) =
    if hd numbers >= sum
    then 0
    else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)

fun what_month(day : int) = 
    let
        val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        number_before_reaching_sum(day, days_of_months) + 1
    end

(* a helper function for month_range *)
fun what_months(days : int list) =
    if null days
    then []
    else what_month(hd days) :: what_months(tl days)

fun month_range(day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
        let fun count_up(from : int, to : int) =
            if from = to
            then [to]
            else from :: count_up(from + 1, to)
        in
            what_months(count_up(day1, day2))
        end

fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else 
        let val tl_ans = oldest(tl dates)
        in
            if isSome(tl_ans) andalso is_older(valOf tl_ans, hd dates)
            then tl_ans
            else SOME(hd dates)
        end

(* challenge problems *)
fun remove_duplicates(items : int list) =
    if null items
    then []
    else
        let fun find(item : int, items : int list) =
            if null items
            then false
            else
                if item = hd items
                then true
                else find(item, tl items)
        in
            if find(hd items, tl items)
            then remove_duplicates(tl items)
            else hd items :: remove_duplicates(tl items)
        end

fun number_in_months_challenge(dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_duplicates(months))

fun dates_in_months_challenge(dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_duplicates(months))

fun is_leap(year : int) =
    if year mod 400 = 0 orelse (year mod 4 = 0 andalso year mod 100 <> 0)
    then true
    else false

fun reasonable_date(date : (int * int * int)) =
    let
        val year = #1 date
        val month = #2 date
        val day = #3 date
        fun get_nth(numbers : int list, n : int) =
            if n = 1
            then hd numbers
            else get_nth(tl numbers, n - 1)
        val days_of_months = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        val days_of_months_leap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
        if year <= 0 orelse month > 12 orelse month < 1 orelse day < 1
        then false
        else
            if is_leap(year)
            then if day > get_nth(days_of_months_leap, month)
                then false
                else true
            else if day > get_nth(days_of_months, month)
                then false
                else true
    end
