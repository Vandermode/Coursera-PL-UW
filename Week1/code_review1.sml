type date = int * int * int


(* helpers *)

fun days_per_month() =
  [31, 28, 31,
   30, 31, 30,
   31, 31, 30,
   31, 30, 31]

fun leap_day(d : date) =
  let
    val y = #1 d
    val m = #2 d
    val is_leap_year = not (y mod 100 = 0) andalso (y mod 4 = 0 orelse y mod 400 = 0)
  in
    if m >= 2 andalso is_leap_year then 1 else 0
  end

fun sum(xs : int list) =
  if null xs
  then 0
  else hd xs + sum(tl xs)

fun take(xs: 'a list, n : int) =
  if n = 0
  then []
  else hd xs::take(tl xs, n - 1)

(* homework *)

fun is_older(a : date, b : date) =
  let
    fun months_to_days(d: date) =
      sum(take(days_per_month(), #2 d - 1)) + leap_day(d)
    fun date_to_days(d: date) =
      #1 d * 365 + months_to_days(d) + #3 d
  in
    date_to_days(a) < date_to_days(b)
  end

fun number_in_month(ds : date list, m : int) =
  if null ds
  then 0
  else number_in_month(tl ds, m) + (if #2 (hd ds) = m then 1 else 0)

fun number_in_months(ds : date list, ms : int list) =
  if null ms
  then 0
  else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

fun dates_in_month(ds : date list, m : int) =
  if null ds
  then []
  else (if #2 (hd ds) = m then [hd ds] else []) @ dates_in_month(tl ds, m)

fun dates_in_months(ds : date list, ms : int list) =
  if null ms
  then []
  else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

fun get_nth(xs : 'a list, n : int) =
  if n = 1
  then hd xs
  else get_nth(tl xs, n - 1)

fun date_to_string(d : date) =
  let
    fun month_to_string(m : int) =
      get_nth(["January", "February", "March",
               "April",   "May",      "June",
               "July",    "August",   "September",
               "October", "November", "December"], m)
  in
    month_to_string(#2 d) ^ " " ^ Int.toString(#3 d) ^ ", " ^ Int.toString(#1 d)
  end

fun number_before_reaching_sum(sum : int, xs : int list) =
  if sum <= 0
  then ~1
  else 1 + number_before_reaching_sum(sum - hd xs, tl xs)

fun what_month(d : int) =
  1 + number_before_reaching_sum(d, days_per_month())

fun month_range(a : int, b : int) =
  if a > b
  then []
  else what_month a :: month_range(a + 1, b)

fun oldest(ds : date list) =
  if null ds
  then NONE
  else
    let
      val tl_oldest = oldest (tl ds)
    in
      if isSome tl_oldest andalso is_older(valOf tl_oldest, hd ds)
      then tl_oldest
      else SOME (hd ds)
    end


(* challenge problems *)

fun contains(xs : int list, x : int) =
  if null xs
  then false
  else if hd xs = x then true else contains(tl xs, x)

fun uniq(xs : int list) =
  if null xs
  then []
  else
    let
      fun iter(ys : int list, zs : int list) =
        if null zs
        then ys
        else iter(if contains(ys, hd zs)
          then ys
          else ys @ [hd zs], tl zs)
    in
      iter([], xs)
    end

fun number_in_months_challenge(ds : date list, ms : int list) =
  number_in_months(ds, uniq ms)

fun dates_in_months_challenge(ds : date list, ms : int list) =
  dates_in_months(ds, uniq ms)

fun reasonable_date(d : date) =
  let
    val year  = #1 d
    val month = #2 d
    val day   = #3 d

    fun between(a : int, b : int, x : int) =
      x >= a andalso x <= b

    fun days_for_month() =
      get_nth(days_per_month(), month) + leap_day(d)
  in
    year > 0 andalso between(1, 12, month) andalso between(1, days_for_month(), day)
  end