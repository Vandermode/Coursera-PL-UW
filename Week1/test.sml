(* This is a comment. This is our first program.*)

val x = 34;

val y = 17;

val z = (x + y) + (y + 2);

val y = 18;

val q = z + 1;

val abs_of_z = if z < 0 then 0 - z else z;

val abs_of_z_simpler = abs z;

fun pow(x : int, exp : int) = 
    if exp = 0
    then 1
    else x * pow(x, exp - 1)

fun cube(x : int) = 
    pow(x, 3)

fun swap(pr : int * bool) = 
    (#2 pr, #1 pr)

fun sum_two_pairs(pr1 : int * int, pr2 : int * int) = 
    (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun div_mod(x : int, y : int) = 
    (x div y, x mod y)

fun sort_pair(pr : int * int) = 
    if (#1 pr) < (#2 pr)
    then pr
    else (#2 pr, #1 pr)

fun sum_list(xs : int list) =
    if null xs
    then 0
    else hd(xs) + sum_list(tl(xs))

fun list_product(xs : int list) =
    if null xs
    then 0
    else
        if null (tl xs)
        then hd xs
        else hd xs * list_product(tl xs)

fun countdown(x : int)  =
    if x = 0
    then []
    else x :: countdown(x - 1)

fun append(xs : int list, ys : int list) = 
    if null xs
    then ys
    else hd xs :: append(tl xs, ys)

fun sum_pair_list(xs : (int * int) list) = 
    if null xs
    then 0
    else #1 (hd xs) + #2 (hd xs) + sum_pair_list(tl xs)

fun firsts(xs : (int * int) list) = 
    if null xs
    then []
    else #1 (hd xs) :: firsts(tl xs)

fun seconds(xs : (int * int) list) =
    if null xs
    then []
    else #2 (hd xs) :: seconds(tl xs)

fun sum_pair_list2(xs : (int * int) list) = 
    sum_list(firsts xs) + sum_list(seconds xs)

fun factorial(n : int) = 
    if n = 0
    then 1
    else n * factorial(n -1)

fun factorial2(n : int) = 
    list_product(countdown(n))    

fun silly() = 
    let
        val x = 1
    in
        (let val x = 2 in x + 1 end) + (let val y = x + 2 in y + 1 end)
    end

fun count(from : int, to : int) = 
    if from = to
    then to :: []
    else from :: count(from + 1, to)

fun countup_from1(x : int) =
    let
        fun count(from : int) = 
            if from = x
            then x :: []
            else from :: count(from + 1)
    in
        count(1)   
    end

fun max_bad(xs : int list) =
    if null xs
    then 0
    else if null(tl xs)
        then hd xs
        else if hd xs > max_bad(tl xs)
            then hd xs
            else max_bad(tl xs)

(* return [from, from + 1, ... to]*)
fun count_up(from : int, to : int) = 
    if from = to
    then [to]
    else from :: count_up(from + 1, to)

(* return [from, from - 1, ... to]*)
fun count_down(from : int, to : int) =
    if from = to
    then [to]
    else from :: count_down(from - 1, to)

fun good_max(xs : int list) =
    if null xs
    then 0
    else
        let val tl_ans = good_max(tl xs)
        in
            if hd xs > tl_ans
            then hd xs
            else tl_ans
        end

(* fn : int list -> int option*)
fun max1(xs : int list) = 
    if null xs
    then NONE
    else let
        val tl_ans = max1(tl xs)
    in
        if isSome tl_ans andalso valOf tl_ans > hd xs
        then tl_ans
        else SOME(hd xs)
    end

fun max2(xs : int list) =
    if null xs
    then NONE
    else let
        fun max_nonempty(xs : int list) =
            if null(tl xs)
            then hd xs
            else 
                let val tl_ans = max_nonempty(tl xs)
                in
                    if hd xs > tl_ans
                    then hd xs
                    else tl_ans
                end
        in
            SOME(max_nonempty xs)
    end
