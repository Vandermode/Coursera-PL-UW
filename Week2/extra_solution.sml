(* solution for extra practice problems -- Week1 *)

(* helper functions *)
fun max(xs : int list) =
    if null xs
    then NONE
    else 
        let fun max_nonempty(xs : int list) =
            if null(tl xs) then hd xs
            else 
                let val tl_ans = max_nonempty(tl xs)
                in Int.max(hd xs, tl_ans) 
                end
        in SOME(max_nonempty xs) 
        end

fun oppsite(xs : int list) =
    if null xs then [] else ~(hd xs) :: oppsite(tl xs)

fun min(xs : int list) =
    let val result = max(oppsite(xs))
    in if isSome(result) then SOME(~(valOf result)) else NONE
    end

fun sum(xs : int list) =
    if null xs
    then 0
    else hd xs + sum(tl xs)

fun reverse(xs : int list) =
    if null xs then []
    else reverse(tl xs) @ [hd xs]

fun list_plus(xs : int list, factor: int) =
    if null xs then []
    else (hd xs + factor) :: list_plus(tl xs, factor)

fun len(xs : int list) =
    if null xs then 0 else 1 + len(tl xs)

fun recycle(xs : int list, t : int) = 
    if t = 1
    then xs
    else xs @ recycle(xs, t - 1)

(* test for helper functions *)

val test_max = max([]) = NONE
val test_min1 = min([]) = NONE
val test_min2 = min([3, 2, 5, 4]) = SOME(2)
val test_sum = sum([1, 2, ~3]) = 0
val test_reverse = reverse([2, 3, 1]) = [1, 3, 2]
val test_list_plus = list_plus([1, 2, 0], 2) = [3, 4, 2]
val test_len = len([1, 2, 3, 4]) = 4
val test_recycle = recycle([1, 2, 3], 2) = [1, 2, 3, 1, 2, 3] 

(* practice problems *)

fun alternate(numbers : int list) =
    if null numbers
    then 0
    else hd numbers - alternate(tl numbers)

fun min_max(numbers : int list) =
    (min(numbers), max(numbers))

fun cumsum(numbers : int list) =
    if null numbers then []
    else hd numbers ::  list_plus(cumsum(tl numbers), hd numbers)

fun greeting(name : string option) =
    if isSome(name) then "Hello there, " ^ valOf(name)
    else "Hello there, you" 

fun repeat(numbers : int list, repeat_times : int list) =
    if null numbers then []
    else
        let fun repeat_for_one(number : int, repeat_time : int) =
            if repeat_time = 0 then [] 
            else number :: repeat_for_one(number, repeat_time - 1)

        in repeat_for_one(hd numbers, hd repeat_times) @ repeat(tl numbers, tl repeat_times)
        end

fun addOpt(opt_num1 : int option, opt_num2 : int option) =
    if isSome(opt_num1) andalso isSome(opt_num2)
    then SOME(valOf(opt_num1) + valOf(opt_num2))
    else NONE

fun addAllOpt(opt_nums : int option list) =
    if null opt_nums then SOME(0)
    else
        if isSome(hd opt_nums)
        then addOpt(hd opt_nums, addAllOpt(tl opt_nums))
        else addAllOpt(tl opt_nums)

fun any(bools : bool list) =
    if null bools 
    then false
    else if hd bools
        then true
        else any(tl bools)

fun all(bools : bool list) =
    if null bools 
    then true
    else if hd bools
        then all(tl bools)
        else false 

fun zip(nums1 : int list, nums2 : int list) =
    if null nums1 orelse null nums2
    then []
    else (hd nums1, hd nums2) :: zip(tl nums1, tl nums2)

fun zipRecycle(nums1 : int list, nums2 : int list) =
    let val l1 = len(nums1)
        val l2 = len(nums2)
    in if l1 > l2
        then zip(nums1, recycle(nums2, (l1 div l2) + 1)) 
        else zip(recycle(nums1, (l2 div l1) + 1), nums2)
    end

fun zipOpt(nums_opt1 : int list option, nums_opt2 : int list option) =
    if isSome(nums_opt1) andalso isSome(nums_opt2) 
    andalso len(valOf(nums_opt1)) = len(valOf(nums_opt2))
    then SOME(zip(valOf(nums_opt1), valOf(nums_opt2)))
    else NONE

fun lookup(items : (string * int) list, key : string) =
    if null items
    then NONE
    else
        if #1 (hd items) = key
        then SOME(#2 (hd items))
        else lookup(tl items, key)

fun splitAt(numbers : int list, threshold : int) =
    if null numbers
    then ([], [])
    else 
        let val items = splitAt(tl numbers, threshold)
        in if hd numbers < threshold
            then (hd numbers :: #1 items, #2 items) 
            else (#1 items, hd numbers :: #2 items)
        end

fun splitup(numbers : int list) =
    splitAt(numbers, 0)

fun isSorted(numbers : int list) =
    if null(tl numbers) orelse null numbers then true
    else (hd numbers < hd(tl numbers)) andalso isSorted(tl numbers)

fun isAnySorted(numbers : int list) =
    isSorted(numbers) orelse isSorted(reverse(numbers))

fun sortedMerge(nums1 : int list, nums2 : int list) =
    if null nums1 then nums2
    else if null nums2 then nums1
    else if hd nums1 < hd nums2
        then hd nums1 :: sortedMerge(tl nums1, nums2)
        else hd nums2 :: sortedMerge(nums1, tl nums2)

fun qsort(numbers : int list) =
    if null numbers then []
    else 
        let val threshold = hd numbers
            val split_items = splitAt(tl numbers, threshold) 
        in qsort(#1 split_items) @ [threshold] @ qsort(#2 split_items)
        end

fun divide(numbers : int list) =
    let fun divide_flag(numbers : int list, flag : bool) =
        if null numbers
        then ([], [])
        else if flag
            then 
                let val items = divide_flag(tl numbers, false)
                in (hd numbers :: #1 items, #2 items) end
            else 
                let val items = divide_flag(tl numbers, true)
                in (#1 items, hd numbers :: #2 items) end
    in divide_flag(numbers, true)
    end

fun not_so_quick_sort(numbers : int list) =
    if null numbers
    then []
    else if null (tl numbers)
        then [hd numbers]
        else 
            let val items = divide(numbers)
            in sortedMerge(not_so_quick_sort(#1 items), not_so_quick_sort(#2 items))
            end

fun fullDivide(k : int, n : int) =
    if n mod k <> 0
    then (0, n)
    else 
        let val items = fullDivide(k, n div k)
        in (1 + #1 items, #2 items)
        end
