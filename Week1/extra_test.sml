(* tests for extra practice problems *)
use "extra_solution.sml";

val test1 = alternate([1, 2, 3, 4]) = ~2

val test2 = min_max([2, 3, 5, ~1]) = (SOME(~1), SOME(5))

val test3 = cumsum([1, 4, 20]) = [1, 5, 25]

val test4 = greeting(SOME("you")) = "Hello there, you"

val test5 = repeat([1, 2, 3], [4, 0, 3]) = [1, 1, 1, 1, 3, 3, 3]

val test6 = addOpt(SOME(2), SOME(3)) = SOME(5)

val test7 = addAllOpt([SOME 1, NONE, SOME 3]) = SOME 4

val test8 = any([false, false, true, false]) = true

val test9 = all([true, true, true, false]) = false

val test10 = zip([1, 2, 3], [4, 6]) = [(1, 4), (2, 6)]

val test11 = zipRecycle([1,2,3], [1, 2, 3, 4, 5, 6, 7]) = [(1,1), (2,2), (3, 3), (1,4), (2,5), (3,6), (1,7)]

val test12 = zipOpt(SOME([1, 2, 3]), SOME([2, 3, 4])) = SOME([(1, 2), (2, 3), (3, 4)])

val test13 = lookup([("1", 1), ("3", 3), ("4", 4)], "2") = NONE

val test14 = splitup([~1, 0, 1]) = ([~1], [0, 1])

val test15 = splitAt([1, 2, 3], 2) = ([1], [2, 3])

val test16 = isSorted([1, 2, 3, 4, 6]) = true

val test17 = isAnySorted([2, 1, 0, ~1]) = true

val test18 = sortedMerge([1,4,7], [5,8,9]) = [1,4,5,7,8,9]

val test19 = qsort([2, 3, 1, 2, ~1, 0]) = [~1,0,1,2,2,3]

val test20 = divide([1, 2, 3, 4, 6]) = ([1,3,6],[2,4])

val test21 = not_so_quick_sort([2, 3, 1, 2, ~1, 0]) = [~1,0,1,2,2,3]

val test22 = fullDivide(2, 40) = (3, 5)
