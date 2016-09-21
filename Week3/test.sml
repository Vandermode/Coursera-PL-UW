(* practice during class *)

fun double x = x + x

fun increment x = x + 1

(* first-order function *)
fun n_times (f, n, x) =
    if n = 0
    then x
    else f (n_times(f, n - 1, x))

val x1 = n_times (double, 4, 7)
val x2 = n_times (increment, 4, 7)
val x3 = n_times (tl, 2, [4, 8, 12, 16])

fun addition (n, x) = n_times (increment, n, x)

fun double_n_times (n, x) = n_times (double, n, x)

fun nth_tail (n, x) = n_times (tl, n, x)

fun times_until_zero (f, x) = 
    let fun aux (x, n) = 
        if x = 0 then 0 else aux (f x, n + 1)
    in
        aux (x, 0)
    end

(* Anonymous function *)
fun triple_n_times (n, x) = 
    n_times ((fn x => 3 * x), n, x)

(* fun binding is syntactic sugar *)
(* fun triple x = x * 3 *)
(* val triple = fn x => 3 * x *)

val rev = List.rev

(* Map and Filter *)
fun map (f, xs) =
    let fun aux ([], done) = done
        | aux (x :: xs', done) = aux (xs', done @ [f x])
    in
        aux (xs, [])
    end

val x4 = map ((fn x => x + 2), [4, 8, 12, 16])

val x5 = map (hd, [[1, 2], [3, 4]])

fun filter (f, xs) =
    let fun aux ([], done) = done
        | aux (x :: xs', done) = if f x then aux (xs', done @ [x]) else aux (xs', done)
    in
        aux (xs, [])
    end

fun all_even xs = filter (fn x => x mod 2 = 0, xs)

datatype exp =
    Constant of int
    | Negate of exp
    | Add of exp * exp
    | Multiply of exp * exp

fun true_all_constants (f, Constant i) =  f i
    | true_all_constants (f, Negate e) = true_all_constants (f, e)
    | true_all_constants (f, Add (e1, e2)) = true_all_constants (f, e1) andalso true_all_constants (f, e2)
    | true_all_constants (f, Multiply (e1, e2)) = true_all_constants (f, e1) andalso true_all_constants (f, e2)

fun all_even_constants e = true_all_constants (fn x => x mod 2 = 0, e)

(* lexical scope *)
fun greaterThanx x = fn y => y > x

fun noNegatives xs = filter (greaterThanx ~1, xs)

fun allGreater (xs, n) = filter (fn x => x > n, xs)

fun allShorterThan1 (xs, s) =
    filter (fn x => String.size x < (print "!"; String.size s), xs)

fun allShorterThan2 (xs, s) =
    let val i = (print "!"; String.size s)
    in
        filter (fn x => String.size x < i, xs)
    end

(* acc : accumulator *)
(* also called reduce *)
fun fold (f, acc, []) = acc
    | fold (f, acc, x :: xs) = fold (f, f (acc, x), xs)

fun sum xs = fold (fn (x, y) => x + y, 0, xs)

fun all xs = fold (fn (x, y) => x andalso y >= 0, true, xs)

fun any xs = fold (fn (x, y) => x orelse y >= 0, false, xs)

fun range_count (xs, lo, hi) = fold (fn (x, y) => x + (if y >= lo andalso y <= hi then 1 else 0), 0, xs)

fun is_all_shorter1 (ss, s) =
    let val i = String.size s
    in
        fold (fn (x, y) => x andalso String.size y < i, true, ss)
    end

fun all_true (g, xs) =
    fold (fn (x, y) => x andalso g y, true, xs)

fun is_all_shorter2 (ss, s) = 
    let val i = String.size s
    in
        all_true (fn s => String.size s < i, ss)
    end

(* closure idioms: Combining Functions *)
fun compose (f, g) = 
    fn x => f (g x)

fun sqrt_of_abs1 i = Math.sqrt (Real.fromInt (abs i))

val sqrt_of_abs2 = (Math.sqrt o Real.fromInt o abs) 

infix !>

fun x !> y = y x

fun sqrt_of_abs3 i = i !> abs !> Real.fromInt !> Math.sqrt

fun backup1 (f, g) = 
    fn x =>
    case f x of
        NONE => g x
        | SOME i => i

fun backup2 (f, g) = fn x => f x handle _ => g x

(* closure idiom: curring *)
fun sorted3_tuple (x, y, z) = z >= y andalso y >= x

val sorted3 = fn x => fn y => fn z => z >= y andalso y >= x

val t = sorted3 3 4 5 = ((sorted3 3) 4) 5

fun sorted3_nicer x y z = z >= y andalso y >= x

fun fold_curring f acc xs = 
    case xs of
        [] => acc
        | x :: xs' => fold_curring f (f (acc, x)) xs'

val is_nonnegative = sorted3 0 0 

val sum = fold_curring (fn (x, y) => x + y) 0 

fun extend_range i j k = if i > j then k else extend_range (i + 1) j (k @ [i])

fun range i j = extend_range i j []

val countup = range 1

fun exists predicate [] = false
    | exists predicate (x :: xs) =
        predicate x orelse exists predicate xs

val has_zero = exists (fn x => x = 0)

val increment_all = List.map (fn x => x + 1)

val remove_zero = List.filter (fn x => x <> 0)

(* val valueRestriction = List.map (fn x => (x, 1)) *)

fun workaround xs = List.map (fn x => (x, 1)) xs

(* curring wrapup *)
fun curry f x y = f (x, y)

fun uncurry f (x, y) = f x y

val range_uncurry = uncurry range

val test_curry = curry range_uncurry 1 10 = range 1 10

(* callback *)
val cbs : (int -> unit) list ref = ref []

fun onKeyEvent f = cbs := f :: (!cbs)

fun onEvent i =
    let fun loop fs =
        case fs of
            [] => ()
            | f :: fs' => (f i; loop fs')
    in loop (!cbs) end

val timePressed = ref 0
val _ = onKeyEvent (fn _ => timePressed := (!timePressed) + 1)

fun printIfPressed i =
    onKeyEvent (fn j =>
        if i = j
        then print ("you pressed " ^ Int.toString i ^ "\n")
        else ())

val _ = printIfPressed 1
val _ = printIfPressed 3
val _ = printIfPressed 5

