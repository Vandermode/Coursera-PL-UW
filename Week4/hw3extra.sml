(* extra practice problem contributed by community *)

(* Write functions fold_map and fold_filter that have the same signatures and behavior as List.map and List.filter correspondingly. Use List.foldr. Do not use pattern matching or any other list functions. *)

fun fold_map f = List.foldr (fn (x, acc) => f x :: acc) [] 

fun fold_filter f = List.foldr (fn (x, acc) => if f x then x :: acc else acc) []

(* Write a function unfold that takes a state transition function and an initial state and produces a list. On each step the current state is fed into the state transition function, which evaluates either to NONE, indicating that the result should contain no more elements, or to SOME pair, where pair contains the next state and the next list element. *)

fun unfold f x =
    let fun loop (x, acc) =
        case f x of
            NONE => acc
            | SOME (state, x') => loop (state, acc @ [x'])
    in
        loop (x, [])
    end

(* Write a function factorial that takes an integer number n and evaluates to n!. Your function should be a composition of unfold and List.foldl. You should not use any other list functions, recursion or pattern matching. *)

fun factorial x = List.foldl (fn (x, acc) => x * acc) 1 ((unfold (fn x => if x >= 1 then SOME (x -1, x) else NONE)) x)

val factorial = (List.foldl (op * ) 1) o (unfold (fn x => if x >= 1 then SOME (x -1, x) else NONE))

(* Write a function unfold_map, that behaves exactly as List.map and fold_map, but that would be implemented in terms of unfold. *)

fun unfold_map f = unfold (fn x => if x = [] then NONE else SOME (tl x, f (hd x)))

(* Write a function do_until that takes three arguments, f, p and x, and keeps applying f to x until p x evaluates to true. Upon reaching that condition, f (f (f ... (f x) ...)) is returned. *)

fun do_until f p x =
    case p x of
        true => x
        | false => do_until f p (f x)

(* Write a function imp_factorial that has the same behavior as the factorial function described above, but is defined in terms of do_until. *)

fun imp_factorial n = #1 (do_until (fn (acc, x) => (acc * x, x - 1)) (fn (_, x) => x <= 1) (1, n))

(* Write a function fixed_point that accepts some function f and an initial value x, and keeps applying f to x until an x is found such that f x = x. Note that the function must have the same domain and codomain, and that the values must be comparable for equality. *)

fun fixed_point f = do_until f (fn x => f x = x)

(* Square root of a real number n is a fixed point of function fn(x)=12(x+nx). Unfortunately, for reasons rooted in the arcane art of numerical analysis, reals are not comparable for equality in Standard ML. Write a function my_sqrt that takes a real number and evaluates to an approximation of its square root. You will probably need to write a version of fixed_point that uses "difference in absolute value less than ϵ" as a test for equality. Use ϵ=0.0001. Use the number itself as an initial guess. *)

fun my_sqrt n = 
    let fun fixed_point x = (x + n / x) / 2.0
    in
        do_until fixed_point (fn x => abs (fixed_point x - x) <= 0.0001) n
    end

(* Write functions tree_fold and tree_unfold that would serve as equivalents of fold and unfold on lists for this data structure. *)

datatype 'a tree = leaf | node of { value : 'a, left : 'a tree, right : 'a tree }

fun tree_fold f acc t =
    case t of
        leaf => acc
        | node {value=v, left=l, right=r} =>
            f (tree_fold f acc l, v, tree_fold f acc r)

fun tree_unfold f x = 
    case f x of
        NONE => leaf
        | SOME (x', v, x'') => node {value=v, left=tree_unfold f x', right=tree_unfold f x''}

(* Let's try to write a simple type inference algorithm for a very simple expression language. We won't deal with functions, variables or polymorphism. *)

datatype expr = literal_bool | literal_int |
    binary_bool_op of expr * expr | binary_int_op of expr * expr |
    comparison of expr * expr | conditional of expr * expr * expr

datatype expr_type = type_bool | type_int

exception TypeError

fun infer_type e =
    case e of
        literal_bool => type_bool
        | literal_int => type_int
        | binary_bool_op (e1, e2) => 
        if infer_type e1 = type_bool andalso infer_type e2 = type_bool 
        then type_bool 
        else raise TypeError
        | binary_int_op (e1, e2) => 
        if infer_type e1 = type_int andalso infer_type e2 = type_int 
        then type_int 
        else raise TypeError
        | comparison (e1, e2) =>
        if infer_type e1 = type_int andalso infer_type e2 = type_int
        then type_bool
        else raise TypeError
        | conditional (e1, e2, e3) =>
        if infer_type e1 = type_bool andalso infer_type e2 = infer_type e3
        then infer_type e2
        else raise TypeError
