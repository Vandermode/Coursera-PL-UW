(* test for Week3 lecture *)

datatype mytype = 
    TwoInts of int * int 
    | Str of string
    | Pizza

fun f(x : mytype) =
    case x of
        Pizza => 3
        | TwoInts(i1, i2) => i1 + i2
        | Str(s) => String.size s

datatype exp = 
    Constant of int 
    | Negate of exp
    | Add of exp * exp
    | Multiply of exp * exp

fun old_eval(e : exp) =
    case e of
        Constant i => i
        | Negate e => ~ (old_eval e)
        | Add(e1, e2) => (old_eval e1) + (old_eval e2)
        | Multiply(e1, e2) => (old_eval e1) * (old_eval e2)

fun eval (Constant i) = i
    | eval (Negate e) = ~ (eval e)
    | eval (Add (e1, e2)) = (eval e1) + (eval e2)
    | eval (Multiply (e1, e2)) = (eval e1) * (eval e2)

fun numbers_of_add(e : exp) = 
    case e of
        Constant i => 0
        | Negate e => numbers_of_add e
        | Add(e1, e2) => 1 + numbers_of_add e1 + numbers_of_add e2
        | Multiply(e1, e2) => numbers_of_add e1 + numbers_of_add e2

fun max_constant e =
    let fun max_of_two(e1, e2) =
        let val m1 = max_constant e1
            val m2 = max_constant e2
        in Int.max(m1, m2) end
    in
        case e of
            Constant i => i
            | Negate e => max_constant e
            | Add(e1, e2) => max_of_two(e1, e2)
            | Multiply(e1, e2) => max_of_two(e1, e2)
    end

datatype my_int_list =
    Empty
    | Cons of int * my_int_list

fun append_my_list(xs, ys) =
    case xs of
        Empty => ys
        | Cons(x, xs') => Cons(x, append_my_list(xs', ys))

fun inc_or_zero int_option =
    case int_option of
        NONE => 0
        | SOME i => i + 1

fun sum_list xs =
    case xs of
        [] => 0
        | x :: xs' => x + sum_list(xs')

fun append(xs, ys) =
    case xs of
        [] => ys
        | x :: xs' => x :: append(xs', ys)

datatype 'a my_option =  
    None 
    | Some of 'a

datatype 'a linked_list = 
    EmptyNode
    | LinkedNode of 'a * ('a linked_list)

datatype ('a, 'b) tree = 
    TreeNode of 'a * ('a, 'b) tree * ('a, 'b) tree
    | Leaf of 'b

fun count_tree_node tree =
    case tree of
        Leaf _ => 1
        | TreeNode(_, tree1, tree2) => 
        1 + count_tree_node(tree1) + count_tree_node(tree2)

fun sum_tree tree =
    case tree of
        Leaf i => i
        | TreeNode(i, ltr, rtr) => i + sum_tree(ltr) + sum_tree(rtr)

fun sum_leaf tree =
    case tree of
        Leaf i => i
        | TreeNode(_, ltr, rtr) => sum_leaf(ltr) + sum_leaf(rtr)

fun same_thing(x, y) =
    if x = y then "yes" else "no"

exception ListLengthMismatch

fun zip3 list_triple = 
    case list_triple of
        ([], [], []) => []
        | (hd1 :: tl1, hd2 :: tl2, hd3 :: tl3) => (hd1, hd2, hd3) :: zip3(tl1, tl2, tl3)
        | _ => raise ListLengthMismatch

fun unzip3 triple_list = 
    case triple_list of
         [] => ([], [], [])
         | (a, b, c) :: tail => 
         let val (l1, l2, l3) = unzip3 tail
         in (a :: l1, b :: l2, c :: l3) end
         (* the type of list is fixed, so '| _ =>' is redundant *)

(* int list -> bool *)
fun nondecreasing xs = 
    case xs of
        [] => true
        | _ :: [] => true
        | x1 :: (x2 :: rest) => x2 >= x1 andalso nondecreasing(x2 :: rest)

datatype sgn = P | N | Z

(* int * int -> sgn *)
fun mult_sign(x1, x2) =
    let fun sign(x) = if x = 0 then Z else if x > 0 then P else N
    in
        case (sign x1, sign x2) of
            (_, Z) => Z
            | (Z, _) => Z
            | (P, P) => P
            | (N, N) => P
            | _ => N
    end

fun maxlist (xs, ex) =
    case xs of
        [] => raise ex
        | x :: [] => x
        | x :: xs' => Int.max(x, maxlist(xs', ex))

val test_handle = maxlist([], ListLengthMismatch)
    handle ListLengthMismatch => ~1

fun fact1 n = 
    if n = 0 then 1 else n * fact1 (n - 1)

(* tail recursion *)
(* tail recursion do not need to maintain a series of stack *)
fun fact2 n =
    let fun aux (n, acc) = 
        if n = 0 
        then acc
        else aux (n - 1, acc * n)
    in aux(n, 1)
    end

(* tail recursion version of sum *)
fun sum2 xs =
    let fun tail_sum (xs, acc) =
        case xs of
            [] => acc
            | x :: xs' => tail_sum(xs', acc + x)
    in
        tail_sum(xs, 0)
    end

(* traditional version of reverse function *)
fun rev xs = 
    case xs of
        [] => []
        | x :: xs' => rev (xs') @ [x]

(* tail version of rev *)
fun rev2 xs =
    let fun aux (xs, acc) =
        case xs of
            [] => acc
            | x :: xs' => aux (xs', x :: acc)
    in
        aux (xs, [])
    end
