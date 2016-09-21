(* Here are some extra programming problems that can be done using the material in this module. 
    Many are similar in difficulty and content to the homework, but they are not the homework, 
    so you are free to discuss solutions, etc. on the discussion forum. 
    Thanks to Pavel Lepin and Charilaos Skiadas for contributing most of these. *)

type student_id = int
type grade = int (* must be in 0 to 100 range *)
type final_grade = { id : student_id, grade : grade option }
datatype pass_fail = pass | fail

fun length_of_a_list (lst) = 
    let fun aux ([], len) = len
        | aux (_ :: others, len) = aux (others, len + 1)
    in
        aux (lst, 0)
    end

fun pass_or_fail ({grade=SOME i, id=_}) = if i >= 75 then pass else fail
    | pass_or_fail(_) = fail

fun has_passed (final_grade) = 
    case pass_or_fail (final_grade) of
        pass => true
        | fail => false

fun number_passed (final_grades) =
    let fun aux ([], n) = n
        | aux (grade :: others, n) = if has_passed grade then aux (others, n + 1) else aux (others, n)
    in
        aux (final_grades, 0)
    end 

fun group_by_outcome (final_grades) = 
    let fun aux ([], [], []) = []
        | aux ([], ps, []) = [(pass, ps)]
        | aux ([], [], fs) = [(fail, fs)]
        | aux ([], ps, fs) = [(pass, ps), (fail, fs)]
        | aux ({grade=g, id=i} :: others, ps, fs) = 
            if has_passed {grade=g, id=i} then aux (others, ps @ [i], fs) else aux (others, ps, fs @ [i])
    in
        aux (final_grades, [], [])
    end

datatype 'a tree = leaf 
                 | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun tree_height (leaf) = 0
    | tree_height (node {value=_, left=left, right=right}) =
        1 + Int.max (tree_height left, tree_height right)

fun sum_tree (leaf) = 0
    | sum_tree (node {value=value, left=left, right=right}) = 
        value + sum_tree (left) + sum_tree (right)

fun gardener (node {value=prune_me, left=_, right=_}) = leaf
    | gardener (node {value=value, left=left, right=right}) = node {value=value, left=gardener left, right=gardener right}
    | gardener (leaf) = leaf
