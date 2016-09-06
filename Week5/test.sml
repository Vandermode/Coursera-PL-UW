(* mutual recursion *)
fun match xs =
    let fun s_need_one xs =
        case xs of
            [] => true
            | x :: xs' => if x = 1 then s_need_two xs' else false
    and s_need_two xs =
        case xs of
            [] => false
            | x :: xs' => if x = 2 then s_need_one xs' else false
    in
        s_need_one xs
    end

