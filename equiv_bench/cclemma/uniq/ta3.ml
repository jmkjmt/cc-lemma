let rec is_in : 'a list -> 'a -> bool
= fun lst a ->
  match lst with
  | [] -> false
  | hd::tl -> if a = hd then true else is_in tl a
;;
let rec uniq3 : 'a list -> 'a list -> 'a list
= fun lst1 lst2 ->
  match lst1 with
  | [] -> lst2
  | hd::tl -> if is_in lst2 hd then uniq3 tl lst2 else uniq3 tl (lst2@[hd])
;;
let uniq : 'a list -> 'a list
= fun lst -> uniq3 lst []
;;
uniq [5;6;5;4] |> List.map string_of_int |> String.concat " " |> print_endline;;