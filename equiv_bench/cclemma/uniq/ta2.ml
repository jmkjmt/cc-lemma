let rec drop : 'a list -> 'a -> 'a list 
= fun lst n ->
  match lst with
  | [] -> []
  | hd::tl -> if hd = n then drop tl n else hd:: drop tl n
;;
let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
  | [] -> []
  | hd::tl -> hd :: uniq (drop tl hd)
;;
  uniq [5;6;5;4] |> List.map string_of_int |> String.concat " " |> print_endline;;