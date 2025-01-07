let rec filter f l =
  let rec aux s f l = match l with
    [] -> s
    | hd::tl -> if (f hd) then (s @ (hd::(filter f tl))) else (s @ (filter f tl))
    in aux [] f l;;
    
let rec uniq : 'a list -> 'a list
= fun lst -> match lst with
  [] -> []
  | hd::tl -> hd :: (uniq (filter (fun x -> x<>hd) tl));;

  uniq [5;6;5;4] |> List.map string_of_int |> String.concat " " |> print_endline;;