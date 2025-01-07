let rec find :  'a -> 'a list -> bool 
= fun x lst ->
  match lst with
    | [] -> false
    | hd::tl -> (x = hd)||find x tl;;

let rec uniq : 'a list -> 'a list
= fun lst -> 
  match List.rev lst with
    | [] -> []
    | hd::tl -> if find hd tl then uniq (List.rev tl) else (uniq (List.rev tl))@[hd];;
    
    uniq [0;1;0] |> List.map string_of_int |> String.concat " " |> print_endline;;