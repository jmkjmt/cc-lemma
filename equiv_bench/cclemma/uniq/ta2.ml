let rec drop : 'a list -> 'a -> 'a list 
= fun lst n ->
  match lst with
  | [] -> []
  | hd::tl -> if hd = n then drop tl n else hd:: drop tl n

let rec uniq : 'a list -> 'a list
= fun lst ->
  match lst with
  | [] -> []
  | hd::tl -> hd :: uniq (drop tl hd)