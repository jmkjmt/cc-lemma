let rec isnotin : 'a list -> 'a -> bool
= fun lst c ->
  match lst with
  | [] -> true
  | hd::tl -> if hd=c then false else isnotin tl c

let rec uniq4 : 'a list -> 'a list -> 'a list
= fun lst1 lst2 ->
  match lst1 with
  | [] -> lst2
  | hd::tl -> if isnotin lst2 hd then uniq4 tl (lst2@[hd]) else uniq4 tl lst2

let uniq : 'a list -> 'a list
= fun lst -> uniq4 lst []