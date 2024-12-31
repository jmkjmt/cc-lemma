type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec sub_check : lambda -> var list -> bool 
= fun ma lst ->
  match ma with
  | V x -> List.mem x lst
  | P (x,e) -> sub_check e (x::lst)
  | C (e1,e2) -> (sub_check e1 lst) && (sub_check e2 lst)

let check : lambda -> bool 
= fun m ->
  match m with
  | V x -> false
  | P (x,e) -> sub_check e [x]
  | C (e1,e2) -> sub_check e1 [] && sub_check e2 []
