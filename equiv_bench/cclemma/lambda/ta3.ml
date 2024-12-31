type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec sub_check : lambda -> var list -> bool
= fun m lst ->
  match m with
  | V na -> List.mem na lst
  | P (st, k) -> if List.mem st lst then sub_check k lst else sub_check k (st::lst)
  | C (me1,me2) -> (sub_check me1 lst) && (sub_check me2 lst)

let check : lambda -> bool
= fun m ->
  sub_check m []