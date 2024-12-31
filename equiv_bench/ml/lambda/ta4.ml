type lambda =
  | V of var
  | P of var * lambda
  | C of lambda * lambda
and var = string

let rec exists : (var -> bool) -> var list -> bool
= fun p l ->
  match l with
  | [] -> false
  | hd::tl -> p hd || exists p tl

let rec sub_check : lambda -> var list -> bool
= fun ma li ->
  match ma with
  | V na -> exists (fun x -> x = na) li
  | P (st, k) -> sub_check k (st::li)
  | C (me1, me2) -> (sub_check me1 li) && (sub_check me2 li)

let check : lambda -> bool
= fun m ->
  match m with
  | V na -> false
  | P (st, k) -> sub_check k [st]
  | C (me1, me2) -> sub_check me1 [] && sub_check me2 []