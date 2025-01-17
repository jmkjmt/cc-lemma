type nat = ZERO | SUCC of nat

let rec natadd n1 n2 =
  match n1 with
  | ZERO -> n2
  | SUCC n -> SUCC (natadd n n2)

let rec natmul n1 n2 =
  match n1 with
  | ZERO -> ZERO
  | SUCC n -> natadd n2 (natmul n n2)