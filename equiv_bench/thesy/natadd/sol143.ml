type nat =
	| ZERO
	| SUCC of nat

let rec natadd : nat -> nat -> nat
= fun n1 n2 -> 
	let rec add a = function
		| ZERO -> a
		| SUCC(nat) -> add (SUCC a) nat in
	if n1 = ZERO then add n2 n1 else add n1 n2
