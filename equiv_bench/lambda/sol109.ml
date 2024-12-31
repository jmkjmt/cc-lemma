type lambda = V of var
			| P of var * lambda
			| C of lambda * lambda
and var = string

let rec varExists : var * var list -> bool = 
	fun (var_foo, var_list) ->
		match var_list with
		[] -> false
		| h::t -> 
			(if var_foo = h then true
			else (varExists (var_foo, t)))
			
let addToNameList : var * var list -> var list = 
	fun (var_foo, var_list) ->
		if (varExists (var_foo, var_list)) then 
			var_list
		else 
			var_foo::var_list
			
let rec checkRec : lambda * var list -> bool = 
	fun (lambda_foo, var_list) ->
		match lambda_foo with
		V(var_bar) -> 
			(varExists(var_bar, var_list))
		| P(var_bar, lambda_bar) -> 
			(checkRec(lambda_bar, addToNameList(var_bar, var_list)))
		| C(lambda_left, lambda_right) ->  
			(checkRec(lambda_left, var_list) 
			&& checkRec(lambda_right, var_list))

let check: lambda -> bool = 
	fun lambda_foo -> checkRec(lambda_foo, [])
