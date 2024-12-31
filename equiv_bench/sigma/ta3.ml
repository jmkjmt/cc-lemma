let sigma f a b =
  let rec aux a b f acc =
    if a > b then acc else aux (a+1) b f (acc + f a)
  in aux a b f 0