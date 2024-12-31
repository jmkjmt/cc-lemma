let sigma f a b =
  if a > b then 0
  else
    let rec aux c d =
      if c >= b then d
      else aux (c + 1) (d + (f (c + 1))) in
    aux a (f a)



