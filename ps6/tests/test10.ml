(fun y ->
  let x = (fun z->y) in
  ((x 1) 1,(x (1,2)) 2)) (fun x -> x)
