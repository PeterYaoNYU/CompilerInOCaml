(fun y -> 
  let x = (fun z -> z y) in
  (y+3,x)
)
