(fun y -> 
  let x = (fun z -> z y) in
  (x (fun w -> isnil w),(y+3))
)
