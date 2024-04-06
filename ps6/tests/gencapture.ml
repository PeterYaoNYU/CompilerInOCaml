(fun y -> 
  let x = (fun z -> z y) in
  (x (fun w -> y + 1),(y+3))
)
