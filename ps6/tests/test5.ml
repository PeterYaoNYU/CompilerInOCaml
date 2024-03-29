(fun y -> 
  (fun z ->
  let x = (fun z -> z y) in
  let x1 = x x in
  (x (fun w -> isnil w),(isnil y,x1))
  )
)
