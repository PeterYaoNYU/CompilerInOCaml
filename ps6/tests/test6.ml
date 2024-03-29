(fun y->
let x = (fun z -> z y) in
x x
)
