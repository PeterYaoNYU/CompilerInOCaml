let m = (fun a -> a*2) in
let f = (fun z -> (fun a -> z (z a))) in
(fun z -> (f m 1,f (fun x -> 1::x) [])) 1
