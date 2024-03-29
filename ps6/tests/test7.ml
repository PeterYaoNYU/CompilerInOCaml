let z = (fun l -> hd (tl l)) in
let x = 1::2::nil in
let y = (1,1)::(2,2)::nil in
(z x,z y)
