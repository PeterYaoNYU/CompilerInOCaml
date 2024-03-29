(fun x ->
  (fun y ->
    if (x<y) then (let z = [] in (1::z,(2,2)::z)) else (nil,(1,1)::nil)))
1 2
