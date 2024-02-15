(*
 * author: Bob Muller

   This library has a few handy definitions for the course.
*)
let fresh =
  let counter = ref 0
  in
  (fun () ->
     let answer = !counter
     in
     counter := !counter + 1;
     answer)

let i2S = string_of_int
let i2F = float_of_int

let f2S = string_of_float
let f2I = int_of_float

let fmt = Printf.sprintf
let pfmt = Printf.printf

let range n =
  let rec repeat n answer =
    match n = 0 with
    | true  -> answer
    | false ->
      let m = n - 1
      in
      repeat m (m :: answer)
  in
  repeat n []

let explode s =
  let n = String.length s in
  let rec repeat i =
    match i = n with
    | true  -> []
    | false -> s.[i] :: repeat (i + 1)
  in
  repeat 0

let implode chars =
  let res = Bytes.create (List.length chars) in
  let rec repeat i chars =
    match chars with
    | [] -> res
    | char :: chars ->
       Bytes.set res i char;
       repeat (i + 1) chars
  in
  Bytes.to_string (repeat 0 chars)

let run_test : string -> (unit -> bool) -> unit
  = Assert.run_test
