module Pervasives = Stdlib

let bracket' ~(init : unit -> 'a) ~(stop : 'b -> 'r) (f : 'a -> 'b) : 'r =
  let acc = init () in
  try
    let acc' = f acc in
    stop acc'
  with exn ->
    let _acc' = stop acc in
    raise exn

let bracket ~(init : unit -> 's) ~(stop : 's -> 'b) (f : 's -> 's) : 'b =
  let acc = init () in
  try
    let acc' = f acc in
    stop acc'
  with exn ->
    let _acc' = stop acc in
    raise exn


let eq_int : int -> int -> bool = Pervasives.(=)

