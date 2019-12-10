
open Streaming


let sort_chunks ?(compare=Pervasives.compare) n =
  let init () = ([], [], 0) in
  let step (acc, chunk, count) x =
    let chunk = x :: chunk in
    let count = count + 1 in
    let acc =
      if count > n then
        let sorted = List.sort compare chunk in
        List.rev_append sorted acc
      else acc in
    (acc, chunk, count) in
  let stop s = s in
  sink ~init ~step ~stop


