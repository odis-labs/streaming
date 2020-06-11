open Streaming

let average1 () =
  Sink.((/) <@> sum <*> len)

let average2 () =
  Sink.(map (fun (x, y) -> x / y) (zip sum len))

let average3 () =
  let open Sink.Syntax in
  let+ sum = Sink.sum
  and+ len = Sink.len in
  sum / len



