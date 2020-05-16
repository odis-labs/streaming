open Streaming

let average1 =
  Sink.((/) <@> sum <*> length)

let average2 =
  Sink.(map (fun (x, y) -> x / y) (both sum length))

(* let average3 = *)
(*   let open Sink.Syntax in *)
(*   let+ sum = Sink.sum *)
(*   and+ len = Sink.length in *)
(*   sum / len *)



(* open Stream.Syntax *)

(* let items () = *)
(*   let* x = Stream.range 10 in *)
(*   let* y = Stream.of_list ['x'; 'y'; 'z'] in *)
(*   if x mod 2 = 0 then *)
(*     yield (x, y) *)
(*   else Stream.empty *)


(* let () = *)
(*   Stream.each (fun (x, y) -> Printf.eprintf ">>> %d, %c\n" x y) (items ()) *)


(*
let x =
  Flow.zip
    (Flow.map (fun x -> x))
    (Flow.map (fun x -> x * 10))
*)


(*
  collect(
    Zip(
      Map(identity),
      Map(x -> 10x),
    1:3)

  3-element Array{Tuple{Int64,Int64,Int64},1}:
    (1, 10)
    (2, 20)
    (3, 30)
*)
