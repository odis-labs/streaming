
open Types

type ('a, 'b) t = ('a, 'b) flow =
  { flow : 'r . ('b, 'r) sink -> ('a, 'r) sink }
  [@@unboxed]


let run {flow} = flow


let identity =
  { flow = fun sink -> sink }

let compose {flow=f} {flow=g} =
  { flow = fun sink -> f (g sink) }

let (<<) f1 f2 = compose f1 f2
let (>>) f2 f1 = compose f1 f2


let map f =
  let flow (Sink k) =
    let push r x = k.push r (f x) in
    Sink { k with push } in
  { flow }


let select pred =
  let flow (Sink k) =
    let push r x = if pred x then k.push r x else r in
    Sink { k with push } in
  { flow }

let filter = select

let reject pred = select (fun x -> not (pred x))


let take n =
  let flow (Sink k) =
    let init () = (k.init (), 0) in
    let push (acc, i) x = (k.push acc x, i + 1) in
    let full (acc, i) = k.full acc || i = n in
    let stop (acc, _) = k.stop acc in
    Sink { init; push; full; stop } in
  { flow }


let take_while pred =
  let flow (Sink k) =
    let init () = (k.init (), true) in
    let push (acc, _) x =
      if pred x then (k.push acc x, true) else (acc, false) in
    let full (acc, taking) = k.full acc || not taking in
    let stop (acc, _) = k.stop acc in
    Sink { init; push; full; stop } in
  { flow }


let drop n =
  let flow (Sink k) =
    let init () = (k.init (), 0) in
    let push (acc, i) x =
      if i < n then (acc, i + 1)
      else (k.push acc x, i) in
    let full (acc, _) = k.full acc in
    let stop (acc, _) = k.stop acc in
    Sink { init; push; full; stop } in
  { flow }


let drop_while p =
  let flow (Sink k) =
    let init () = (k.init (), true) in
    let push (acc, dropping) x =
      if p x && dropping then (acc, true)
      else (k.push acc x, false) in
    let full (acc, _) = k.full acc in
    let stop (acc, _) = k.stop acc in
    Sink { init; push; full; stop } in
  { flow }


(* let cycle this = *)
(*   let flow (Sink k) = *)
(*     let stop r = k.stop r in *)
(*     Sink { k with stop } in *)
(*   { flow } *)


let buffer n =
  if n <= 0 then
    invalid_arg "Streaming.Flow.buffer: invalid buffer size, must be (> 0).";
  let flow (Sink k) =
    let buf = ref [||] in
    let idx = ref 0 in
    let reset () =
      buf := Array.make n (Obj.magic 0);
      idx := 0 in
    let init () = reset (); k.init () in
    let push acc x =
      Array.set !buf !idx x;
      if !idx = n - 1 then begin
        let x = !buf in
        reset ();
        k.push acc x
      end else begin
        incr idx;
        acc
      end in
    let stop acc =
      let acc =
        if !idx > 0 && !idx < n then begin
          let x = Array.sub !buf 0 !idx in
          k.push acc x
        end else acc in
      k.stop acc in
    Sink { k with init; push; stop } in
  { flow }


let through (Sink k0) =
  let flow (Sink k1) =
    let acc0 = ref (k0.init ()) in
    let push acc1 x0 =
      if k0.full !acc0 then
        let x1 = k0.stop !acc0 in
        acc0 := k0.init ();
        k1.push acc1 x1
      else begin
        acc0 := k0.push !acc0 x0;
        acc1
      end in
    Sink { k1 with push } in
  { flow }



