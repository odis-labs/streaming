open Types
open Utils

type 'a t = 'a stream =
  { stream : 'b . ('a, 'b) sink -> 'b }
  [@@unboxed]


(* Adaptors *)

let run ~from:(Source src) ~via:{flow} ~into:snk =
  let (Sink snk) = flow snk in
  let rec loop r s =
    if snk.full r then
      (* Sink is full. We capture the current source state into [init].
         This means that the consumers will have to dispose the source if the
         source leftover is not needed. *)
      let r' = snk.stop r in
      let leftover = Source { src with init = (fun () -> s) } in
      (r', Some leftover)
    else match src.pull s with
      | Some (x, s') -> loop (snk.push r x) s'
      | None ->
        (* The source was exhausted, stop src and snk. *)
        src.stop s;
        let r' = snk.stop r in
        (r', None) in

  (* Create the sink state. If this fails, there's nothing we can do. *)
  let r0 = snk.init () in
  (* Check if k is full, if so, return (the full src is leftover). *)
  if snk.full r0 then (snk.stop r0, Some (Source src)) else
  (* Create the src state. If this fail, we close the snk state. *)
  let s0 = try src.init () with exn -> let _ = snk.stop r0 in raise exn in
  try loop r0 s0 with exn ->
    (* Computation failed, close both (initial) states. *)
    src.stop s0;
    let _r' = snk.stop r0 in
    raise exn


let from (Source src) =
  let stream (Sink k) =
    (* Loop will terminate both src and k when src is exhausted. *)
    let rec loop r s =
      if k.full r then k.stop r else
      match src.pull s with
      | None -> src.stop s; k.stop r
      | Some (x, s') -> loop (k.push r x) s'
    in
    (* Create the sink state. If this fails, there's nothing we can do. *)
    let r0 = k.init () in
    (* Check if k is full, if so, return (src is not initialized). *)
    if k.full r0 then k.stop r0 else
    (* Create the src state. If this fail, we close the snk state. *)
    let s0 = try src.init () with exn -> let _ = k.stop r0 in raise exn in
    try loop r0 s0 with exn ->
      src.stop s0;
      let _ = k.stop r0 in
      raise exn
  in
  { stream }


let into sink this =
  this.stream sink


(* Conv *)

let to_list stream = into Sink.list stream

let of_list xs =
  let stream (Sink k) =
    let rec loop s r =
      if k.full r then r
      else match s with
        | [] -> r
        | x :: s' -> loop s' (k.push r x) in
    bracket (loop xs) ~init:k.init ~stop:k.stop in
  { stream }


let to_array stream = into Sink.array stream

let of_array xs = from (Source.array xs)

let to_string stream = into Sink.(premap (String.make 1) string) stream

let of_string xs = from (Source.string xs)


(* Sinks *)

let each f =
  into (Sink.each f)

let fold f z =
  into (Sink.fold f z)

let is_empty stream =
  into Sink.is_empty stream

let len stream =
  into Sink.len stream

let first stream =
  into Sink.first stream

let last stream =
  into Sink.last stream

let drain stream =
  into Sink.drain stream


(* Creating a stream *)

let empty =
  let stream (Sink k) = k.stop (k.init ()) in
  { stream }


let single x =
  let stream (Sink r) = r.stop (r.push (r.init ()) x) in
  { stream }


let double x1 x2 =
  let stream (Sink k) = k.stop (k.push (k.push (k.init ()) x1) x2) in
  { stream }


let triple x1 x2 x3 =
  let stream (Sink k) = k.stop (k.push (k.push (k.push (k.init ()) x1) x2) x3) in
  { stream }


let unfold s0 pull =
  from (Source.unfold s0 pull)


let generate ~len f =
  from (Source.generate ~len f)



let count n =
  let stream (Sink k) =
    let rec loop s r =
      if k.full r then r
      else loop (s + 1) (k.push r s) in
    bracket (loop n) ~init:k.init ~stop:k.stop in
  { stream }


let iterate x f = from (Source.iterate x f)


let range ?by:(step=1) n m =
  if n > m then invalid_arg "Streaming.Stream.range: invalid range" else
  unfold n (fun i -> if i >= m then None else Some (i, i + step))

let iota n =
  range 0 n

let (-<) n m = range n m

let (--) n m = range n (m - 1)


let repeat ?times:n x =
  let stream (Sink k) =
    match n with
    | None ->
      let rec loop r =
        if k.full r then r
        else loop (k.push r x) in
      bracket loop ~init:k.init ~stop:k.stop
    | Some n ->
      let rec loop i r =
        if k.full r || i = n then r
        else loop (i + 1) (k.push r x) in
      bracket (loop 0) ~init:k.init ~stop:k.stop in
  { stream }


let repeatedly ?times:n f =
  let stream (Sink k) =
    match n with
    | None ->
      let rec loop r =
        if k.full r then r
        else loop (k.push r (f ())) in
      bracket loop ~init:k.init ~stop:k.stop
    | Some n ->
      let rec loop i r =
        if k.full r || i = n then r
        else loop (i + 1) (k.push r (f ())) in
      bracket (loop 0) ~init:k.init ~stop:k.stop in
  { stream }


(* Combining streams *)

let flat_map f this =
  let stream (Sink k) =
    let push r x =
      (f x).stream (Sink { k with
          init = (fun () -> r);
          stop = (fun r -> r)
        }) in
    this.stream (Sink { k with push }) in
  { stream }


let concat this that =
  let stream (Sink k) =
    let stop r =
      if k.full r then k.stop r else
      that.stream (Sink {k with init = (fun () -> r)}) in
    this.stream (Sink { k with stop }) in
  { stream }

let (++) = concat


let append x stream =
  concat stream (single x)

let prepend x stream =
  concat (single x) stream


let flatten nested =
  fold concat empty nested


let cycle ?times:(n = -1) this =
  if n = 0 then empty else
  if n = 1 then this else
  let i = ref 1 in
  let stream (Sink k) =
    let rec stop r =
      if k.full r || !i = n then k.stop r else
      (incr i;
       this.stream (Sink {k with init = (fun () -> r); stop }))
    in
    this.stream (Sink { k with stop }) in
  { stream }


(* let interleave this that = *)
(*   let stream (Sink k) = *)
(*     let push acc x = *)
(*       k.push acc x *)
(*     in *)
(*     this.stream (Sink {k with push}) *)
(*   in *)
(*   { stream } *)


let interpose sep self =
  let stream (Sink k) =
    let started = ref false in
    let push acc x =
      if !started then
        let acc = k.push acc sep in
        if k.full acc then acc
        else k.push acc x
      else begin
        started := true;
        k.push acc x
    end in
    self.stream (Sink {k with push})
  in
  { stream }


let via {flow} this =
  let stream sink = into (flow sink) this in
  { stream }


let map f this =
  via (Flow.map f) this


let filter pred this =
  via (Flow.filter pred) this


let take n this =
  via (Flow.take n) this


let take_while pred this =
  via (Flow.take_while pred) this


let drop n this =
  via (Flow.drop n) this


let drop_while pred this =
  via (Flow.drop_while pred) this


let rest self =
  drop 1 self


let indexed self =
  let stream (Sink k) =
    let i = ref 0 in
    let push acc x =
      let acc' = k.push acc (!i, x) in
      incr i; acc' in
    self.stream (Sink { k with push })
  in
  { stream }


(* Groupping *)

let partition n self =
  if n = 0 then empty else
  let stream (Sink k) =
    let init () = (k.init (), 0, empty) in
    let push (r, i, acc) x =
      if i = n then (k.push r acc, 1, single x)
      else (r, i + 1, acc ++ single x) in
    let stop (r, i, acc) =
      let r' = if i < n then (k.push r acc) else r in
      k.stop r' in
    let full (r, _, _) = k.full r in
    self.stream (Sink { init; push; full; stop })
    in
  { stream }


(* How efficient is this for > 1K elements? *)
let split ~by:pred self =
  let stream (Sink k) =
    let init () = (k.init (), empty) in
    let push (r, acc) x =
      if pred x then (k.push r acc, empty)
      else (r, acc ++ single x) in
    let stop (r, acc) = k.push r acc |> k.stop in
    let full (r, _) = k.full r in
    self.stream (Sink { init; push; full; stop })
    in
  { stream }



let group ?equal:(_ =Pervasives.(=)) self =
  let stream (Sink k) =
    let push r x =
      k.push r x
    in
    self.stream (Sink { k with push })
    in
  { stream }




(* IO *)

let of_file path =
  (* Using a lazy val will avoid opening the file if not needed. *)
  let ic = lazy (open_in path) in
	let stream (Sink k) =
		let rec loop r =
      if k.full r then r
      else
				match input_line (Lazy.force ic) with
        | x -> loop (k.push r x)
			  | exception End_of_file -> r in
    let stop r =
      if Lazy.is_val ic then close_in (Lazy.force ic);
      k.stop r in
    bracket loop ~init:k.init ~stop in
	{ stream }


let to_file path =
  into (Sink.file path)


let stdin =
  let stream (Sink k) =
    let rec loop r =
      if k.full r then r
      else
        try loop (k.push r (input_line Pervasives.stdin))
        with End_of_file -> r in
    bracket loop ~init:k.init ~stop:k.stop in
  { stream }


let stdout =
  into Sink.stdout


let stderr =
  into Sink.stderr



(**/**)

let yield x = single x

(**/**)


module Syntax = struct
  let yield x = yield x

  let (let*) t f = flat_map f t
end

