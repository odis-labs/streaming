open Types
open Utils

type ('a, 'b) t = ('a, 'b) sink =
  Sink : {
    init : unit -> 'acc;
    push : 'acc -> 'a -> 'acc;
    full : 'acc -> bool;
    stop : 'acc -> 'b;
  } -> ('a, 'b) t



let make ~init ~push ?(full=fun _ -> false) ~stop () =
  Sink { init; push; full; stop }


let fill b =
  Sink {
    init = (fun () -> ());
    push = (fun () _ -> ());
    full = (fun () -> true);
    stop = (fun () -> b)
  }


let map f (Sink k) =
  Sink { k with stop = (fun x -> f (k.stop x)) }


let (<@>) = map


let premap f (Sink k) =
  Sink { k with push = (fun acc x -> k.push acc (f x)) }


let prefilter f (Sink k) =
  Sink { k with push = (fun acc x -> if f x then k.push acc x else acc ) }


let zip (Sink l) (Sink r) =
  let init () = (l.init (), r.init ()) in
  let push (l_acc, r_acc) x = (l.push l_acc x, r.push r_acc x) in
  let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc) = (l.stop l_acc, r.stop r_acc) in
  Sink { init; push; full; stop }


let zip_left (Sink l) (Sink r) =
  let init () = (l.init (), r.init ()) in
  let push (l_acc, r_acc) x = (l.push l_acc x, r.push r_acc x) in
  let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc) =
    let l = l.stop l_acc in
    let _r = r.stop r_acc in
    l in
  Sink { init; push; full; stop }


let zip_right (Sink l) (Sink r) =
  let init () = (l.init (), r.init ()) in
  let push (l_acc, r_acc) x = (l.push l_acc x, r.push r_acc x) in
  let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc) =
    let _l = l.stop l_acc in
    let r = r.stop r_acc in
    r in
  Sink { init; push; full; stop }


let zip_with f (Sink l) (Sink r) =
  let init () = (l.init (), r.init ()) in
  let push (l_acc, r_acc) x = (l.push l_acc x, r.push r_acc x) in
  let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc) =
    let l = l.stop l_acc in
    let r = r.stop r_acc in
    f l r in
  Sink { init; push; full; stop }

let (<&>) = zip
let (<& ) = zip_left
let ( &>) = zip_right


let unzip (Sink l) (Sink r) =
  let init () = (l.init (), r.init ()) in
  let push (l_acc, r_acc) (x, y) = (l.push l_acc x, r.push r_acc y) in
  let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc) = (l.stop l_acc, r.stop r_acc) in
  Sink { init; push; full; stop }

let unzip_left (Sink l) (Sink r) =
  let init () = (l.init (), r.init ()) in
  let push (l_acc, r_acc) (x, y) = (l.push l_acc x, r.push r_acc y) in
  let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc) =
    let l = l.stop l_acc in
    let _r = r.stop r_acc in
    l in
  Sink { init; push; full; stop }


let unzip_right (Sink l) (Sink r) =
  let init () = (l.init (), r.init ()) in
  let push (l_acc, r_acc) (x, y) = (l.push l_acc x, r.push r_acc y) in
  let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc) =
    let _l = l.stop l_acc in
    let r = r.stop r_acc in
    r in
  Sink { init; push; full; stop }


let unzip_with f (Sink l) (Sink r) =
  let init () = (l.init (), r.init ()) in
  let push (l_acc, r_acc) (x, y) = (l.push l_acc x, r.push r_acc y) in
  let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc) =
    let l = l.stop l_acc in
    let r = r.stop r_acc in
    f l r in
  Sink { init; push; full; stop }


let (<*>) = unzip
let (<* ) = unzip_left
let ( *>) = unzip_right

let distribute (Sink l) (Sink r) =
  let init () = (l.init (), r.init (), true) in
  let push (l_acc, r_acc, flag) x =
    if flag then (l.push l_acc x, r_acc, not flag)
    else (l_acc, r.push r_acc x, not flag) in
  let full (l_acc, r_acc, _) = (l.full l_acc || r.full r_acc) in
  let stop (l_acc, r_acc, _) = (l.stop l_acc, r.stop r_acc) in
  Sink { init; push; full; stop }


type ('a, 'b) race =
  | Left of 'a
  | Right of 'b
  | Both of 'a * 'b

let race (Sink l) (Sink r) =
  let init () = Both (l.init (), r.init ()) in
  let push state x =
    match state with
    | Both (l_acc, r_acc) ->
      let l_acc' = l.push l_acc x in
      let r_acc' = r.push r_acc x in
      if l.full l_acc' then Left  l_acc' else
      if r.full r_acc' then Right r_acc' else
        Both (l_acc', r_acc')
    | _ ->
        invalid_arg "Streaming.Sink.race: one of the sinks is already filled" in
  let full = function Both _ -> false | _ -> true in
  let stop = function
    | Left  l_acc -> Left (l.stop l_acc)
    | Right r_acc -> Right (r.stop r_acc)
    | Both (l_acc, r_acc) -> Both (l.stop l_acc, r.stop r_acc)
  in
  Sink { init; push; full; stop }

let (<|>) = race


let seq (Sink l) (Sink r) =
  let init () = `L (l.init ()) in
  let push state x =
    match state with
    | `L l_acc ->
      let l_acc' = l.push l_acc x in
      if l.full l_acc'
      then `R (l.stop l_acc', r.init ())
      else `L l_acc
    | `R (l_r, r_acc) -> `R (l_r, r.push r_acc x) in
  let full = function
    | `L l_acc -> l.full l_acc
    | `R (_, r_acc) -> r.full r_acc in
  let stop = function
    | `L l_acc ->
      let l_r = l.stop l_acc in
      let r_r = r.stop (r.init ()) in
      (l_r, r_r)
    | `R (l_r, r_acc) ->
      let r_r = r.stop r_acc in
      (l_r, r_r) in
  Sink { init; push; full; stop }


let seq_left (Sink l) (Sink r) =
  let init () = `L (l.init ()) in
  let push state x =
    match state with
    | `L l_acc ->
      let l_acc' = l.push l_acc x in
      if l.full l_acc'
      then `R (l.stop l_acc', r.init ())
      else `L l_acc
    | `R (l_r, r_acc) -> `R (l_r, r.push r_acc x) in
  let full = function
    | `L l_acc -> l.full l_acc
    | `R (_, r_acc) -> r.full r_acc in
  let stop = function
    | `L l_acc -> l.stop l_acc
    | `R (l_r, r_acc) ->
      let _r_r = r.stop r_acc in
      l_r in
  Sink { init; push; full; stop }


let seq_right (Sink l) (Sink r) =
  let init () = `L (l.init ()) in
  let push state x =
    match state with
    | `L l_acc ->
      let l_acc' = l.push l_acc x in
      if l.full l_acc'
      then (ignore (l.stop l_acc'); `R (r.init ()))
      else `L l_acc
    | `R r_acc -> `R (r.push r_acc x) in
  let full = function
    | `L l_acc -> l.full l_acc
    | `R r_acc -> r.full r_acc in
  let stop = function
    | `L l_acc ->
      ignore (l.stop l_acc);
      r.stop (r.init ())
    | `R r_acc -> r.stop r_acc in
  Sink { init; push; full; stop }

let (<+>) = seq
let (<+ ) = seq_left
let ( +>) = seq_right


type ('top, 'a, 'b) flat_map =
  | Flat_map_top : 'top -> ('top, 'a, 'b) flat_map
  | Flat_map_sub : {
    init: 'sub;
    push: 'sub -> 'a -> 'sub;
    full: 'sub -> bool;
    stop: 'sub -> 'b;
  } -> ('top, 'a, 'b) flat_map


let _flat_map f (Sink top) =
  let init () = Flat_map_top (top.init ()) in
  let push s x =
    match s with
    | Flat_map_top acc ->
      let acc' = top.push acc x in
      if top.full acc' then
        let r = top.stop acc' in
        let Sink sub = f r in
        Flat_map_sub {
          init = sub.init ();
          push = sub.push;
          full = sub.full;
          stop = sub.stop;
        }
      else
        Flat_map_top acc'
    | Flat_map_sub sub -> Flat_map_sub { sub with init = sub.push sub.init x } in
  let full = function
    | Flat_map_top acc -> top.full acc
    | Flat_map_sub sub -> sub.full sub.init in
  let stop = function
    | Flat_map_top acc -> top.stop acc (* XXX: This constrains the type to 'a. *)
    | Flat_map_sub sub -> sub.stop sub.init in
  Sink { init; push; full; stop }



let fold f z =
  Sink {
    init = (fun () -> z);
    push = f;
    full = (fun _ -> false);
    stop = (fun r -> r);
  }

let fold_while full f z =
  Sink {
    init = (fun () -> z);
    push = f;
    full = full;
    stop = (fun r -> r);
  }


let full =
  Sink {
    init = (fun () -> ());
    push = (fun () _ -> ());
    full = (fun () -> true);
    stop = (fun () -> ());
  }

let drain =
  Sink {
    init = (fun () -> ());
    push = (fun () _ -> ());
    full = (fun () -> false);
    stop = (fun () -> ());
  }

let each f =
  Sink {
    init = (fun () -> ());
    push = (fun () x -> f x);
    full = (fun () -> false);
    stop = (fun () -> ());
  }

let len =
  Sink {
    init = (fun () -> 0);
    push = (fun acc _ -> acc + 1);
    full = (fun _ -> false);
    stop = (fun acc -> acc);
  }

let mean =
  let init () = (0.0, 0.0) in
  let push (r, n) x =
     let n' = n +. 1.0 in
    ((r +. (x -. r) /. n'), n') in
  let stop (r, _) = r in
  let full _ = false in
  Sink { init; push; full; stop }



let nth_pure n =
  let init () = `Searching 0 in
  let push s x =
    match s with
    | `Searching i when eq_int i n -> `Found x
    | `Searching i -> `Searching (i + 1)
    | `Found x -> `Found x in
  let full s =
    match s with
    | `Searching _  -> false
    | `Found _ -> true in
  let stop s =
    match s with
    | `Searching _ -> None
    | `Found x -> Some x in
  Sink { init; push; full; stop }


let nth n =
  let r = ref (Obj.magic 0) in
  let init () = 0 in
  let push s x =
    if s = n then r := x;
    s + 1 in
  let full s = s > n in
  let stop s = if full s then Some !r else None in
  Sink { init; push; full; stop }


let first =
  Sink {
    init = (fun () -> None);
    push = (fun _ x -> Some x);
    full = (function None -> false | _ -> true);
    stop = (fun acc -> acc);
  }

let last =
  Sink {
    init = (fun () -> None);
    push = (fun _ x -> Some x);
    full = (fun _ -> false);
    stop = (fun acc -> acc);
  }


(* Finding elements *)

let contains ~where:pred =
  Sink {
    init = (fun () -> false);
    push = (fun found x -> if pred x then true else found);
    full = (fun found -> found);
    stop = (fun found -> found);
  }

let find ~where:pred =
  Sink {
    init = (fun () -> None);
    push = (fun none x -> if pred x then Some x else none);
    full = (function None -> false | _ -> true);
    stop = (fun acc -> acc);
  }

let index ~where:pred =
  let i = ref 0 in
  Sink {
    init = (fun () -> false);
    push = (fun acc x -> if pred x then true else (incr i; acc));
    full = (fun found -> found);
    stop = (fun found -> if found then Some !i else None);
  }


let maximum ~by:(>) =
  let push state x =
    match state with
    | Some y when y > x -> Some y
    | _ -> Some x in
  Sink {
    init = (fun () -> None);
    push;
    full = (fun _ -> false);
    stop = (fun acc -> acc);
  }


let minimum ~by:(<) =
  let push state x =
    match state with
    | Some y when y < x -> Some y
    | _ -> Some x in
  Sink {
    init = (fun () -> None);
    push;
    full = (fun _ -> false);
    stop = (fun acc -> acc);
  }


let is_empty =
  Sink {
    init = (fun () -> true);
    push = (fun _ _ -> false);
    full = (fun _ -> false);
    stop = (fun acc -> acc);
  }

let all ~where:pred =
  Sink {
    init = (fun () -> true);
    push = (fun acc x -> pred x && acc);
    full = (fun acc -> not acc);
    stop = (fun acc -> acc);
  }


let any ~where:pred =
  Sink {
    init = (fun () -> false);
    push = (fun acc x -> pred x || acc);
    full = (fun acc -> acc);
    stop = (fun acc -> acc);
  }


let print =
  each print_endline


let rev_list =
  Sink {
    init = (fun () -> []);
    push = (fun acc x -> x :: acc);
    full = (fun _ -> false);
    stop = (fun acc -> acc);
  }


let list =
  Sink {
    init = (fun () -> []);
    push = (fun acc x -> x :: acc);
    full = (fun _ -> false);
    stop = (fun acc -> List.rev acc);
  }


let array =
  let init () = ([], 0) in
  let push (acc, len) x = (x :: acc, len + 1) in
  let full _ = false in
  let stop (acc, len) =
    match acc with
    | [] -> [||] | [x] -> [|x|]
    | x :: xs ->
      let arr = Array.make len x in
      let rec loop i = function
        | [] -> arr
        | x :: xs ->
          Array.unsafe_set arr i x;
          loop (i - 1) xs in
      loop (len - 2) xs in
  Sink { init; push; full; stop }


let buffer n =
  if n < 0 then
    invalid_arg "Streaming.Sink.buffer: negative buffer size";
  if n = 0 then fill [||] else
  let buf = ref (Array.make n (Obj.magic 0)) in
  let init () = 0 in
  let push idx x =
    Array.set !buf idx x;
    idx + 1 in
  let full idx = (idx = n) in
  let stop idx =
    if idx < n then
      Array.sub !buf 0 idx
    else !buf in
  Sink { init; push; full; stop }



let queue =
  Sink {
    init = Queue.create;
    push = (fun acc x -> Queue.add x acc; acc);
    full = (fun _ -> false);
    stop = (fun acc -> acc);
  }


let sum =
  Sink {
    init = (fun () -> 0);
    push = (+);
    full = (fun _ -> false);
    stop = (fun x -> x);
  }


let product =
  Sink {
    init = (fun () -> 1);
    push = (fun acc x -> acc * x);
    full = (fun x -> x = 0);
    stop = (fun x -> x);
  }


let bytes =
  Sink {
    init = (fun () -> Buffer.create 128);
    push = (fun buf x -> Buffer.add_bytes buf x; buf);
    full = (fun _ -> false);
    stop = (fun buf -> Buffer.to_bytes buf);
  }


let string =
  Sink {
    init = (fun () -> Buffer.create 128);
    push = (fun buf x -> Buffer.add_string buf x; buf);
    full = (fun _ -> false);
    stop = (fun buf -> Buffer.contents buf);
  }


let file path =
  Sink {
    init = (fun () -> Stdlib.open_out path);
    push = (fun chan x -> Stdlib.output_string chan (x ^ "\n"); chan);
    full = (fun _ -> false);
    stop = close_out
  }


let stderr =
  Sink {
    init = (fun () -> ());
    push = (fun () x -> output_string Pervasives.stderr (x ^ "\n"); flush Pervasives.stderr);
    full = (fun _ -> false);
    stop = (fun () -> ());
  }


let stdout =
  Sink {
    init = (fun () -> ());
    push = (fun () x -> output_string Pervasives.stdout (x ^ "\n"); flush Pervasives.stdout);
    full = (fun _ -> false);
    stop = (fun () -> ());
  }


let dispose (Sink snk) =
  snk.stop (snk.init ())


module Syntax = struct
  let (let+) f t = map t f
  let (and+) a b = zip a b
end



module Functor = struct
  type ('a, 'b) t = ('a, 'b) sink

  let map  = map
end


module Applicative = struct
  type ('a, 'b) t = ('a, 'b) sink

  let pure = fill

  let (<*>) (Sink l) (Sink r) =
    let init () = (l.init (), r.init ()) in
    let push (l_acc, r_acc) x = (l.push l_acc x, r.push r_acc x) in
    let full (l_acc, r_acc) = (l.full l_acc || r.full r_acc) in
    let stop (l_acc, r_acc) = l.stop l_acc (r.stop r_acc) in
    Sink { init; push; full; stop }
end

