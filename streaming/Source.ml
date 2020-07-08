open Types

type 'a t = 'a source =
  Source : {
    init : unit -> 's;
    pull : 's -> ('a * 's) option;
    stop : 's -> unit;
  } -> 'a t


let unfold seed pull =
  Source { init = (fun () -> seed); pull; stop = (fun _ -> ()) }


let count n =
  unfold n (fun i -> Some (i, i + 1))


let iterate x f =
  unfold x (fun x -> Some (x, f x))


let make ~init ~pull ?(stop = ignore) () =
  Source { init; pull; stop }


let list l =
  let pull = function
    | []    -> None
    | x::xs -> Some (x, xs) in
  Source { init = (fun () -> l); pull; stop = (fun _ -> ()) }

let seq s =
  let pull s =
    match s () with
    | Seq.Nil   -> None
    | Seq.Cons (x, rest) -> Some (x, rest) in
  Source { init = (fun () -> s); pull; stop = (fun _ -> ()) }


let array a =
  let len = Array.length a in
  let pull i =
    if i >= len then None
    else Some (Array.unsafe_get a i, i + 1) in
  Source { init = (fun () -> 0); pull; stop = (fun _ -> ()) }


let string s =
  let len = String.length s in
  let pull i =
    if i >= len then None
    else Some (String.unsafe_get s i, i + 1) in
  Source { init = (fun () -> 0); pull; stop = (fun _ -> ()) }


let bytes s =
  let len = Bytes.length s in
  let pull i =
    if i >= len then None
    else Some (Bytes.unsafe_get s i, i + 1) in
  Source { init = (fun () -> 0); pull; stop = (fun _ -> ()) }


let queue q =
  let pull q =
    if Queue.is_empty q then None
    else Some (Queue.pop q, q) in
  Source { init = (fun () -> q); pull; stop = (fun _ -> ()) }


let empty =
  Source { init = ignore; pull = (fun () -> None); stop = ignore }


let zero = empty


let single a =
  let init () = false in
  let pull = function false -> Some (a, true) | true -> None in
  let stop _ = () in
  Source { init; pull; stop }


let two a b =
  list [a; b]


let three a b c =
  list [a; b; c]

let generate ~len:n f =
  let pull i =
    if i >= n then None
    else Some (f i, i + 1) in
  Source { init = (fun () -> 0); pull; stop = (fun _ -> ()) }

let take n (Source src) =
  let init () = (src.init (), 0) in
  let pull (s, i) =
    if i >= n then None
    else match src.pull s with
      | Some (a, s') -> Some (a, (s', i + 1))
      | None -> None in
  let stop (s, _) = src.stop s in
  Source { init; pull; stop }


let take_while p (Source src) =
  let pull s =
    match src.pull s with
    | Some (a, s') when p a -> Some (a, s')
    | _ -> None in
  Source { src with pull }


let drop n (Source src) =
  let init () = (src.init(), 0) in
  let pull (s, i) =
    let rec loop s i =
      match src.pull s with
      | Some (_, s') when i < n -> loop s' (i + 1)
      | Some (a, s') -> Some (a, (s', i))
      | None -> None in
    loop s i in
  let stop (s, _) = src.stop s in
  Source { init; pull; stop }


let drop_while p (Source src) =
  let init () = (src.init(), true) in
  let pull (s, dropping) =
    let rec loop s =
      match src.pull s with
      | Some (a, s') when p a && dropping -> loop s'
      | Some (a, s') -> Some (a, (s', false))
      | None -> None in
    loop s in
  let stop (s, _) = src.stop s in
  Source { init; pull; stop }


let select p (Source src) =
  let pull s =
    let rec loop s =
      match src.pull s with
      | Some (a, s') when p a -> Some (a, s')
      | Some (_, s') -> loop s'
      | None -> None in
    loop s in
  Source { src with pull }


let reject p src =
  select (fun x -> not (p x)) src


let filter = select



let map f (Source src) =
  let pull s =
    match src.pull s with
    | Some (a, s') -> Some (f a, s')
    | None -> None in
  Source { src with pull }


let zip_with f (Source src1) (Source src2) =
  let init () = (src1.init (), src2.init ()) in
  let pull (s1, s2) =
    match src1.pull s1 with
    | Some (x1, s1') ->
        begin match src2.pull s2 with
        | Some (x2, s2') -> Some (f x1 x2, (s1', s2'))
        | None -> None
        end
    | None -> None in
  let stop (s1, s2) = src1.stop s1; src2.stop s2 in
  Source { init; pull; stop }


let zip src1 src2 =
  zip_with (fun x y -> (x, y)) src1 src2


(* FIXME: not resource safe *)
let fold f r0 (Source src) =
  let rec loop r s =
    match src.pull s with
    | None -> r
    | Some (a, s') -> loop (f r a) s' in
  let s0 = src.init () in
  try
    let r = loop r0 s0 in
    src.stop s0;
    r
  with exn ->
    src.stop s0;
    raise exn


let len src =
  fold (fun count _ -> count + 1) 0 src


let each f (Source src) =
  let rec loop s =
    match src.pull s with
    | None -> ()
    | Some (a, s') -> f a; loop s' in
  let s0 = src.init () in
  try
    loop s0;
    src.stop s0
  with exn ->
    src.stop s0;
    raise exn


let file path : string t =
  Source {
    init = (fun () -> print_endline "opening"; open_in path);
    pull = (fun ic -> Some (input_line ic, ic));
    stop = (fun ic -> print_endline "closing"; close_in ic);
  }


let dispose (Source src) =
  src.stop (src.init ())


