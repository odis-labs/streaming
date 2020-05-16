let fmt = Format.asprintf

module C = struct
  let color_pp color =
    fmt "\027[%dm%s\027[0m"
      (match color with
       | `Black   -> 30
       | `Red     -> 31
       | `Green   -> 32
       | `Yellow  -> 33
       | `Blue    -> 34
       | `Magenta -> 35
       | `Cyan    -> 36
       | `White   -> 37)

  let blue               = color_pp `Blue
  let red                = color_pp `Red
  let yellow             = color_pp `Yellow
  let magenta            = color_pp `Magenta
  let cyan               = color_pp `Cyan
  let white              = color_pp `White
  let green              = color_pp `Green
  let bright_white x     = fmt "\027[1;37m%s\027[0m" x
  let bright_blue x      = fmt "\027[1;34m%s\027[0m" x
  let bright_magenta x   = fmt "\027[1;35m%s\027[0m" x
  let violet x           = fmt "\027[0;34m%s\027[0m" x
  let bright_red x       = fmt "\027[1;31m%s\027[0m" x
  let bright_green x     = fmt "\027[1;32m%s\027[0m" x
  let start_bright_white = fmt "\027[1;37m"
  let start_white        = fmt "\027[37m"
  let end_color          = "\027[0m"
  let italic x           = "\027[3m" ^ x ^ "\027[0m"
  let underline x        = "\027[4m" ^ x ^ "\027[0m"
  let blink x            = "\027[5m" ^ x ^ "\027[0m"
end


module type Testable = sig
  type t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

type 'a testable = (module Testable with type t = 'a)

module Testable = struct
  let pp (type a) (t: a testable) = let (module T) = t in T.pp
  let equal (type a) (t: a testable) = let (module T) = t in T.equal
end


let time ?fmt f x =
  let t0 = Unix.gettimeofday () in
  let fx = f x in
  let t1 = Unix.gettimeofday () -. t0 in
  let () = match fmt with
  | Some fmt -> Printf.eprintf "%s\n" (fmt fx t1)
  | None     -> Printf.eprintf "Elapsed time: %f sec\n" t1 in
  fx

let test ?(verbose = true) ty msg ~actual ~expected () =
  let ok = Testable.equal ty actual expected in
  begin if not ok then begin
    Fmt.pr "  %s %s@." (C.bright_red "✗") (C.bright_white msg);
    Fmt.pr "    - %a@." (Testable.pp ty) expected;
    Fmt.pr "    + %a@." (Testable.pp ty) actual
  end else if verbose then
    Fmt.pr "  %s %s@." (C.bright_green "✓") (C.bright_white msg)
  end;
  ok


let raises ?(verbose = true) msg ~exn f () =
  let state =
    try let _ = f () in `No_exn
    with e -> if e = exn then `Ok else `Other e in
  match state with
  | `No_exn ->
    Fmt.pr "  %s %s@." (C.bright_red "✗") (C.bright_white msg);
    Fmt.pr "    * did not raise %s@." (Printexc.to_string exn);
    false
  | `Other actual ->
    Fmt.pr "  %s %s@." (C.bright_red "✗") (C.bright_white msg);
    Fmt.pr "    - %s@." (Printexc.to_string exn);
    Fmt.pr "    + %s@." (Printexc.to_string actual);
    false
  | `Ok ->
    if verbose then
      Fmt.pr "  %s %s@." (C.bright_green "✓") (C.bright_white msg);
    true


let testable (type a) ?(equal: a -> a -> bool = (=)) (pp: a Fmt.t) : a testable =
  let module M = struct
    type t = a
    let pp = pp
    let equal = equal
  end in
  (module M)


let group name tests =
  Fmt.pr "━━━ %s ━━━@." (C.bright_blue name);
  let t0 = Unix.gettimeofday () in
  let s, f =
    List.fold_left begin fun (s, f) test ->
        if test () then (s + 1, f) else (s, f + 1)
      end
      (0, 0) tests in
  let t = Unix.gettimeofday () -. t0 in
  let msg =
    match s, f with
    | 1, 0 -> "Test passed"
    | s, 0 -> fmt "%d tests passed" s
    | 0, 1 -> "Test failed"
    | 0, f -> fmt "%d tests failed" f
    | s, f -> fmt "%d tests passed, %d tests failed" s f in
  Fmt.pr "  %s %s in %0.2fms@." (C.bright_magenta "•") msg (t *. 1000.0)

let int    : 'a testable = testable Fmt.int
let float  : 'a testable = testable Fmt.float
let char   : 'a testable = testable Fmt.char
let bool   : 'a testable = testable Fmt.bool
let unit   : 'a testable = testable (Fmt.unit "()")
let int32  : 'a testable = testable ~equal:Int32.equal Fmt.int32
let int64  : 'a testable = testable ~equal:Int64.equal Fmt.int64
let string : 'a testable = testable ~equal:String.equal Fmt.string


let list e =
  let rec equal l1 l2 =
    match (l1, l2) with
    | (x::xs, y::ys) -> Testable.equal e x y && equal xs ys
    | ([], []) -> true
    | _ -> false in
  testable (Fmt.Dump.list (Testable.pp e)) ~equal

let sorted_list (type a) (a : a testable) compare =
  let l = list a in
  let equal l1 l2 = Testable.equal l (List.sort compare l1) (List.sort compare l2) in
  testable (Testable.pp l) ~equal

let array e =
  let equal a1 a2 =
    let (m, n) = Array.(length a1, length a2) in
    let rec go i = i = m || (Testable.equal e a1.(i) a2.(i) && go (i + 1)) in
    m = n && go 0 in
  testable (Fmt.Dump.array (Testable.pp e)) ~equal

let pair a b =
  let equal (a1, b1) (a2, b2) =
    Testable.equal a a1 a2 && Testable.equal b b1 b2 in
  testable (Fmt.Dump.pair (Testable.pp a) (Testable.pp b)) ~equal

let option e =
  let equal x y =
    match (x, y) with
    | (Some a, Some b) -> Testable.equal e a b
    | (None, None) -> true
    | _ -> false in
  testable (Fmt.Dump.option (Testable.pp e)) ~equal

let result a e =
  let equal x y =
    match (x, y) with
    | (Ok x, Ok y) -> Testable.equal a x y
    | (Error x, Error y) -> Testable.equal e x y
    | _ -> false in
  testable (Fmt.Dump.result ~ok:(Testable.pp a) ~error:(Testable.pp e)) ~equal

