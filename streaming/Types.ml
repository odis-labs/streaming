type +'a source =
  | Source : {
      init : unit -> 's;
      pull : 's -> ('a * 's) option;
      stop : 's -> unit;
    }
      -> 'a source

type ('a, 'r) sink =
  | Sink : {
      init : unit -> 's;
      push : 's -> 'a -> 's;
      full : 's -> bool;
      stop : 's -> 'r;
    }
      -> ('a, 'r) sink

type 'a stream = { stream : 'r. ('a, 'r) sink -> 'r } [@@unboxed]

type ('a, 'b) flow = { flow : 'r. ('b, 'r) sink -> ('a, 'r) sink } [@@unboxed]
