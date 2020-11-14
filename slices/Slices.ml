

type 'a slice =
  Slice : {
    init : unit -> 's;
    get  : int -> 's -> 'a option;
    stop : 's -> unit;
  } -> 'a slice



