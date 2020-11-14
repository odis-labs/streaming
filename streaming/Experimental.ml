

module Pipes : sig
  module Reader : sig
    type t = Reader : {
      init : unit -> 'r;
      push : 'r -> 'a -> 'r;
    } -> t
  end

  module Writer : sig
    type t

    val complete : t -> unit
  end

  type 'a pipe

  val pipe : unit -> 'a pipe

  val reader : 'a pipe -> Reader.t
  val writer : 'a pipe -> Writer.t
end = struct

  module Reader = struct
    type t = Reader : {
      init : unit -> 'r;
      push : 'r -> 'a -> 'r;
    } -> t
  end

  module Writer = struct
    type t = unit

    let complete _w = ()
  end

  type 'a pipe = unit


  let pipe () = ()
  let reader _p = failwith ""
  let writer _p = ()
end


module Use_pipes = struct
  let write_some_data w =
    ignore w

  let read_some_data r =
    ignore r

  let test () =
    let pipe = Pipes.pipe () in
    let writer = Pipes.writer pipe in
    write_some_data writer;
    Pipes.Writer.complete writer;
    read_some_data (Pipes.reader pipe);
end


module Span = struct
  type t =
    | Bytes : bytes -> t
    | Virtual : (int -> char) -> t

end


module Use_socket = struct
  (* let stream_from_socket socket = *)
  (*   let Sink k = Sink.buffer 10 in *)
  (*   let buf = k.init () in *)
  (*   Unix.read socket buf 0 1024; *)
end
