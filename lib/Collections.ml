
(** Hey *)

module type Iterable_base = sig
  type 'a t
  type 'a state

  val init : unit -> 'a state
  val next : 'a state -> ('a * 'a state) option
  val stop : 'a state -> 'a t
end

(** {1 Hey} *)
module type Iterable_extension = sig
  type 'a t

  val all : ('a -> bool) -> 'a t -> bool
  val any : ('a -> bool) -> 'a t -> bool
  val elem : 'a -> 'a t -> bool
  val elem_index : 'a -> 'a t -> int option
  val elem_indices : 'a -> 'a t -> int list
  val is_empty : 'a t -> bool
  val find : ('a -> bool) -> 'a t -> 'a t
  val find_index : ('a -> bool) -> 'a list -> int option
  val find_indices : ('a -> bool) -> 'a list -> int list
  val find_max : ?by: ('a -> 'a -> int) -> 'a t -> 'a
  val find_min : ?by: ('a -> 'a -> int) -> 'a t -> 'a
  val each : ('a -> unit) -> 'a t -> unit
  val nth : int -> 'a t -> 'a option
  val head : 'a t -> 'a option
  val last : 'a t -> 'a option
  val bool_or : bool t -> bool
  val bool_and : bool t -> bool
  val reduce : ('r -> 'a -> 'r) -> 'a t -> 'r option
  val length : 'a t -> int
  val fold_left : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
  val fold_right : ('a -> 'r -> 'r) -> 'r -> 'a t -> 'r
  val fold_while : ('r -> 'a -> [ `Continue of 'r | `Break of 'r ]) -> 'r -> 'a t -> 'r
  val sum : float t -> float
  val product : float t -> float
  val mean : float t -> float

  val to_list : 'a t -> 'a list
  val to_seq : 'a t -> 'a Seq.t
  val to_array : 'a t -> 'a array
  val to_queue : 'a t -> 'a Queue.t
  val to_string : char t -> string
end



module type Reducible_base = sig
  type 'a t
  type 'a state

  val init : unit -> 'a state
  val step : 'a state -> 'a -> 'a state
  val full : 'a state -> bool
  val stop : 'a state -> 'a t
end


module type Reducible_extension = sig
  type 'a t

  val unfold : 's -> ('s -> ('a * 's) option) -> 'a t
  val one : 'a -> 'a t
  val two : 'a -> 'a -> 'a t
  val three : 'a -> 'a -> 'a -> 'a t
  val count : int -> int t
  val repeat : 'a -> 'a t
  val replicate : int -> 'a -> 'a t
  val cycle : 'a t -> 'a t
  val iterate : ('a -> 'a) -> 'a -> 'a t
  val of_list : 'a list -> 'a t
end


module Reducible_extend (Base : Reducible_base) = struct
  let unfold seed next =
    let rec loop s r =
      match next s with
      | Some (x, s') -> loop s' (Base.step r x)
      | None -> r in
    Base.stop (loop seed (Base.init ()))


  let of_list l =
    let rec loop s r =
      match s with
      | []    -> r
      | x::s' -> loop s' (Base.step r x) in
    Base.stop (loop l (Base.init ()))


  let init n f =
    let next count =
      if count <= 0 then None
      else Some (f count, count - 1) in
    unfold n next


  let replicate n x = init n (fun _ -> x)


  let one x = replicate 1 x
  let two x y = of_list [x; y]
  let three x y z = of_list [x; y; z]

  let iterate _f _x = failwith "TODO"

  let cycle _xs = failwith "TODO"
  let repeat _x = failwith "TODO"
  let count _n  = failwith "TODO"
end

