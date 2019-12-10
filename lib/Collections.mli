
module type Iterable_base = sig
  type 'a t
  type 'a state

  val init : unit -> 'a state
  val next : 'a state -> ('a * 'a state) option
  val stop : 'a state -> 'a t
end

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


(** Describes values that can be built by incrementally consuming values.

    {4 Examples}

    {[
    module List_reducible = struct
      type 'a t = 'a list
      type 'a state = 'a list

      let init () = []
      let step x state = x :: state
      let full _ = false
      let stop state = List.rev state
    end
    ]} *)
module type Reducible_base = sig
  type 'a t
  type 'a state

  val init : unit -> 'a state
  val step : 'a state -> 'a -> 'a state
  val full : 'a state -> bool
  val stop : 'a state -> 'a t
end

(* Extension module with operations defined from the base implementation. *)
module type Reducible_extension = sig
  type 'a t

  val unfold : 's -> ('s -> ('a * 's) option) -> 'a t
  (** [unfold seed next] is a finite source created from a [seed] state and a
      function that produces elements and an updated state.

      This is an alias for {!val:finite}. *)

  val one : 'a -> 'a t
  (** [one a] is a source with a single element [a]. *)

  val two : 'a -> 'a -> 'a t
  (** [two a b] is a source with two elements [a] and [b]. *)

  val three : 'a -> 'a -> 'a -> 'a t
  (** [three a b c] is a source with elements [a], [b] and [c]. *)

  val count : int -> int t
  (** [count n] is an infinite source with integers starting from [n]. *)

  val repeat : 'a -> 'a t
  (** [repeat x] produces a container by repeating [x] ad infinitum. *)

  val replicate : int -> 'a -> 'a t
  (** [replicate n x] is a collection with the element [x] replicated [n] times. *)

  val cycle : 'a t -> 'a t
  (** Repeat a collection cyclically ad infinitum. *)

  val iterate : ('a -> 'a) -> 'a -> 'a t
  (** [iterate f x] is an infinite source where the first item is calculated by
      applying [f] to [x], the second item by applying the function on the
      previous result and so on. *)

  val of_list : 'a list -> 'a t
  (** [of_list items] is a collection with all elements in the list [items]. *)
end

module Reducible_extend (Base : Reducible_base) : Reducible_extension with type 'a t := 'a Base.t
