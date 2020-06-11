
(** Streaming abstractions that combine, transform and reduce large amounts of
sequential data efficiently, in constant space and without leaking resources. *)

(** {1:sources Sources}

    Sources are decoupled producer of values.

    Elements are pulled from a source when needed. A source can have an internal
    state that will be lazily initialized when (and if) a consumer requests
    elements. The internal state will be safely disposed when the source runs
    out of elements, when the consumer terminates, or if an exception is raised
    at any point in the streaming pipeline.

    Sources are a great way to define decoupled producers that can be consumed
    with {!val:Stream.from}. To learn more about how to create sources see
    {{:Source/index.html#creating-a-source} "Creating a source"}.

    The following example creates a source that counts down to zero:

    {[
      let countdown n =
        let init () = n in
        let pull i =
          if i = 0 then None
          else Some (i, i - 1) in
        Source.make ~init ~pull ()
    ]}

    It can be consumed with:

    {[
    # Stream.(from (countdown 3) |> into Sink.sum)
    - : int = 6
    ]} *)


(** Type for sources that produce elements of type ['a]. *)
type 'a source


(** Module with defintions for sources.

    Elements are pulled from a source when needed. A source can have an internal
    state that will be lazily initialized when (and if) a consumer requests
    elements. The internal state will be safely disposed when the source runs
    out of elements, when the consumer terminates, or if an exception is raised
    at any point in the streaming pipeline.

    Sources are a great way to define decoupled producers that
    can be consumed with {!val:Stream.from}.

    Sources are "single shot" and will haver their input exhausted by most
    operations. Consider {{!val:Sink.buffer} buffering} sources if you need to
    reuse their input. *)
module Source : sig

  type 'a t = 'a source
  (** The type for sources that produce elements of type ['a]. *)

  (** {1 Creating a source}

    Implementing your own custom sources enables access to many useful
    operations. The most flexible way to create a source is with the
    {!val:Source.make} function.

    The following example creates a source that counts down to zero:

    {[
      let countdown n =
        let init () = n in
        let pull i =
          if i = 0 then None
          else Some (i, i - 1)) in
        Source.make ~init ~pull
    ]}

    Alternatively, existing {!val:list}/{!val:array}/{!val:seq}/{!val:string}
    sources, or others listed below, can be used. *)

  val empty : 'a t
  (** [zero] is an empty source. *)

  val single : 'a -> 'a t
  (** [single a] is a source with a single element [a]. *)

  val list : 'a list -> 'a t
  (** [list items] is a source with all elements from the [items] list. *)

  val seq : 'a Seq.t -> 'a t
  (** [seq items] is a source with all elements from the [items] sequence. *)

  val array : 'a array -> 'a t
  (** [array items] is a source with all elements from the [items] array. *)

  val string : string -> char t
  (** [string str] is a source with all characters from the [str] string. *)

  val bytes : bytes -> char t
  (** [bytes b] is a source with all characters from the [b] bytes. *)

  val queue : 'a Queue.t -> 'a t
  (** [queue q] is a source with all characters from the [q] queue. *)

  val generate : len:int -> (int -> 'a) -> 'a t
  (** [generate ~len f] generates a source of length [len] mapping each index to an
      element with [f]. *)

  val count : int -> int t
  (** [count n] is an infinite source with integers starting from [n]. *)

  val iterate : 'a -> ('a -> 'a) -> 'a t
  (** [iterate x f] is an infinite source where the first item is calculated by
      applying [f] to [x], the second item by applying the function on the
      previous result and so on. *)

  val unfold : 's -> ('s -> ('a * 's) option) -> 'a t
  (** [unfold seed next] is a finite source created from a [seed] state and a
      function that produces elements and an updated state. *)

  val make : init:(unit -> 's) -> pull:('s -> ('a * 's) option) ->
    ?stop:('s -> unit) -> unit -> 'a t
  (** [make ~init ~pull ~stop ()] is a value source created from the [init], [pull]
      and [stop]. This function is similar to {!val:unfold} but
      without lazy state initialization and state termination functions.

      {b Note}: For better performance, it is recommended that the [pull]
      function caches the termination condition in case it is expensive. *)


  (** {1 Zipping sources} *)

  val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [zip_with f src1 src2] is a source that pulls elements from [src1] and
      [src2] one by one, combining them with [f]. *)

  val zip : 'a t -> 'b t -> ('a * 'b) t
  (** [zip src1 src2] is a source of pairs with elements elements pulled from
      [src1] and [src2] one by one.

      Equivalent to [zip_with (fun x y -> (x, y)) src1 src2]. *)


  (** {1 Transforming a source}

      {b Note}: Instead of applying the transformation functions at the source,
      consider using {!val:Stream.from} or defining your compuation as a
      {!module:Flow} to make it reusable. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** A source with all elements transformed with a mapping function. *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** A source that includes only the elements that satisfy a predicate. *)

  val take : int -> 'a t -> 'a t
  (** Take first [n] elements from the source and discard the rest. *)

  (* val take_last : int -> 'a t -> 'a t *)
  (** Similar to {!val:take} but takes the last [n] elements. *)

  val take_while : ('a -> bool) -> 'a t -> 'a t
  (** Take first elements from the source that satisfy a predicate and discard
      the rest. *)

  val drop : int -> 'a t -> 'a t
  (** Drop first [n] elements from the source and keep the rest. *)

  (* val drop_last : int -> 'a t -> 'a t *)
  (** Similar to {!val:drop} but drops the last [n] elements. *)

  val drop_while : ('a -> bool) -> 'a t -> 'a t
  (** Drpo first elements from the source that satisfy a predicate and keep
      the rest. *)


  (** {1 Consuming a source}

      Many consumers are available in the {!module:Sink} module. You can consume any source using a sink with:

      {[
      let source = Source.count 10 in
      source
      |> Stream.from
      |> Stream.into Sink.last
      ]}

      Alternatively use the source consumers below for simple operations. *)

  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
  (** [fold step init source] reduces the values of [source] with the [step]
      function, starting with [init].

      If the [step] function raises an exception, the source will be properly
      terminated. *)

  val len : 'a t -> int
  (** [len src] is the count of elements in [src]. *)

  val each : ('a -> unit) -> 'a t -> unit
  (** [each f src] applies an effectful function [f] to all elements in [src]. *)


  (** {1 Resource handling} *)

  val dispose : 'a t -> unit
  (** [dispose source] forces the termination of the source state. This
      function is useful in situations when a leftover source is
      produced in {!val:Stream.run}.

      {b Note:} If the source is not already initialized, calling this function
      will first initialize its state before it is terminated. *)
end



(** {1:sinks Sinks}

  Sinks are decoupled consumer of values.

  Sinks are streaming abstractions that consume values and produce an
  aggregated value as a result. The result value is extracted from an internal
  state that is built incrementally. The internal state can aquire resources
  that are guaranteed to be terminated when the sink is filled.

  Sinks are a great way to define decoupled consumers that can be filled with
  {!val:Stream.into}. To learn more about how to create sinks see
  {{:Sink/index.html#creating-a-sink} "Creating a sink"}.

  The following example demonstrates a sink that consumes all elements into a list:

  {[
    let list_sink =
      let init () = [] in
      let push acc x = x :: acc in
      let stop acc = List.rev acc in
      Sink.make ~init ~push ~stop ()
  ]}

  It can be used with:

  {[
    # Stream.(iota 5 |> into list_sink)
    - : int list = [0; 1; 2; 3; 4]
  ]} *)

(** Type for sinks that consume elements of type ['a] and, once done, produce
    a value of type ['b]. *)
type ('a, 'b) sink


(** Module with defintions for sinks.

    Sinks are streaming abstractions that consume values and produce an
    aggregated value as a result. The result value is extracted from an internal
    state that is built incrementally. The internal state can aquire resources
    that are guaranteed to be terminated when the sink is filled.

    Sinks are a great way to define decoupled consumers that can be filled with
    {!val:Stream.into}.

    Sinks are independent from sources and streams. You can think of them as
    packed arguments for folding functions with early termination. Formally,
    they can also be interpreted as
    {{:https://en.wikipedia.org/wiki/Moore_machine} Moore machine}. *)
module Sink : sig

  (*
    TODO: Move to the "Performance considerations" section.
    - The calls to `full` should be cheap as this function will be called to
    avoid allocation of unnecessary resources. If the computation required to
    decide if the reducer is full is expensive, consider caching it whenever
    possible.

    - If the producer's initialization is cheap, it should assume that
    reducer's initialization is expensive and, if possible, avoid calling init
    on the reducer.

    - If the producer's initialization is expensive, it should assume that
    reducer's initialization is cheap and, if possible, avoid it's own
    initialization.


    Consider adding explicit implementations for:
      - [ ] Category
      - [ ] Monoid
      - [ ] Profunctor
      - [ ] Strong (first, second)
      - [ ] Choice (left, right)
      - [ ] Alternative
      - [ ] Bifunctor
      - [ ] Comonad
      - [ ] Functor
      - [x] Applicative

    Consider adding:
      - lift
      - before
      - after
      - await?
      - or_else
      - recover
      - repeat

    Is this possible?
      let* x = Sink.await in
      let* y = Sink.await in
      Sink.return (x + y)
  *)

  type ('a, 'b) t = ('a, 'b) sink
  (** Type for sinks that consume elements of type ['a] and, once done, produce
      a value of type ['b]. *)

  (** {1 Creating a sink}

    Implementing custom sinks is useful to create a collection of reusable
    streaming consumers for your application.

    The following example demonstrates a sink that consumes all elements into a list:

    {[
      let list_sink =
        let init () = [] in
        let push acc x = x :: acc in
        let stop acc = List.rev acc in
        Sink.make ~init ~push ~stop ()
    ]}

    Alternatively, existing {!val:list}/{!val:array}/{!val:string}/{!val:queue}
    sinks, or others listed below, can be used. *)

  val fill : 'r -> ('a, 'r) t
  (** [fill result] use [result] to fill the sink. This sink will not consume
      any input and will immediately produce [result] when used. *)

  val fold : ('r -> 'a -> 'r) -> 'r -> ('a, 'r) t
  (** [fold f init] is a sink that reduces all input elements with the stepping
      function [f] starting with the accumulator value [init]. *)

  val fold_while : ('r -> bool) -> ('r -> 'a -> 'r) -> 'r -> ('a, 'r) t
  (** [fold_while full f init] is similar to [fold] but can terminate early if
      [full] returns [true]. *)

  val make
    : init:(unit -> 'acc)
    -> push:('acc -> 'a -> 'acc)
    -> ?full:('acc -> bool)
    -> stop:('acc -> 'r)
    -> unit
    -> ('a, 'r) t
  (** Creates a sink from a function that [init]ializes a state value, a
      [step]ping function to update that state and a [stop] function that
      produces the final result value. Optionally a [full] function can be
      passed to decide when the sink should terminate early.

      {b Note:} The calls to [full] should be cheap as this function will be
      called to avoid allocation of unnecessary resources. If the computation
      required to decide if the sink is full is expensive, consider caching it
      whenever possible. *)


  (** {1 Basic sinks} *)

  val full : ('a, unit) t
  (** A full sink that will not consume any input and will not produce any
      results. *)

  val is_empty : ('a, bool) t
  (** [is_empty] is [true] if the sink did not consume any elements and [false]
      otherwise. *)

  val each : ('a -> unit) -> ('a, unit) t
  (** Applies an effectful action to all input elements producing nothing. *)

  val len : ('a, int) t
  (** Consumes and counts all input elements. *)

  val first : ('a, 'a option) t
  (** The first input element, or [None] if the sink did not receive enough
      input.

      Equivalent to [nth 0]. *)

  val last : ('a, 'a option) t
  (** The last input element, or [None] if the sink did not receive enough
      input. *)

  val nth : int -> ('a, 'a option) t
  (** The n-th input element, or [None] if the sink did not receive enough
      input. *)

  val drain : ('a, unit) t
  (** Consumes all elements producing nothing. Useful for triggering actions in
      effectful streams. *)


  (** {1 Finding elements} *)

  val contains : where:('a -> bool) -> ('a, bool) t
  (** [contains ~where:pred] finds the first element that satisfies [pred]
      returning [None] if there is no such element. *)

  val find : where:('a -> bool) -> ('a, 'a option) t
  (** [find ~where:pred] finds the first element that satisfies [pred]
      returning [None] if there is no such element. *)

  val index : where:('a -> bool) -> ('a, int option) t
  (** Similar to [find] but returns the index of the element that
      satisfies the predicate. *)

  val minimum : by:('a -> 'a -> bool) -> ('a, 'a option) t
  (** Finds the minimum element in the sequence, using the given predicate as
      as the comparison between the input elements. *)

  val maximum : by:('a -> 'a -> bool) -> ('a, 'a option) t
  (** Finds the maximum element in the sequence, using the given predicate as
      as the comparison between the input elements. *)


  (** {1 Logical predicates} *)

  val all : where:('a -> bool) -> ('a, bool) t
  (** [all ~where:pred] is [true] if all input element satisfy [pred]. Will
      stop consuming elements when the first element that does not satisfy
      [pred] is found. Results in [true] for empty input. *)

  val any : where:('a -> bool) -> ('a, bool) t
  (** [any ~where:pred] is [true] if at least one input element satisfies
      [pred]. Will stop consuming elements when such an element is found.
      Results in [false] for empty input. *)


  (** {1 Data sinks} *)

  val list : ('a, 'a list) t
  (** Puts all input elements into a list. *)

  val array : ('a, 'a array) t
  (** Puts all input elements into an array. *)

  val buffer : int -> ('a, 'a array) t
  (** Similar to {!val:array} buf will only consume [n] elements. *)

  val queue : ('a, 'a Queue.t) t
  (** Puts all input elements into a queue. *)

  val string : (string, string) t
  (** Consumes and concatenates strings. *)

  val bytes : (bytes, bytes) t
  (** Consumes and concatenates bytes. *)


  (** {1 IO sinks} *)

  val print : (string, unit) t
  (** Prints all input string elements to standard output as lines. *)

  val file : string -> (string, unit) t
  (** [file path] is a sink that writes input strings as lines into a file
      located at [path]. *)

  val stdout : (string, unit) t
  (** A sink that writes input strings as lines to STDOUT. *)

  val stderr : (string, unit) t
  (** A sink that writes input strings as lines to STDERR. *)


  (** {1 Numeric compuations} *)

  val sum : (int, int) t
  (** Adds all input integer values. *)

  val product : (int, int) t
  (** Product of input integer values. Stops if any input element is [0]. *)

  val mean : (float, float) t
  (** Computes a numerically stable arithmetic mean of all input elements. *)


  (** {1 Combining sinks} *)

  val zip : ('a, 'r1) t -> ('a, 'r2) t -> ('a, 'r1 * 'r2) t
  (** [zip left right] computes both [left] and [right] at the same time
      with the same input being sent to both sinks. The results of both sinks
      are produced. *)

  val zip_left : ('a, 'r) t -> ('a, _) t -> ('a, 'r) t
  (** [zip_left left right] similar to {!val:zip}, but only produces the
      result of the [left] sink. *)

  val zip_right : ('a, _) t -> ('a, 'r) t -> ('a, 'r) t
  (** [zip_left left right] similar to {!val:zip}, but only produces the
      result of the [right] sink. *)

  val zip_with : ('r1 -> 'r2 -> 'r) -> ('a, 'r1) t -> ('a, 'r2) t -> ('a, 'r) t
  (** [zip_with f left right] similar to {!val:zip}, but applies an aggregation
      function to results produced by [left] and [right]. *)

  val (<&>) : ('a, 'r1) t -> ('a, 'r2) t -> ('a, 'r1 * 'r2) t
  (** [left <&> right] is an operator version of [zip left right]. *)

  val (<&) : ('a, 'r) t -> ('a, _) t -> ('a, 'r) t
  (** [left <& right] is an operator version of [zip_left left right]. *)

  val (&>) : ('a, _) t -> ('a, 'r) t -> ('a, 'r) t
  (** [left &> right] is an operator version of [zip_right left right]. *)

  val unzip : ('a, 'r1) t -> ('b, 'r2) t -> ('a * 'b, 'r1 * 'r2) t
  (** [unzip left right] is a sink that receives pairs ['a * 'b], sending the
      first element into [left] and the second into [right]. Both sinks are
      computed at the same time and their results returned as an output pair.

      The sink becomes full when either [left] or [right] get full. *)

  val unzip_left : ('a, 'r) t -> ('b, _) t -> ('a * 'b, 'r) t
  (** [unzip_left left right] is similar to {!val:unzip}, but only produces the
      result of the [left] sink.

      If [right] terminates first, [left] will be forced to terminate. *)

  val unzip_right : ('a, _) t -> ('b, 'r) t -> ('a * 'b, 'r) t
  (** [unzip_left left right] is similar to {!val:unzip}, but only produces the
      result of the [right] sink.

      If [left] terminates first, [right] will be forced to terminate. *)

  val unzip_with : ('r1 -> 'r2 -> 'r) -> ('a, 'r1) t -> ('b, 'r2) t -> ('a * 'b, 'r) t
  (** [unzip_with f left right] similar to {!val:unzip}, but applies an
      aggregation function to results produced by [left] and [right]. *)

  val (<*>) : ('a, 'r1) t -> ('b, 'r2) t -> ('a * 'b, 'r1 * 'r2) t
  (** [left <*> right] is an operator version of [unzip left right]. *)

  val (<* ) : ('a, 'r) t -> ('b, _) t -> ('a * 'b, 'r) t
  (** [left <* right] is an operator version of [unzip_left left right]. *)

  val ( *> ) : ('a, _) t -> ('b, 'r) t -> ('a * 'b, 'r) t
  (** [left *> right] is an operator version of [unzip_right left right]. *)

  val distribute : ('a, 'r1) t -> ('a, 'r2) t -> ('a, 'r1 * 'r2) t
  (** [distribute left right] is similar to [zip] but distributes the
      consumed elements over [left] and [right] alternating in a round robin
      fashion. *)

  (** Type for {!val:race} result values. *)
  type ('a, 'b) race =
    | Left of 'a
    | Right of 'b
    | Both of 'a * 'b

  val race : ('a, 'r1) t -> ('a, 'r2) t -> ('a, ('r1, 'r2) race) t
  (** [race left right] runs both [left] and [right] sinks at the same time
      producing the result for the one that fills first.

      If the sink is terminated prematurely, before either [left] or [right]
      are filled, {{:#type-race.Both} [Both]} of their values are produced.

      {4 Examples}

      {[
      let sink = Sink.(race (find ~where:(fun x -> x > 10)) (nth 8)) in
      let result = Stream.of_list [1; 9; 0; 8; 30; 4] |> Stream.into sink in
      assert (result = Sink.Left (Some 30))
      ]} *)

  val (<|>) : ('a, 'r1) t -> ('a, 'r2) t -> ('a, ('r1, 'r2) race) t
  (** [left <|> right] is the operator version of [race left right]. *)

  val seq : ('a, 'r1) t -> ('a, 'r2) t -> ('a, 'r1 * 'r2) t
  (** [seq left right] runs [left] and then [right] sequentially producing both of their results.

      If the resulting sink is stopped before [right] was started, it will be
      forced to initialize and terminate. *)

  val seq_left : ('a, 'r) t -> ('a, _) t -> ('a, 'r) t
  (** [seq_left left right] is similar to [seq], but only produces the result
      of the [left] sink. *)

  val seq_right : ('a, _) t -> ('a, 'r) t -> ('a, 'r) t
  (** [seq_right left right] is similar to [seq], but only produces the result
      of the [right] sink. *)

  val (<+>) : ('a, 'r1) t -> ('a, 'r2) t -> ('a, 'r1 * 'r2) t
  (** [left <+> right] is an operator version of [seq left right]. *)

  val (<+) : ('a, 'r) t -> ('a, _) t -> ('a, 'r) t
  (** [left <+ right] is an operator version of [seq_left left right]. *)

  val (+>) : ('a, _) t -> ('a, 'r) t -> ('a, 'r) t
  (** [left +> right] is an operator version of [seq_right left right]. *)


  (** {1 Mapping and filtering sinks} *)

  val map : ('r1 -> 'r2) -> ('a, 'r1) t -> ('a, 'r2) t
  (** [map f sink] is a sink [sink] with the result transformed with [f]. *)

  val ( <@> ) : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
  (** [f <@> sink] is the operator version of [map f sink]. *)

  val premap : ('b -> 'a) -> ('a, 'r) t -> ('b, 'r) t
  (** [premap f sink] is a sink that {e premaps} the input values.


      {4 Examples}

      If [sink] consumes integers, but we have an input with strings, we can
      provide a conversion from strings to integers to [premap]:

      {[
      let sink = Sink.(premap int_of_string sum) in
      let result = Stream.of_list ["1"; "2"; "3"] |> Stream.into sink in
      assert (result = 6)
      ]} *)

  val prefilter : ('a -> bool) -> ('a, 'r) t -> ('a, 'r) t
  (** [prefilter predicate sink] is a sink that filter the input value for
      [sink]. *)


  (** {1 Resource management} *)

  val dispose : ('a, 'r) t -> 'r
  (** Close the sink and produce the currently accumulated result. Any internal
      state will be terminated. *)


  (** {1 Syntax definitions}

      In addition to using the sinks and operations defined above, it is
      possible to create sinks with a convenient [(let)] notation.

      A common example of a composed sink is the sink that computes the
      arithmetic mean:

      {[
      let mean =
        let open Sink.Syntax in
        let+ total = Sink.sum
        and+ count = Sink.len in
        total / count
      ]}

      The resulting sink has type [(int, int) sink] and will only consume the
      input once! *)

  (** Module with syntax definitions for sinks. *)
  module Syntax : sig
    val (let+) : ('c, 'a) t -> ('a -> 'b) -> ('c, 'b) t
    val (and+) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  end

  (** {1 Interface implementations} *)

  (** Module that implements the "Functor" interface. *)
  module Functor : sig
    type ('input, 'a) t = ('input, 'a) sink

    val map : ('a -> 'b) -> ('input, 'a) t -> ('input, 'b) t
  end

  (** Module that implements the "Applicative" interface. *)
  module Applicative : sig
    type ('input, 'a) t = ('input, 'a) sink

    val pure : 'a -> ('b, 'a) t

    val ( <*> ) : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t
    (** [f <*> sink] is a function application for functions contained in a sink.

        This operator can be combined with {!val:(<@>)} to run multiple sinks at
        the same time:

        {[
        let mean = Sink.((/) <@> sum <*> len)
        ]}

        Which is equivalent to the following version without operators:

        {[
        let mean = Sink.(map (fun (total, count) -> total / count) (both sum len))
        ]} *)
  end
end



(** {1:flows Flows}

    Flows are decoupled transformers of values.

    Flows define streaming transformation, filtering or groupping operations
    that are fully disconnected from input and output. Their implementation
    intercepts an internal folding function and modifies the input one value at
    a time.

    Flows are a great way to define decoupled transformations that can be used
    with {!val:Stream.via}.

    A flow can be applied to a stream with {!val:Stream.via}:

    {[
    # Stream.range 10 100
      |> Stream.via (Flow.map (fun x -> x + 1))
      |> Stream.into Sink.sum
    - : int = 4995
    ]}

    Flows can also be composed to form a pipeline:

    {[
    # let flow = Flow.(map (fun x -> x + 1) >> filter (fun x -> x mod 2 = 0)) in
      Stream.range 10 100
      |> Stream.via flow
      |> Stream.into Sink.sum
    - : int = 2475
    ]} *)

(** Stream transformers that consume values of type ['a] and produce values of
    type ['b]. *)
type ('a, 'b) flow


(** Module with definitions for flows.

  Flows define streaming transformation, filtering or groupping operations
  that are fully disconnected from input and output. Their implementation
  intercepts an internal folding function and modifies the input one value at
  a time.

  Flows are a great way to define decoupled transformations that can be used
  with {!val:Stream.via}.

  A flow can be applied to a stream with {!val:Stream.via}:

  {[
  # Stream.range 10 100
    |> Stream.via (Flow.map (fun x -> x + 1))
    |> Stream.into Sink.sum
  - : int = 4995
  ]}

  Flows can also be composed to form a pipeline:

  {[
  # let flow = Flow.(map (fun x -> x + 1) >> filter (fun x -> x mod 2 = 0)) in
    Stream.range 10 100
    |> Stream.via flow
    |> Stream.into Sink.sum
  - : int = 2475
  ]} *)
module Flow : sig
  type ('a, 'b) t = ('a, 'b) flow

  (** {1 Transforming a flow} *)

  val filter : ('a -> bool) -> ('a, 'a) t
  (** A flow that includes only the elements that satisfy a predicate. *)

  val map : ('a -> 'b) -> ('a, 'b) t
  (** A flow with all elements transformed with a mapping function. *)

  val take : int -> ('a, 'a) t
  (** Take first [n] elements from the source and discard the rest. *)

  (** {1 Buffering flow elements} *)

  val buffer : int -> ('a, 'a array) t
  (** Collects [n] elements into an array buffer. Once the buffer is full it is
      emmited as a stream item. *)


  (** {1 Flowing into a sink} *)

  val through : ('a, 'r) sink -> ('a, 'r) t
  (** [through sink] repeatedly processes incoming elements with [sink]
      producing computed results.

      {b Note:} The provided sink might consume the whole input if it is
      infinite, or if the input terminates before filling the sink. *)


  (** {1 Composing flows} *)

  val identity : ('a, 'a) t
  (** A neutral flow that does not change the elements. *)

  val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** Compose two flows to form a new flow. *)

  val (<<) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  (** Composes two flows from right to left.

      More precisely [f1 << f2] is [compose f1 f2]. *)

  val (>>) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  (** Composes two flows from left to right.

      More precisely [f1 >> f2] is [compose f2 f1]. *)
end



(** {1:streams Streams}

    Streams combine sources, sinks and flows into a flexible streaming toolkit.

    Stream is a purely functional abstraction for incremental, push-based,
    sequential processing of elements. Streams can be easily and efficiently
    transformed and concatenated.

    Stream operations do not leak resources. This is guaranteed in the presence
    of early termination (when not all stream elements are consumed) or in case
    of exceptions in the streaming pipeline.

    Streams are built to be compatible with {{:#sources} sources}, {{:#sinks}
    sinks} and {{:#flows} flows}. To create a stream that produces all elements
    from a source use {!val:Stream.from}, to consume a stream with a sink use
    {!val:Stream.into} and to transform stream elements with a flow use
    {!val:Stream.via}. For more sophisticated pipelines that might have source
    leftovers, {!val:Stream.run} can be used.

    A simple echo program that loops over standard input and prints every line
    to standard output until Ctrl-D is hit:

    {[
      # Stream.stdin |> Stream.stdout;;
      hello<Enter>
      hello
      world<Enter>
      world
      <Ctrl+d>
      - : unit = ()
    ]} *)

type 'a stream
(** Type for streams with elements of type ['a]. *)


(** Module with defintions for streams.

    Stream is a purely functional abstraction for incremental, push-based,
    sequential processing of elements. Streams can be easily and efficiently
    transformed and concatenated.

    Stream operations do not leak resources. This is guaranteed in the presence
    of early termination (when not all stream elements are consumed) and in case
    of exceptions in the streaming pipeline.

    Streams are built to be compatible with {{!module:Source} sources}, {{!module:Sink}
    sinks} and {{!module:Flow} flows}. To create a stream that produces all elements
    from a source use {!val:Stream.from}, to consume a stream with a sink use
    {!val:Stream.into} and to transform stream elements with a flow use
    {!val:Stream.via}. For more sophisticated pipelines that might have source
    leftovers, {!val:run} can be used.

    A simple echo program that loops over standard input and prints every line
    to standard output until Ctrl-D is hit:

    {[
      # Stream.stdin |> Stream.stdout;;
      hello<Enter>
      hello
      world<Enter>
      world
      <Ctrl+d>
      - : unit = ()
    ]} *)
module Stream : sig
  type 'a t = 'a stream
  (** Type for streams with elements of type ['a]. *)

  (*

  Monoid
    - zero
    - append

  Applicative
    - pure
    - apply

  Functor
      - map

  Monad
    - bind

  Alternative
    - empty
    - <|>

   *)

  (** {1 Creating a stream} *)

  val empty : 'a t
  (** Empty stream with no elements. *)

  val single : 'a -> 'a t
  (** [single a] is a stream with a single element [a]. *)

  val double : 'a -> 'a -> 'a t
  (** [double a b] is a stream with two elements: [a] and [b]. *)

  val triple : 'a -> 'a -> 'a -> 'a t
  (** [triple a b c] is a stream with elements: [a], [b] and [c]. *)

  val count : int -> int t
  (** [count n] is an infinite stream with integers starting from [n]. *)

  val range : ?by:int -> int -> int -> int t
  (** [range ~by:step n m] is a sequence of integers starting from [n] to
      [m] (excluding [m]) incremented by [step]. The range is open on the right
      side. *)

  val iota : int -> int t
  (** [iota n] is [range ~by:1 0 n], that is a range from [0] to [n]
      incremented by [1]. *)

  val (-<) : int -> int -> int t
  (** [n -< m] is [range n m]. *)

  val (--) : int -> int -> int t
  (** [n -- m] is [range n (m - 1)]. *)

  val generate : len:int -> (int -> 'a) -> 'a t
  (** [generate ~len f] generates a stream of length [n] mapping each index to an
      element with [f]. *)

  val repeat : ?times:int -> 'a -> 'a t
  (** [repeat ~times:n x] produces a stream by repeating [x] [n] times. If
      [times] is omitted, [x] is repeated {e ad infinitum}. *)

  val repeatedly : ?times:int -> (unit -> 'a) -> 'a t
  (** [repeatedly ~times:n f] produces a stream by repeatedly calling [f ()] [n]
      times. If [times] is omitted, [f] is called {e ad infinitum}. *)

  val iterate : 'a -> ('a -> 'a) -> 'a t
  (** [iterate x f] is an infinite source where the first item is calculated by
      applying [f] to [x], the second item by applying the function on the
      previous result and so on. *)

  val unfold : 's -> ('s -> ('a * 's) option) -> 'a t
  (** [unfold seed next] is a stream created from a [seed] state and a
      function that produces elements and an updated state. The stream will
      terminate when [next] produces [None]. *)

  val yield  : 'a -> 'a t
  (** [yield x] is a stream with a single element [x]. *)

  (** {1 Stream converters} *)

  val of_list : 'a list -> 'a t
  (** [of_list items] is a stream with all elements in the list [items]. *)

  val to_list : 'a t -> 'a list
  (** [to_list stream] converts [stream] into a list. *)

  val of_array : 'a array -> 'a t
  (** [of_array items] is a stream with all elements in the array [items]. *)

  val to_array : 'a t -> 'a array
  (** [to_array stream] converts [stream] into an array. *)

  val of_string : string -> char t
  (** [of_string string] is a stream with all characters in [string]. *)

  val to_string : char t -> string
  (** [to_string stream] converts [stream] of characters into a string. *)



  (** {1 Transformerming a stream} *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** A stream with all elements transformed with a mapping function. *)

  val filter : ('a -> bool) -> 'a t -> 'a t
  (** A stream that includes only the elements that satisfy a predicate. *)

  val take : int -> 'a t -> 'a t
  (** Take first [n] elements from the stream and discard the rest. *)

  val take_while : ('a -> bool) -> 'a t -> 'a t
  (** Take first elements from the stream that satisfy a predicate and discard
      the rest. *)

  val drop_while : ('a -> bool) -> 'a t -> 'a t
  (** Drpo first elements from the stream that satisfy a predicate and keep
      the rest. *)

  val drop : int -> 'a t -> 'a t
  (** Drop first [n] elements from the stream and keep the rest. *)

  val rest : 'a t -> 'a t
  (** Drops the first element of the stream. *)

  (* TODO *)
  (* val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t *)

  val indexed : 'a t -> (int * 'a) t
  (** Adds an index to each element in the stream. *)


  (** {1 Combining streams} *)

  val concat : 'a t -> 'a t -> 'a t
  (** [concat stream1 stream2] is a stream that exhausts all elements from
      [stream1] and then all elements from [stream2].

      {4 Examples}

      {[
        # let stream1 = Stream.of_list ['a'; 'b'; 'c'] in
          let stream2 = Stream.of_list ['d'; 'e'; 'f'] in
          Stream.to_list (Stream.concat stream1 stream2)
        - : char list = ['a'; 'b'; 'c'; 'd'; 'e'; 'f']
      ]} *)

  val (++) : 'a t -> 'a t -> 'a t
  (** [stream1 ++ stream2] is the infix operator version of [concat stream1 stream2]. *)

  val append : 'a -> 'a t -> 'a t
  (** [append x stream] adds the element [x] to the end of [stream]. *)

  val prepend : 'a -> 'a t -> 'a t
  (** [prepend x stream] adds the element [x] to the beginning of [stream]. *)

  val flatten : 'a t t -> 'a t
  (** Concatenates a stream of streams. *)

  val flat_map : ('a -> 'b t) -> 'a t -> 'b t
  (** [flat_map f stream] is a stream concatenated from sub-streams
      produced by applying [f] to all elements of [stream].

      {[
      let duplicated =
        [1; 2; 3]
        |> String.of_list
        |> String.flat_map (fun x -> Stream.of_list [x; x])
        |> Stream.to_list in
      assert (duplicated = [1; 1; 2; 2; 3; 3])
      ]} *)

  val cycle : ?times:int -> 'a t -> 'a t
  (** [cycle ~times:n stream] produces a stream by repeating all elements from
      [stream] [n] times. If [times] is omitted, [x] is repeated {e ad infinitum}. *)

  (* TODO: These two can only work with sources. *)
  (* val zip : 'a t -> 'b source -> ('a * 'b) t *)
  (* val interleave : 'a t -> 'a source -> 'a t *)

  val interpose : 'a -> 'a t -> 'a t
  (** Inserts a separator element between each stream element. *)


  (** {1 Groupping and splitting} *)

  (* TODO: Consider renaming this, "chunking a stream" sounds strange. *)
  (* Potential name: partition. *)
  (* TODO: Consider adding ?fill to fill the missing elements. *)
  (* See: https://clojuredocs.org/clojure.core/partition *)
  val partition : int -> 'a t -> 'a t t
  (** [partition n] partitions the stream into sub-streams of size [n]. *)

  (* TODO *)
  (* val group : by *)


  (* TODO: Add variants for splitting once. Consider renaming: divide. *)
  (* split, partition, divide, etc is too confusing. *)
  val split: by:('a -> bool) -> 'a t -> 'a t t
  (** [split ~by:predicate stream] splits [stream] whe *)



  (** {1 Consumers}

      Operations that traverse the the stream computing a single result value.

      If the stream is infinite and the consumer accumulates the elements, the
      processing will not terminate, potentially resulting in a memory leak. *)

  val len : 'a t -> int
  (** [len stream] counts the number of elements in [stream].

     Will exhaust the stream during processing.

      {4 Examples}

      {[
      # Stream.len (Stream.of_list ['a'; 'b'; 'c']);
      - : int = 3
      ]} *)

  val each : ('a -> unit) -> 'a t -> unit
  (** [each f stream] applies an effectful function [f] to all elements of
      [stream]. *)

  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r
  (** [fold step init stream] reduces the values of [stream] with the [step]
      function, starting with [init].

      If the [step] function raises an exception, the stream will be properly
      terminated. *)

  val is_empty : 'a t -> bool
  (** [is_empty stream] is [true] if the stream has no elements and [false]
      otherwise. This operations consumes the first elements of the stream. *)

  val first : 'a t -> 'a option
  (** Return the first element in the stream. *)

  val last : 'a t -> 'a option
  (** Return the last element in the stream, in linear time. *)

  val drain : 'a t -> unit

  (* Is this even possible? *)
  (* val peek : 'a t -> 'a option *)


  (** {1 IO Streams} *)

  val of_file : string -> string t
  (** [of_file path] is a stream of lines read from the file located at
      [path].

      The file is opened lazily only when the stream is consumed and will be
      closed even if the stream processing terminates with an exception. *)

  val to_file : string -> string t -> unit
  (** [to_file path stream] writes lines from [stream] into the file located at
      [path]. *)

  val stdin : string t
  (** The stream that reads lines from the standard input channel. *)

  val stdout : string t -> unit
  (** The stream that writes lines to standard output channel. *)

  val stderr : string t -> unit
  (** The stream that writes lines to standard error channel. *)


  (** {1 Adaptors}

      Integration adaptors for {{:#sources} sources}, {{:#sinks} sinks} and
      {{:#flows} flows}. *)

  val from : 'a source -> 'a t
  (** [from source] is a stream created from a source.

      {4 Examples}

      {[
        # Stream.len (Stream.from (Source.list [0; 1; 2]))
        - : int = 3
      ]} *)

  val into : ('a, 'b) sink -> 'a t -> 'b
  (** [into sink stream] is the result value produced by streaming all elements
      of [stream] into [sink].

      {4 Examples}

      {[
        # Stream.into Sink.sum (Stream.of_list [0; 1; 2])
        - : int = 3
      ]} *)

  (* val fill : ('a, 'b) sink -> 'a t -> 'a t *)
  (* val fill : ('a, 'b) sink -> 'a t -> 'b * 'a t option *)
  (** [fill sink stream] is similar to [into] but, in addition to the result
      value produced by [sink], will optionally return a leftover stream with
      elements that were not consumed by [sink]. *)

  val via : ('a, 'b) flow -> 'a stream -> 'b stream
  (** [via flow stream] is stream produced by transforming all elements of
      [stream] via [flow].

      {4 Examples}

      {[
        Stream.count 100
        |> Stream.via (Flow.filter (fun x -> x mod 2 = 0))
        |> Stream.via (Flow.take 50)
        |> Stream.into Sink.sum
      ]} *)

  val run : from:'a source -> via:('a, 'b) flow -> into:('b, 'r) sink -> ('r * 'a source option)
  (** Fuses sources, sinks and flows and produces a result and a leftover.

      {[
      let (r, leftover) = Stream.run ~from:source via:flow ~into:sink
      ]}

      Streams elements from [source] into [sink] via a stream transformer [flow]. In
      addition to the result value [r] produced by [sink], a [leftover] source is
      returned, if [source] was not exhausted.

      {b Warning:} If a leftover source is produced, it is required to either
      consume it or manually {{!val:Source.dispose} dispose} its resources. Not
      doing so might lead to resource leaks.


      {4 Examples}

      {[
      # let (x, leftover) =
          let source = Source.list ["1"; "2"; "3"] in
          let flow = Flow.map int_of_string in
          Stream.run ~from:source ~via:flow ~into:Sink.first
      val x : int option = Some 1
      val leftover : string source option = Some <abstr>
      # match leftover with
        | Some source -> Source.dispose source
        | None -> print_endline "No leftover"
      - : unit = ()
      ]} *)


  (** {1 Syntax defintions}

      Streams can be constructed with the let-binding syntax which is similar
      to {{: https://en.wikipedia.org/wiki/List_comprehension} list
      comprehensions}. The following example demonstrates this feature:

      {[
      open Stream.Syntax

      let items =
        let* n = Stream.range 1 3 in
        let* c = Stream.of_list ['x'; 'y'] in
        yield (n, c) in
      assert (Strea.to_list items = [(1, 'x'); (1, 'y'); (2, 'x'); (2, 'y')])
      ]} *)

  (** Module with syntax definitions for streams. *)
  module Syntax : sig
    val yield  : 'a -> 'a t
    (** [yield x] is a stream with a single element [x]. *)

    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  end

end



