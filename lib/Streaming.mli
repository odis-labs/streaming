
(** Pure, safe and fast streaming abstractions that compose. *)

(** {1:sources Sources}

    Sources are streaming producers of values.

    Elements are pulled from a source on demand. A source can have an internal
    state that will be lazily initialized when (and if) a consumer requests
    elements. The internal state will be safely disposed when the source runs
    out of elements, when the consumer terminates, or if an exception is raised
    at any point in the streaming pipeline.

    {2 Custom sources}

    Implementing your own custom sources enables access to many useful
    operations without much effort. To create a source use the
    {!val:Source.make} function.

    The following example creates a source that counts down to zero:

    {[
      let countdown n =
        let pull i =
          if i = 0 then None
          else Some (i, i - 1)) in
        Source.make ~init:n ~pull
    ]}

    For more details on sources see the {!module:Source} module. *)


(** A streaming producer of values of type ['a].

    The easiest way to create a source is to use the {!val:Source.unfold}
    function. *)
type 'a source


(** Source implementations and operations on sources. *)
module Source : sig
  (* NOTE: For better performance, it is recommended that the [pull] function
     caches the termination condition in case it is expensive. *)

  type 'a t = 'a source
  (** The type for sources that produce elements of type ['a]. *)

  (** {1 Creating a source} *)

  val empty : 'a t
  (** [zero] is an empty source. *)

  val single : 'a -> 'a t
  (** [single a] is a source with a single element [a]. *)

  val list : 'a list -> 'a t
  (** [list items] is a source with all elements from the [items] list. *)

  val array : 'a array -> 'a t
  (** [array items] is a source with all elements from the [items] array. *)

  val string : string -> char t
  (** [string str] is a source with all characters from the [str] string. *)

  val bytes : bytes -> char t
  (** [bytes b] is a source with all characters from the [b] bytes. *)

  val queue : 'a Queue.t -> 'a t
  (** [queue q] is a source with all characters from the [q] queue. *)

  val generate : int -> (int -> 'a) -> 'a t
  (** [generate n f] generates a source of length [n] mapping each index to an
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
    stop:('s -> unit) -> unit -> 'a t
  (** [make ~init ~pull ~stop ()] is a source created from the [init], [pull]
      and [stop] functions. *)


  (** {1 Zipping sources} *)

  val zip_with : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** [zip_with f src1 src2] is a source that pulls elements from [src1] and
      [src2] one by one, combining them with [f]. *)

  val zip : 'a t -> 'b t -> ('a * 'b) t
  (** [zip src1 src2] is a source of pairs with elements elements pulled from
      [src1] and [src2] one by one.

      Equivalent to [zip_with (fun x y -> (x, y)) src1 src2]. *)


  (** {1 Transforming a source} *)

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


  (** {1 Consuming a source} *)

  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r

  val length : 'a t -> int
  (** [length src] is the count of elements in [src]. *)

  val each : ('a -> unit) -> 'a t -> unit
  (** [each f src] applies an effectful function [f] to all elements in [src]. *)


  (** {1 Resource management} *)

  val dispose : 'a t -> unit
end



(** {1:sinks Sinks}

  Sinks are streaming abstractions that consume values and produce a single
  aggregated value as a result.

  The result value is extracted from an internal state that is built
  incrementally.

  {2 Combining sinks}

  It is possible to {{:./Sink/index.html#combining-sinks} combine} sinks and produce a result
  computed by multiple sinks at the same time. The most common example of this
  is a sink that calculates the arithmetic mean, defined as:

  {[
    let mean =
      let+ total = Sink.sum
      and+ count = Sink.length in
      total / count
  ]}

  {e Note:} This example uses the new applicative let-syntax.

  When [mean] starts processing the input elements it will compute the sum
  and the length at the same time, avoiding the need to process the input two
  times.

  Sinks are independent from sources and streams. You can think of them as
  packed arguments for folding functions.

  A convenient way to feed a sink is to use the {!val:Stream.into} function.

  {[
    Stream.into Sink.mean (Stream.of_list [2.0; 4.0; 5.0])
  ]}

  {2 Custom sinks}

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


  *)

(** A streaming consumer of values of type ['a] that, once done, produces a
    value of type ['b]. *)
type ('a, 'b) sink


(** Operations on sinks and sink instances. *)
module Sink : sig
  (**

    - The calls to `full` should be cheap as this function will be called to
      avoid allocation of unnecessary resources. If the computation required to decide if the reducer is full is expensive, consider caching it whenever possible.

    - If the producer's initialization is cheap, it should assume that reducer's
      initialization is expensive and, if possible, avoid calling init on the
      reducer.

    - If the producer's initialization is expensive, it should assume that
      reducer's initialization is cheap and, if possible, avoid it's own initialization.


      - Category
      - Monoid
      - Profunctor
      - Strong (first, second)
      - Choice (left, right)
      - Alternative
      - Bifunctor
      - Monad
      - Applicative
      - Comonad

      - consider adding: lift, before or after
      - await?

      let* x = Sink.await in
      let* y = Sink.await in
      Sink.return (x + y)

      race

      or_else, recover
      repeat
  *)

  type ('a, 'b) t = ('a, 'b) sink


  val make
    : init:(unit -> 's)
    -> push:('s -> 'a -> 's)
    -> ?full:('s -> bool)
    -> stop:('s -> 'b)
    -> unit
    -> ('a, 'b) t
  (** Creates a sink from a function that [init]ializes a state value, a
      [step]ping function to update that state and a [stop] function that
      produces the final result value. Optionally a [full] function can be passed
      to decide when the sink should terminate early. *)

  val map : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t

  val ( <@> ) : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t

  val fill : 'a -> ('b, 'a) t

  val ( <*> ) : ('a, 'b -> 'c) t -> ('a, 'b) t -> ('a, 'c) t


  (** {1 Combining sinks} *)

  val both : ('a, 'r1) t -> ('a, 'r2) t -> ('a, 'r1 * 'r2) t
  (** [both sink1 sink2] computes both [sink1] and [sink2] at the same time. *)

  val distribute : ('a, 'r1) t -> ('a, 'r2) t -> ('a, 'r1 * 'r2) t
  (** [distribute sink1 sink2] is similar to [both] but distributes the
      consumed elements over [sink1] and [sink2] alternating in round robin
      fashion. *)

  val unzip : ('a, 'r1) t -> ('b, 'r2) t -> ('a * 'b, 'r1 * 'r2) t
  (** [unzip sink1 sink2] is a sink that receives pairs, sending the first
      element into [sink1] and the second into [sink2]. Both sinks are computed
      at the same time and their results returned as an output pair. *)


  (** {1 Sinks} *)

  val full : ('a, unit) t
  (** A full sink that will not consume any elements and will not produce any results. *)

  val drain : ('a, unit) t
  (** Consumes all elements producing nothing. Useful for triggering actions in
      effectful streams. *)

  val fold : ('b -> 'a -> 'b) -> 'b -> ('a, 'b) t
  (** [fold f init] is a sink that reduces all input elements with the stepping
      function [f] starting with the accumulator value [init]. *)

  val is_empty : ('a, bool) t
  (** [is_empty] is [true] if the sink did not consume any elements and [false]
      otherwise. *)

  val each : ('a -> unit) -> ('a, unit) t
  (** Applies an effectful action to all input elements producing nothing. *)

  val length : ('a, int) t
  (** Consumes and counts all input elements. *)

  val first : ('a, 'a option) t
  (** The first input element. Equivalent to [nth 0]. *)

  val last : ('a, 'a option) t
  (** The last input element. *)

  val nth : int -> ('a, 'a option) t
  (** The n-th input element. *)

  val mean : (float, float) t
  (** Computes a numerically stable arithmetic mean of all input elements. *)


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

  val all : where:('a -> bool) -> ('a, bool) t
  (** [all ~where:pred] is [true] if all input element satisfy [pred]. Will
      stop consuming elements when the first element that does not satisfy
      [pred] is found. Results in [true] for empty input. *)

  val any : where:('a -> bool) -> ('a, bool) t
  (** [any ~where:pred] is [true] if at least one input element satisfies
      [pred]. Will stop consuming elements when such an element is found.
      Results in [false] for empty input. *)

  val print : (string, unit) t
  (** Prints all input string elements to standard output as lines. *)

  val list : ('a, 'a list) t
  (** Puts all input elements into a list. *)

  val array : ('a, 'a array) t
  (** Puts all input elements into an array. *)

  val buffer : int -> ('a, 'a array) t
  (** Similar to {!val:array} buf will only consume [n] elements. *)

  val queue : ('a, 'a Queue.t) t
  (** Puts all input elements into a queue. *)

  val sum : (int, int) t
  (** Adds all input integer values. *)

  val product : (int, int) t
  (** Product of input integer values. Stops if any input element is [0]. *)

  val string : (string, string) t
  (** Consumes and concatenates strings. *)

  val bytes : (bytes, bytes) t
  (** Consumes and concatenates bytes. *)

  val file : string -> (string, unit) t
  (** [file path] is a sink that writes input strings as lines into a file located at [path]. *)

  val stdout : (string, unit) t
  (** A sink that writes input strings as lines to STDOUT. *)

  val stderr : (string, unit) t
  (** A sink that writes input strings as lines to STDERR. *)


  (** {1 Resource management} *)

  val dispose : ('a, 'r) t -> 'r
  (** Close the sink and produce the currently accumulated result. *)


  module Syntax : sig
    val return : 'b -> ('a, 'b) t
    val let__plus : ('c, 'a) t -> ('a -> 'b) -> ('c, 'b) t
    val and__plus : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  end
end



(** {1:flows Flows}

    Flows are composable stream transformers.

    A flow can be applied to a stream with {!val:Stream.via}:

    {[
      Stream.range 10
      |> Stream.via (Flow.map (fun x -> x + 1))
      |> Stream.into Sink.sum
    ]}

    Flows can be composed to form pipelines:

    {[
      let flow = Flow.(map (fun x -> x + 1) >> filter (fun x -> x mod 2 = 0)) in
      Stream.range 10
      |> Stream.via flow
      |> Stream.into Sink.sum
    ]}


    *)

(** Stream transformers that consume values of type ['a] and produce values of
    type ['b]. *)
type ('a, 'b) flow


(** Operations on flows and common flow instances. *)
module Flow : sig
  type ('a, 'b) t = ('a, 'b) flow


  val filter : ('a -> bool) -> ('a, 'a) t
  (** A flow that includes only the elements that satisfy a predicate. *)

  val map : ('a -> 'b) -> ('a, 'b) t
  (** A flow with all elements transformed with a mapping function. *)

  val take : int -> ('a, 'a) t
  (** Take first [n] elements from the source and discard the rest. *)

  val buffer : int -> ('a, 'a array) t
  (** Collects [n] elements into an array buffer. Once the buffer is full it is
      emmited as a stream item. *)


  val into : ('a, 'r) sink -> ('a, 'r) t
  (** Repeatedly processes incoming elements with a sink producing computed results.

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

    Stream is a functional abstraction for incremental sequential processing of
    elements. Streams can be easily and efficiently transformed and
    concatenated.

    Stream operations do not leak resources. This is guaranteed in the presence
    of early termination (when not all stream elements are consumed) or in case
    of exceptions in the streaming pipeline.

    Streams are built to be compatible with {{:#sources} sources}, {{:#sinks}
    sinks} and {{:#flows} flows}. To create a stream that produces all elements
    from a source use {!val:Stream.from}, to consume a stream with a sink use
    {!val:Stream.into} and to transform stream elements with a flow use
    {!val:Stream.via}.

    {4 Examples}

    A simple echo program that loops over standard input and prints every line
    to standard output until Ctrl-D is hit:

    {[
      # Stream.stdin |> Stream.stdout;;
      hello<Enter>
      hello
      world<Enter>
      world
      <Ctrl+D>
      - : unit = ()
    ]} *)

type 'a stream
(** A stream of elements of type ['a]. *)


module Stream : sig
  type 'a t = 'a stream
  (** A stream of elements of type ['a]. *)

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

  val of_list : 'a list -> 'a t
  (** [of_list items] is a stream with all elements in the list [items]. *)

  val count : int -> int t
  (** [count n] is an infinite stream with integers starting from [n]. *)

  val range : ?by:int -> int -> int -> int t
  (** [range ~by:step n m] is a sequence of integers starting from [n] to
      [m] (excluding [m]) incremented by [step]. *)

  val iota : int -> int t
  (** [iota n] is [range ~by:1 0 n], that is a range from [0] to [n]
      incremented by [1]. *)

  val (--) : int -> int -> int t
  (** [n -- m] is [range ~from:n m]. The range is open on the right side. *)

  val generate : int -> (int -> 'a) -> 'a t
  (** [generate n f] generates a stream of length [n] mapping each index to an
      element with [f]. *)

  val repeat : ?n:int -> 'a -> 'a t
  (** [repeat ~n x] produces a container by repeating [x] [n] times. If [n] is
      omitted, [x] is repeated {e ad infinitum}. *)

  val repeatedly : ?n:int -> (unit -> 'a) -> 'a t
  (** [repeatedly ~n f] produces a container by repeatedly calling [f ()] [n]
      times. If [n] is omitted, [f] is called {e ad infinitum}. *)

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


  (** {1 Transformers} *)

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

  val cycle : 'a t -> 'a t
  (** Repeat a collection cyclically, {e ad infinitum}. *)

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

  (* val fill : ('a, 'b) sink -> 'a t -> 'a t *)
  (* val fill : ('a, 'b) sink -> 'a t -> 'b * 'a t option *)
  (** [fill sink stream] is similar to [into] but, in addition to the result
      value produced by [sink], will optionally return a leftover stream with
      elements that were not consumed by [sink]. *)

  val to_list : 'a t -> 'a list

  val length : 'a t -> int
  (** [length stream] counts the number of elements in [stream].

     Will exhaust the stream during processing.

      {4 Examples}

      {[
      # Stream.length (Stream.of_list ['a'; 'b'; 'c']);
      - : int = 3
      ]} *)

  val each : ('a -> unit) -> 'a t -> unit

  val fold : ('r -> 'a -> 'r) -> 'r -> 'a t -> 'r

  val is_empty : 'a t -> bool

  val first : 'a t -> 'a option
  (** Return the first item in the stream. *)

  val last : 'a t -> 'a option
  (** Return the last item in the stream, in linear time. *)

  val drain : 'a t -> unit

  (* Is this even possible? *)
  (* val peek : 'a t -> 'a option *)


  (** {1 IO Streams} *)

  val file : string -> char t

  val stdin : string t

  val stdout : string t -> unit

  val stderr : string t -> unit


  (** {1 Adaptors}

      Integration adaptors for {{:#sources} sources}, {{:#sinks} sinks} and
      {{:#flows} flows}. *)

  val from : 'a source -> 'a t
  (** [from source] is a stream created from a source.

      {4 Examples}

      {[
        # Stream.length (Stream.from (Source.list [0; 1; 2]))
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
        # let source = Source.list ["1"; "2"; "3"] in
          let flow = Flow.map int_of_string in
          let (x, leftover) = Stream.run ~from:source ~via:flow ~into:Sink.head;;
        val (x, leftover) : (int option * string source option) = (Some 1, <source>)
        # Source.dispose leftover;;
      ]} *)


  (** {1 Syntax defintions}

      Streams can be constructed by using custom let-binding syntax which is
      similar to {{: https://en.wikipedia.org/wiki/List_comprehension} list
      comprehensions}. The following example demonstrates this feature.

      {4 Example}

      {[
      open Stream.Syntax

      let items =
        let* x = Stream.of_list [1; 2] in
        let* y = Stream.of_list ['a'; 'b'] in
        yield (x, y) in
      assert (Strea.to_list items = [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b')])
      ]} *)

  module Syntax : sig
    val yield  : 'a -> 'a t
    (** [yield x] is a stream with a single element [x]. *)

    val let__star : 'a t -> ('a -> 'b t) -> 'b t
  end

end



