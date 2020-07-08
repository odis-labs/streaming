# Streaming - Notes

## 2020-07-08

### Lazier resource initialization

Sources and sinks in streaming offer lazy resource acquisition when possible.
State is only initialized when the stream needs to produce or consume values.
For example, when `Stream.take 0` is put after `Stream.from src`, `src` will
not be initialized. Unfortunately the sink will still be initialized in this
situation.

#### Unnecessary sink initialization

```ocaml
let dummy_src =
  Source.make ()
    ~init:(fun () -> print_endline "dummy_src.init")
    ~stop:(fun () -> print_endline "dummy_src.stop")
    ~pull:(fun _ -> assert false)

let dummy_snk =
  Sink.make ()
    ~init:(fun () -> print_endline "dummy_snk.init")
    ~stop:(fun () -> print_endline "dummy_snk.stop")
    ~push:(fun () _ -> ())

# let () =
    dummy_src
    |> Stream.from
    |> Stream.take 0
    |> Stream.into dummy_snk
dummy_snk.init
dummy_snk.stop
```

Another example of unnecessary sink state initialization is when multiple sinks
are combined:

```ocaml
let combined_snk =
  let open Sink.Syntax in
  let* x = Sink.fill 42 in
  dummy_snk

# Stream.empty |> Stream.into combined_snk
dummy_snk.init
dummy_snk.stop
```

Even though the input is empty, `combined_snk` is forcing the inner sink to be
initialized. This makes sense from the semantics perspective, because
`flat_map`'s result is the result of the inner sink. What if there was a way
for `dummy_snk` to know if it received input and based on that decide the
result?


#### Why not Lazy?

It seems like the main problem with the thunk-based, _i.e._ `unit -> 'state`,
approach is that, when we need to close `state`, we do so unconditionally. If
only we could know if `state` was previously updated, or if it's still
uninitialized. Turns out this is very easy to achieve with `Lazy`.

```ocaml
let lazy_snk =
  Sink.make ()
    ~init:(fun () -> lazy (print_endline "snk.init"))
    ~stop:(fun state -> if Lazy.is_val state then print_endline "snk.stop")
    ~push:(fun state _ -> state)
```

Let's see what happens if we use this sink with our previous examples:


```ocaml
# let () =
    dummy_src
    |> Stream.from
    |> Stream.take 0
    |> Stream.into lazy_snk

# Stream.empty |> Stream.into combined_snk
```

Nothing is printed, as desired. And we achieved this without any changes to the
core types. Of course, we are still using a thunk for `init` in addition to the
lazy state. Maybe the thunk is not actually useful at all?

#### Conclusion

By wrapping states into lazy values, we can avoid the unnecessary resource
initialization, which is specially problematic when an external expensive
resource is acquired just to be immediately released.

I'm not currently aware of any disadvantages to this approach. Using `lazy` is
an opt-in for situations when precise control over resource acquisition and
termination is needed; no performance overhead is added for common
sources and sinks.

In addition to that, the lazy values are defined as existentials, so users
cannot interact with them directly.
