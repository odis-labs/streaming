# Streaming

Streaming abstractions that combine, transform and reduce large amounts of
sequential data efficiently, in constant space and without leaking resources.

## Overview

_Streaming_ uses composable stream producers (sources), consumers (sinks) and
transformers (flows). The central model that abstracts over them is a `Stream`.

The following features are provided:

- **Constant memory usage**: large or infinite streams can be computed in constant
  and small space. Buffering of the input is possible when needed.
- **Excellent performance**: all models were designed with performance at the
  core. See [benchmarks](https://github.com/rizo/streams-bench) for detailed
  comparison with other libraries.
- **Resource safety**: resources in effectful streaming pipelines are allocated
  lazily and released as early as possible. Resources are guaranteed to
  be terminated even when streams rise exceptions.
- **Flexibility**: both push-based and pull-based models are implemented to
  allow efficient zipping, concatenation and other streaming operations.
- **Streaming notation**: build streams and sinks using a convenient
  comprehension and applicative notations.

Read the [library documenation](https://odis-labs.github.io/streaming) for more details.


## Acknowledgements

This library is based on ideas found in other libraries and research projects
such as: Haskell's [Pipes](https://github.com/Gabriel439/Haskell-Pipes-Library)
and [Foldl](https://github.com/Gabriel439/Haskell-Foldl-Library) libraries,
Scala's [ZIO Streams](https://zio.dev), Clojure's
[Transducers](https://clojure.org/reference/transducers) and the
[Iteratees](http://okmij.org/ftp/Streams.html) streaming model by Oleg
Kiselyov.

