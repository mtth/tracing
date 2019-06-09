# Distributed tracing

An [OpenTracing](https://opentracing.io/)-compliant, simple, and extensible
distributed tracing library.

+ _Simple:_ add a single `MonadTrace` constraint to start tracing (without making
  your code harder to test)!
+ _Extensible:_ use the built-in [Zipkin](http://zipkin.io) backend or implement
  your own.

```haskell
-- A traced action with its root span and two children.
run :: MonadTrace m => m ()
run = rootSpan (sampledEvery 10) do
  childSpan "part-a" runA
  childSpan "part-b" runB
```

To learn more, hop on over to
[`Monitor.Tracing`](https://hackage.haskell.org/package/tracing/docs/Monitor-Tracing.html).
