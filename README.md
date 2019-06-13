# Tracing [![Stackage LTS](https://stackage.org/package/tracing/badge/lts)](https://stackage.org/lts/package/tracing) [![Stackage Nightly](https://stackage.org/package/tracing/badge/nightly)](https://stackage.org/nightly/package/tracing) [![Hackage](https://img.shields.io/hackage/v/tracing.svg)](https://hackage.haskell.org/package/tracing)

An [OpenTracing](https://opentracing.io/)-compliant, simple, and extensible
distributed tracing library.

+ _Simple:_ add a single `MonadTrace` constraint to start tracing, without
  making your code harder to test!
+ _Extensible:_ use the built-in [Zipkin](http://zipkin.io) backend or hook in
  your own trace publication logic.

```haskell
import Monitor.Tracing

-- A traced action with its root span and two children.
run :: MonadTrace m => m ()
run = rootSpan alwaysSampled "parent" $ do
  childSpan "child-a" runA
  childSpan "child-b" runB
```

To learn more, hop on over to
[`Monitor.Tracing`](https://hackage.haskell.org/package/tracing/docs/Monitor-Tracing.html),
or take a look at examples in the `examples/` folder.
