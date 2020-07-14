# Tracing [![Hackage](https://img.shields.io/hackage/v/tracing-control.svg)](https://hackage.haskell.org/package/tracing-control)

**Important note**: this is a fork of the original [tracing](https://github.com/mtth/tracing) library in which `unliftio` has been replaced by `monad-control`.

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
