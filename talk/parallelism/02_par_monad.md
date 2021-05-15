# [The Par monad](http://hackage.haskell.org/package/monad-par)

Original paper: https://cs.indiana.edu/~rrnewton/papers/haskell2011_monad-par.pdf

Examples (complex) at: https://github.com/simonmar/monad-par/tree/master/examples/src

A Strategy consumes a lazy data structure and evaluates parts of it in parallel.

Advantages: decoupling of the algorithm from the parallelism.

We don't always want laziness, it's tricky to understand and diagnose performance.

Another parallel programming model, __the Par monad__. The goal of the Par monad
is to be more explicit about granularity and data dependencies, and to avoid the reliance on lazy evaluation.

The par monad is implemented entirely as a Haskell library and the implementation can be readily moodified to
accommodate alternative scheduling strategies.

```haskell
newtype Par a
instance Applicative Par
instance Monad Par

runPar   :: Par a -> a
runParIO :: Par a -> IO a
```

Create a parallel task:

```haskell
fork :: Par () -> Par () -- the child (i.e. the argument) is executed in parallel.
```

Values can be passed between Par computations using the _IVar type_ and its operations:

```haskell
data IVar a -- instance Eq

new :: Par (IVar a) -- newFull :: NFData a => a -> Par (IVar a)
put :: NFData a => IVar a -> a -> Par () -- put put = e
get :: IVar a -> Par a -- waits until is filled
```

~ MVar but IVar can only be written once.

> Don't return an IVar from a runPar and pass it to another call of runPar.

Example at _parmonad.hs_.

```haskell
runPar $ do
  i <- new
  j <- new
  fork $ put i (fib n)
  fork $ put j (fib m)
  a <- get i
  b <- get j
  return (a + b)
```

The library works by keeping track of all the computations that can currently be performed (_a work pool_), and dividing those amongst the available processors using an appropriate scheduling strategy.

This technique is called __dataflow__. We only describe the data dependencies, thereby exposing all the implicit
parallelism to be exploited.

Equivalent of `parMap` combinator that we saw earlier. The `spawn` function forks a computation in parallel and returns an IVar that can be used to wait for the result. It is already provided by _Control.Monad.Par_.

```haskell
spawn :: NFData a => Par a -> Par (IVar a)
swap p = do
  i <- new
  fork (do x <- p; put i x)
  return i

spawn_ :: Par a -> Par (IVar a)
spawnP :: NFData a => a -> Par (IVar a)
```

Provided by _Control.Monad.Par_:

```haskell
parMapM :: NFData b => (a -> Par b) -> [a] -> Par [b]
parMapM f as = do
  ibs <- mapM (spawn . f) as
  mapM get ibs
```

`f :: a -> Par b`: this means that f itself can create further parallelism using fork and other Par operations. Use
`return` if you don't need the further parallelism.

Unlike the parMap from _parallel_, parMapM and parMap wait for all the results before returning.

The put function calls deepseq on the value it puts in the IVar.

The idea is that this forces the work to happen when we expect it, rather than being passed to the consumer of the IVar and performed later, which often results in less parallelism than expected.

```haskel
put_ :: IVar a -> a -> Par () -- evaluates to WHNF only.
```

## Example: Shortest Paths in a Graph

_Floyd-Warshall algorithm_ finds lengths of the shortest paths between all pairs of nodes in a weighted directed graph.

```
shortestPath :: Graph -> Vertex -> Vertex -> Vertex -> Weight
shortestPath g i j 0 = weight g i j
shortestPath g i j k = min (shortestPath g i j (k-1))
                           (shortestPath g i k (k-1) + shortestPath g k j (k-1))
```

Sequential version:

```haskell
type Vertex = Int
type Weight = Int
type Graph = IntMap (IntMap Weight)

shortestPaths :: [Vertex] -> Graph -> Graph
-- Folds can only be parallelized when the op being folded is associative, and
-- then the linear fold can be turned into a tree.
shortestPaths vs g = foldl' update g vs
 where
   -- This one can be parallelized: traverseWithKey
  update g k = Map.mapWithKey shortmap g
   where
     shortmap :: Vertex -> IntMap Weight -> IntMap Weight
     shortmap i jmap = foldr shortest Map.empty vs
        where shortest j m =
                case (old,new) of
                   (Nothing, Nothing) -> m
                   (Nothing, Just w ) -> Map.insert j w m
                   (Just w,  Nothing) -> Map.insert j w m
                   (Just w1, Just w2) -> Map.insert j (min w1 w2) m
                where
                  old = Map.lookup j jmap
                  new = do w1 <- weight g i k
                           w2 <- weight g k j
                           return (w1+w2)
```

Parallel version using Par monad:

```haskell
shortestPaths :: [Vertex] -> Graph -> Graph
shortestPaths vs g = foldl' update g vs
 where
  update g k = runPar $ do
    m <- Map.traverseWithKey (\i jmap -> spawn (return (shortmap i jmap))) g
    traverse get m
   where
    shortmap :: Vertex -> IntMap Weight -> IntMap Weight
    ...
```

Let's execute it and see the results:

```zsh
# Let's compare the overhead
$ stack exec fwsparse 1000 800 -- +RTS -s
$ stack exec fwsparse1 1000 800 -- +RTS -s

# Let's see the speedup
$ stack exec fwsparse1 1000 800 -- +RTS -s -N
```

## Pipeline Parallelism (data streaming)

Alternative to data parallelism:

```
Producer  --->  Mapper  --->  Consumer
```

We would like each of the pipeline stages to run on a separate core, with the data streaming between them.

```haskell
data IList a = Nil | Cons a (IVar (IList a))
type Stream a = IVar (ILis a)
```

Producer of stream:

```haskell
streamFromList :: NFData a => [a] -> Par (Stream a)
streamFromList xs = do
  var <- new
  fork $ loop xs var
  return var
    where
      loop [] var = put var Nil
      loop (x:xs) var = do
        tail <- new
        put var (Cons x tail)
        loop xs tail
```

Next, we'll write a consumer of stream:

```haskell
streamFold :: (a -> b -> a) -> a -> Stream b -> Par a
streamFold n !acc instrm = do
  ilist <- get instrm
  case ilist of
    Nil -> return acc
    Cons h t -> streamFold fn (fn acc h) t
```

If the streamFold consumes all the available stream elements and catches up with the producer, it will
block in the `get` call waiting for the next element.


Mapper over stream (this is both a producer and a consumer):

```haskell
streamMap :: NFData b => (a -> b) -> Stream a -> Par (Stream b)
streamMap :: fn instrm = do
  outstrm <- new
  fork $ loop instrm outstrm
  return outstrm
    where
      loop instrm outstrm = do
        ilist <- get instrm
        case ilist of
          Nil -> put outstrm Nil
          Cons h t -> do
            newtl <- new
            put outstrm (Cons (fn h) newtl)
            loop t newtl
```

Implementation can be found [here](https://github.com/simonmar/monad-par/blob/master/monad-par/Control/Monad/Par/Stream.hs).

_rsa-pipeline.hs_: example of RSA encryption where encrypt produces a stream and decrypt consumes it.

```haskell
encrypt :: Integer -> Integer -> Stream ByteString -> Par (Stream ByteString)

decrypt :: Integer -> Integer -> Stream ByteString -> Par (Stream ByteString)

pipeline :: Integer -> Integer -> Integer -> ByteString -> ByteString
pipeline n e d b = runPar $ do
  s0 <- streamFromList (chunk (size n) b)
  s1 <- encrypt n e s0
  s2 <- decrypt n d s1
  xs <- streamFold (\x y -> (y : x)) [] s2
  return (B.unlines (reverse xs))
```

Encryption takes longer than decryption, one processor is waiting for the other to finish it task. The consumer is faster than the producer.

## Rate-Limiting the Producer

Consumer faster than producer: OK
Producer faster than consumer: KO (producer will create a long IList chain in memory-heap, gc must do extra work).

There's a trick that adds some automatic rate-limiting to the stream API.

```haskell
data IList a = Nil
             | Cons a (IVar (IList a))
             | Fork (Par ()) (IList a)
```

Produce a fixed amount of the list and insert a Fork constructor containing another Par computation that will
produce more of the list.

Consumer upon finding a Fork, calls fork to start production of the next chunk of the list.

## Limitations of Pipeline Parallelism

We can expose only as much parallelism as we have pipeline stages.

> Tends to be less effective than data parallelism.

We cannot produce a lazy stream  from runPar itself, `streamFold` accumulates the entire list before it returns.

```zsh
stack exec rsa-pipeline /usr/share/dict/words -- +RTS -s -N
```

## Using Different Schedulers

Recall that Par monad is implemented as a library in Haskell and its behaviour can be changed.

Changing strategies: certain scheduling strategies are better suited to certain patterns of execution.

The default implementation is based on a work-stealing scheduler that divides the work as evenly as possible between the available processors at runtime:

- Direct (default): no trace data structure
- Spark: uses sparks (par/pseq) directly
- Trace (from the paper): based on a lazy Trace data structure that separates the scheduler from the Par monad method implementations.

It's worth trying all with your code.

To change the scheduler use `import Control.Monad.Par.Scheds.Direct/Sparks/Trace` instead of `Control.Monad.Par`

## The Par Monad Compared to Strategies

- If your algorithm naturally produces a lazy data structure: `Strategy`. Otherwise: `Par monad`
- `runPar` is expensive, `runEval` is free. So when using Par monad, yo should usually try to thread the Par monad
  around to all places that need parallelism to avoid needing multiple `runPar` calls. If this is inconvenient, then
  `Eval` or `Strategies` might be a better choice. Nested `runPar` usually give poor results.
- `Strategies` allow a separation between algorithm and parallelism.
- `Par` monad has more overhead that `Eval` monad. `Eval` tends to perform better at finer granularities (direct runtime
  system support for sparks).
- `Par` monad is implemented entirely in a Haskell library.
- `Eval` monad has more diagnostics in `ThreadScope`.
- Speculative parallelism in the `Par` monad is not supported.
