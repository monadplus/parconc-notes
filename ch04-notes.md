# Chapter 4. Dataflow Parallelism: The Par Monad (package: _monad-par_)

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

runPar :: Par a -> a
```

Create a parallel task:

`fork :: Par () -> Par ()`: the argument passed (the child) is executed in parallel with the caller of fork (the
parent).

Values can be passed between Par computations using the _IVar type_ and its operations:

```haskell
data IVar a -- instance Eq

new :: Par (IVar a)
put :: NFData a => IVar a -> a -> Par ()
get :: IVar a -> Par a
```

Starts as an empty bot, get waits until the box is filled by put.

put put == error

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

The library works by keeping track of all the computations that can currently be performed (_a work pool_), and dividing
those amongst the available processors using an appropriate scheduling strategy.

This technique is called __dataflow__. We only describe the data dependencies, thereby exposing all the implicit
parallelism to be exploited.

Equivalent of `parMap` combinator that we saw earlier. The `spawn` function forks a computation in parallel and returns
an IVar that can be used to wait for the result. It is already provided by _Control.Monad.Par_.

```haskell
spawn :: NFData a => Par a -> Par (IVar a)
swap p = do
  i <- new
  fork (do x <- p; put i x)
  retur i
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

- Unlike the parMap from _parallel_, parMapM and parMap wait for all the results before returning.

  - If you don't want to wait for the results, then you could always `mapM (spaw . f)`

The put function calls deepseq on the value it puts in the IVar.
Put causes a traversal of the value stored in the IVar __which can be expensive__.

`put_ :: IVar a -> a -> Par ()` evaluates to WHNF only.

### Example: Shortest Paths in a Graph

_Floyd-Warshall algorithm_ finds lengths of the shortest paths between all pairs of nodes in a weighted directed graph.

```
# The overhead is visibile.
$ ./fwsparse  1000 800 +RTS -s
$ ./fwsparse1 1000 800 +RTS -s

# But the speed up is also: ~3.00
$ ./fwsparse1 1000 800 +RTS -s -N4
```

### Pipeline Parallelism (data streaming)

Producer  --->  Mapper  --->  Consumer

We would like each of the pipeline stages to run on a separate core, with the data streaming between them.

Producer forks (will run in thread1) and Consumer also forks (will run in thread2).

```haskell
data IList a = Nil | Cons a (IVar (IList a ))

type Stream a = IVar (ILis a)
```

Producer of streams:

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

Next, we'll write a consumer of Streams:

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


Mapper over Streams (this is both a producer and a consumer):

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

### Rate-Limiting the Producer

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

Exercise. Modify stremaFromList (streamFromList.hs), streamFold, StreamMap(should fork) to incorporate Fork.


### Limitations of Pipeline Parallelism

We can expose only as much parallelism as we have pipeline stages.

Tends to be less effective than data parallelism.

We cannot produce a lazy stream  from runPar itself, `streamFold` accumulates the entire list before it returns.

Still useful.

### Example: A Conference Timetable

_Constraint satisfaction problem_: find assignments for variables (talk slots) that satisfy the constraints (attendees' preferences).

Problem requires __exhaustive search__ but we can be more clever than generating all the possible assignments and
testing each one.

Incremental solution: assign a talk to the first slot of the first track, assign a talk to the first slot of the second track and so on. If we can't fill a slot without causing conflict ,we __backtrack__ to the previous slot and choose a different talk instead (do this recursively). The search pattern is a tree.

Algorithm that have this tree-shaped structure are often called __divide and conquer__. They parallelize well, branches are independent of one another.

- Technique: build  a _parallel skeleton_ (e. g. parMap)

We will use the _search skeleton_:

- First build the skeleton (_timetable1.hs_)
- Then add parallelism to the skeleton(_timetable2.hs_)

Here are the main points to take away from this example:

- Tree-shaped (divide and conquer) computations parallelize well.
- You can abstract the parallel pattern as a skeleton using higher-order functions.
- To control the granularity in a tree-shaped computation, add a depth threshold, and use the sequential version of the algorithm below a certain depth.

### Example: A parallel Type Inferencer

Purpose:

- Parallelism can be applied tp program analysis problems
- Dataflow model works well even when the structure of hte parallleism is entirely dependent on the input and cannot be
 predicted beforehand.

Problem: given a list of bindings of the for `x = e`, infer the types for the variables.

f ----> g ---> j              g and h can be infered in parallel.
   \ -> h /

Have a look at parinfer/Infer.hs inferBind and inferTop. This is where paralellism happens.

### Using Different Schedulers

Recall that Par monad is implemented as a library in Haskell and its behaviour can be changed.

Changing strategies: certain scheduling strategies are better suited to certain patterns of execution.

The library comes with:

- Trace
- Direct (default)

It's worth trying both with your code.

To change the scheduler use `import Control.Monad.Par.Scheds.Trace` instead of `Control.Monad.Par`

### The Par Monad Compared to Strategies

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

