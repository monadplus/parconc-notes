# Chapter 11. Higher-Level Concurrency Abstractions

The complete library is available in the `async` package on Hackage.

Build trees of threads, such that when a thread dies for whatever reason, two things happen:
any children it has are automatically terminated, and its parent is informed.

## Avoiding Thread Leakage

Let's rewrite the last version of _Async_ API:

```haskell
data Async

async        :: IO a -> IO ( Async a )
cancel       :: Async a -> IO ()

withCatchSTM :: Async a -> SMT (Either SomeException a)
withCatch    :: Async a -> IO (Either SomeException a)

waitSTM      :: Async a -> STM a
wait         :: Async a -> IO a
waithEither  :: Async a -> Async b -> IO (Either a b)
```

If Async a1 fails, the first `wait` throws the same exception, which gets propagated up to the top of main.
But this is untidy, we left a2 running.

```haskell
main = do
  a1 <- async (getURL "http://www.wikipedia.org/wiki/Shovel")
  a2 <- async (getURL "http://www.wikipedia.org/wiki/Spade")
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)
```

We want the following:

`bracket (async io) cancel operation`

We could rewrite the previous code this way:

```haskell
main = do
  async (getURL "http://www.wikipedia.org/wiki/Shovel")) cancel $ \a1 -> do
    async (getURL "http://www.wikipedia.org/wiki/Spade")) cancel $ \a2 -> do
      r1 <- wait a1
      r2 <- wait a2
      print (B.length r1, B.length r2)
```

Let's package up the _bracket_ pattern into a function instead:

```haskell
withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync io op = bracket (async io) cancel op
```

Now our main functions become (_geturls7.hs_):

```haskell
main =
  withAsync (getURL "http://www.wikipedia.org/wiki/Shovel") $ \a1 ->
  withAsync (getURL "http://www.wikipedia.org/wiki/Spade")  $ \a2 -> do
  r1 <- wait a1
  r2 <- wait a2
  print (B.length r1, B.length r2)
```

The second Async is cleaned up if the first one fails.

## Symmetric Concurrency Combinators

The problem with the previous example is that if a2 fails, then  we must wait for a1 to finish before noticing the failure of a2.

We should be able to notice the failure of either a2 or a2, whichever one happens first.

This is somewhat like the `waitEither` operation: `waitEither :: Async a -> Async b -> IO (Either a b)`

But here we want to wait for _both_ results and terminate early if either _Async_ raises an exception.

```haskell
waitBoth :: Async a -> Async b -> IO (a,b)
waitBoth a1 a2 =
  atomically $ do
    r1 <- waitSTM a1 `orElse` (do waitSTM a2; retry) -- 1
    r2 <- waitSTM a2
    return (r1,r2)
```

- If a1 threw an exception, the the exception is rethrown.
- If a1 returned a result, then we proceed to the next line and wait for a2's result.
- If waitSTM a1 retries, then we enter the right side of `orElse`:
  - If a2 threw an exception, then the exception is rethrow nhere.
  - IF a2 returned a result, then we ignore it and call `retry`, so the whole transaction retries.

Now, using `withAsync` and `waitBoth`, we can build a nice symmetric function that runs two IO actions concurrently
but aborts if either one fails with an exception:

```haskell
concurrently :: IO a -> IO b -> IO (a, b)
concurrently ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitBoth a b
```

Code at _geturls8.hs_.

We can fold it over a _list_ of urls:

```haskell
main = do
  xs <- foldr conc (return []) (map getURL sites)
  print (map B.length xs)
 where
  conc ioa ioas = do
    (a,as) <- concurrently ioa ioas
    return (a:as)
```

The _concurrently_ function has a companion, if we swap `waitBoth` for `waitEither`:

```haskell
race :: IO a -> IO b -> IO (Either a b)
race ioa iob =
  withAsync ioa $ \a ->
  withAsync iob $ \b ->
    waitEither a b
```

The race function runs two IO actions concurrently, but as soon as one of them returns a result or throws an exception, the other is immediately cancelled.

By using multiple `race` and `concurrently` calls, we can build up larger trees of threads. If we use these functions consistently,
we can be sure that the tree of threads constructed will always be collapsed from the bottom up:

- If a parent throws an exception or receives an asynchronous exception, then the children are automatically cancelled. This happens recursively.
- If one child receives an exception, thenn its sibiling is also cancelled.
- The parent chooses whether to wait for a result from both children or just one, bu using `race` or `concurrently`, respectively.

### Timeouts Using race

Implement `timeout` using race _(timeout2.hs_):

```haskell
timeout :: Int -> IO a -> IO (Maybe a)
timeout n m
  | n < 0      = fmap Just m
  | n == 0     = return Nothing
  | otherwise  = do
      r <- race (threadDelay n) m
      case r of
        Left _ -> return Nothing
        Right a -> return (Just a)
```

(technical) This implementation of `timeout` does have a few differences from the one in _timeout.hs_.

- First, it doesn't have precisely the same semantics in the case where another thread sends the current thread an exception using `throwTo`. With the original `timeout`, the exception would be delivered to the computation m, whereas here the exception is delivered to `race`, which then terminates `m` with `killThread`.

- Secondly, the exception throw to m in the case of timeout is `ThreadKilled`, not a special `Timeout` exception. This might be important if the thread wanted to act on the Timeout exception.

- Finally, race creates an extra thread, which makes this implementation of `timeout` a little less efficient.

## Adding a Functor Instance

We would like to emulate the `waitAny` function defined previously:

```haskell
waitAny :: [Async a] -> IO a
waitAny asyncs =
  atomically $ foldr orElse retry $ map waitSTM asyncs
```

But if our Asyncs don't all have the same result type, then we can't put them in a list.

A better solution to the problem is to make Async an instance of Functor.

`data Async a = Async ThreadId (TMVar (Either SomeException a))`

Instead of storing the TMVar in the Async, we need to store something more compositional that we can compose with fmap:

`data Async a = Async ThreadId (STM (Either SomeException a))`

The change is very minor. We only need to move the `readTMVar` from `waitCatchSTM` to `async`:

```haskell
async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO
    t <- forkFinally action (atomically . putTMVar var)
      return (Async t (readTMVar var))

waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ stm) = stm
```

And now we can define fmap:

```haskell
instance Functor Async where
  fmap f (Async t stm) = Async t stm'
    where stm' = do
      r <- stm
      case r of
        Left e  -> return (Left e)
        Right a -> return (Right (f a))
```

