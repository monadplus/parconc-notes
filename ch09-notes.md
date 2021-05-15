# Chapter 9. Cancellation and Timeouts

One thread interrupt the execution of another thread.

Approaches:

- Polling for cancellation (imperative languages).
- Asynchronous cancellation (haskell).

__Fully asynchronous cancellation is the only sensible default in Haskell.__ (pure fp rocks)

## Asynchronous Exceptions

We would like `bracket` to work even if a thread is cancelled, so
cancellation should behave like an exception.

However, there's a fundamental difference, __the exception may arise at any time__. They
are called __asynchronous exceptions__.

Exceptions thrown using the normal `throw` and `throwIO` are called __synchronous exceptions__.

Asynchronous exceptions are throw using `throwTo`.

`throwTo :: Exception e => ThreadId -> e -> IO ()`

ThreadId is returned by a previous call to `forkIO` (thread mmay be _running_, _blocked_ or _finished_)

Extends `Async` Api with cancellation at _geturlscancel.hs_

### Masking Asynchronous Exceptions

Example:

"A thread wishes to call takeMVar, peform an operation depending on the value
of the MVar, and finally put the result of the operation in the MVar. The code must be
responsive to asynchronous exception arrives after the takeMVar but before the final putMVar,
the MVar should not be left empty. Instead, the original value should be restored."

If the asynchronous exception hits between 1-2 or 2-3 the invariant is broken and MVar is left empty.

```haskell
problem :: MVar a -> (a -> IO a) -> IO ()
problem m f = do
  a <- takeMVar m
  r <- f a `catch` \e -> do putMVar m a; throw e
  putMVar m r
```

To fix this problem, haskell provides the `mask` combinator:

`mask :: ((IO a -> IO b) -> IO b) -> IO b`

__Asynchronous exceptions inside mask can only happen inside the restore function.__

The restore function actually restores asynchronous exceptions to its present state during execution of the argument to mask.


```haskell
problem :: MVar a -> (a -> IO a) -> IO ()
problem m f = mask $ \restore -> do
  a <- takeMVar m
  r <- restore (f a) `catch` \e -> do putMVar m a; throw e
  putMVar m r
```

__All operations that may block indefenitely are designated as interruptible.__

__Interruptible operations may receive asynchronous exceptions even inside mask.__

For example `takeMVar` and `putMVar`.

`uninterruptibleMask :: ((IO a -> IO a) -> IO b) -> IO b`

This works just like mask, except that interruptible operations may not receive asynchronous exceptions. __Be very careful with uninterruptibleMask__; accidental misuse may leave your application unresponsive.

For __debugging__ you have in _Control.Exception_ the following function:

```haskell
getMaskingState :: IO MaskingState

data MaskingState = Unmasked | MaskedInterruptible | MaskedUninterruptible
```

The `problem` function does already exists (modifyMVar):

```haskell
modifyMVar_ :: MVar a -> (a -> IO a)      -> IO ()

modifyMVar :: MVar a  -> (a -> IO (a, b)) -> IO b
```

An example:

```haskell
compareAndSwap :: Eq a => MVar a -> a -> a -> IO Bool
compareAndSwap old new =
  modifyMVar m $ \cur ->
    if cur == old
       then return (new,True)
       else return (cur,False)
```

Working on multiple MVars is possible by nesting calls to `modifyMVar`

See _modifytwo.hs_ for a simple example.

### The bracket Operation

`bracket` is actually defined with `mask` to make it safe in the presence of async exceptions.

If before returns, after is guaranteed to be executed in the future.

```haskell
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket before after thing =
  mask $ \restore -> do
    a <- before --
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r
```

If an exception is raised while before is blocked, is ok.

Something else to watch out for here is using blocking operationns in after.
If you need to do this, thenn be aware that your blocking operation is intnerruptible
and might receive an asynchronous exception.

### Asynchronous Exception Safety for Chanenels

See _chan3.hs_ (there's plenty of comments).

### Timeouts

`timeout :: Int -> IO a -> IO (Maybe a)`

Finish as soon as the action returns or raises an exception. Otherwise, an asynchronous exception
of the form of `Timeout u` is sent.

Not suited for real time semantics, the timeout is approximated at microseconds scale.

The code from below was taken from the library _System.Timeout_.

Code at _timeout.hs_:

```haskell
timeout t m
    | t <  0    = fmap Just m                           -- <1>
    | t == 0    = return Nothing                        -- <1>
    | otherwise = do
        pid <- myThreadId                               -- <2>
        u <- newUnique                                  -- <3>
        let ex = Timeout u                              -- <3>
        handleJust                                      -- <4>
           (\e -> if e == ex then Just () else Nothing) -- <5>
           (\_ -> return Nothing)                       -- <6>
           (bracket
             (forkIO $ do threadDelay t;throwTo pid ex)
             (\tid -> throwTo tid ThreadKilled)
             (\_ -> fmap Just m))
```

```haskell
handleJust
  :: Exception e
  => (e -> Maybe b) -- ^ Which exception to catch
  -> (b -> IO a)    -- ^ Exception handler
  -> IO a           -- ^ computation to run
  -> IO a
```

What happens if _both_ the child thread and the parent thread try to call _throwTo_ at the same time ?

In order to this implementation of timeout to work properly, the call to _bracket_ must not be able to return
while the Timeout exception can still be thrown; otherwise, the exception can leak. Hence, the call to _throwTo_ that
kills the child thread must be synchronous. __Once this call returns, the child thread cannot throw its exception anymore.__

Indeed, this guarantee is provided by the semantics of _throwTo_. A call to _throwTo_ returns only
after the exception has been raised i nthe target thread. Hence _throwTo_ may block if the child thread is currently
masking asynchronous exceptions with _mask_, and because _throwTo_ may block, it is therefore _interruptible_ and
may itself receive asynchronous exceptions.

### Catching Asynchronous Exceptions

There is one important pitfall to be aware of here: it is easy to accidentally remain inside the implicit mask by tail-calling out of an exception handler. Here's an example program to illustrate the problem: the program takes a list of filenames on the command line and counts the number of lines in each file, ignoring files that do not exist.

See _catch-mask.hs_.

```haskell
$ ./catch-mask xxx yyy   # stack exec catch-mask -- xx yy
  Unmasked
  MaskedInterruptible
```

This is not intended, the problem arouse because we made a recursive call to
loop from the exception handler; thus the recursive call is made inside the implicit mask of handle.

A better way to code this example is to use  try instead:

See _catch-mask2.hs_.

## mask and forkIO

Let's return to our `Async` API for a moment:

In fact, __there’s a bug here__. If this Async is cancelled, and the exception strikes just after the try but before the putMVar, then the thread will die without putting anything into the MVar and the application will deadlock when it tries to wait for the result of this Async.

```haskell
async :: IO a -> IO (Async a)
async action = do
   m <- newEmptyMVar
   t <- forkIO (do r <- try action; putMVar m r)
   return (Async t m) -- This can return empty..
```

We could close this hole with a mask, but there’s another one: the exception might also arrive just before the try, with the same consequences. So how do we mask asynchronous exceptions in that small window between the thread being created and the call to try? Putting a call to mask inside the forkIO isn’t enough. There is still a possibility that the exception might be thrown even before mask is called.

For this reason, __forkIO is specified to create a thread that inherits the masking state of the parent thread__. This means that we can create a thread that is born in the masked state by wrapping the call to forkIO in a mask, for example:

```haskell
async :: IO a -> IO (Async a)
async action = do
   m <- newEmptyMVar
   t <- mask $ \restore ->
          forkIO (do r <- try (restore action); putMVar m r)
   return (Async t m)
```

This pattern of performing some action when a thread has completed is fairly common, so we can embody it as a variant of forkIO:

```haskell
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action fun =
  mask $ \restore ->
    forkIO (do r <- try (restore action); fun r)
```

See _geturlscancel2.hs_ for a complete example.

## Asynchronous Exceptions: Discussion

- All non-IO haskell code is automatically safe by construction. This is the one factor that makes asynchronous
  exceptions feasible.
- When working with resources: _bracket_ and so have built in support for asynchronous-exceptions.
- When working with MVar: use _modifyMVar_ and so.

A couple of thecniques can simplify matters:

- Large chunks of heavily stateful code can be wrapped in a mask, which drops into polling mode for asynchronous exceptions. This is much easier to work with. The problem then boils down to finding the interruptible operations and ensuring that exceptions raised by those will not cause problems. The GHC I/O library uses this technique: __every Handle operation runs entirely inside mask.__

- Using __software transactional memory (STM) instead of MVars or other state representations__ can sweep away all the complexity in one go. STM allows us to combine multiple operations in a single atomic unit, which means we don’t have to worry about restoring state if an exception strikes in the middle. We will describe STM in Chapter 10.

### Benefits

- __Many exceptional conditions map naturally onto asynchronous exceptions__. For example, stack overflow and user interrupt (e.g., Ctrl+C at the console) are mapped to asynchronous exceptions in Haskell. Hence, Ctrl+C not only aborts the program but also does so cleanly, running all the exception handlers. Haskell programmers don’t have to do anything to enable this behavior.

- __Computation can always be interrupted, even if it is third-party library code__. (There is an exception to this, namely calls to foreign functions, which we shall discuss in Threads and Foreign Out-Calls).

- __Threads never just die in Haskell__. It is guaranteed that __a thread always gets a chance to clean up and run its exception handlers__.
