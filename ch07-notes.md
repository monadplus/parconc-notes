# Chapter 7. Basic Concurrency: Threads and MVars

- paralellism = make program faster (code run in multiple CPU)
- concurrency = program with multiple interactions.

Forking a new thread of control: `forkIO :: IO () -> IO ThreadId`, if the thread has effects, those effects will be
innterleaved in an indeterminate fashion with the effects from other threads.

An example can be found at _fork.hs_.

### A Simple Example: Reminders

`threadDelay :: Int -> IO () -- microseconds`

Example at _reminders.hs_ and _reminders2.hs_.

- The program terminates when main returns, even if there are other threads still running. The other threads simply
stop running and cease to exist after main returns.
  - We can wait for a thread to terminate.

### Communicationn: MVars

MVar, the basic communication mechanism provided by Concurrent Haskell.

```haskell
data MVar a  -- abstract

newEmptyMVar :: IO (MVar a)
newMVar      :: a -> IO (MVar a)
takeMVar     :: MVar a -> IO a
putMVar      :: MVar a -> a -> IO ()
```

- takeMVar: removes the value from the MVar and waits/blocks if empty.
- putMVar: puts or waits.

```haskell
main = do
  m <- newEmptyMVar
  forkIO $ do putMVar m 'x'; putMVar m 'y'
  r <- takeMVar m
  print r
  r <- takeMVar m
  print r
```

If the _takeMVar_ blocks forever, the runtime system detects it and throws a _BlockedIndefinitelyOnMVar_.
Avoid deadlocks.

Ways MVar can be used:

- one-place channel: messages between threads)
- container for shared mutable state
- building block: constructing larger concurrent Datastrcutures.

#### MVar as a Simple Channel: A Logging Service

Thread where the rest of the progams can send messages, and this thred will store them.

Fire and forget task.

```haskell
data Logger

initLogger :: IO Logger
logMessage :: Logger -> String -> IO ()
logStop    :: Logger -> IO () -- We need to finish the jobs before closing the main thread
                              -- Otherwise, it will kill the rest of the threads.
```

See _logger.hs_ implementation

Any problem ? The main issue is that if multiple threads try to log they will be blocked for a short ammount of time. We need to implement an _unbounded channel_

#### MVar as a Container for a Shared State

Example at _phonebook.hs_.


#### MVar as a Building block: Unbounded Channels

Building blocks for larger abstractions.

This implementation is available at module _Control.Concurrent.Chan_

```haskell
Chan a

newChan   :: IO (Chan a)
readChan  :: Chan a -> IO a
writeChan :: Chan a -> a -> IO ()
```

Code at _chan.hs_.

> Programming large structures with MVar can be much trickier than it appears.

### Fairnness

No thread shoulld be starved of CPU time.

GHC uses a simple round-robin scheduler . It does guarantee that no thread
is starved indefinitely, but does not guarantee fairness.

The MVar implementation also provides an important fariness guarantee:

> No thread can be blocked indefinitely on an MVar unless another thread holds that MVar indefinitely.

Blocking threads in a FIFO queue attached to the MVar.

