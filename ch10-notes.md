# Chapter 10. Software Transactional Memory

Techinique to simplify concurrent programming: __allow multiple state-changing operations to be grouped together and performed as a single atomic operation__.

For reference throughout the following sections, the types and operations of the STM interface are:

_Control.Concurrent.STM_

```haskell
data STM a    -- abstract
instance Monad STM

atomically :: STM -> IO a

data TVar a   -- abstract
newTVar   :: a -> STM (TVar a)
readTVar  :: TVar a -> STM a
writeTVar :: TVar a -> a -> STM ()

retry  :: STM a
orElse :: STM a -> STM a -> STM a

throwSTM :: Exception e => e -> STM a
catchSTM :: Exception e => STM a -> (e -> STM a) -> STM a
```

## Running Example: Managing Windows

A display consist of a number of Desktops, each of which is displaying a set of Windows.

```haskell
data Desktop -- abstract
data Window -- abstract
```

To put it another way, a display is a mapping from Desktop to a set of Window objects. The mapping changes over time, so we want to make it mutable, and the state needs to be shared among multiple threads.

`type Display = MVar (Map Desktop (Set Window))`

This would work but the `MVar` is a single point of contention. __This structure doesn't allow as much concurrency as we would like__.

To allow operations on separate desktops to proceed without impeding each other:

`type Display = Map Desktop (MVar (Set Window))`

Unfortunately, __this approach also quickly runs into problems__:

```haskell
moveWindow :: Display -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp win a b = do
  -- We must take both MVar before we can put the results,
  -- otherwise another thread could potentially obserserve the display
  -- in a state in which the window we are movign does not exist.
  wa <- takeMVar ma
  wb <- takeMVar mb
  putMVar ma (Set.delete win wa)
  putMVar mb (Set.insert win wb)
 where
  ma = disp ! a
  mb = disp ! b
```

__What if there is a concurrent call to `moveWindow` trying to move a window in the opposite direction ?__

- Probably a deadlock. (dining philosophers problem)

A possible solution could be imposing an ordering on the MVars and require
that all agents take MVars in the correct order and release them in the opposite order.

__Software transactional memory provides a way to avoid this deadlock problem
without imposing a requirement for ordering on the programer__

`type Display = Map Desktop (Tvar (Set Window))`

`atomically` performs a transacction. No other thread can observe an intermediate
state in which only somme of the operations of the transaction have taken place.
The STM computation passed to atomically can be arbitrarily large and can contain
any number of TVar operations, but __there are perfomance implications for large transactions__.

Equivalent moveWindow using STM at _windowman.hs_:

```haskell
moveWindow :: Display -> Window -> Desktop -> Desktop -> IO ()
moveWindow disp win a b = atomically $ moveWindowSTM disp win a b

moveWindowSTM :: Display -> Window -> Desktop -> Desktop -> STM ()
moveWindowSTM disp win a b = do
  wa <- readTVar ma
  wb <- readTVar mb
  writeTVar ma (Set.delete win wa)
  writeTVar mb (Set.insert win wb)
 where
  ma = disp ! a
  mb = disp ! b
```

This version is deadlock free ! We don't even need to have both readTVar together:

```haskell
wa <- readTVar ma
writeTVar ma (Set.delete win wa)
wb <- readTVar mb
writeTVar mb (Set.insert win wb)
```

We want to swap windows:

- move window V from desktop A to B
- move window W from desktop B to A

```haskell
swapWindows :: Display
            -> Window -> Desktop
            -> Window -> Desktop
            -> IO ()
swapWindows disp w a v b = atomically $ do
  moveWindowSTM disp w a b
  moveWindowSTM disp v b a
```

Composability of STM rocks. STM operations are usually providede without the `atomically` wrapper so that clients can compose them as necessary.

## Blocking

- `retry :: STM a`: "abandon the current transaction and run it again".

Let's consider how to implement TMVar (_tmvar.hs_):

`newtype TMVar a = TMVar (TVar (Maybe a))`

newEmptyTMVar:

```haskell
newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = do
  t <- newTVar Nothing
  return (TMVar t)
```

takeTMVar:

```haskell
takeTMVar :: TMVar a -> STM a
takeTMVar (TMVar t) = do
  m <- readTVar t
	case m of
		Nothing -> retry       -- 2
		Just a  -> do
			writeTVar t Nothing
```

-- 2: If the TVar contains Nothing, then the TMVar is empty, so we need to block. The retry operation says, __“Run the current transaction again,”__ which will have the desired effect: we keep rerunning the transaction until the TVar no longer contains Nothing and the other case branch is taken. Of course, we don’t really want to blindly rerun the transaction over and over again, making our CPU hot for no good reason. The STM implementation knows that there is no point rerunning the transaction unless something different is likely to happen, and that can be true only if one or more of the TVars that were read by the current transaction have changed. In fact, __what happens is that the current thread is blocked until one of the TVars that it is reading is written to, at which point the thread is unblocked again and the transaction is rerun__.

putTMVar:

```haskell
putTMVar :: TMVar a -> a -> STM ()
putTMVar (TMVar t) a = do
  m <- readTVar t
  case m of
    Nothing -> writeTVar t (Just a)
    Just _  -> retry
```

This STM trasaction succeeds when and only when both TMVars are full; otherwise it is blocked.
This explains why `retry` abandon the whole transaction.

```haskell
atomically $ do
	a <- takeTMVar ta
	b <- takeTMVar tb
	return (a,b)
```

### Blocking Until Something Changes

`retry` allow us to block on arbitrary conditions.

Example of rendering windows:

`render :: Set Window -> IO`

The current focused desktop:

`type UserFocus = TVar Desktop`

`getWindows` takes the Display and the UserFocus and
returns the set of windows to render in the STM monad.

_windowman.hs_

```haskell
getWindows :: Display -> UserFocus -> STM (Set Window)
getWindows disp focus = do
  desktop <- readTVar focus
  readTVar (disp ! desktop)
```

The rendering thread:

```haskell
renderThread :: Display -> UserFocus -> IO ()
renderThread disp focus = do
  wins <- atomically $ getWindows disp focus
  loop wins
  where
    loop wins = do
      render wins
      next <- atomically $ do
                wins' <- getWindows disp focus
                if (wins == wins') then retry else returns wins'
      loop next
```

`retry` is the best.

### Merging with STM

`orElse :: STM a -> STM a -> STM a`

- First, a is executed. If a returns a result, then the orElse call returns it and ends.
- If a calls _retry_ instead, a's effect are discarded and b is executed instead.

For example, take at most one of two MVars:

```haskell
takeEitherTMVar :: TMVar a -> TMVar b -> STM (Either a b)
takeEitherTMVar ma mb =
  lMVar `orElse` rMVar where
    lMVar = fmap Left  (takeTMVar ma)
    rMVar = fmap Right (takeTMVar mb)
```

orElse is left biased.

STM provides two complementary ways to compose blocking operations togheter:
__the ordinary monad bind gives us "and", and _orElse_ gives us "or".__

### Async Revisited

STM's _orElse_ now allows us to defined `waitEither` much more efficiently.

The translation is straightforward, we just replace MVar with TMVar.

`data Async a = Async ThreadId (TMVar (Either SomeException a))`

async is almost the same:

```haskell
async :: IO a -> IO (Async a)
async action = do
  var <- newEmptyTMVarIO -- like newEmptyMVar but in the IO Monad
  t <- forkFinally action (atomically . putTMVar var)
  return (Async t var)
```

The waitCatchSTM is like waitCatch, but in the STM monad:

```haskell
waitCatchSTM :: Async a -> STM (Either SomeException a)
waitCatchSTM (Async _ var) = readTMVar var
```

Same for waitSTM:

```haskell
waitSTM :: Async a -> STM a
waitSTM a = do
  r <- waitCatchSTM
  case r of
    Left e -> throwSTM e  -- Throwing an exception in STM aborts the transaction and
                             propagates the exception.
    Right a -> return a
```

Now we can define waitEither by composing two calls to waitSTM using orElse:

```haskell
waitEither :: Async a -> Async b -> IO (Either a b)
waitEither a b = atomically $
  fmap Left (waitSTM a)
    `orElse`
	fmap Right (waitSTM b)
```

More generally:

```haskell
waitAny :: [Async a] -> IO a
waitAny asyncs = atomically $
  foldr orElse retry (map waitSTM asyncs)
```

- See _geturlsfirst.hs_ for a real example.

### Implementing Channels with STM

The STM version of _Chan_ is called _TChan_, and the interface we wish
to implement is as follows:

```haskell
data TChan a

newTChan:   :: STM (TChan a)
writeTChan: :: TChan a -> a -> STM ()
readTChan:  :: TChan a -> STM a
```

The full code for the implementation is given at _TChan.hs_:

```haskell
data TChan a = TChan (TVar (TVarList a)) -- R
                     (TVar (TVarList a)) -- W

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a (TVarList a)

newTChan :: STM (TChan a)
newTChan = do
  hole <- newTVar TNil
  read <- newTVar hole
  write <- newTVar hole
  return (TChan read write)

readTChan :: TChan a -> STM a
readTChan (TChan readVar _) = do
  listHead <- readTVar readVar
  head     <- readTVar listHead
  case head of
    TNil           -> retry
    TCons val tail -> do
        writeTVar readVar tail
        return val

writeTChan :: TChan a -> a -> STM ()
writeTChan (TChan _ writeVar) a = do
  newListEnd <- newTVar TNil
  listEnd    <- readTVar writeVar
  writeTVar writeVar newListEnd -- The order doesn't matter
  writeTVar listEnd (TCons a newListEnd)
```

- All operations are in the STM monad.
- The TList types needs a TNil constructor to indicate an empty list
- Blocking in readTChan is implemented by a call to retry
- Nowhere did we have to worry about what happens when a read executes concurrently with a write.

#### More operations are possible

_unGetChan_ could not be implemented in terms of MVars (it could deadlock).

```haskell
unGetTChan :: TChan a -> a -> STM ()
unGetTChan (TChan readVar _) a = do
  listHead <- readTVar readVar
  newHead <- newTVar (TCons a listHead)
  writeTVar readVar newHead
```

Other operations that were not possible with MVars are straightforward with STM:

```haskell
isEmptyTChan :: TChan a -> STM Bool
isEmptyTChan (TChan read _) = do
  listhead <- readTVar read
  head     <- readTVar listhead
  case head of
    TNil      -> return True
    TCons _ _ -> return False
```

#### Composition of Blocking operations

Reads a value from either of the two TChans passed as arguments, or blocks if they are both empty.

```haskell
readEitherTChan :: TChan a -> TChan b -> STM (Either a b)
readEitherTChan a b =
  fmap Left (readTChan a)
    `orElse`
  fmap Right (readTChan b)
```

### Asynchronous Exception Safety

```haskell
throwSTM :: Exception e => e -> STM a
catchSTM :: Exception e -> STM a -> (e -> STM a) -> STM a
```

In `catchSTM m h`, if m raises an exception, then __all of its effects are discarded__, and then the handler h is invoked. If there is no enclosing _catchSTM_, then all of the effects of the transaction are discarded and the exception is propagated out of _atomically_.

An example:

```haskell
readCheck :: TChan a -> STM a
readCheck chan = do
  a <- readTChan chan
  checkValue a
```

If _checkValue_ raises an exception, we don't want readTChan to happen (and element would be lost).

This is what _catchSTM_ will does: discards the readTChan effect.

In STM, you don't have to worry about mask and so.

__STM provides a nice way to write code that is automatically safe with respect to asynchrnous exceptions__.

## An Alternative Channel Implementation

_TList.hs_:

```haskell
newtype TList a = TList (TVar [a])

newTList :: STM (TList a)
newTList = do
  v  <- newTVar []
  return (TList v)

writeTList :: TList a -> a -> STM ()
writeTList (TList v) a = do
  list <- readTVar v
  writeTVar v (list ++ [a])

readTList :: TList a -> STM a
readTList (TList v) = do
  xs <- readTVar v
  case xs of
    []      -> retry
    (x:xs') -> do
      writeTVar v xs'
      return x
```

There is a performance problem with this representation: `writeTList` O(n).

The solution is to use a different queue data structure that supports O(1) (amortized) enqueue.

Queue as two list: xs and ys. where the content of the list = `xs ++ reverse ys`. Take from the front xs, put to the front of ys.

When xs is empty, we must reverse ys and let it become the new xs.

_TQueue.hs_:

```haskell
data TQueue a = TQueue (TVar [a]) (TVar [a])

newTQueue :: STM (TQueue a)
newTQueue = do
  read  <- newTVar []
  write <- newTVar []
  return (TQueue read write)

writeTQueue :: TQueue a -> a -> STM ()
writeTQueue (TQueue _read write) a = do
  listend <- readTVar write
  writeTVar write (a:listend)

readTQueue :: TQueue a -> STM a
readTQueue (TQueue read write) = do
  xs <- readTVar read
  case xs of
    (x:xs') -> do writeTVar read xs'
                  return x
    [] -> do ys <- readTVar write
             case ys of
               [] -> retry
               _  -> do let (z:zs) = reverse ys -- <1> do it lazy.
                        writeTVar write []
                        writeTVar read zs
                        return z
```

<1> `let`(lazy) instead of `case` (strict). Otherwise the STM transaction could not complete until the reverse finished.

Having a separate TVar for each list makes read and write independent which improves parallelism.

This implementation outperforms both the MVar-based Chan and the TVar-based TChan.
A simple benchmark progran can be found in _chanbench.hs_:

```haskell
$ ghc -O2 -threaded chanbench.hs
$ ./chanbench 0 2000000 +RTS -N4 -s
```

## Bounded Channels

The unbounded channel has a different pathology: if the reading thread cannot keep up with the writers, the size of the channel will keep growing without bound, and in the worst case we could run out of memory.

One-place channels (MVar and TMVar)

_TBQueue.hs_:

```haskell
data TBQueue a = TBQueue (TVar Int) (TVar [a]) (TVar [a])

newTBQueue :: Int -> STM (TBQueue a)
newTBQueue size = do
  read  <- newTVar []
  write <- newTVar []
  cap   <- newTVar size
  return (TBQueue cap read write)

writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue cap _read write) a = do
  avail <- readTVar cap
  if avail == 0
     then retry
     else writeTVar cap (avail -1)
  listend <- readTVar write
  writeTVar write (a:listend)

readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue cap read write) = do
  avail <- readTVar cap
  writeTVar cap (avail + 1) -- retry will undo it if the list is empty.
  xs <- readTVar read
  case xs of
    (x:xs') -> do writeTVar read xs'
                  return x
    [] -> do ys <- readTVar write
             case ys of
               [] -> retry
               _  -> do let (z:zs) = reverse ys
                        writeTVar write []
                        writeTVar read zs
                        return z
```

The danger with bounded channels is that it is possible to write a program with a lurking deadlock that is only discovered much later when the program is running in production. This is because the vast majority of the time writeTBQueue does not block, but once in a while, probably under heavy load, the channel fills up and writeTBQueue blocks. If the program depends on writeTBQueue not blocking, it may deadlock. How might we get into this situation? It is the dining philosophers problem again:

```
thread 1:
  x <- atomically $ readTBQueue q1
  y <- atomically $ readTBQueue q2

thread 2:
  atomically $ writeTBQueue q2 y
  atomically $ writeTBQueue q1 x
```

This sequence will work perfectly well until q2 becomes full, at which point we get a deadlock. If the communication pattern is obscured by other code, we might not realize there’s a problem.

There’s no silver bullet. The best advice is to test your code thoroughly with a buffer size of 1, because that will tend to expose any deadlocks of this kind during testing. Note that deadlocks will often be detected by the runtime system and result in an exception rather than a hang; see Detecting Deadlock.

## What Can We Not Do with STM ?

MVar is faster than STM. But, we should not assume that using MVar will always result in faster code (e.g. TQueue).

MVar does have one other important advantage over STM: _fairness_.  When multiple threads block on an MVar, they are guaranteed to be woken up in FIFO order. In contrast, in STM the runtime must wake up all blocked threads.

Suppose we want to add fairness to our TMVar implementation:

```haskell
data TMVar a = TMVar (TVar (Maybe a))
                     (TVar [TVar (Maybe a)]) -- List of blocked threads (takeTMVar, putTMVar)
```

Now consider how `putMVar` would work:

1. The TMVar is empty, and there are no blocked takeTMVars

Store the value in the TMVar and return.

2. The TMVar is empty, and there are some blocked takeTMVars

Removes the first blocked takeTMVar from the queue and put the value in its TVar.

3. The TMVar is full

We must create a new TVar containing Just a (the value to be put), add this to the end of the list of blocked putTMVars, and then wait until the TVar contents becomes Nothing.

Step _3_ is tricky: we cannot write a code that adds to the list and calls `retry`.

__The only way to implement fairness is to abandon composability.__

We can implement a TMVar with the structure I suggested, but the operations must be in the IO monad, not the STM monad.

The class of operations that STM cannot express are those that involve multi-way communication between threads:

- The simplest example is a synchronous channel, in which both the reader and the writer must be present simultaneously for the operation to go ahead. We cannot implement this in STM, at least compositionally, for the same reason that we cannot implement TMVar with fairness: the operations need to block and have a visible effect—advertise that there is a blocked thread—simultaneously.

## Internals - Performance

An STM transaction works by accumulating _a log_ of readTVar and writeTVar operations that have happened so far during the transaction.

- By storing `writeTVar` operations in the log rather than applying them to main memory immediately.
- Each `readTVar` must traverse the log to check whether the TVar was written by an earlier `writeTVar`. Hence, `readTVar` is an O(n) operation in the length of the log.
- The log contains a record of all the `readTVar` operations, it can be used to discover the full set of TVars read during the transaction, which we need to know in order to implement `retry`.

At the end, the STM implementation compares the log against the contents of memory. If the current contents of memory match the values read by `readTVar`, the effect of the transaction are _committed_ to memory, and if not, the log is discarded and the transaction runs again from the beginning.

This process takes place atomically by lockinng all the TVars involved in the transaction for the duration, so transactions operating on disjoint sets of TVars can proceed without interference.

Rules of thumb:

- __Never read an unbounded number of `TVars` in a single transaction__ because the O(n) performance of `readTVar` then gives O(n^2) for the whole transaction.

- __Try to avoid expensive evaluation inside a transaction__ because this will cause the transaction to take a long time, increasing the chance that another transaction will modify one or more of the same TVars, causing the the current transaction to be re-executed indefinitely because it is repeatedly aborted by shorter transactions.

One other thing to watch out for is __composing too many blocking operations together__. If we wanted to wait for a lsit of TMVars to
become full, we might be tempted to do this:

```haskell
atomically $ mapM takeTMVar ts
```

Each time a new TMVar becomes full, the transaction wakes up and runs again, going to sleep at the next empty TMVar. We'll run the transaction from the start, once for every element of `ts`, so the whole operation is _O(n^2)_. If instead, we had written this code:

```haskell
mapM (atomically . takeTMVar) ts
```

then it is _O(n)_ (although now the semantics are different).


