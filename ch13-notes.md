# Chapter 13. Parallel Programming Using Threads

When downloading multiple URLs simultaneously, the goal was to speed up the program by overlapping the I/O,
but it is not true parallelism because we don't need multiple processors to achieve a speedup; the speedup
was obtained by overlapping the time spent waiting for multiple web servers to respond.

Concurrency can also achieve true parallelism. In this book, we have tried to emphasize the use of the parallel
programming modeels - Eval, Strategies, the Par monad, and so on - for parallelism where possible, but
there are some problems for which these pure parallel programming models cannot be used:

- Problems where the work involves doing some I/O
- Algorithms that rely on saome nondeterminism internally.

## How to Achieve Parallelism with Concurrency

You need to do two things to run a program on multiple cores:

- Compile the program with `-threaded`
- Run the program with `+RTS -N{cores}` (`-N` to use all cores in your machine) (`-s` for statistics).

GHC automatically migrates threads between cores so that no cores are left iddle. Its load-balancing
algorithm isn't very sophisticated, though, so don't expect the scheduling policy to be fair.

Many issues from _Part I_ also arise when using concurrency to program parallelism;
for example, static versus dynamic partitioning, and granularity.

- Forking a fixed number of threads will gain only a fixed amount of parallelism
- Forking too many threads creates overhead tat we want to avoid.

## Example: Searching for Files

### Sequential Version

_findseq.hs_:

```haskell
find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
     then return (Just (d </> s))
     else loop fs'
 where
  loop [] = return Nothing
  loop (f:fs)  = do
    let d' = d </> f
    isdir <- doesDirectoryExist d'
    if isdir
       then do r <- find s d'
               case r of
                 Just _  -> return r
                 Nothing -> loop fs
       else loop fs
```

Try to run it: `$ stack exec findseq "findseq.hs" "/Users/arnau/" -- +RTS -s`

The haskell program is using the notoriously inefficient String type and doing Unicode conversion.
If you were optimizing this program for real, it would obviously be important to fix these inefficiencies before trying to
parallelize it, but we gloss over that here.

### Parallel Version

- Stop searching the others as soon as possible.
- If an error is encountered at any point, then we need to propagate the exception correctly.

To implement this, we're going to use the `Async` API with its `withAsync` facility for creating threads
and automatically cancelling them later.

Recall the type: `withAsync :: IO a -> (Async a -> IO b) -> IO b`

To set off several searches in parallel, we have to nest multiple calls of `withAsync`. This implies a fold of some
kind, and furthermore we need to collect up the `Async` values so we can wait for the results:

```haskell
subfind :: String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)

subfind s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
     then inner asyncs
     else withAsync (find s p) $ \a -> inner (a:asyncs)
```

Now, we can update the `find` function to create a new `Async` for eachh directory.

_findpar.hs_

```haskell
find :: String -> FilePath -> IO (Maybe FilePath)
find s d = do
  fs <- getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
     then return (Just (d </> s))
     else do
       let ps = map (d </>) fs'
       foldr (subfind s) dowait ps $ []
 where
   dowait as = loop (reverse as) -- The fold generated the list in reverse order.

   loop [] = return Nothing
   loop (a:as) = do
      r <- wait a
      case r of
        Nothing -> loop as
        Just a  -> return (Just a)
```

### Performance and Scaling

You might wonder whether creating a thread for every subdirectory is expensive, both in terms of time and space.

Let's compare `findseq` and `findpar` on the same directory, searching for a file that does not exist so that
the search is forced to traverse the whole tree:

```zsh
$ ./findseq nonexistent ~/code +RTS -s

   2,392,886,680 bytes allocated in the heap
      76,466,184 bytes copied during GC
       1,179,224 bytes maximum residency (26 sample(s))
          37,744 bytes maximum slop
               4 MB total memory in use (0 MB lost due to fragmentation)

  MUT     time    1.05s  (  1.06s elapsed)
  GC      time    0.07s  (  0.07s elapsed)
  Total   time    1.13s  (  1.13s elapsed)

$ ./findpar nonexistent ~/code +RTS -s

   2,523,910,384 bytes allocated in the heap
     601,596,552 bytes copied during GC
      34,332,168 bytes maximum residency (21 sample(s))
       1,667,048 bytes maximum slop
              80 MB total memory in use (0 MB lost due to fragmentation)

  MUT     time    1.28s  (  1.29s elapsed)
  GC      time    1.16s  (  1.16s elapsed)
  Total   time    2.44s  (  2.45s elapsed)
```

The parallel version does indeed take about twice as long, and it needs a lot more memory (80MB compared to 4MB).
But let's see how well it scales:

```zsh
$ ./findpar nonexistent ~/code +RTS -s -N2

   2,524,242,200 bytes allocated in the heap
     458,186,848 bytes copied during GC
      26,937,968 bytes maximum residency (21 sample(s))
       1,242,184 bytes maximum slop
              62 MB total memory in use (0 MB lost due to fragmentation)

  MUT     time    1.28s  (  0.65s elapsed)
  GC      time    0.86s  (  0.43s elapsed)
  Total   time    2.15s  (  1.08s elapsed)
```

This program scales super-linearly (better than double performance with two cores).

The reason for super-linear performance may be because running in parallel allowed some of the data structures to be garbage-collected
earlier than they were when running sequentially ( note the low GC time)

Running with `-N4`:

```zsh
$ ./findpar nonexistent ~/code +RTS -s -N4

   2,524,666,176 bytes allocated in the heap
     373,621,096 bytes copied during GC
      23,306,264 bytes maximum residency (23 sample(s))
       1,084,456 bytes maximum slop
              55 MB total memory in use (0 MB lost due to fragmentation)

  MUT     time    1.42s  (  0.36s elapsed)
  GC      time    0.83s  (  0.21s elapsed)
  Total   time    2.25s  (  0.57s elapsed)
```

This is a speedup of two on four cores. We can do better

### Limiting the Number of Threads with a Semaphore

The `findpar` program is caling quite nicely, which indicates that there is plenty of parallelism available.

Have a glence at ThreadScope profile to confirm it.

```zsh
$ stack exec findpar "idontexist" "/Users/arnau/haskell" -- +RTS -N4 -s -l
$ threadscope findpar.eventlog
```

So the reason for the lack of speedup relative to the seuqential version is the extra overhead in the parallel program.

We must reduce overhead. The obvious target is the creaton of an `Async`, and therefore a thread, for every single subdirectory.

The granularity is too fine.

One solution to granularity is chunking but here the computation is tree-shaped.

A depth threshold is more appropiate for a divide-and-conquer algorithm, but here the problem is that the
tree shape is dependent on the filesystem structure and is therefore not naturally balanced.

So here we will try a different approach. Remember that what we are trying to do is limit the number of threads created so we have just the right amount to keep all the cores busy. So let’s program that behavior explicitly: keep a shared counter representing the number of threads we are allowed to create, and if the counter reaches zero we stop creating new ones and switch to the sequential algorithm. When a thread finishes, it increases the counter so that another thread can be created.

A counter used in this way is often called a __semaphore__.

In our case we want something simpler (no blocking). If there are no units available, then the program will do something different (fall back to the sequential algorithm).

_findpar2.hs_

```haskell
newtype NBSem = NBSem (MVar Int)

newNBSem :: Int -> IO NBSem
newNBSem i = do
  m <- newMVar i
  return (NBSem m)

tryAcquireNBSem :: NBSem -> IO Bool
tryAcquireNBSem (NBSem m) =
  modifyMVar m $ \i ->
    if i == 0 then return (i, False) else let !z = i-1 in return (z, True)

releaseNBSem :: NBSem -> IO ()
releaseNBSem (NBSem m) =
  modifyMVar m $ \i ->
    let !z = i+1 in retunr (z, ())
```

We will use the semaphore in `subfind`:

```haskell
subfind :: NBSem -> String -> FilePath
        -> ([Async (Maybe FilePath)] -> IO (Maybe FilePath))
        ->  [Async (Maybe FilePath)] -> IO (Maybe FilePath)

subfind sem s p inner asyncs = do
  isdir <- doesDirectoryExist p
  if not isdir
     then inner asyncs
     else do
       q <- tryAcquireNBSem sem
       if q
          then do
            let dofind = find sem s p `finally` releaseNBSem sem
            withAsync dofind $ \a -> inner (a:asyncs)
          else do
            r <- find sem s p
            case r of
              Nothing -> inner asyncs
              Just _  -> return r
```

The whole code is at _findpar2.hs_.

Let's compare to the plain sequential version:

```zsh
./findpar2 0 nonexistent ~/code +RTS -N1 -s

   2,421,849,416 bytes allocated in the heap
      84,264,920 bytes copied during GC
       1,192,352 bytes maximum residency (34 sample(s))
          33,536 bytes maximum slop
               4 MB total memory in use (0 MB lost due to fragmentation)

  MUT     time    1.09s  (  1.10s elapsed)
  GC      time    0.08s  (  0.08s elapsed)
  Total   time    1.18s  (  1.18s elapsed)
```

This ran in 1.18s, which is close to the 1.14s. So the NBSem impacts performance by around 4%.

```zsh
$ ./findpar2 1 nonexistent ~/code +RTS -N2 -s

   2,426,329,800 bytes allocated in the heap
      90,600,280 bytes copied during GC
       2,399,960 bytes maximum residency (40 sample(s))
          80,088 bytes maximum slop
               6 MB total memory in use (0 MB lost due to fragmentation)

  MUT     time    1.23s  (  0.65s elapsed)
  GC      time    0.16s  (  0.08s elapsed)
  Total   time    1.38s  (  0.73s elapsed)
```

If you experiment a little, you might find that setting `n == 2` is slightly better.

```zsh
$ ./findpar2 8 nonexistent ~/code +RTS -N4 -s

   2,464,097,424 bytes allocated in the heap
     121,144,952 bytes copied during GC
       3,770,936 bytes maximum residency (47 sample(s))
          94,608 bytes maximum slop
              10 MB total memory in use (0 MB lost due to fragmentation)
  MUT     time    1.55s  (  0.47s elapsed)
  GC      time    0.37s  (  0.09s elapsed)
  Total   time    1.92s  (  0.56s elapsed)
```

The original `findpar` ran in about 0.57 with -N4 so the advantage of findpar2 at -N2 has evaporated at -N4.

Where is the bottleneck ? (See threadscope)

The threads are blocked at MVar ! (high contention)

One solution would be to use STM because STM transaction do not block, they just reexecute repeatedly.

In fact STM does work here, but instead we will introduce:

`IORef` to store the semaphore and operate on it using `atomicModifyIORef`:

`atomicModifyIORef :: IORef a -> (a -> (a, b) -> IO b`

It's like an STM but simpler (less overhead because it is more limited).

_findpar3.hs_:

We use the bang-pattern to avoid building up a large expression inside the `IORef`

The function getNumCapabilities comes from GHC.Conc and returns the value passed
to +RTS -N, which is the number of cores that the program is using.

The results with -N4 look like this:

```zsh
$ ./findpar3 nonexistent ~/code +RTS -s -N4

   2,495,362,472 bytes allocated in the heap
     138,071,544 bytes copied during GC
       4,556,704 bytes maximum residency (50 sample(s))
         141,160 bytes maximum slop
              12 MB total memory in use (0 MB lost due to fragmentation)

  MUT     time    1.38s  (  0.36s elapsed)
  GC      time    0.35s  (  0.09s elapsed)
  Total   time    1.73s  (  0.44s elapsed)
```

This represents a speedup of about 2.6 - our best yet, but we can improve it

### The ParIO monad

In [Chapter 4](./ch04-notes.md), we encountered the _Par_ monad, a simple API for programming deterministic
parallelism as a dataflow graph.

There is another version of the Par monad called `ParIO`, provided by _Control.Monad.Par.IO_ with two important
differences from `Par`:

- IO operations are allowed inside `ParIO`. To inject an IO operations into a ParIO computation, use `liftIO`.
- For this reason, the pure `runPar` is not available for ParIO. Instead use:
  - `runParIO :: ParIO a -> IO a`

Unlike Par, ParIO computations are not guaranteed to be deterministic. Nevertheless, the full power of the
Par framework is available: very lightweight tasks, multicore scheduling, and the same dataflow API based on IVars.

ParIO is ideal for paralle programming in the IO monad.

See the code at _findpar4.hs_:

```haskell
main = do
  [s,d] <- getArgs
  runParIO (find s d) >>= print
-- >>

-- <<find
find :: String -> FilePath -> ParIO (Maybe FilePath)
find s d = do
  fs <- liftIO $ getDirectoryContents d
  let fs' = sort $ filter (`notElem` [".",".."]) fs
  if any (== s) fs'
     then return (Just (d </> s))
     else do
       let ps = map (d </>) fs'
       foldr (subfind s) dowait ps []
 where
   dowait vs = loop (reverse vs)

   loop [] = return Nothing
   loop (v:vs) = do
      r <- get v
      case r of
        Nothing -> loop vs
        Just a  -> return (Just a)
-- >>

-- <<subfind
subfind :: String -> FilePath
        -> ([IVar (Maybe FilePath)] -> ParIO (Maybe FilePath))
        ->  [IVar (Maybe FilePath)] -> ParIO (Maybe FilePath)

subfind s p inner ivars = do
  isdir <- liftIO $ doesDirectoryExist p
  if not isdir
     then inner ivars
     else do v <- new                   -- <1>
             fork (find s p >>= put v)  -- <2>
             inner (v : ivars)          -- <3>
```

Let's see how well it performs, at `-N4`:

```zsh
$ ./findpar4 nonexistent ~/code +RTS -s -N4
Nothing
   2,460,545,952 bytes allocated in the heap
     102,831,928 bytes copied during GC
       1,721,200 bytes maximum residency (44 sample(s))
          78,456 bytes maximum slop
               7 MB total memory in use (0 MB lost due to fragmentation)

  MUT     time    1.26s  (  0.32s elapsed)
  GC      time    0.27s  (  0.07s elapsed)
  Total   time    1.53s  (  0.39s elapsed)
```

Beats NBSem implementation. Why ? We didn't have to consult some shared state and choose whether to fork
or continue our operation in the current thread.

__However, we cheated slightly here. `ParIO` has no error handling.__ Exceptions raised by an IO computation might (or might not) be silently dropped.

An attempt of fixing this can be found in _findpar5.hs_.

