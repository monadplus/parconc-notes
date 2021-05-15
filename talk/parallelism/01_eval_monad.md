# [Parallel: the Eval monad](https://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel.html)

The _Control.Parallel_ module provides:

```haskell
par  :: a -> b -> b -- Indicates that it may be beneficial to evaluate the first argument in parallel with the second
pseq :: a -> b -> b -- Semantically identical to seq, but with a subtle operational difference: seq is strict in both its arguments, so the compiler may, for example, rearrange a `seq` b into b `seq` a `seq` b.
```

[Control.Seq](https://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Seq.html): provides a set of combinators to evaluate data structures.

[Parallel Strategies](https://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel-Strategies.html) let you separate the description of the parallelism from the logic of your program, enabling modular parallelism:

```haskell
data Eval a
instance Monad Eval

runEval :: Eval a -> a

rpar :: a -> Eval a -- My argument could be evaluated in parallel
rseq :: a -> Eval a -- Evaluate my argument and wait for the result
```

- Evaluation is to WHNF
- argument to rpar should be an unevaluated computation - a thunk.

## Example: Paralleling a Sudoku Solver

_Sudoku1.hs_ (sequential version):

```haskell
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
      solutions = map solve puzzles -- solve :: String -> Maybe Grid

  print (length (filter isJust solutions))
```

Let's execute it:

```zsh
$ ghc -O2 sudoku1.hs -rtsopts
$ ./sudoku1 sudoku17.1000.txt +RTS -s
```

_Sudoku2.hs_ (parallel version):

```haskell
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file

      (as,bs) = splitAt (length puzzles `div` 2) puzzles

      solutions = runEval $ do
                    as' <- rpar (force (map solve as))
                    bs' <- rpar (force (map solve bs))
                    rseq as'
                    rseq bs'
                    return (as' ++ bs')

  print (length (filter isJust solutions))
```

Let's execute it:

```zsh
$ stack exec sudoku2 sudoku17.1000.txt -- +RTS -s -N2 -l # requires compilation with ghc-options: -threaded -eventlog
# Let's analyze in detail
$ threadscope sudoku2.eventlog
```

The argument to rpar is called a __spark__. The runtime collects sparks in a pool and uses this as a source of work when there are spare processors available, using a technique called _work stealing_. Sparks may be evaluated at some point in the future, or they might not-it all depends on whether there is a spare core available. Sparks are very cheap to create.

```haskell
parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
      bs <- parMap f as
         return (b:bs)
```

_Sudoku3.hs_:

```haskell
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
      solutions = runEval (parMap solve puzzles)

  print (length (filter isJust solutions))
```

The previous code consumes a lot of memory! Let's fix it:

_Sudoku4.hs_:

```haskell
main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles   = lines file
      solutions = runEval (parMap solve puzzles)

  evaluate (length puzzles) -- evaluate = IO . seq
  print (length (filter isJust solutions))
```

In sudoku, we can use it to force evaluation of lines before starting the par part: `evaluate (length puzzles)`

## Evaluation Strategies

Separate algorithm from parallelism.

`type Strategy a = a -> Eval a`: take a data structure as input, traverses the structure creating parallelism with `rpar` and `rseq`, and then returns the original value.

Let's create a Strategy for pairs:

```haskell
pairPair :: Strategy (a,b)
pair (a,b) = do
  a' <- rpar a
  b' <- rpar b
  return (a',b')
```

`runEval (parPair (fib 35, fib 36))`.

This works fine but its betteer to use:

```haskell
using :: a -> Strategy a -> a
x `using` s = runEval (s x)
```

`(fib 35, fib 36) `using` parPair`

## Parameterized Strategies

```haskell
evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa  sb (a, b) = do
  a' <- sa a
  b' <- sb b
  retturn (a', b')
```

Recall that rpar:: a -> Eval a ~ Strategy a

```haskell
parPair :: Strategy (a, b)
parPair = evalPair rpar rpar
```

`parPair` always evaluates to WHNF.

We can make a Strategy that fully evaluates its argument:

```haskell
rdeepseq  :: NFData a => Strategy a
rdeepseq x = rseq (force x)
```

How  do we combine `rpar` and `rdeepseq` ?

_Control.Parallel.Strategies_ provides: `rparWith :: Strategy a -> Strategy a`

```haskell
parPair :: Strategy a -> Strategy b -> Strategy (a,b)
parPair sa sb = evalPair (rparWith sa) (rparWith sb)
```

Now we can write a Strategy that __fully evaluates both components of a pair in paralllel__:

```haskell
pairPair rdeepseq rdeepseq
-- (NFData a, NFData B) => Strategy (a,b)
````

Don't evaluate this component at all:

```haskell
r0 :: Strategy a
r0 x = return x

evalPair (evalPair rpar r0) (evalPair rpar r0) :: Strategy ((a,b),(c,d))
```

## A Strategy for Evaluating a List in Parallel

The `parList` function is a Strategy on lists that evaluates the list elements in parallel.

```haskell
parMap :: (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rseq

parList :: Strategy a -> Strategy [a]
parList strat = evalList (rparWith strat)

evalList :: Strategy a -> Strategy [a]
evalList strat [] = return []
evalList strat (x:xs) = do
  x'  <- strat x
  xs' <- evalList strat xs
  return (x':xs')
```

Both `evalList` and `parList` are already provided by _Control.Parallel.Strategies_.

## Example: the k-means problem

Sequential version:

```haskell
kmeans_seq :: Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_seq nclusters points clusters =
  let
      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters = do
        let clusters' = step nclusters clusters points
        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters
```

Parallel version:

```haskell
kmeans_strat :: Int -> Int -> [Point] -> [Cluster] -> IO [Cluster]
kmeans_strat numChunks nclusters points clusters =
  let
      chunks = split numChunks points

      loop :: Int -> [Cluster] -> IO [Cluster]
      loop n clusters = do
        let clusters' = parSteps_strat nclusters clusters chunks
        if clusters' == clusters
           then return clusters
           else loop (n+1) clusters'
  in
  loop 0 clusters

split :: Int -> [a] -> [[a]]
split numChunks xs = chunk (length xs `quot` numChunks) xs

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs

parSteps_strat :: Int -> [Cluster] -> [[Point]] -> [Cluster]
parSteps_strat nclusters clusters pointss
  = makeNewClusters $
      foldr1 combine $
          (map (assign nclusters clusters) pointss
            `using` parList rseq)

-- ignore
combine :: Vector PointSum -> Vector PointSum -> Vector PointSum
combine = Vector.zipWith addPointSums

-- ignore
assign :: Int -> [Cluster] -> [Point] -> Vector PointSum
assign nclusters clusters points = Vector.create $ do
    vec <- MVector.replicate nclusters (PointSum 0 0 0)
    let
        addpoint p = do
          let c = nearest p; cid = clId c
          ps <- MVector.read vec cid
          MVector.write vec cid $! addToPointSum ps p

    mapM_ addpoint points
    return vec
 where
  nearest p = fst $ minimumBy (compare `on` snd)
                        [ (c, sqDistance (clCent c) p) | c <- clusters ]
```

## GC'd Sparks and Speculative Parallelism

__Speculative parallelism__: not necessarily required, and the runtime will automatically discard speculative tasks that
it can prove will never be required. For example, if only few elements of the list are used.

Not tail-recursive. Requires stack space linear in the length of the input list.

```haskell
evalList :: Strategy a -> Strategy [a]
evalList strat [] = return []
evalList strat (x:xs) = do
  x'  <- strat x
  xs' <- evalList strat xs
  return (x':xs')
```

We could do it tail-recursive:

```haskell
parList :: Strategy a -> Strategy [a]
parList strat xs = do
  go xs
  return xs
 where
  go []     = return ()
  go (x:xs) = do rparWith strat x
                 go xs
```

nb. This version has a serious problem: __all the parallelism it creates will be discarded by the garbage collector.__

The GHC runtime regularly checks the spark pool for any entries that are not required by the program and removes them.

How does the runtime know whether an entry is needed? The same way it knows whether any item in memory is needed: There must be a pointer to it from something else that is needed. This is the reason that parList creates a new list xs'. Suppose we did not build the new list xs', as in the tail-recursive version of parList above. Then the only reference to each strat box in the heap would be from the spark pool, and hence the runtime would automatically sweep all those references from the spark pool, discarding the parallelism. So we build a new list xs' to hold references to the strat calls that we need to retain.

## Parallelizing Lazy Streams with parBuffer

Lazy list as a Stream so that your program can consume input while simultaneously produce output in constant space.

_rsa.hs_:

```haskell
encrypt, decrypt :: Integer -> Integer -> ByteString -> ByteString
encrypt n e = B.unlines
            . map (B.pack . show . power e n . code)
            . chunk (size n)
decrypt n d = B.concat
            . map (B.pack . decode . power d n)
            . integers
            . B.lines
```

Let's run it:

```zsh
$ ./rsa encrypt /usr/share/dict/words >/dev/null +RTS -s
   8,040,128,392 bytes allocated in the heap
      66,756,936 bytes copied during GC
         186,992 bytes maximum residency (71 sample(s))
          36,584 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)
```

The _/usr/share/dict/words_ file is about 1 MB in size, but the program has a maximum residency (live memory) of 186,992 bytes.

Let's try to parallelize the program (_rsa1.h_):

```haskell
encrypt, decrypt :: Integer -> Integer -> ByteString -> ByteString
encrypt n e = B.unlines
            . withStrategy (parList rdeepseq)
            . map (B.pack . show . power e n . code)
            . chunk (size n)
decrypt n d = B.concat
            . map (B.pack . decode . power d n)
            . integers
            . B.lines
```


After applying the first naive parallelization the maximum residency memory has increased to 2.3 MB because the __parList Strategy forces the whole spine of the list, preventing the program from streaming in constant space.__

Let's do it better (rsa2.hs):

```haskell
encrypt, decrypt :: Integer -> Integer -> ByteString -> ByteString
encrypt n e = B.unlines
            . withStrategy (parBuffer 100 rdeepseq)
            . map (B.pack . show . power e n . code)
            . chunk (size n)
decrypt n d = B.concat
            . map (B.pack . decode . power d n)
            . integers
            . B.lines
```
