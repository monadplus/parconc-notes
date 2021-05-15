# Chapter 3. Evaluation Strategies

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

1. example: `runEval (parPair (fib 35, fib 36))`.

This works fine but its betteer to use:

```haskell
using :: a -> Strategy a -> a
x `using` s = runEval (s x)
```

2. example: `(fib 35, fib 36) `using` parPair`

Try _strats.hs_.

### Parameterized Strategies

nb. this is not rpar !

```haskell
evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa  sb (a, b) = do
  a' <- sa a
  b' <- sb b
  retturn (a', b')
```

rpar is an Strategy.

```haskell
parPair :: Strategy (a, b)
parPair = evalPair rpar rpar
```

parPair always evaluates to WHNF. We can make a Strategy that fully evaluates its argumnent:

```haskell
rdeepseq  :: NFData a => Strategy a
rdeepseq x = rseq (force x)
```

How  do we combine `rpar` and `rdeepseq` ?

_Control.Parallel.Strategies_ provides:

`rparWith :: Strategy a -> Strategy a`

```haskell
parPair :: Strategy a -> Strategy b -> Strategy (a,b)
parPair sa sb = evalPair (rparWith sa) (rparWith sb)
```

Now we can write a Strategy that __fully evaluates both components of a pair in paralllel__:

`pairPair rdeepseq rdeepseq :: (NFData a, NFData B) => Strategy (a,b)`

Don't evaluate this component at all:

```haskell
r0 :: Strategy a
r0 x = return x

evalPair (evalPair rpar r0) (evalPair rpar r0) :: Strategy ((a,b),(c,d))
```


### A Strategy for Evaluating a List in Parallel

```haskell
parMap :: (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rseq
```

The `parList` function is a Strategy on lists that evaluates the list elements in parallel.

```haskell
evalList :: Strategy a -> Strategy [a]
evalList strat [] = return []
evalList strat (x:xs) = do
  x'  <- strat x
  xs' <- evalList strat xs
  return (x':xs')
```


```haskell
parList :: Strategy a -> Strategy [a]
parList strat = evalList (rparWith strat)
```

Both `evalList` and `parList` are already provided by _Control.Parallel.Strategies_.

### Example: the k-means problem

Code can be found at: `kmeans/`

- ThreadScope "Raw Events" (bottom tabs) to see the +RTS events:

```
0.851404792s HEC 3: stopping thread 4 (making a foreign call)
0.851405771s HEC 3: running thread 4”
```

There are some foreign calls at kmeans.strat.

Usually FFI indicates some kind of I/O (in this case the print statements).

Comment them out to improve the parallel speed up !

- Visualizing Spark Activity:

Creation and use of sparks: "Traces" (top left tabs)

You can actually see that 64 sparks are created at each iteration start

### GC'd Sparks and Speculative Parallelism


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

__Speculative parallelism__: not necessarily required, and the runtime will automatically discard speculative tasks that
it can prove will never be required. For example, if only few elements of the list are used.

Although the runtime system’s discarding of unreferenced sparks is certainly useful in some cases, it can be tricky to work with because there is no language-level support for catching mistakes. Fortunately, the runtime system will tell us if it garbage-collects unreferenced sparks. For example, if you use the tail-recursive parList with the Sudoku solver from Chapter 2, the +RTS -s stats will show something like this:

`SPARKS: 1000 (2 converted, 0 overflowed, 0 dud, 998 GC'd, 0 fizzled)`

Garbage-collected sparks are reported as “GC’d.” ThreadScope will also indicate GC’d sparks in its spark graphs.

If you see that a large number of sparks are GC’d, it’s a good indication that sparks are being removed from the spark pool before they can be used for parallelism. Unless you are using speculation, a non-zero figure for GC’d sparks is probably a bad sign.

- _Control.Parallel.Strategies_ rules of thumb:

Use `using` to apply Strategies instead of `runEval`.

This is wrong:

```haskell
do
  ...
  rpar (f x)   OR      rparWith strat x
  ...
```

This is Ok:

```haskell
do
  ...
  y <- rpar (f x)
  ... y ...
```

### Parallelizing Lazy Streams with parBuffer

Lazy list as a Stream so that your program can consume input while simultaneously produce output in constant space.

RSA example (rsa.hs): `$ echo "Hello World!" | ./rsa encrypt - | ./rsa decrypt -`

Constant space memory:

```zsh
$ ./rsa encrypt /usr/share/dict/words >/dev/null +RTS -s
   8,040,128,392 bytes allocated in the heap
      66,756,936 bytes copied during GC
         186,992 bytes maximum residency (71 sample(s))
          36,584 bytes maximum slop
               2 MB total memory in use (0 MB lost due to fragmentation)
```

The _/usr/share/dict/words_ file is about 1 MB in size, but the program has a maximum residency (live memory) of 186,992 bytes.

Let's try to parallelize the program (rsa1.h):

_Data.ByteString.Lazy_ to achieve streaming: `encrypt :: Integer -> Integer -> ByteString -> ByteString`

`withStrategy`: same as `using` but with the arguments flipped.

After applying the first naive parallelization the maximum residency memory has increased to 2.3 MB because the __parList Strategy forces the whole spine of the list, preventing the program from streaming in constant space.__

_Control.Parallel.Strategies_ provides a Strategy to solve exactly this problem:

`parBuffer :: Int -> Strategy a -> Strategy [a]`: creates sparks for only the first N elements of the list, and then
creates more sparks as the result list is consumed.

Parallelization at rsa2.hs

### Chunking Strategies

_Control.Parallel.Strategies_ library provides a version of `parList` that has chunking build in:

`parListChunk :: Int -> Strategy a -> Strategy [a]`

`parList` to spark every element
`parListChunk` to sparke very n elements

### The identity Property

`x 'using' s` is equivalent to `x` only if the Strategy s obeys the __identity property__.


x `using` s might be _less defined_ than x:

```
-- OK
print $ snd (1 `div` 0, "Hello!")
- KO
print $ snd ((1 `div` 0, "Hello!") `using` rdeepseq)
```

Rarely a problem in practise, you should change the Strategy because is wasting time evaluating things that are not going to be used.

