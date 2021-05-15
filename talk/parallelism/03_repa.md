# Data Parallel Programming with [Repa](http://hackage.haskell.org/package/repa-3.4.1.4/docs/Data-Array-Repa.html)

Strategies and Par don't work well on large arrays.

We can't use `Strategies` because they need lazy data structures (boxed arrays ok, unboxed ko).

Par doesn't work well because in `Par` the data is passed in IVars.

__Repa library__: REgular PArallel arrays, the library provides a range of efficient operatins for creating arrays and operating on arrays in parallel.

## Arrays, Shapes and Indices

```haskell
data Array r sh e
```

- e Element
- r representation type
- sh shape/dimensions

Indexes are built out of two type constructors:

```haskell
data Z
data h :. t
```

- Z               = scalar (single element)
- Z :. Int        = vector
- Z :. Int :. Int = matrix

Handy type synonyms are provided:

```haskell
type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int
```

Simple way to build an array:

```haskell
fromListUnboxed :: (Shape sh, Unbox a) => sh -> [a] -> Array U sh a -- U representation stands for Unboxed
```

10 element array with the numbers 1..10: `fromListUnboxed (Z :. 10) [1..10]` (insufficient type information)

You must add some type information: `fromListUnboxed (Z :. 10) [1..10] :: Array U DIM1 Int`

Two dimensional array, 3 rows of 5 columns: `fromListUnboxed (Z :. 3 :. 5) [1..15] :: Array U DIM2 Int`

> Internally the array is stored as a single vector (after all, computer memory is one-dimensional)

```zsh
# (!) :: (Shape sh, Source r e) => Array r sh e -> sh -> e`

>>> import Data.Array.Repa as Repa
>>> let arr = fromListUnboxed (Z :. 3 :. 5) [1..15] :: Array U DIM2 Int
>>> arr ! (Z:.2:.1)
    12

# reshape :: (Shape sh1, Shape sh2, Source r1 e) => sh2 -> Array r1 sh1 e -> Array D sh2 e
>>> (reshape (Z:.5:.3) arr) ! (Z:.2:.1 :: DIM2)
    8
```

```haskell
rank :: Shape sh => sh -> Int  -- number of dimensions
size :: Shape sh => sh -> Int  -- number of elements
extent :: (Shape sh, Source r e) => Array r sh e -> sh   -- shape of the array
```

For example:

```haskell
>>> extent arr
    (Z :. 3) :. 5
>>> rank (extent arr)
    2
>>> size (extent arr)
    15
```

## Operations on Arrays

```haskell
-- | D stands for Delay; this menas that the array has not been computed yet.
Repa.map :: (Shape sh, Source r a) => (a -> b) -> Array r sh a -> Array D sh b
```

A delayed array is represented by a function from indices to elements.

To compute the array we have to call:

```haskell
computeS :: (Load r1 sh e, Target r2 e) => Array r1 sh e -> Array r2 sh e
```

The most important instances of these two classes are:

- D for class Load
- U for class Target

```zsh
>>> let a = fromListUnboxed (Z :. 10) [1..10] :: Array U DIM1 Int
>>> computeS (Repa.map (+1) a) :: Array U DIM1 Int
    AUnboxed (Z :. 10) (fromList [2,3,4,5,6,7,8,9,10,11])
```

This optimization is called __fusion__, the intermediate maps are not build.

Create delayed array:

```haskell
fromFunction :: sh -> (sh -> a) -> Array D sh a
```

For example: `let a = fromFunction (Z :. 10) (\(Z:.i) -> i :: Int)`

We can call `a ! (Z:.5)`: indeexing a delayed array works by just calling the function that we supplied to
_fromFunction_ with the given index.

The _computeS_ function creates the array and for each of the indices of the array, it calls the function stored in the delayed array to find the element at that position.

The _map_ function can be derived from _fromFunction_: `let mymap f a = fromFunction (extent a) (\ix -> f (a ! ix))`

## computeP

```haskell
computeP :: (Monad m, Source r2 e, Target r2 e, Load r1 sh e)
         => Array r1 sh e
         -> m (Array r2 sh e)
```

computeS computes an array sequentially, computeP uses the available cores to compute the array in parallel.

computeS almost same signature but runs in an arbitrary monad to ensure operations are performed in sequence and not nested. Identity is a valid monad. A call to computeP cannot refer to another array calculated with computeP, unless the inner computeP has already been evaluated.

## Example: Computing Shortests Paths

Floyd-Warshall algorithm for computing the lengths of shortest paths in a sparse weighted directed graph  but this time over a dense graph.

For reference, here is the pseudocode definition of the algorithm:

```haskell
shortestPath :: Graph -> Vertex -> Vertex -> Vertex -> Weight
shortestPath g i j 0 = weight g i j
shortestPath g i j k = min (shortestPath g i j (k-1))
                           (shortestPath g i k (k-1) + shortestPath g k j (k-1))
```

Sequential version (_fwdense.hs_):

```haskell
type Weight = Int
type Graph r = Array r DIM2 Weight -- Adjacency Matrix

shortestPaths :: Graph U -> Graph U
shortestPaths g0 = go g0 0
  where
    Z :. _ :. n = extent g0
  -- You could have used foldl' but Repa works much better for explicit recursion
  -- when all the code is visible to the compiler (if you have time, try both approaches!)
    go !g !k | k == n    = g
             | otherwise =
                 -- computeS on each step to avoid calling a nest of k functions
                 -- every time we index into the current graph
                 let g' = computeS (fromFunction (Z:.n:.n) sp)
                 in  go g' (k+1)
     where
       sp (Z:.i:.j) = min (g ! (Z:.i:.j))
                          (g ! (Z:.i:.k) + g ! (Z:.k:.j))
```

Parallel version (_fwdense1.hs_):

```haskell
shortestPaths :: Graph U -> Graph U
shortestPaths g0 = runIdentity $ go g0 0
  where
    Z :. _ :. n = extent g0

    go !g !k | k == n    = return g
             | otherwise = do
                 g' <- computeP (fromFunction (Z:.n:.n) sp)
                 go g' (k+1)
     where
        sp (Z:.i:.j) = min (g ! (Z:.i:.j))
                           (g ! (Z:.i:.k) + g ! (Z:.k:.j))
```

>  ghc-options: -fllv  enables GHC's LLVM backend, which significantly improves the performance of Repa code.

To see the parallelism overhead just compare:

```zsh
# Sequential (weird execution time)
$ stack exec fwdense 500 -- +RTS -s
# Parallel
$ stack exec fwdense1 500 -- +RTS -s
```

Then run it on all cores:

```zsh
$ stack exec fwdense1 500 -- +RTS -s -N4
```
