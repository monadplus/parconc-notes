# Chapter 5. Data Parallel Programming with Repa

Strategies and Par don't work well on large arrays.

We can't use `Strategies` because they need lazy data structures (boxed arrays ok, unboxed ko).

Par doesn't work well because in `Par` the data is passed in IVars.

- Repa library: REgular PArallel arrays, the library provides a range of efficient operatins for creating arrays and
  operating on arrays in parallel.

```
cabal install repa
$ ghci
ghci> import Data.Array.Repa as Repa
```

### Arrays, Shapes and Indices

`data Array r sh e`

- e Element
- r representation type
- sh shape/dimensions

Shapes are built out of two type constructors:

```haskell
data Z = Z
data tail :. head = tail :. head
```

Z               = scalar (single element)
Z :. Int        = vector
Z :. Int :. Int = matrix

Handy type synonyms are provided:

```haskell
type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. In
```

Simple way to build an array:

`fromListUnboxed :: (Shape sh, Unbox a) => sh -> [a] -> Array U sh a`

U representation stands for Unboxed

10 element array with the numbers 1..10: `fromListUnboxed (Z :. 10) [1..10]` (insufficient type information)

You must add some type information: `fromListUnboxed (Z :. 10) [1..10] :: Array U DIM1 Int`

Two dimensional array, 3 rows of 5 columns: `fromListUnboxed (Z :. 3 :. 5) [1..15] :: Array U DIM2 Int`

> Internally the array is stored as a single vector (after all, computer memory is one-dimensional)

`(!) :: (Shape sh, Source r e) => Array r sh e -> sh -> e`

```haskell
>>> import Data.Array.Repa as Repa
>>> let arr = fromListUnboxed (Z :. 3 :. 5) [1..15] :: Array U DIM2 Int
>>> arr ! (Z:.2:.1)
    12

>>> reshape (Z:.5:.3) arr ! (Z:.2:.1 :: DIM2)
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


### Operations on Arrays

- `Repa.map :: (Shape sh, Source r a) => (a -> b) -> Array r sh a -> Array D sh b`

D stands for Delay; this menas that the array has not been computed yet.

A delayed array is represented by a function from indices to elements.

To compute the array we have to call:

- `computeS :: (Load r1 sh e, Target r2 e) => Array r1 sh e -> Array r2 sh e`

The most important instances of these two classes are:

- D for class Load
- U for class Target

```haskell
>>> let a = fromListUnboxed (Z :. 10) [1..10] :: Array U DIM1 Int
>>> computeS (Repa.map (+1) a) :: Array U DIM1 Int
    AUnboxed (Z :. 10) (fromList [2,3,4,5,6,7,8,9,10,11])
```

This optimization is called __fusion__, the intermediate maps
are not build.

Let's see how it works:

Create delayed array:

- `fromFunction :: sh -> (sh -> a) -> Array D sh a`

For example: `let a = fromFunction (Z :. 10) (\(Z:.i) -> i :: Int)`

We can call `a ! (Z:.5)`: indeexing a delayed array works by just calling the function that we supplied to
_fromFunction_ with the given index.

The _computeS_ function creates the array and for each of the indices of the array, it calls the function stored in the delayed array to find the element at that position.

The _map_ function can be derived from _fromFunction_

`let mymap f a = fromFunction (extent a) (\ix -> f (a ! ix))`

### Example: Computing Shortests Paths

Same example of Floyd-Warshall algorithm for computing the lengths of shortest paths in a sparse
weighted directed graph  but this time over a dense graph.

For reference, here is the pseudocode definition of the algorithm:

```haskell
shortestPath :: Graph -> Vertex -> Vertex -> Vertex -> Weight
shortestPath g i j 0 = weight g i j
shortestPath g i j k = min (shortestPath g i j (k-1))
                           (shortestPath g i k (k-1) + shortestPath g k j (k-1))
```

See _fwdense.hs_

`> ghc fwdense.hs -O2 -fllvm` enables GHC's LLVM backend, which significantly improves
the performance of Repa code.

#### Parallelizing the Program

> Don't forget to add the -fllvm option if your computer supports it

```haskell
computeP :: (Monad m, Source r2 e, Target r2 e, Load r1 sh e)
         => Array r1 sh e
         -> m (Array r2 sh e)
```

computeS computes an array sequentially, computeP uses the available cores to compute the array in parallel.

computeS almost same signature but runs in an arbitrary monad to ensure operations are performed
in sequence and not nested. Identity is a valid monad.

To see the parallelism overhead just compare:

`$ stack exec fwdense 500 -- +RTS -s` - `$ stack exec fwdense1 500 -- +RTS -s`

Then run it on 4 cores:

`$ stack exec fwdense1 500 -- +RTS -s -N4` ~ 2.5 speed up with 0 effort.

#### Monads and computeP

A call to computeP cannot refer to another array calculated with computeP, unless the inner computeP has already been evaluated.

The monad requirement in computeP is there to help us avoid this problem (don't run `runIdentity . computeP` on each step)

### Folding and Shape-Polymorphism

Folding:

```haskell
sumAllS :: (Num a, Shape sh, Source r a, Unbox a, Elt a)
        => Array r sh a
        -> a
```

Fold over just one dimension. The input array has one more
dimension than the output array. __The fold takes place over the inner
dimension of the array__.

```haskell
foldS :: (Shape sh, Source r a, Elt a, Unbox a)
      => (a -> a -> a)
      -> a
      -> Array r (sh :. Int) a
      -> Array U sh a
```

Example: obtain the maximum distance from each vertex to any other vertex.
If we fold once more, we'll find the longest distance between any two nodes in the graph:

```haskell
>>> foldS maxDistance inf (shortestPaths testGraph)
AUnboxed (Z :. 6) (fromList [20,19,31,18,15,21])

>>> foldS maxDistance inf (foldS maxDistance inf (shortestPaths testGraph))
AUnboxed Z (fromList [31])
```

Fold in parallel (arbitrary monad).
Careful, the function argument must be _associative_.

```haskell
foldP :: (Shape sh, Source r a, Elt a, Unbox a, Monad m)
      => (a -> a -> a)
      -> a
      -> Array r (sh :. Int) a
      -> m (Array U sh a)
```

### Example: Image Rotation

Rotate image by its center by a specified number of degrees.

Repa provides an interface to the _DevIL_ library, which is a cross-platform C library
for image manipulation. _DevIL_ supports reading and writing various common image formats.

This library is wrapped by the haskell package _repa-devil_.

```haskell
readImage :: FilePath -> IL Image
writeImage :: FilePath -> Image -> IL ()

data Image
  = RGBA (Array F DIM3 Word8) -- e.g. PNG
  | RGB  (Array F DIM3 Word8) -- e.g. JPEG
  | BGRA (Array F DIM3 Word8)
  | BGR  (Array F DIM3 Word8)
  | Grey (Array F DIM2 Word8)
```

The __F representation type__ same as _U_ but indicates the data is hold in __foreign memory__.

_IL monad_ to ensure the library is initialized properly.

`runIL :: IL a -> IO a` (mutiple calls is perfectly fine in your program)

Code at _rotateimage.hs_

