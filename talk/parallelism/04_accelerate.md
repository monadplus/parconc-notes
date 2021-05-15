# GPU Programming with [Accelerate](http://hackage.haskell.org/package/accelerate)

GPU have 10-100 times more  raw compute power than the general purpose CPU.

GPU programs: highly parallel where the operations to perform on each data item are identical.

Different instruction set architecture. Special compiler is needed to compile code for GPU.

GPU languages: NVidia CUDA and Open-CL.

_Accelerate_ is an EmbeddedDSL (EDSL) for programming the GPU. Write haskell code and have it run on the GPU.

Backends:

- http://hackage.haskell.org/package/accelerate-1.2.0.1/docs/Data-Array-Accelerate-Interpreter.html
- http://hackage.haskell.org/package/accelerate-llvm-ptx CUDA
- http://hackage.haskell.org/package/accelerate-llvm-native multicore CPUs (e.g. x86)

Since Accelerate is an embedded language, programs written in Accelerate are not compiled by the Haskell compiler (GHC). Rather, each Accelerate backend is a runtime compiler which generates and executes parallel SIMD code of the target language at application runtime.

Both steps happen while the programming is running; there's no extra compile step, apart from compiling the Haskell program itself.

## Arrays and Indices

Accelerate is a framework for programming with arrays. Takes arrays as inputs and delivers one or more arrays as outputs.

`data Array sh e`

~ operations are fused in much the same way as in Repa.

```haskell
data Z = Z
data tail :. head = tail :. head

type DIM0 = Z
type DIM1 = DIM0 :. Int
type DIM2 = DIM1 :. Int


type Scalar e = Array DIM0 e
type Vector e = Array DIM1 e
```

Build array from list:

```haskell
indexArray :: forall sh e. Array sh e -> sh -> e
fromList :: (Shape sh, Elt e) => sh -> [e] -> Array sh e

>>> import Data.Array.Accelerate as A
>>> import Data.Array.Accelerate.Interpreter as I
>>> let arr = fromList (Z:.3:.5) [1..] :: Array DIM2 Int
>>> indexArray arr (Z:.2:.1)
    12
```
Array data is store unboxed in an unzipped struct-of-array representation. Elements are laid out in row-major order (the right-most index of a Shape is the fastest varying). The allowable array element types are members of the Elt class, which roughly consists of:

- Signed and unsigned integers (8, 16, 32, and 64-bits wide).
- Floating point numbers (single and double precision)
- Char
- Bool
- ()
- Shapes formed from Z and (:.)
- Nested tuples of all of these, currently up to 15-elements wide.

Note that Array itself is not an allowable element type - there are no nested arrays in Accelerate, regular arrays only!

Arrays of tuples is ok (interally will be translated to a tuple of arrays).

## Running a Simple Accelerate Computation

An Accelerate computation takes the form:

`run :: Arrays a => Acc a -> a`

The class _Arrays_ allows `a` to be either an array or a tuple of arrays.

A value of type `Acc a` is really a data structure and the `run` function evaluates the data structure to produce a result.

There are to version of run:

- One exported by _Data.Array.Accelerate.Interpreter_ (testing)
- One exported by _Data.Array.Accelerate.CUDA_ (from _accelerate-cuda_ package).

```haskell
A.map :: (Shape ix, Elt a, Elt b)
      => (Exp a -> Exp b)
      -> Acc (Array ix a)
      -> Acc (Array ix b)
```

`Exp a`: is like `Acc a` but for single values instead of arrays. There's an instance for `Num (Exp a)`

```haskell
use :: Arrays arrays => arrays -> Acc arrays -- Haskell to Accelerate computation.
                                             -- might actually involve copying the array from the computer's main memory into the GPU's memory.
```

```haskell
>>> let arr = fromList (Z:.3:.5) [1..] :: Array DIM2 Int

>>> run $ A.map (+1) (use arr)
    Array (Z:.3:.5) [2,3,4,...,16]

>>> run $ A.map (^2) (use arr)
    Array (Z:.3:.5) [1,4,9,...,225]
```

## Scalar Arrays

If we want to return a single value, we have to wrap it in an array first.

The unit operation is provided for this purpose:

```haskell
unit :: Elt e => Exp e -> Acc (Scalar e)

>>> run $ unit (3 :: Exp Int) -- Array (Z) [3]
```

The dual to unit is _the_, which extracts a single value from a Scalar:

```haskell
the :: Elt e => Acc (Scalar e) -> Exp e
```

## Indexing Arrays

```haskell
index1 :: Exp Int -> Exp (Z :. Int)

(!) :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp ix -> exp e -- _indexArray_ but in the Accelerate World.
```

Putting this together:

```haskell
>>> let arr = fromList (Z:.10) [1..10] :: Array DIM1 Int
>>> run $ unit (use arr ! index1 3)
    Array (Z)  [4]
```

## Creating Arrays inside Acc

```haskell
-- >>> run $ fill (constant (Z:.10)) 0 :: Vector Float
-- Vector (Z :. 10) [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
fill :: (Shape sh, Elt e)
     => Exp sh -> Exp e -> Acc (Array sh e)

-- enumFromN (index1 N) M == use $ fromList (Z:.N) [M..]
enumFromN :: (Shape sh, Elt e, IsNum e)
          => Exp sh -> Exp e -> Acc (Array sh e)

enumFromStepN :: (Shape sh, Elt e, IsNum e)
              => Exp sh -> Exp e -> Exp e -> Acc
```


```zsh
>>> run $ enumFromStepN (index2 3 5) 15 (-1) :: Array DIM2 Int
    Array DIM2 Int
    Array (Z:.3:.5)
    [15,14,...,1]
```

A more general way to create arrays is provided by _generate_:

```haskell
generate :: (Shape ix, Elt a)
         => Exp ix -> (Exp ix -> Exp a)
         -> Acc (Array ix a)
```

Example:

```haskell
>>> run $ generate (index2 3 5) (\ix -> let Z:.y:.x = unlift ix in x + y) :: Matrix Int
[0,1,2,3,4,1,2,3,4,5,2,3,4,5,6]
```

```haskell
unlift :: Exp (Z :. Int :. Int) -> Z :. Exp Int :. Exp Int

-- dual of unlift
lift ::   Z :. Exp Int :. Exp Int -> Exp (Z :. Int :. Int)

-- index2 is defined in terms of lift:
index2 :: Exp Int -> Exp Int -> Exp DIM2
index2 i j = lift (Z :. i :. j)
```

## Zipping Two Arrays

```haskell
zipWith :: (Shape ix, Elt a, Elt b, Elt c)
        => (Exp a -> Exp b -> Exp c)
        -> Acc (Array ix a) -- Same shape
        -> Acc (Array ix b) -- Same shape
        -> Acc (Array ix c)
```

For example, zipping two arrays with (+):

```haskell
>>> let a = enumFromN (index2 2 3) 1 :: Acc (Array DIM2 Int)
>>> let b = enumFromStepN (index2 2 3) 6 (-1) :: Acc (Array DIM2 Int)
>>> run $ A.zipWith (+) a b
Array (Z :. 2 :. 3) [7,7,7,7,7,7]
```

_zipWith_ requires both arrays to have same dimensionality, but size might be different. The final array has the shape of the overlapping portion of the two arrays.

```haskell
>>> let a = enumFromN (index2 2 3) 1 :: Acc (Array DIM2 Int)
>>> let b = enumFromStepN (index2 3 5) 10 10 :: Acc (Array DIM2 Int)
>>> run $ A.zipWith (+) a b
    Array (Z :. 2 :. 3) [11,22,33,64,75,86]
```

## Constants

What if we already have an Int value and we need an Exp Int ?

`constant :: Elt t => t -> Exp t`

## Example: Shortest Paths (Floyd-Warshall)

See _fwaccel.hs_:

```haskell
shortestPaths :: Graph -> Graph
shortestPaths g0 = run (shortestPathsAcc n (use g0))
  where
    -- arrayShape :: Shape sh => Array sh e -> sh
    Z :. _ :. n = arrayShape g0

shortestPathsAcc :: Int -> Acc Graph -> Acc Graph
shortestPathsAcc n g0 = foldl1 (>->) steps g0
 where
  steps :: [ Acc Graph -> Acc Graph ]
  steps =  [ step (unit (constant k)) | k <- [0 .. n-1] ]

-- You can't pass Int, it must be an Scalar.
step :: Acc (Scalar Int) -> Acc Graph -> Acc Graph
-- generate :: (Shape sh, Elt a) => Exp sh -> (Exp sh -> Exp a) -> Acc (Array sh a)
step k g = generate (shape g) sp -- shape :: (Shape sh, Elt e) => Acc (Array sh e) -> Exp sh
 where
   k' = the k -- Acc (Scalar e) -> Exp e

   sp :: Exp DIM2 -> Exp Weight
   sp ix = let
             (Z :. i :. j) = unlift ix -- Exp (Z :. Int :. Int) -> (Z :. Exp Int :. Exp Int)
           in
             A.min (g ! (index2 i j))
                   (g ! (index2 i k') + g ! (index2 k' j))
```

## Example: A Mandelbrot Set Generator

Super cool blogpost using Accelerate to generate a Mandelbrot set: http://www.acceleratehs.org/examples/mandelbrot.html

