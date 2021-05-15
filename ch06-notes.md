# Chapter 6. GPU Programming with _Accelerate_

GPU have 10-100 times more  raw compute power than the general purpose CPU.

GPU programs: highly parallel where the operations to perform on each data item are identical.

Different instruction set architecture. Special compiler is needed to compile code for GPU.

GPU languages: NVidia CUDA and Open-CL.

_Accelerate_ is an EmbeddedDSL (EDSL) for programming the GPU. Write haskell code and have it run on the GPU.

```haskell
$ cabal install accelerate
$ ghci
>>> import Data.Array.Accelerate as A
>>> import Data.Array.Accelerate.Interpreter as I
```

- _Data.Array.Accelerate_: constructing array computations.
- _Data.Array.Accelerate.Interpreter_: for interpreting them.

Accelerate code works like this:

- The haskell code generates a data structure in an internal representation.
- This structure is then compiled into GPU code using the _accelerate-cuda_ package
and run directly on the GPU. When you don't have a GPU, the _accelerate_ package interprets
the code instead. Running on the GPU is much faster.

Both steps happen while the programming is running; there's no extra compile step,
apart from compiling the Haskell program itself.

### Arrays and Indices

Accelerate is a framework for programming with arrays. Takes arrays as inputs and delivers
one or more arrays as outputs.

`data Array sh e`

Even though Accelerate does have delayed arrays internally and composition
of arrays operations are fused in much the same way as in Repa.

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
fromList :: (Shape sh, Elt e) => sh -> [e] -> Array sh e

>>> fromList (Z:.10) [1..10] :: Vector Int
>>> let arr = fromList (Z:.3:.5) [1..] :: Array DIM2 Int
>>> indexArray arr (Z:.2:.1)
    12
```

> Arrays can't be nested ! (they must map directly into flat arrays on the GPU)

Arrays of tuples is ok (interally will be translated to a tuple of arrays).

### Running a Simple Accelerate Computation

An Accelerate computation takes the form:

`run :: Arrays a => Acc a -> a`

The class _Arrays_ allows `a` to be either an array or a tuple of arrays.

A value of type `Acc a` is really a data structure and
the `run` function evaluates the data structure to produce a result.

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

`use :: Arrays arrays => arrays -> Acc arrays`: Haskell World to Accelerate computation (might actually involve copying the array from the computer's main memory into the GPU's memory:

```haskell
>>> let arr = fromList (Z:.3:.5) [1..] :: Array DIM2 Int
>>> run $ A.map (+1) (use arr)
    Array (Z:.3:.5) [2,3,4,...,16]
>>> run $ A.map (^2) (use arr)
    Array (Z:.3:.5) [1,4,9,...,225]
```

#### Type classes: Elt, Arrays, and Shape (_Data.Array.Accelerate_)

- Elt: arrray elements (arrays are not instance of Elt to avoid nesting).
- Arrays: arrays and tuples of arrays.
- Shape: Z and :.

### Scalar Arrays

If we want to return a single value, we have to wrap it in an array first in array first. The unit operation is provided for this purpose:

```haskell
unit :: Elt e => Exp e -> Acc (Scalar e)

>>> run $ unit (3 :: Exp Int)
    Array (Z) [3]
```

The dual to unit is _the_, which extracts a single value from a Scalar:

```haskell
the :: Elt e => Acc (Scalar e) -> Exp e
```

### Indexing Arrays

```haskell
(!) :: (Shape ix, Elt e) => Acc (Array ix e) -> Exp ix -> exp e
```

_(!)_ like _indexArray_ but in the Accelerate World.

```haskell
index1 :: Exp Int -> Exp (Z :. Int)
```

Putting this together:

```haskell
>>> let arr = fromList (Z:.10) [1..10] :: Array DIM1 Int
>>> run $ unit (use arr ! index1 3)
    Array (Z)  [4]
```

### Creating Arrays Inside Acc

```haskell
fill :: (Shape sh, Elt e)
     => Exp sh -> Exp e -> Acc (Array sh e)

enumFromN :: (Shape sh, Elt e, IsNum e)
          => Exp sh -> Exp e -> Acc (Array sh e)

enumFromStepN :: (Shape sh, Elt e, IsNum e)
              => Exp sh -> Exp e -> Exp e -> Acc
```

- enumFromN (index1 N) M == use (fromList (Z:.N) [M..])

```haskell
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
>>> run $ generate (index2 3 5) (\ix -> let Z:.y:.x = unlift ix in x + y)
    Array (Z:.3:.5)
    [0,1,2,3,4,1,2,3,4,5,2,3,4,5,6]
```

`(\ix -> let Z:.y:.x = unlift ix in x + y)` must have type `Exp DIM2 -> Exp Int`

```haskell
unlift :: Exp (Z :. Int :. Int) -> Z :. Exp Int :. Exp Int

-- dual of unlift
lift ::   Z :. Exp Int :. Exp Int -> Exp (Z :. Int :. Int)

-- index2 is defined in terms of lift:
index2 :: Exp Int -> Exp Int -> Exp DIM2
index2 i j = lift (Z :. i :. j)
```

### Zipping Two Arrays

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

_zipWith_ requires both arrays to have same dimensionality, but size might be different.
The finall array has the shape of the overlapping portion of thee two arrays.

```haskell
>>> let a = enumFromN (index2 2 3) 1 :: Acc (Array DIM2 Int)
>>> let b = enumFromStepN (index2 3 5) 10 10 :: Acc (Array DIM2 Int)
>>> run $ A.zipWith (+) a b
    Array (Z :. 2 :. 3) [11,22,33,64,75,86]
```

### Constants

What if we already have an Int value and we need an Exp Int ?

`constant :: Elt t => t -> Exp t`

### Example: Shortest Paths (Floyd-Warshall)

See _fwaccel.hs_.

- Why did we pass in the _k_ value as an Acc (Scalar Int) rather than a plain Int ?

See books' explanation (large and complex).

To execute it  on the interpreter: `>>> shortestPaths testGraph`

#### Running on the GPU (very fast)

First install: `cabal install accelerate-cuda -fdebug`

And use the import: `import Data.Array.Accelerate.CUDA` in place of `import Data.Array.Accelerate.Interpreter`.

```haskell
$ ghc -O2 fwaccel.hs -threaded
$ ./fwaccel 2000 +RTS -s
```

#### Debugging the CUDA Backend

When the _accelerate-cuda_ package is compiled with _-fdebug_ there are a few extra debuggin options available.

- dverbose: prints some information about the type and capabilities of the GPU being used.
- ddump-cc: prints information about CUDA kernels as they are compiled and run.

### Example: A Mandelbrot Set Generator

Points in the complex plane.

A particular point is said to be in the set if, the magnitude of _z_ (written as _lzl_)
does not diverge to infinity (in practice, we iterate the equation for a fixed number of times).

```haskell
Z(n+1) = c + (Zn)^2

and Z0 = c
```

To generate a pretty picture, we remember the iteration at which each point diverged and map
the iteration values to a color gradient.

We know that _lzl_ will definitely diverge it it is greater than 2. The magnitude of a complex number _x + iy_ is given by sqrt(x^2 + y^2) so we can simplify squaring both sides, __giving us this condition of divergence: x^2 + y^2 > 4__

Take a look at: _mandel/mandel.hs_
