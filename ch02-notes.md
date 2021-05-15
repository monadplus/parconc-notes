# Chapter 2. Basic Parallelism: The Eval Monad (package: _parallel_)

## Evaluation in Haskell

__Normal form__: an expression in normal form is fully evaluated, and no sub-expression could be evaluated any further (i.e. it contains no un-evaluated thunks).

__WHNF__: an expression in weak head normal form has been evaluated to the outermost data constructor or lambda abstraction (the head). Sub-expressions may or may not have been evaluated. Therefore, every normal form expression is also in weak head normal form, though the opposite does not hold in general.

- Defining an expression causes a thunk to be built representing that expression.

- A thunk remains unevaluated until its value is required, Once evaluated, the thunk is replaced by its value.

Example:

```haskell
>>> let xs = map (+1) [1..10] :: [Int]
>>> :sprint xs
xs = _
>>> seq xs ()
>>> :sprint xs
xs = _ : _
>>> length xs
10
>>> :sprint xs
xs = [_,_,_,_,_,_,_,_,_,_]
>>> sum xs
65
>>> :sprint xs
xs = [2,3,4,5,6,7,8,9,10,11]
```

Force evaluation up to WHNF:

```haskell
seq :: a -> b -> b
```

Space leaks:

```haskell
let xs = foldl (+) 0 [1..100000]

-- Solution:
let xs = foldl' (+) 0 [1..100000]
```

Do you see any problem here ?

```haskell
let f (acc, len) x = (acc + x, len + 1) -- this should be a monoid

let xs = foldl' f (0, 0) [1.10000]
```

## [Control.DeepSeq](https://hackage.haskell.org/package/deepseq-1.4.4.0/docs/Control-DeepSeq.html) (Normal form)

```haskell
class NFData where
  rnf :: a -> () -- reduce to normal-form
  -- Since GHC 7.2
  default rnf :: (Generic a, GNFData Zero (Rep a)) => a -> ()
  rnf = grnf RnfArgs0 . from
```

```haskell
data Tree a = Empty |  Branch (Tree a) a (Tree a)

instance NFData a => NFData (Tree a) where
  rnf Empty = ()
  rnf (Branch l a r) = rnf l `seq` rnf a `seq` rnf r
```

The idea is to just recursively apply `rnf` to the components of the data type, composing the calls to `rnf` together with `seq`.

```Haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import GHC.Generics (Generic)
import Control.DeepSeq

data Foo a = Foo a String
             deriving (Eq, Generic, Generic1, NFData, NFData1)

data Colour = Red | Green | Blue
              deriving (Generic, NFData)
```

You will probably use:

```haskell
deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b

force :: NFData a => a -> a -- fully evaluates its argument.
force x = x `deepseq` x
```

Evaluating something to normal form involves traversing the whole of its structures:

- `seq` = O(1)
- `force/deepseq` = O(n)

## [Parallel: the Eval monad](https://hackage.haskell.org/package/parallel-3.2.2.0/docs/Control-Parallel.html)

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

### Example: Paralleling a Sudoku Solver

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

## Amdahl Law

Amdahl's law: `1 / ((1 - P) + P/N)`

P: portion that can be parallelized (over 1)
N: processors.
