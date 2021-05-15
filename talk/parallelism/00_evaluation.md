# Evaluation in Haskell

__Normal form__: an expression in normal form is fully evaluated, and no sub-expression could be evaluated any further (i.e. it contains no un-evaluated thunks).

__WHNF__: an expression in weak head normal form has been evaluated to the outermost data constructor or lambda abstraction (the head). Sub-expressions may or may not have been evaluated. Therefore, every normal form expression is also in weak head normal form, though the opposite does not hold in general.

- Defining an expression causes a thunk to be built representing that expression.

- A thunk remains unevaluated until its value is required, once evaluated, the thunk is replaced by its value.

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

## [NFData](https://hackage.haskell.org/package/deepseq-1.4.4.0/docs/Control-DeepSeq.html)

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

## Amdahl's Law

Amdahl's law: `1 / ((1 - P) + P/N)`

- P: portion that can be parallelized (over 1)
- N: processors

