{-# LANGUAGE InstanceSigs #-}
{-
Add your code to this file in the positions indicated with "Problem N Answer:".
The names of the functions SHOULD NOT BE CHANGED. We may grade (parts of) this
assignment with a script - if your code doesn't define the functions with the exact names
specified, they will be assumed skipped. We WILL NOT adjust by hand to account for this.
Give type signatures for all top level functions.

Once you've finished editing this file, submit on BrightSpace as normal.
-}

{-
To start ghci, start the command line and type:
    ghci
To (re)load this file type:
    :l HW8.hs
-}

module HW9 where

import Prelude hiding (map, filter, lookup)

{-
Problem 1:

Consider the below Map data structure, which allows mapping keys k to values v.
Implement insert, to insert key/value pairs into the Map.
Implement lookup, to lookup the value of a particular key (or nothing, if that key is not
in the Map.)
Design your functions to use a binary search.

You may add typeclass requirements to the type signatures, but do not change them in any other way.
-}

data Map k v = Branch (Map k v) k v (Map k v) | Leaf deriving Show

insert :: Ord k => k -> v -> Map k v -> Map k v
insert key val Leaf = Branch Leaf key val Leaf
insert key val (Branch leftT k v rightT)
  | key < k = Branch (insert key val leftT) k v rightT
  | key > k = Branch leftT k v (insert key val rightT)
  | key == k = Branch leftT key val rightT

-- *HW9> insert 5 6 (insert 9 6 (insert 4 12 (insert 7 1 Leaf)))
-- Branch (Branch Leaf 4 12 (Branch Leaf 5 6 Leaf)) 7 1 (Branch Leaf 9 6 Leaf)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup _ Leaf = Nothing
lookup key (Branch leftT k v rightT)
  | key < k = lookup key leftT
  | key > k = lookup key rightT
  | key == k = Just v

-- *HW9> lookup 9 (Branch (Branch Leaf 4 12 (Branch Leaf 5 6 Leaf)) 7 1 (Branch Leaf 9 6 Leaf))
-- Just 6
-- *HW9> lookup 1 (Branch (Branch Leaf 4 12 (Branch Leaf 5 6 Leaf)) 7 1 (Branch Leaf 9 6 Leaf))
-- Nothing


{-
Problem 2:

Fill in the below Functor and Foldable instances for Map.
Keep in mind that the intializer passed to a fold function should be used exactly once.
When folding, the values in the Map should be passed to the combining operator
in descending order of their associated keys (assuming a valid binary tree.)
You may find it helpful to define a helper function to implement Foldable.
-}

instance Functor (Map k) where
  fmap :: (a -> b) -> Map k a -> Map k b
  fmap f Leaf = Leaf
  fmap f (Branch leftT k v rightT) = Branch (fmap f leftT) k (f v) (fmap f rightT)

-- *HW9> fmap (+ 1) (Branch (Branch Leaf 4 8 Leaf) 9 6 (Branch (Branch Leaf 7 21 Leaf) 8 5 Leaf))
-- Branch (Branch Leaf 4 9 Leaf) 9 7 (Branch (Branch Leaf 7 22 Leaf) 8 6 Leaf)

instance Foldable (Map k) where
  foldr :: (a -> b -> b) -> b -> Map k a -> b
  foldr f acc Leaf = acc
  foldr f acc (Branch leftT _ v rightT) =
    let rightFold = foldr f acc rightT
        midFold = f v rightFold
    in foldr f midFold leftT

-- *HW9> foldr (-) 0 (Branch (Branch Leaf 4 8 Leaf) 6 6 (Branch (Branch Leaf 7 21 Leaf) 8 5 Leaf))
-- 18

{-
Problem 3:

Hoogle is a search engine for Haskell functions:
    https://hoogle.haskell.org
You can enter function names, type names, or typeclass names into Hoogle to find those functions.
You can also enter types, and find functions with those types.  For example, if you enter:
    (p -> q) -> [p] -> [q]
Hoogle will find the function:
    map :: (a -> b) -> [a] -> [b]
(and a large number of slight variants on map.)

The Applicative typeclass extends Functors to allow applying functions wrapped in a Functor.
Look up Applicative using Hoogle.

Also note the laws for the Applicative typeclass. You will need to consider these laws to answer
the following question.

Consider the below definition of an Applicative instance for Map.
What is wrong with this definition of Applicative?  Is it fixable?  Explain.

Write your answer in a comment.

-}
{-
This definition of an Applicative instance for Map violates some of the laws for the Applicative typeclass.
It violates the identity law via the line pure x = Leaf, where according to the identity law, pure x should output
the initial value, or x. The defintion of m1 <*> m2 also violates the composition law of Applicative typeclasses, because
<*> should just be applying the function onto the values in m2, but the defintion below could alter the structure
of the Map, which means it would be the violation of the composition law. If you were to rewrite the code to preserve
the map structure when using <*> and keep the identity law , there would be no violations, so I believe this code could
be fixed.

instance Ord k => Applicative (Map k) where
    pure x = Leaf
    m1 <*> m2 = foldr f Leaf (keyValPairs m2) 
        where
            f (k, v) m = case lookup k m1 of
                                Just f -> insert k (f v) m
                                Nothing -> m

keyValPairs :: Map k v -> [(k, v)]
keyValPairs (Branch l k v r) = keyValPairs l ++ (k, v):keyValPairs r
keyValPairs Leaf = []
-}

{-
Problem 4:

Both map and filter can be defined by passing the correct function and initalizer to foldr.
Fill in the below two definitions to define map and filter using foldr.
-}

map :: (a -> b) -> [a] -> [b]
map f = foldr doFunc []
  where
    doFunc val result = (f val):result

filter :: (a -> Bool) -> [a] -> [a]
filter pred = foldr ifTrue []
  where
    ifTrue val result
      |  pred val = val:result
      |  otherwise = result

{- Problem 5:

Peano numbers are a way of representing the natural numbers
using a Zero constructor and a Succ (or Successor) constructor.

0 is represented as Zero.
1 is represented as (Succ Zero).
2 is represented as (Succ (Succ Zero)).
3 is represented as (Succ (Succ (Succ Zero))).
And so on...


(a) Use a deriving clause to get Eq, Ord, and Show instances for the below definition of Peano numbers.
Test the instances.  What bug do you notice?  Adjust the data definition to fix the bug.

I noticed Eq and Show worked fine but Ord was off because it thought  Zero was greater than Succ Zero.
To fix this bug I adjusted Zero to come before Succ Peano in the data defintion then Ord began working.

(b) Using Hoogle, look up the Enum typeclass.  Define (by hand) an instance of Enum for Peano.
(In practical cases, Enum can be derived, just like Eq or Ord.)
You may assume that toEnum is passed a non-negative Int.

I made the instance of Enum for Peano, but for some reason toEnum I think the deafult toEnum for ints is being used 
instead of mine throwing a weird error, i've provided toEnumP to show the exact same definiton works when the name
doesn't conflict with anything. 

(c) Define an instance of Num for Peano.

Strictly speaking, Num does not have any laws, but often instances are expected to form a Ring.
    https://mathworld.wolfram.com/Ring.html
Is this possible in the case of Peano?  Are there compromises or decisions you had to make in
defining the Num instance?  Explain in a comment.

In the case of Peano inside my Num instance I decided not to implement (-), negate, or abs because there is no way to 
represent negative numbers with Peano Numbers. I think I could of done a partial subtraction but I figured if it doesn't
work for all cases why bother trying to implement it. Also the Ring cannot  be formed because the additive inverse cannot 
be fufilled again due to negative numbers not being possible.

(d) Define an instance of Foldable for Peano, or explain why defining an instance of
Foldable for Peano is not possible/does not make sense. 

I don't think that definiting an instance of Foldable for Peano makes sense. This is because foldable is useful for doing
something like a filter or functor over a dataset like a list or something, but since Peano numbers just represent a single
natural number, there's not really a point in Foldable being implemented for it.  
-}


data Peano = Zero | Succ Peano deriving (Eq, Ord, Show)
toEnumP :: Int -> Peano
toEnumP 0 = Zero
toEnumP v = Succ (toEnumP (v - 1))

instance Enum Peano where
  fromEnum :: Peano -> Int
  fromEnum Zero = 0
  fromEnum (Succ v) = 1 + fromEnum v
  toEnum :: Int -> Peano
  toEnum 0 = Zero
  toEnum v = (Succ(toEnum (v-1)))

instance Num Peano where
  (+):: Peano -> Peano -> Peano
  Zero + v = v
  (Succ a) + b = Succ(a + b)
  (*):: Peano -> Peano -> Peano
  Zero * v = Zero
  (Succ a) * b = b + (a * b)
  fromInteger:: Integer -> Peano
  fromInteger 0 = Zero
  fromInteger v = Succ (fromInteger (v - 1))
  signum 0 = 0
  signum _ = 1
