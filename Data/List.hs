{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}

module Data.List (
   -- * Basic functions

     (++)              -- :: [a] -> [a] -> [a]
   , head              -- :: [a] -> a
   , last              -- :: [a] -> a
   , tail              -- :: [a] -> [a]
   , init              -- :: [a] -> [a]
   , null              -- :: [a] -> Bool
   , length            -- :: [a] -> Int

   -- * List transformations
   , map               -- :: (a -> b) -> [a] -> [b]
   , reverse           -- :: [a] -> [a]

   , intersperse       -- :: a -> [a] -> [a]
   , intercalate       -- :: [a] -> [[a]] -> [a]
   , transpose         -- :: [[a]] -> [[a]]

   , subsequences      -- :: [a] -> [[a]]
   , permutations      -- :: [a] -> [[a]]

   -- * Reducing lists (folds)

   , foldl             -- :: (a -> b -> a) -> a -> [b] -> a
   , foldl'            -- :: (a -> b -> a) -> a -> [b] -> a
   , foldl1            -- :: (a -> a -> a) -> [a] -> a
   , foldl1'           -- :: (a -> a -> a) -> [a] -> a
   , foldr             -- :: (a -> b -> b) -> b -> [a] -> b
   , foldr1            -- :: (a -> a -> a) -> [a] -> a

   -- ** Special folds

   , concat            -- :: [[a]] -> [a]
   , concatMap         -- :: (a -> [b]) -> [a] -> [b]
   , and               -- :: [Bool] -> Bool
   , or                -- :: [Bool] -> Bool
   , any               -- :: (a -> Bool) -> [a] -> Bool
   , all               -- :: (a -> Bool) -> [a] -> Bool
   , sum               -- :: (Num a) => [a] -> a
   , product           -- :: (Num a) => [a] -> a
   , maximum           -- :: (Ord a) => [a] -> a
   , minimum           -- :: (Ord a) => [a] -> a

   -- * Building lists

   -- ** Scans
   , scanl             -- :: (a -> b -> a) -> a -> [b] -> [a]
   , scanl1            -- :: (a -> a -> a) -> [a] -> [a]
   , scanr             -- :: (a -> b -> b) -> b -> [a] -> [b]
   , scanr1            -- :: (a -> a -> a) -> [a] -> [a]

   -- ** Accumulating maps
   , mapAccumL         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
   , mapAccumR         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])

   -- ** Infinite lists
   , iterate           -- :: (a -> a) -> a -> [a]
   , repeat            -- :: a -> [a]
   , replicate         -- :: Int -> a -> [a]
   , cycle             -- :: [a] -> [a]

   -- ** Unfolding
   , unfoldr           -- :: (b -> Maybe (a, b)) -> b -> [a]

   -- * Sublists

   -- ** Extracting sublists
   , take              -- :: Int -> [a] -> [a]
   , drop              -- :: Int -> [a] -> [a]
   , splitAt           -- :: Int -> [a] -> ([a], [a])

   , takeWhile         -- :: (a -> Bool) -> [a] -> [a]
   , dropWhile         -- :: (a -> Bool) -> [a] -> [a]
   , span              -- :: (a -> Bool) -> [a] -> ([a], [a])
   , break             -- :: (a -> Bool) -> [a] -> ([a], [a])

   , stripPrefix       -- :: Eq a => [a] -> [a] -> Maybe [a]

   , group             -- :: Eq a => [a] -> [[a]]

   , inits             -- :: [a] -> [[a]]
   , tails             -- :: [a] -> [[a]]

   -- ** Predicates
   , isPrefixOf        -- :: (Eq a) => [a] -> [a] -> Bool
   , isSuffixOf        -- :: (Eq a) => [a] -> [a] -> Bool
   , isInfixOf         -- :: (Eq a) => [a] -> [a] -> Bool

   -- * Searching lists

   -- ** Searching by equality
   , elem              -- :: a -> [a] -> Bool
   , notElem           -- :: a -> [a] -> Bool
   , lookup            -- :: (Eq a) => a -> [(a,b)] -> Maybe b

   -- ** Searching with a predicate
   , find              -- :: (a -> Bool) -> [a] -> Maybe a
   , filter            -- :: (a -> Bool) -> [a] -> [a]
   , partition         -- :: (a -> Bool) -> [a] -> ([a], [a])

   -- * Indexing lists
   -- | These functions treat a list @xs@ as a indexed collection,
   -- with indices ranging from 0 to @'length' xs - 1@.

   , (!!)              -- :: [a] -> Int -> a

   , elemIndex         -- :: (Eq a) => a -> [a] -> Maybe Int
   , elemIndices       -- :: (Eq a) => a -> [a] -> [Int]

   , findIndex         -- :: (a -> Bool) -> [a] -> Maybe Int
   , findIndices       -- :: (a -> Bool) -> [a] -> [Int]

   -- * Zipping and unzipping lists

   , zip               -- :: [a] -> [b] -> [(a,b)]
   , zip3
   , zip4, zip5, zip6, zip7

   , zipWith           -- :: (a -> b -> c) -> [a] -> [b] -> [c]
   , zipWith3
   , zipWith4, zipWith5, zipWith6, zipWith7

   , unzip             -- :: [(a,b)] -> ([a],[b])
   , unzip3
   , unzip4, unzip5, unzip6, unzip7

   -- * Special lists

   -- ** Functions on strings
   , lines             -- :: String   -> [String]
   , words             -- :: String   -> [String]
   , unlines           -- :: [String] -> String
   , unwords           -- :: [String] -> String

   -- ** \"Set\" operations

   , nub               -- :: (Eq a) => [a] -> [a]

   , delete            -- :: (Eq a) => a -> [a] -> [a]
   , (\\)              -- :: (Eq a) => [a] -> [a] -> [a]

   , union             -- :: (Eq a) => [a] -> [a] -> [a]
   , intersect         -- :: (Eq a) => [a] -> [a] -> [a]

   -- ** Ordered lists
   , sort              -- :: (Ord a) => [a] -> [a]
   , insert            -- :: (Ord a) => a -> [a] -> [a]

   -- * Generalized functions

   -- ** The \"@By@\" operations
   -- | By convention, overloaded functions have a non-overloaded
   -- counterpart whose name is suffixed with \`@By@\'.

   -- *** User-supplied equality (replacing an @Eq@ context)
   -- | The predicate is assumed to define an equivalence.
   , nubBy             -- :: (a -> a -> Bool) -> [a] -> [a]
   , deleteBy          -- :: (a -> a -> Bool) -> a -> [a] -> [a]
   , deleteFirstsBy    -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
   , unionBy           -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
   , intersectBy       -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
   , groupBy           -- :: (a -> a -> Bool) -> [a] -> [[a]]

   -- *** User-supplied comparison (replacing an @Ord@ context)
   -- | The function is assumed to define a total ordering.
   , sortBy            -- :: (a -> a -> Ordering) -> [a] -> [a]
   , insertBy          -- :: (a -> a -> Ordering) -> a -> [a] -> [a]
   , maximumBy         -- :: (a -> a -> Ordering) -> [a] -> a
   , minimumBy         -- :: (a -> a -> Ordering) -> [a] -> a

   -- ** The \"@generic@\" operations
   -- | The prefix \`@generic@\' indicates an overloaded function that
   -- is a generalized version of a "Prelude" function.

   , genericLength     -- :: (Integral a) => [b] -> a
   , genericTake       -- :: (Integral a) => a -> [b] -> [b]
   , genericDrop       -- :: (Integral a) => a -> [b] -> [b]
   , genericSplitAt    -- :: (Integral a) => a -> [b] -> ([b], [b])
   , genericIndex      -- :: (Integral a) => [b] -> a -> b
   , genericReplicate  -- :: (Integral a) => a -> b -> [b]
  ) where

import "base" Data.List hiding
    ( splitAt, -- wrong strictness
      -- FTP-generalised verbs
      all, and, any, concat, concatMap, elem, find,
      foldl, foldl1, foldl', foldr, foldr1, mapAccumL,
      mapAccumR, maximum, maximumBy, minimum, minimumBy,
      length, notElem, null, or, product, sum )

import GHC.List
    ( -- FTP-generalised verbs
      all, and, any, concat, concatMap, elem,
      foldl, foldl1, foldl', foldr, foldr1,
      maximum, minimum,
      length, notElem, null, or, product, sum )

import GHC.Base ( Int, Ordering(..), Bool, error, Maybe(..), (.), oneShot )
import "base" Data.Maybe (listToMaybe)

-- The GHC's version of 'splitAt' is too strict in 'n' compared to
-- Haskell98/2010 version. Ticket #1182.

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@.
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.
splitAt                :: Int -> [a] -> ([a],[a])
splitAt n xs           =  (take n xs, drop n xs)


----------------------------------------------------------------------------
-- the code below has been copied from `base-4.8.0.0` as it wasn't exported via modules

-- | The 'maximumBy' function takes a comparison function and a list
-- and returns the greatest element of the list by the comparison function.
-- The list must be finite and non-empty.
maximumBy               :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ []          =  error "List.maximumBy: empty list"
maximumBy cmp xs        =  foldl1 maxBy xs
                        where
                           maxBy x y = case cmp x y of
                                       GT -> x
                                       _  -> y

-- | The 'minimumBy' function takes a comparison function and a list
-- and returns the least element of the list by the comparison function.
-- The list must be finite and non-empty.
minimumBy               :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ []          =  error "List.minimumBy: empty list"
minimumBy cmp xs        =  foldl1 minBy xs
                        where
                           minBy x y = case cmp x y of
                                       GT -> y
                                       _  -> x

-- | The 'find' function takes a predicate and a list and returns the
-- first element in the list matching the predicate, or 'Nothing' if
-- there is no such element.
find            :: (a -> Bool) -> [a] -> Maybe a
find p          = listToMaybe . filter p

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a list, passing
-- an accumulating parameter from left to right, and returning a final
-- value of this accumulator together with the new list.
mapAccumL :: (acc -> x -> (acc, y)) -- Function of elt of input list
                                    -- and accumulator, returning new
                                    -- accumulator and elt of result list
          -> acc            -- Initial accumulator
          -> [x]            -- Input list
          -> (acc, [y])     -- Final accumulator and result list
{-# NOINLINE [1] mapAccumL #-}
mapAccumL _ s []        =  (s, [])
mapAccumL f s (x:xs)    =  (s'',y:ys)
                           where (s', y ) = f s x
                                 (s'',ys) = mapAccumL f s' xs

{-# RULES
"mapAccumL" [~1] forall f s xs . mapAccumL f s xs = foldr (mapAccumLF f) pairWithNil xs s
"mapAccumLList" [1] forall f s xs . foldr (mapAccumLF f) pairWithNil xs s = mapAccumL f s xs
 #-}

pairWithNil :: acc -> (acc, [y])
{-# INLINE [0] pairWithNil #-}
pairWithNil x = (x, [])

mapAccumLF :: (acc -> x -> (acc, y)) -> x -> (acc -> (acc, [y])) -> acc -> (acc, [y])
{-# INLINE [0] mapAccumLF #-}
mapAccumLF f = \x r -> oneShot (\s ->
                         let (s', y)   = f s x
                             (s'', ys) = r s'
                         in (s'', y:ys))
  -- See Note [Left folds via right fold] in base

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a list, passing
-- an accumulating parameter from right to left, and returning a final
-- value of this accumulator together with the new list.
mapAccumR :: (acc -> x -> (acc, y))     -- Function of elt of input list
                                        -- and accumulator, returning new
                                        -- accumulator and elt of result list
            -> acc              -- Initial accumulator
            -> [x]              -- Input list
            -> (acc, [y])               -- Final accumulator and result list
mapAccumR _ s []        =  (s, [])
mapAccumR f s (x:xs)    =  (s'', y:ys)
                           where (s'',y ) = f s' x
                                 (s', ys) = mapAccumR f s xs
