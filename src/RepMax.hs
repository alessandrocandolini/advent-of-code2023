{-# LANGUAGE TupleSections #-}
module RepMax where

import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid

repMax :: (Functor f, Foldable f, Ord a) => f a -> f a
repMax as = as $> m
 where
  m = maximum as

repMaxAux :: (Ord a) => a -> [a] -> (a, [a])
repMaxAux candidate [] = (candidate, [])
repMaxAux candidate [a] = (a, [candidate])
repMaxAux candidate (a : as) = (m, candidate : as')
 where
  (m', as') = repMaxAux candidate as
  m = max a m'

repMax1 :: Ord a => [a] -> [a]
repMax1 as = as'
 where
  (m, as') = repMaxAux m as

-- can i express repMaxAux with a fold?

repMaxAux2 :: Ord a => a -> [a] -> (a, [a])
repMaxAux2 candidate l = foldr step (head l, []) l where
   step a' (m, as') = (max m a' , candidate : as')

repMax2 :: Ord a => [a] -> [a]
repMax2 as = as' where
  (m, as') = repMaxAux2 m as

mean :: (Fractional a) => NonEmpty a -> a
mean as = sum as / fromIntegral (N.length as)

mean1 :: Fractional a => NonEmpty a -> a
mean1 = ((/) <$> getSum . snd <*> getSum . fst)  . foldMap ((1,) . Sum)

meanDeviation :: (Fractional a) => (a -> a -> a) -> NonEmpty a -> NonEmpty a
meanDeviation distance as = fmap (`distance` m) as where m = mean as
