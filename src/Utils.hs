module Utils where

import Data.List (groupBy)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Exts (sortWith)
import Witherable (mapMaybe)

groupOn :: (Ord k) => (a -> k) -> [a] -> Map k (NonEmpty a)
groupOn f =
  M.fromList
    . fmap flat
    . mapMaybe N.nonEmpty
    . groupBy compareByKey
    . sortWith fst
    . map (\a -> (f a, a))
 where
  compareByKey (k1, _) (k2, _) = k1 == k2
  flat:: NonEmpty (k,a) -> (k, NonEmpty a)
  flat kas = (fst (N.head kas), fmap snd kas)
