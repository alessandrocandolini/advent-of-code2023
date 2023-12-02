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
    . fmap addKey
    . mapMaybe N.nonEmpty
    . groupBy compareByKey
    . sortWith f
 where
  compareByKey a1 a2 = f a1 == f a2
  addKey as = (f (N.head as), as)
