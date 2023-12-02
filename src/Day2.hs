module Day2 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parser
import Data.Functor (($>))
import Control.Applicative ((<|>))
import Data.Tuple (swap)
import Witherable (mapMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int deriving (Eq, Show)

logic :: T.Text -> Answer
logic = Answer . part1

bag :: Bag
bag =
  Bag
    $ M.fromList
      [ (Red, 12)
      , (Green, 13)
      , (Blue, 14)
      ]

part1 :: T.Text -> Int
part1 =
  getSum
    . foldMap (Sum . gameId)
    . filter (isGamePossible bag)
    . parseGames

data Colour = Blue | Green | Red deriving (Eq, Show, Bounded, Enum, Ord)

newtype Bag = Bag (Map Colour Int) deriving (Eq, Show)
newtype Sample = Sample [(Colour, Int)] deriving (Eq, Show)

data Game = Game
  { gameId :: Int
  , samples :: [Sample]
  }
  deriving (Eq, Show)

isCubePossible :: Bag -> (Colour, Int) -> Bool
isCubePossible (Bag m) (colour, n) =
  let
    maxAvailable = fromMaybe 0 (M.lookup colour m)
   in
    n <= maxAvailable

isSamplePossible :: Bag -> Sample -> Bool
isSamplePossible b (Sample cubes) = all (isCubePossible b) cubes

isGamePossible :: Bag -> Game -> Bool
isGamePossible b (Game _ s) = all (isSamplePossible b) s

colourP :: Parser Colour
colourP = string "blue" $> Blue <|> string "red" $> Red <|> string "green" $> Green

gameIdP :: Parser Int
gameIdP = string "Game " *> decimal <* string ":"

sampleP :: Parser Sample
sampleP = Sample <$> pairP `sepBy` char ',' where
   pairP :: Parser (Colour, Int)
   pairP = swap <$> ((,) <$> (space *> decimal <* space) <*> colourP)

samplesP :: Parser [Sample]
samplesP = sampleP `sepBy` char ';'

gameP :: Parser Game
gameP = Game <$> gameIdP <*> samplesP

parseGames :: T.Text -> [Game]
parseGames = mapMaybe (parseAll gameP . T.unpack) . T.lines
