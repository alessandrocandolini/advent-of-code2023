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

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer deriving (Eq, Show)

logic :: T.Text -> Answer
logic = const Answer

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

gameIdP = (string "Game " *> decimal <* string ": ")

-- Game 1: 1 green, 4 blue; 1 blue, 2 green, 1 red; 1 red, 1 green, 2 blue; 1 green, 1 red; 1 green; 1 green, 1 blue, 1 red

parseGames :: T.Text -> [Game]
parseGames = undefined
