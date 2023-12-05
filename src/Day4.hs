module Day4 where

import Control.Applicative (many, some)
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parser
import qualified Witherable as W

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int deriving (Eq, Show)

logic :: T.Text -> Answer
logic = Answer . answer1 . parseInput

newtype CardId = CardId Int deriving (Eq, Show, Ord)

data Card = Card
  { cardId :: CardId
  , winningNumbers :: S.Set Int
  , numbers :: S.Set Int
  }
  deriving (Eq, Show)

answer1 :: [Card] -> Int
answer1 = getSum . foldMap (Sum . score)

score :: Card -> Int
score = points . (S.intersection <$> winningNumbers <*> numbers)
 where
  points winning
    | null winning = 0
    | otherwise = 2 ^ (S.size winning - 1)

allCards = undefined

parseInput :: T.Text -> [Card]
parseInput = W.mapMaybe (parseCard . T.unpack) . T.lines

parseCard :: String -> Maybe Card
parseCard = parseAll cardP

cardP :: Parser Card
cardP = Card <$> (string "Card" *> spaces *> cardIdP <* string ":" <* spaces) <*> numbersP <*> (string " |" *> spaces *> numbersP)
 where
  cardIdP = CardId <$> decimal

numbersP :: Parser (S.Set Int)
numbersP = fmap S.fromList (decimal `sepBy` many space)
