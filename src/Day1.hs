module Day1 where

import Control.Applicative (many)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid ()
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Parser
import Witherable (catMaybes, mapMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int Int deriving (Eq, Show)

data Digit
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Show, Bounded, Enum)

toInt :: Digit -> Int
toInt = (+ 1) . fromIntegral . fromEnum

fromChar :: Char -> Maybe Digit
fromChar '1' = Just One
fromChar '2' = Just Two
fromChar '3' = Just Three
fromChar '4' = Just Four
fromChar '5' = Just Five
fromChar '6' = Just Six
fromChar '7' = Just Seven
fromChar '8' = Just Eight
fromChar '9' = Just Nine
fromChar _   = Nothing

logic :: T.Text -> Answer
logic = Answer <$> sum . part1 <*> sum . part2

part1 :: T.Text -> [Int]
part1 = processLines . parseLines parser1

part2 :: T.Text -> [Int]
part2 = processLines . parseLines parser2

parseLines :: Parser [Digit] -> T.Text -> [NonEmpty Digit]
parseLines p = mapMaybe parseLine . T.lines
 where
  parseLine = (=<<) N.nonEmpty . parseAll p . T.unpack

processLines :: [NonEmpty Digit] -> [Int]
processLines = fmap processLine
 where
  processLine = read . foldMap (show . toInt) . firstAndLast
  firstAndLast l = [N.head l, N.last l]

-- parsing

fromCharP :: Parser Digit
fromCharP = mapMaybe fromChar anyChar

fromNameP :: Parser Digit
fromNameP = mkEnumParser

parser1 :: Parser [Digit]
parser1 =
  catMaybes
    <$> many
      ( choice
          [ Just <$> fromCharP
          , Nothing <$ anyChar
          ]
      )

parser2 :: Parser [Digit]
parser2 =
  catMaybes
    <$> many
      ( choice
          [ Just <$> fromCharP
          , Just <$> retain fromNameP
          , Nothing <$ anyChar
          ]
      )
