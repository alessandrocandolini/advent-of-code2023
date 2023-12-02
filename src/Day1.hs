module Day1 where

import Data.Char (digitToInt, isDigit)
import Data.Functor (($>))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid ()
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Witherable (catMaybes, mapMaybe)
import Parser
import Control.Applicative (many)

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

allDigits :: [Digit]
allDigits = [minBound .. maxBound]

toInt :: Digit -> Int
toInt = (+ 1) . fromIntegral . fromEnum

digitFromChar :: Char -> Maybe Digit
digitFromChar c
  | isDigit c = find ((==) (digitToInt c) . toInt) allDigits
  | otherwise = Nothing

toString :: Digit -> String
toString One = "one"
toString Two = "two"
toString Three = "three"
toString Four = "four"
toString Five = "five"
toString Six = "six"
toString Seven = "seven"
toString Eight = "eight"
toString Nine = "nine"

logic :: T.Text -> Answer
logic = Answer <$> sum . part1 <*> sum . part2

part1 :: T.Text -> [Int]
part1 = processLines . parseLines parser1

part2 :: T.Text -> [Int]
part2 = processLines . parseLines parser2

parseLines1 :: T.Text -> [NonEmpty Digit]
parseLines1 = mapMaybe parseLine . T.lines
 where
  parseLine = N.nonEmpty . mapMaybe digitFromChar . T.unpack

parseLines :: Parser [Digit] -> T.Text -> [NonEmpty Digit]
parseLines p = mapMaybe (parseLine . T.unpack) . T.lines
 where
  parseLine = (=<<) N.nonEmpty . parseAll p

processLines :: [NonEmpty Digit] -> [Int]
processLines = fmap processLine
 where
  processLine = read . foldMap (show . toInt) . firstAndLast
  firstAndLast l = [N.head l, N.last l]

-- parsing

digitFromCharP :: Parser Digit
digitFromCharP = mapMaybe digitFromChar anyChar

digitFromNameP :: Parser Digit
digitFromNameP = choice $ fmap (build <$> toString <*> id) allDigits
 where
  build :: String -> Digit -> Parser Digit
  build s d = string s $> d

parser1 :: Parser [Digit]
parser1 = catMaybes <$> many (choice [Just <$> digitFromCharP, Nothing <$ anyChar])

parser2 :: Parser [Digit]
parser2 =
  catMaybes
    <$> many
      ( choice
          [ Just <$> digitFromCharP
          , Just <$> retain digitFromNameP
          , Nothing <$ anyChar
          ]
      )
