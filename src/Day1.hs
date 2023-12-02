{-# LANGUAGE DeriveFunctor #-}

module Day1 where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Char (digitToInt, isDigit)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid ()
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Witherable (Filterable, catMaybes, mapMaybe)
import qualified Witherable as W

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
  parseLine = (=<<) N.nonEmpty . parse p

processLines :: [NonEmpty Digit] -> [Int]
processLines = fmap processLine
 where
  processLine = read . foldMap (show . toInt) . firstAndLast
  firstAndLast l = [N.head l, N.last l]

-- Digression: Parser combinators from scratch to support the retain custom combinator

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)} deriving (Functor)

parse :: Parser a -> String -> Maybe a
parse p = fmap snd . W.filter (null . fst) . runParser p

anyChar :: Parser Char
anyChar = Parser p
 where
  p [] = Nothing
  p (c : t) = Just (t, c)

instance Filterable Parser where
  mapMaybe f (Parser p) = Parser $ mapMaybe (traverse f) . p

char :: Char -> Parser Char
char c = W.filter (c ==) anyChar

instance Applicative Parser where
  pure a = Parser $ \s -> Just (s, a)
  (Parser p) <*> (Parser q) = Parser r
   where
    r s = do
      (s', f) <- p s
      (s'', a) <- q s'
      return (s'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p) <|> (Parser q) = Parser $ \s -> p s <|> q s

string :: String -> Parser String
string = traverse char

choice :: [Parser a] -> Parser a
choice = asum

retain :: Parser a -> Parser a
retain (Parser p) = Parser $ \s -> fmap ((,) <$> const (drop 1 s) <*> snd) (p s)

digitFromCharP :: Parser Digit
digitFromCharP = mapMaybe digitFromChar anyChar

digitFromNameP :: Parser Digit
digitFromNameP = choice $ fmap (\d -> build (toString d) d) allDigits
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
          [ Just <$> retain digitFromCharP
          , Just <$> retain digitFromNameP
          , Nothing <$ anyChar
          ]
      )
