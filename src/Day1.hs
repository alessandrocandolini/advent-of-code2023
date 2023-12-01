module Day1 where

import Control.Applicative (Alternative (many))
import Data.Char (digitToInt, isDigit)
import Data.Functor (($>))
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Monoid ()
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Witherable (catMaybes, mapMaybe)
import Data.Either.Combinators (rightToMaybe)

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

toString :: Digit -> T.Text
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

parseLines :: Parser [Digit] -> T.Text -> [NonEmpty Digit]
parseLines p = mapMaybe parseLine . T.lines where
  parseLine = (=<<) N.nonEmpty . rightToMaybe . P.parse p ""

processLines :: [NonEmpty Digit] -> [Int]
processLines = fmap processLine
 where
  processLine = read . foldMap (show . toInt) . tupleToList . firstAndLast
  firstAndLast = (,) <$> N.head <*> N.last
  tupleToList (a, b) = [a, b]

type Parser = P.Parsec Void T.Text
type ParserError = P.ParseErrorBundle T.Text Void

emap :: (a -> Maybe b) -> Parser a -> Parser b
emap f = (=<<) (failIfNothing . f)
 where
  failIfNothing (Just a) = pure a
  failIfNothing Nothing = fail "error parsing"

digitFromCharP :: Parser Digit
digitFromCharP = emap digitFromChar P.digitChar

digitFromNameP :: Parser Digit
digitFromNameP = P.choice $ fmap (\d -> build (toString d) d) allDigits
 where
  build :: T.Text -> Digit -> Parser Digit
  build s d = P.string s $> d

parser1 :: Parser [Digit]
parser1 = catMaybes <$> many (P.choice [Just <$> digitFromCharP, Nothing <$ P.anySingle])

parser2 :: Parser [Digit]
parser2 = catMaybes <$> many (P.choice [Just <$> digitFromCharP, Just <$> digitFromNameP, Nothing <$ P.anySingle])
