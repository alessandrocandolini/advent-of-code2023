{-# LANGUAGE DeriveFunctor #-}

module Day3 where

import Control.Applicative ((<|>), Alternative (many))
import Data.Char (isDigit, digitToInt)
import Data.Functor (($>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Parser
import qualified Witherable as W

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer deriving (Eq, Show)

logic :: T.Text -> Answer
logic = const Answer

data GridElement
  = Digit Int
  | Blank
  | Symbol Char
  deriving (Eq, Show)

data Grid a = Grid
  { gridWidth :: Int
  , gridHeight :: Int
  , content :: V.Vector a
  }
  deriving (Eq, Show, Functor)

gridElementP :: Parser GridElement
gridElementP = digitP <|> blankP <|> symbolP
 where
  blankP = char '.' $> Blank
  digitP = Digit . digitToInt <$> digit
  symbolP = (fmap Symbol . W.filter (\c -> c /= '.' && c /= '\n' && (not . isDigit) c)) anyChar

gridElementsP :: Parser [[GridElement]]
gridElementsP = many gridElementP `sepBy` newline

