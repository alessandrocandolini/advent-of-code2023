{-# LANGUAGE DeriveFunctor #-}

module Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Foldable (asum)
import Data.Monoid ()
import Witherable (Filterable, mapMaybe)
import qualified Witherable as W

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)} deriving (Functor)

parseAll :: Parser a -> String -> Maybe a
parseAll p = fmap snd . W.filter (null . fst) . runParser p

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
