{-# LANGUAGE DeriveFunctor #-}

module Parser where

import Control.Applicative (Alternative (empty, (<|>)), many, some)
import Data.Char (isDigit, toLower)
import Data.Foldable (asum)
import Data.Functor (($>))
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

newline :: Parser Char
newline = char '\n'

space :: Parser Char
space = char ' '

spaces :: Parser [Char]
spaces = some space

digit :: Parser Char
digit = W.filter isDigit anyChar

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

decimal :: Parser Int
decimal = read <$> some digit

choice :: [Parser a] -> Parser a
choice = asum

retain :: Parser a -> Parser a
retain (Parser p) = Parser $ \s -> fmap ((,) <$> const (drop 1 s) <*> snd) (p s)

between :: Char -> Char -> Parser a -> Parser a
between before after p = char before *> p <* char after

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

optional :: Parser a -> Parser (Maybe a)
optional p = Just <$> p <|> pure Nothing

mkEnumParser :: (Enum a, Bounded a, Show a) => Parser a
mkEnumParser = (choice . fmap (uncurry build . (\a -> (fmap toLower (show a), a)))) [minBound .. maxBound]
 where
  build :: String -> a -> Parser a
  build s a = string s $> a
