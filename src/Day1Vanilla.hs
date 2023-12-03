module Day1Vanilla where

import Data.Char (isDigit)
import Data.List (tails)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Witherable (mapMaybe)

{- This module solves AOC 2023 Day 1 relying intentionally as much as possible  on built-in types (eg, Char) and vanilla techniques.
 - For a type safer solution (eg, modelling digits as ADT), that relies on parser combinators, see Day1.hs
 -
 - The preference is to avoid explicit recursion whenever possible: here it's absorbed behind `tails` (of comonadic inspiration), leaving the rest of the code non-recursive
 -}

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . readFile

logic :: String -> (Int, Int)
logic input = (result1, result2)
 where
  result1 = (sum . part1) input
  result2 = (sum . part2) input

part1 :: String -> [Int]
part1 =
  fmap (read . firstAndLast)
    . mapMaybe (N.nonEmpty . filter isDigit)
    . lines

part2 :: String -> [Int]
part2 =
  fmap (read . firstAndLast)
    . mapMaybe (N.nonEmpty . parseAll)
    . lines

firstAndLast :: NonEmpty a -> [a]
firstAndLast l = [N.head l, N.last l]

parse :: String -> Maybe Char
parse [] = Nothing
parse ('o' : 'n' : 'e' : _) = Just '1'
parse ('t' : 'w' : 'o' : _) = Just '2'
parse ('t' : 'h' : 'r' : 'e' : 'e' : _) = Just '3'
parse ('f' : 'o' : 'u' : 'r' : _) = Just '4'
parse ('f' : 'i' : 'v' : 'e' : _) = Just '5'
parse ('s' : 'i' : 'x' : _) = Just '6'
parse ('s' : 'e' : 'v' : 'e' : 'n' : _) = Just '7'
parse ('e' : 'i' : 'g' : 'h' : 't' : _) = Just '8'
parse ('n' : 'i' : 'n' : 'e' : _) = Just '9'
parse (c : _)
  | isDigit c = Just c
  | otherwise = Nothing

parseAll :: String -> String
parseAll = mapMaybe parse . tails
