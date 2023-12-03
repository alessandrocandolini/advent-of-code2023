module Day1Vanilla where

import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Witherable (mapMaybe)

{- Compared to Day1.hs, this module solves AOC 2023 Day 1 relying as much as possible on built-in types (eg, Char) and vanilla recursion
 -
 - For a type safer solution, and that relies on techniques like parser combinators, see Day1.hs
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
    . mapMaybe (N.nonEmpty . parse)
    . lines

firstAndLast :: NonEmpty a -> [a]
firstAndLast l = [N.head l, N.last l]

parse :: String -> String
parse [] = []
parse ('o' : 'n' : 'e' : s) = '1' : parse ('e' : s)
parse ('t' : 'w' : 'o' : s) = '2' : parse ('o' : s)
parse ('t' : 'h' : 'r' : 'e' : 'e' : s) = '3' : parse ('e' : s)
parse ('f' : 'o' : 'u' : 'r' : s) = '4' : parse s
parse ('f' : 'i' : 'v' : 'e' : s) = '5' : parse ('e' : s)
parse ('s' : 'i' : 'x' : s) = '6' : parse s
parse ('s' : 'e' : 'v' : 'e' : 'n' : s) = '7' : parse ('n' : s)
parse ('e' : 'i' : 'g' : 'h' : 't' : s) = '8' : parse ('t' : s)
parse ('n' : 'i' : 'n' : 'e' : s) = '9' : parse ('e' : s)
parse (c : s)
  | isDigit c = c : parse s
  | otherwise = parse s
