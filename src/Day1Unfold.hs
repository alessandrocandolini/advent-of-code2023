module Day1Unfold where

import Data.Char (isDigit)
import Data.List (unfoldr)
import qualified Data.List.NonEmpty as N
import Data.Tuple (swap)
import Witherable (mapMaybe, Filterable (catMaybes))

{- This module solves AOC 2023 Day 1 relying as much as possible on built-in types (ie, Char) and vanilla techniques.
 - For a type safer solution (eg, modelling digits as ADT), that relies on parser combinators, see Day1.hs
 -
 - Explicit recursion is avoided, by relying on `unfoldr` (the code is easy to be proven equivalent to use `tails` of comonadic inspiration, descrived in Day1Vanilla)
 -}

program :: FilePath -> IO ()
program = (=<<) print . fmap solve . readFile

solve :: String -> (Int, Int)
solve input = (answer1, answer2)
 where
  answer1 = (sum . part1) input
  answer2 = (sum . part2) input

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

firstAndLast :: N.NonEmpty a -> [a]
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
parseAll = catMaybes . unfoldr step
 where
  step :: String -> Maybe (Maybe Char, String)
  step [] = Nothing
  step s = Just (parse s, drop 1 s)

