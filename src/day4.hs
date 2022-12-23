module Main where

import Data.Maybe
import Text.ParserCombinators.ReadP
import Solution

type Range = (Int, Int)

contains :: Range -> Int -> Bool
contains (s, e) n = s <= n && n <= e

fullOverlap :: (Range, Range) -> Bool
fullOverlap (r1, r2) = firstContainsSecond r1 r2 || firstContainsSecond r2 r1
  where firstContainsSecond r (s, e) = contains r s && contains r e

partialOverlap :: (Range, Range) -> Bool
partialOverlap (r1, r2) = firstOverlapsSecond r1 r2 || firstOverlapsSecond r2 r1
  where firstOverlapsSecond r (s, e) = contains r s || contains r e

pRange :: ReadP Range
pRange = (,) <$> pInt <*> (char '-' *> pInt)
  where pInt = readS_to_P reads

pPair :: ReadP (Range, Range)
pPair = (,) <$> pRange <*> (char ',' *> pRange)

pPairs :: ReadP [(Range, Range)]
pPairs = many $ pPair <* char '\n'

solveA :: Solution
solveA = mkSolution pPairs $ length . filter fullOverlap

solveB :: Solution
solveB = mkSolution pPairs $ length . filter partialOverlap

main :: IO ()
main = runSolution "day4.txt" $ solveA <> solveB

