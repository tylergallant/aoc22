module Main where

import Data.Maybe
import Text.ParserCombinators.ReadP

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

parse :: ReadP a -> String -> Maybe a
parse p = listToMaybe . map fst . filter (null . snd) . readP_to_S p

solveA :: String -> String
solveA = show . length . filter fullOverlap . catMaybes . map (parse pPair) . lines

solveB :: String -> String
solveB = show . length . filter partialOverlap . catMaybes . map (parse pPair) . lines

main :: IO ()
main = interact $ \i -> solveA i ++ "\n" ++ solveB i

