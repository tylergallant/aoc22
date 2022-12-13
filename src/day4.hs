module Main where

import Data.Maybe
import Text.ParserCombinators.ReadP

type Range = (Int, Int)

fullOverlap :: (Range, Range) -> Bool
fullOverlap (r1, r2) = firstContainsSecond r1 r2 || firstContainsSecond r2 r1
  where firstContainsSecond (s1, e1) (s2, e2) = s1 <= s2 && e1 >= e2

pRange :: ReadP Range
pRange = (,) <$> pInt <*> (char '-' *> pInt)
  where pInt = readS_to_P reads

pPair :: ReadP (Range, Range)
pPair = (,) <$> pRange <*> (char ',' *> pRange)

parse :: ReadP a -> String -> Maybe a
parse p = listToMaybe . map fst . filter (null . snd) . readP_to_S p

solve :: String -> String
solve = show . length . filter fullOverlap . catMaybes . map (parse pPair) . lines

main :: IO ()
main = interact solve
