module Main where

import Data.List
import Solution
import Text.ParserCombinators.ReadP

noDupes :: Eq a => [a] -> Bool
noDupes chars =  length chars == length (nub chars)

markerCandidates :: Int -> [a] -> [[a]]
markerCandidates n = takeNFromEach . dropLast n . tails
  where
    dropLast n xs = zipWith const xs $ drop n xs
    takeNFromEach = map $ take n

solve :: Int -> Solution
solve n = mkSolution parser $ maybe (-1) (+n) . findIndex noDupes . markerCandidates n
  where parser = manyTill get eof

main :: IO ()
main = runSolution "day6.txt" $ solve 4 <> solve 14

