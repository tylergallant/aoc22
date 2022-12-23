module Main where

import Data.List
import Solution
import Text.ParserCombinators.ReadP

pElves :: ReadP [[Int]]
pElves = sepBy pElf $ char '\n'
  where pElf = many $ readS_to_P reads <* char '\n'

solveA :: Solution
solveA = mkSolution pElves $ maximum . map sum

solveB :: Solution
solveB = mkSolution pElves $ sum . take 3 . sortBy (flip compare) . map sum

main :: IO ()
main = runSolution "day1.txt" $ solveA <> solveB

