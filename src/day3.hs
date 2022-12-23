module Main where

import Data.Char
import Data.List
import Solution
import Text.ParserCombinators.ReadP

itemPriority :: Char -> Int
itemPriority c
  | isLower c = distanceFrom 'a'
  | isUpper c = distanceFrom 'A' + 26
  | otherwise = 0
  where distanceFrom d = ord c - ord (pred d)

bisect :: [a] -> ([a], [a])
bisect xs = splitAt middle xs
  where middle = length xs `div` 2

commonElements :: Eq a => [a] -> [a] -> [a]
commonElements xs ys = intersect (nub xs) (nub ys)

groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs
  | n <= 0 = []
  | otherwise = take n xs : groupN n (drop n xs)

parser :: ReadP [String]
parser = many $ manyTill get $ char '\n'

solveA :: Solution
solveA = mkSolution parser $ sum . concat . map commonItemPriority
  where commonItemPriority = map itemPriority . uncurry commonElements . bisect

solveB :: Solution
solveB = mkSolution parser $ sum . map itemPriority . findBadges
  where findBadges = concat . map (foldr1 commonElements) . groupN 3

main :: IO ()
main = runSolution "day3.txt" $ solveA <> solveB

