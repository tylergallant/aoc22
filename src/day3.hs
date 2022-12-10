module Main where

import Data.Char
import Data.List
import Data.Maybe

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

solve :: ([String] -> [Maybe Char]) -> String -> String
solve f = show . sum . map itemPriority . catMaybes . f . lines

solveA :: String -> String
solveA = solve checkBags
  where checkBags = map $ listToMaybe . uncurry commonElements . bisect

solveB :: String -> String
solveB = solve $ findBadges . groupN 3
  where findBadges = map $ listToMaybe . foldr1 commonElements

main :: IO ()
main = interact $ \i -> solveA i ++ "\n" ++ solveB i

