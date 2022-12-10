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

solve :: String -> String
solve = show . sum . map itemPriority . catMaybes . map checkBag . lines
  where checkBag = listToMaybe . uncurry commonElements . bisect

main :: IO ()
main = interact solve

