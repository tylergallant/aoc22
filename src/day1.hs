module Main where

import Data.List

nonEmpty :: String -> String -> Bool
nonEmpty s1 s2 = not $ any null [s1, s2]

readAndSum :: [String] -> Int
readAndSum = sum . map read

sumElves :: String -> [Int]
sumElves = map readAndSum . filter (/= [""]) . groupBy nonEmpty . lines

solveA :: String -> String
solveA = show . maximum . sumElves

solveB :: String -> String
solveB = show . sum . take 3 .  sortBy (flip compare) . sumElves

main :: IO ()
main = interact $ \i -> solveA i ++ "\n" ++ solveB i

