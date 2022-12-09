module Main where

import Control.Applicative
import Data.Maybe
import Text.ParserCombinators.ReadP

data Shape = Rock | Paper | Scissors deriving (Eq, Enum)

data Outcome = Win | Lose | Draw

shapeToBeat :: Shape -> Shape
shapeToBeat Rock = Paper
shapeToBeat Paper = Scissors
shapeToBeat Scissors = Rock

roundOutcome :: Shape -> Shape -> Outcome
roundOutcome opponent player
  | player == opponent = Draw
  | player == shapeToBeat opponent = Win
  | otherwise = Lose

scoreShape :: Shape -> Int
scoreShape Rock = 1
scoreShape Paper = 2
scoreShape Scissors = 3

scoreOutcome :: Outcome -> Int
scoreOutcome Lose = 0
scoreOutcome Draw = 3
scoreOutcome Win = 6

scoreRound :: (Shape, Shape) -> Int
scoreRound (opponent, player) = scoreShape player + scoreOutcome outcome
  where outcome = roundOutcome opponent player

parseShape :: [Char] -> ReadP Shape
parseShape = asum . zipWith (<$) [Rock ..] . map char

parseOpponentShape :: ReadP Shape
parseOpponentShape =  parseShape ['A', 'B', 'C']

parsePlayerShape :: ReadP Shape
parsePlayerShape = parseShape ['X', 'Y', 'Z']

parseRound :: ReadP (Shape, Shape)
parseRound = (,) <$> parseOpponentShape <*> (skipSpaces *> parsePlayerShape)

solveA :: String -> String
solveA = show . sum . map scoreRound . catMaybes . map parse . lines
  where
    fullParse = null . snd
    parse = fmap fst . listToMaybe . filter fullParse . readP_to_S parseRound

main :: IO ()
main = interact solveA

