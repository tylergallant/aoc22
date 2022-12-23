module Main where

import Control.Applicative hiding (many)
import Solution
import Text.ParserCombinators.ReadP

data Shape = Rock | Paper | Scissors deriving (Eq, Enum)

data Outcome = Win | Lose | Draw deriving Enum

shapeToWin :: Shape -> Shape
shapeToWin Rock = Paper
shapeToWin Paper = Scissors
shapeToWin Scissors = Rock

shapeToLose :: Shape -> Shape
shapeToLose Rock = Scissors
shapeToLose Paper = Rock
shapeToLose Scissors = Paper

roundOutcome :: Shape -> Shape -> Outcome
roundOutcome opponent player
  | player == opponent = Draw
  | player == shapeToWin opponent = Win
  | otherwise = Lose

shapeForOutcome :: Outcome -> Shape -> Shape
shapeForOutcome Win = shapeToWin
shapeForOutcome Lose = shapeToLose
shapeForOutcome Draw = id

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

scoreDesiredOutcome :: (Shape, Outcome) -> Int
scoreDesiredOutcome (opponent, outcome) = scoreOutcome outcome + shapeScore
  where shapeScore = scoreShape $ shapeForOutcome outcome opponent

parseCharsAs :: [a] -> String -> ReadP a
parseCharsAs values = asum . zipWith (<$) values . map char

parseShape :: String -> ReadP Shape
parseShape = parseCharsAs [Rock ..]

parseOpponentShape :: ReadP Shape
parseOpponentShape =  parseShape "ABC"

parsePlayerShape :: ReadP Shape
parsePlayerShape = parseShape "XYZ"

parseOutcome :: ReadP Outcome
parseOutcome = parseCharsAs [Win ..] "ZXY"

parseRoundShapes :: ReadP (Shape, Shape)
parseRoundShapes = (,) <$> parseOpponentShape <*> (skipSpaces *> parsePlayerShape)

parseShapeOutcome :: ReadP (Shape, Outcome)
parseShapeOutcome = (,) <$> parseOpponentShape <*> (skipSpaces *> parseOutcome)

parseLinesWith :: ReadP a -> ReadP [a]
parseLinesWith p = many $ p <* char '\n'

solveA :: Solution
solveA = mkSolution parser $ sum . map scoreRound
  where parser = parseLinesWith parseRoundShapes

solveB :: Solution
solveB = mkSolution parser $ sum . map scoreDesiredOutcome
  where parser = parseLinesWith parseShapeOutcome

main :: IO ()
main = runSolution "day2.txt" $ solveA <> solveB

