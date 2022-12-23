module Solution where

import Paths_aoc22
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP

type Solution = String -> [String]

parse :: ReadP a -> String -> Maybe a
parse p = listToMaybe . map fst . filter (null . snd) . readP_to_S p

mkSolution :: Show b => ReadP a -> (a -> b) -> Solution
mkSolution p f = fromMaybe ["Parse error"] . fmap g . parse p
  where g = pure . show . f

runSolution :: FilePath -> Solution -> IO ()
runSolution inputPath solution = do
  fName <- getDataFileName inputPath
  input <- readFile fName
  putStrLn . concat . intersperse "\n" $ solution input

