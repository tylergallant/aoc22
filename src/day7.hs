module Main where

import Data.List
import Solution
import Text.ParserCombinators.ReadP

type Size = Integer
type Name = String

data LsOutput = FileOutput Size Name | DirOutput Name deriving Show

data Command = CdDir Name | CdParent | CdRoot | Ls [LsOutput] deriving Show

data FileSystem = File Size Name | Dir Name [FileSystem]

pCd :: ReadP Command
pCd = string "$ cd " *> pDestination <* char '\n'
  where
    pDestination = pCdRoot <++ pCdParent <++ pCdDir
    pCdRoot = CdRoot <$ char '/'
    pCdParent = CdParent <$ string ".."
    pCdDir = CdDir <$> many get

pNodeName :: ReadP String
pNodeName = char ' ' *> many get

pFile :: ReadP LsOutput
pFile = FileOutput <$> readS_to_P reads <*> pNodeName

pDir :: ReadP LsOutput
pDir = fmap DirOutput $ string "dir" *> pNodeName

pLs :: ReadP Command
pLs = fmap Ls $ string "$ ls\n" *> pFileNodes
  where
    pFileNodes = many $ pFileNode <* char '\n'
    pFileNode = pDir <++ pFile

pCommand :: ReadP Command
pCommand = pCd <++ pLs

parser :: ReadP [Command]
parser = manyTill pCommand eof

solve :: Solution
solve = mkSolution parser id

main :: IO ()
main = runSolution "day7.txt" solve

