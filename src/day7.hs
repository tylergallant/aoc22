module Main where

import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Solution
import Text.ParserCombinators.ReadP

type Size = Integer
type Name = String
type Path = [Name]
type FileSystem = Map Path Size

data Command = CdDir Name | CdParent | CdRoot | Ls Size

evalLs :: Path -> Size -> FileSystem -> FileSystem
evalLs cwd size fs
  | M.member cwd fs = fs
  | otherwise = M.insert cwd size $ M.mapWithKey addSizeToParents fs
  where addSizeToParents dir = if elem dir $ tails cwd then (+size) else id

evalCommands :: [Command] -> FileSystem
evalCommands = snd . foldl f ([], M.empty)
  where
    f (_, fs) CdRoot = ([], fs)
    f ([], fs) CdParent = ([], fs)
    f ((_:parent), fs) CdParent = (parent, fs)
    f (cwd, fs) (CdDir dir) = (dir:cwd, fs)
    f (cwd, fs) (Ls size) = (cwd, evalLs cwd size fs)

sumDirsUpTo :: Size -> FileSystem -> Size
sumDirsUpTo limit = sum . filter (<= limit) . M.elems

sizeOfDirToDelete :: FileSystem -> Size
sizeOfDirToDelete fs = foldr min totalFsSize $ M.filter (>= neededSpace) fs
  where
    totalDiskSpace = 70000000
    totalNeededSpace = 30000000
    totalFsSize = M.findWithDefault 0 [] fs
    freeSpace = totalDiskSpace - totalFsSize
    neededSpace = totalNeededSpace - freeSpace

pNewLine :: ReadP Char
pNewLine = char '\n'

pCd :: ReadP Command
pCd = string "$ cd " *> pDestination
  where
    pDestination = pCdRoot <++ pCdParent <++ pCdDir
    pCdRoot = pure CdRoot <* char '/' <* pNewLine
    pCdParent = pure CdParent <* string ".." <* pNewLine
    pCdDir = CdDir <$> manyTill get pNewLine

pFile :: ReadP Size
pFile = readS_to_P reads <* char ' ' <* manyTill get pNewLine

pLs :: ReadP Command
pLs = fmap Ls $ string "$ ls\n" *> pFileNodes
  where
    pFileNodes = sum <$> many pFileNode
    pFileNode = pDirNode <++ pFile
    pDirNode = pure 0 <* string "dir" <* manyTill get pNewLine

pCommand :: ReadP Command
pCommand = pCd <++ pLs

parser :: ReadP [Command]
parser = manyTill pCommand eof

solveA :: Solution
solveA = mkSolution parser $ sumDirsUpTo 100000 . evalCommands

solveB :: Solution
solveB = mkSolution parser $ sizeOfDirToDelete . evalCommands

main :: IO ()
main = runSolution "day7.txt" $ solveA <> solveB

