module Main where

import Data.Foldable
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Solution
import Text.ParserCombinators.ReadP

type Crate = Char

type Crates = Map Int [Crate]

type Move = Dual (Endo Crates)

pickupCrate :: Int -> Crates -> Maybe (Crate, Crates)
pickupCrate from crates = lookupCol >>= grabTop >>= doPickup
  where
    lookupCol = M.lookup from crates
    grabTop = listToMaybe
    doPickup crate = pure (crate, M.adjust tail from crates)

putDownCrate :: Int -> (Crate, Crates) -> Crates
putDownCrate to (crate, crates) = M.alter upsert to crates
  where
    upsert Nothing = Just [crate]
    upsert (Just xs) = Just $ crate : xs

moveOne :: Int -> Int -> Move
moveOne from to = Dual $ Endo $ \crates -> fromMaybe crates $ doPickup crates >>= doPutdown
  where
    doPickup crates = pickupCrate from crates
    doPutdown = pure . putDownCrate to

moveN :: Int -> Int -> Int -> Move
moveN qty from to =  fold . replicate qty $ moveOne from to

pCrate :: ReadP Crate
pCrate = char '[' *> get <* char ']'

pMaybeCrate :: ReadP (Maybe Crate)
pMaybeCrate = pJustCrate +++ pNothing
  where
    pJustCrate = Just <$> pCrate
    pNothing = Nothing <$ string "   "

pCrateRow :: ReadP [Maybe Crate]
pCrateRow = sepBy pMaybeCrate $ char ' '

pCrates :: ReadP Crates
pCrates = makeCols <$> pRows <*> pColLabels
  where
    pRows = many $ pCrateRow <* char '\n'
    pColLabels = char ' ' *> sepBy pColKey pColSep <* string " \n"
    pColKey = readS_to_P reads
    pColSep = string "   "
    makeCols rows keys = M.fromList $ zip keys $ fmap catMaybes $ transpose rows

pMove :: ReadP Move
pMove = moveN <$> pQty <*> pFrom <*> pTo
  where
    pQty = string "move " *> pInt
    pFrom = string " from " *> pInt
    pTo = string " to " *> pInt
    pInt = readS_to_P reads

pMoves :: ReadP Move
pMoves = fmap fold $ char '\n' *> many pMoveLine
  where pMoveLine = pMove <* char '\n'

parser :: ReadP (Crates, Move)
parser = (,) <$> pCrates <*> pMoves

solveA :: Solution
solveA = mkSolution parser $ getTopCrates . applyMoves
  where
    applyMoves (crates, Dual moves) = appEndo moves crates
    getTopCrates = fmap (head . snd) . M.toList

main :: IO ()
main = runSolution "day5.txt" solveA

