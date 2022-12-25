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

type Mover = Int -> Int -> Int -> Move

pickupCrate :: Int -> Crates -> Maybe (Crate, Crates)
pickupCrate from crates = lookupCol >>= grabTop >>= doPickup
  where
    lookupCol = M.lookup from crates
    grabTop = listToMaybe
    doPickup crate = pure (crate, M.adjust tail from crates)

pickupMultiple :: Int -> Int -> Crates -> Maybe ([Crate], Crates)
pickupMultiple qty from crates = lookupCol >>= grabN >>= doPickup
  where
    lookupCol = M.lookup from crates
    grabN = pure . take qty
    doPickup grabbed = pure (grabbed, M.adjust removeN from crates)
    removeN = drop qty

putDownCrate :: Int -> (Crate, Crates) -> Crates
putDownCrate to (crate, crates) = M.alter upsert to crates
  where
    upsert Nothing = Just [crate]
    upsert (Just xs) = Just $ crate : xs

putDownMultiple :: Int -> ([Crate], Crates) -> Crates
putDownMultiple to (grabbed, crates) = M.alter upsert to crates
  where
    upsert Nothing = Just grabbed
    upsert (Just xs) = Just $ grabbed ++ xs

moveOne :: Int -> Int -> Move
moveOne from to = Dual . Endo $ \crates -> fromMaybe crates $ doPutdown <$> doPickup crates
  where
    doPickup crates = pickupCrate from crates
    doPutdown = putDownCrate to

moveN :: Mover
moveN qty from to =  fold . replicate qty $ moveOne from to

moveMultiple :: Mover
moveMultiple qty from to = Dual . Endo $ \crates -> fromMaybe crates $ doPutdown <$> doPickup crates
  where
    doPickup = pickupMultiple qty from
    doPutdown = putDownMultiple to

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

pMove :: Mover -> ReadP Move
pMove mover = mover <$> pQty <*> pFrom <*> pTo
  where
    pQty = string "move " *> pInt
    pFrom = string " from " *> pInt
    pTo = string " to " *> pInt
    pInt = readS_to_P reads

pMoves :: Mover -> ReadP Move
pMoves mover = fmap fold $ char '\n' *> many pMoveLine
  where pMoveLine = pMove mover <* char '\n'

parser :: Mover -> ReadP (Crates, Move)
parser mover = (,) <$> pCrates <*> pMoves mover

solve :: Mover -> Solution
solve mover = mkSolution p $ getTopCrates . applyMoves
  where
    applyMoves (crates, Dual moves) = appEndo moves crates
    getTopCrates = fmap (head . snd) . M.toList
    p = parser mover

main :: IO ()
main = runSolution "day5.txt" $ solve moveN <> solve moveMultiple

