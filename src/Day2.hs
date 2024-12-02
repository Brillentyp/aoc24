module Day2 (d2p1, d2p2) where

import Data.List.Split (splitOn)

data Monotony = MI | MD | SAME | NEITHER deriving (Eq, Show)

parseInput :: String -> [[Int]]
parseInput = map (map read . splitOn " ") . filter (not . null) . lines

maxDiff :: [Int] -> Int
maxDiff = _maxDiff 0
  where
    _maxDiff diff (x : y : zs) = _maxDiff (max diff (abs (x - y))) (y : zs)
    _maxDiff diff _ = diff

comM :: (Ord a) => a -> a -> Monotony
comM x y
  | x == y = SAME
  | x < y = MI
  | x > y = MD
  | otherwise = error "This should literally be impossible"

combineM :: Monotony -> Monotony -> Monotony
combineM x y
  | x == y = x
  | otherwise = NEITHER

getMonotony :: (Ord a) => [a] -> Monotony
getMonotony l@(x : y : _) = _getMonotony l (comM x y)
getMonotony _ = SAME

_getMonotony :: (Ord a) => [a] -> Monotony -> Monotony
_getMonotony (x : y : zs) m =
  let nm = combineM (comM x y) m
   in if nm /= NEITHER
        then _getMonotony (y : zs) nm
        else nm
_getMonotony _ m = m

d2p1 :: String -> Int
d2p1 = sum . map lf . parseInput
  where
    lf x =
      let m = getMonotony x
       in if m /= NEITHER && m /= SAME && maxDiff x <= 3 then 1 else 0

makeLists :: [a] -> [[a]]
makeLists l = [take i l ++ drop (i + 1) l | i <- [0 .. length l]]

p2_sol :: [Int] -> Bool
p2_sol = any lf . makeLists
  where
    lf x =
      let m = getMonotony x
       in m /= NEITHER && m /= SAME && maxDiff x <= 3

d2p2 :: String -> Int
d2p2 = length . filter id . map p2_sol . parseInput