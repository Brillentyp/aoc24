{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Day8 (d8p1, d8p2) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Bounds = (Int, Int)

type Position = (Int, Int)

parseInput :: String -> (Bounds, Map.Map Char [Position])
parseInput inp =
  let l = (filter (not . null) . lines) inp
      bounds = (length (head l), length l)
   in (bounds, parseLines 0 l Map.empty)

parseLine :: Int -> Int -> String -> Map.Map Char [Position] -> Map.Map Char [Position]
parseLine x y (l : ls) m = if l /= '.' then parseLine (x + 1) y ls (Map.insertWith (++) l [(x, y)] m) else parseLine (x + 1) y ls m
parseLine _ _ [] m = m

parseLines :: Int -> [String] -> Map.Map Char [Position] -> Map.Map Char [Position]
parseLines currY (l : ls) m =
  let nm = parseLine 0 currY l m
   in parseLines (currY + 1) ls nm
parseLines _ [] m = m

createPairings :: [Position] -> [(Position, Position)]
createPairings pos = concat (cP pos)
  where
    cP (p : ps) = map (\x -> (p, x)) ps : cP ps
    cP [] = []

posAdd :: Position -> Position -> Position
posAdd (x, y) (a, b) = (x + a, y + b)

posSub :: Position -> Position -> Position
posSub (x, y) (a, b) = (x - a, y - b)

posScale :: Int -> Position -> Position
posScale x (a, b) = (x * a, x * b)

antinodesFromPair :: (Position, Position) -> [Position]
antinodesFromPair (p1, p2) =
  let p1ToP2 = posSub p2 p1
   in [posAdd p2 p1ToP2, posAdd p1 (posScale (-1) p1ToP2)]

listCandidates :: Bounds -> [Position] -> [Position]
listCandidates bounds p =
  let pairings = createPairings p
   in concatMap (filter (inBounds bounds) . antinodesFromPair) pairings

inBounds :: Bounds -> Position -> Bool
inBounds (bx, by) (x, y) = x >= 0 && y >= 0 && x < bx && y < by

createAntinodeSet :: Bounds -> Map.Map Char [Position] -> Set.Set Position
createAntinodeSet bounds m =
  let pL = map snd (Map.toList m)
   in Set.fromList (concatMap (listCandidates bounds) pL)

d8p1 :: String -> Int
d8p1 inp =
  let (bounds, m) = parseInput inp
      antinodeSet = createAntinodeSet bounds m
   in Set.size antinodeSet

antinodesFromPair2 :: Bounds -> (Position, Position) -> [Position]
antinodesFromPair2 bounds (p1, p2) =
  let p1ToP2 = posSub p2 p1
   in (takeWhile (inBounds bounds) [posAdd p1 (posScale i p1ToP2) | i <- [0 ..]]) ++ takeWhile (inBounds bounds) [posAdd p1 (posScale i p1ToP2) | i <- iterate (+ (-1)) (-1)]

listCandidates2 :: Bounds -> [Position] -> [Position]
listCandidates2 bounds p =
  let pairings = createPairings p
   in concatMap (antinodesFromPair2 bounds) pairings

createAntinodeSet2 :: Bounds -> Map.Map Char [Position] -> Set.Set Position
createAntinodeSet2 bounds m =
  let pL = map snd (Map.toList m)
   in Set.fromList (concatMap (listCandidates2 bounds) pL)

d8p2 :: String -> Int
d8p2 inp =
  let (bounds, m) = parseInput inp
      antinodeSet = createAntinodeSet2 bounds m
   in Set.size antinodeSet
