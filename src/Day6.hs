{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day6 (d6p1, d6p2) where

import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, fromJust)
import Prelude hiding (pred)

turn :: (Eq a1, Eq a2, Num a1, Num a2, Num a3, Num b) => (a1, a2) -> (a3, b)
turn (0, -1) = (1, 0)
turn (1, 0) = (0, 1)
turn (0, 1) = (-1, 0)
turn (-1, 0) = (0, -1)
turn _ = error "not a valid direction (for this problem)"

parseInput :: [Char] -> ((Map.Map (Int, Int) Bool, (Int, Int)), (Int, Int))
parseInput inp =
  let input = (filter (not . null) . lines) inp
   in (parse Map.empty (0, 0) 0 input, (length input, length (head input)))
  where
    parse obsMap gPos y (l : ls) =
      let (m, g) = parseLine obsMap gPos 0 y l
       in parse m g (y + 1) ls
    parse obsMap gPos _ [] = (obsMap, gPos)

handleMovement :: (Ord b, Num b, Num a1, Enum b) => Map.Map (b, b) a2 -> (b, b) -> Map.Map (b, b) a1 -> (b, b) -> (b, b) -> (Map.Map (b, b) a1, (b, b), Bool)
handleMovement obsMap gPos passedMap direction bounds =
  -- (-1,-1) might not be the best choice, but should not matter for input sizes greater 1
  insertTillBounds passedMap bounds (-1, -1) (takeWhile (isNothing . flip Map.lookup obsMap) (getIndicesInDirection gPos direction))

getIndicesInDirection :: (Num b, Enum b) => (b, b) -> (b, b) -> [(b, b)]
getIndicesInDirection (gx, gy) (dx, dy) = map (\l -> (gx + l * dx, gy + l * dy)) [0 ..]

-- return True if bounds are overstepped (that is how we know we are finished)
-- insertTillBounds :: Map.Map (a1, b) a2 -> (a1, b) -> t -> [(a1, b)] -> (Map.Map (a1, b) a2, t, Bool)
insertTillBounds :: (Ord b, Ord a1, Num a1, Num b, Num a2) => Map.Map (a1, b) a2 -> (a1, b) -> (a1, b) -> [(a1, b)] -> (Map.Map (a1, b) a2, (a1, b), Bool)
insertTillBounds passedMap b@(xMax, yMax) lastElem ((x, y) : ls)
  | y < yMax && x < xMax && x >= 0 && y >= 0 = insertTillBounds (Map.insert (x, y) 1 passedMap) b (x, y) ls
  | otherwise = (passedMap, lastElem, True)
insertTillBounds passedMap _ lastElem [] = (passedMap, lastElem, False)

-- There is no particular reason I put Bool as element in the map
parseLine :: Map.Map (Int, Int) Bool -> (Int, Int) -> Int -> Int -> [Char] -> (Map.Map (Int, Int) Bool, (Int, Int))
parseLine obsMap gPos x y (l : ls) = parseLine (if l == '#' then Map.insert (x, y) True obsMap else obsMap) (if l == '^' then (x, y) else gPos) (x + 1) y ls
parseLine obsMap gPos _ _ [] = (obsMap, gPos)

handleAllMovement :: Map.Map (Int, Int) Bool -> Map.Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) Int
handleAllMovement obsMap passedMap gPos dir bounds =
  let (npm, ngp, finished) = handleMovement obsMap gPos passedMap dir bounds
   in if finished then npm else handleAllMovement obsMap npm ngp (turn dir) bounds

startDir :: (Int, Int)
startDir = (0, -1)

d6p1 :: String -> Int
d6p1 s =
  let ((obstacleMap, guardPosition), bounds) = parseInput s
   in Map.size (handleAllMovement obstacleMap Map.empty guardPosition startDir bounds)

newGuardPos :: (Ord a1, Num a1, Enum a1) => Map.Map (a1, a1) a2 -> (a1, a1) -> (a1, a1) -> (a1, a1) -> (a1, a1)
newGuardPos obsMap (xMax, yMax) currentGPos dir =
  -- the bounds conditions are changed so that we get an out of bounds index with takeLastBefore
  takeLastBefore (\(x, y) -> isJust (Map.lookup (x, y) obsMap) || x > xMax || y > yMax || x < -1 || y < -1) (getIndicesInDirection currentGPos dir)

takeLastBefore :: (a -> Bool) -> [a] -> a
takeLastBefore pred (x : y : xs)
  | pred x = x
  | pred y = x
  | otherwise = takeLastBefore pred (y : xs)
takeLastBefore _ [] = error "Only for infinite lists" -- maybe better to just return the last element
takeLastBefore _ _ = error "Should not happen"

inBounds :: (Ord a1, Ord a2, Num a1, Num a2) => (a1, a2) -> (a1, a2) -> Bool
inBounds (xMax, yMax) (x, y) = x >= 0 && y >= 0 && x < xMax && y < yMax

-- if we and up at the same position, with the same direction, we are in a loop
isLoop :: (Num b, Enum b, Ord b) => (b, b) -> Map.Map (b, b) a2 -> (b, b) -> (b, b) -> Map.Map (b, b) [(b, b)] -> Bool
isLoop bounds obsMap gPos dir posDirMap
  | not (inBounds bounds ngp) = False
  | isJust  maybeDirList = (dir `elem` fromJust maybeDirList) || isLoop bounds obsMap ngp newDir (Map.insert ngp (dir:fromJust maybeDirList) posDirMap) -- insertWith might be smarte
  | otherwise = isLoop bounds obsMap ngp newDir (Map.insert ngp [dir] posDirMap)
  where
    ngp = newGuardPos obsMap bounds gPos dir
    maybeDirList = Map.lookup ngp posDirMap
    newDir = turn dir

d6p2 :: [Char] -> Int
d6p2 s =
  let ((obstacleMap, guardPosition), bounds@(xMax, yMax)) = parseInput s
   in length (filter id [isLoop bounds (Map.insert newObs True obstacleMap) guardPosition startDir Map.empty | newObs <- [(x,y) | x <- [0..xMax - 1], y <- [0..yMax - 1]], newObs /= guardPosition])


example :: String
example =   "....#.....\n\
            \.........#\n\
            \..........\n\
            \..#.......\n\
            \.......#..\n\
            \..........\n\
            \.#..^.....\n\
            \........#.\n\
            \#.........\n\
            \......#...\n"