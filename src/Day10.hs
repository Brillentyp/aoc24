{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Day10(d10p1, d10p2) where

import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set

type Bounds = (Int, Int)

readDigit :: Char -> Int
readDigit '0' = 0
readDigit '1' = 1
readDigit '2' = 2
readDigit '3' = 3
readDigit '4' = 4
readDigit '5' = 5
readDigit '6' = 6
readDigit '7' = 7
readDigit '8' = 8
readDigit '9' = 9
readDigit _ = error "not a digit"

inputToMap :: String -> Map.Map (Int, Int) Int
inputToMap s = tm 0 Map.empty (filter (not . null) (lines s))
  where
    tm cur_y m (l : ls) = tm (cur_y + 1) (lineToMap m 0 cur_y l) ls
    tm _ m [] = m

lineToMap :: Map.Map (Int, Int) Int -> Int -> Int -> String -> Map.Map (Int, Int) Int
lineToMap m cur_x cur_y (l : ls) = lineToMap (Map.insert (cur_x, cur_y) (readDigit l) m) (cur_x + 1) cur_y ls
lineToMap m _ _ [] = m

inBounds :: Bounds -> (Int, Int) -> Bool
inBounds (mx, my) (x, y) = x < mx && y < my && x >= 0 && y >= 0

neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

transformCoord :: Bounds -> (Int, Int) -> Int
transformCoord (bx, _) (x, y) = bx * y + x

-- neighboursFromMap :: Bounds -> Map.Map (Int, Int) Int -> (Int, Int) -> Int -> [Int]
neighboursFromMap :: Bounds -> Map.Map (Int, Int) Int -> (Int, Int) -> Int -> [(Int, Int)]
neighboursFromMap b m index level = map (\(i, l) -> (transformCoord b i, l)) (filter ((== level + 1) . snd) (map (\x -> (x, fromJust (Map.lookup x m))) (filter (inBounds b) (neighbours index))))

-- Graph and indices with Value 0
posMaptoGMap :: Map.Map (Int, Int) Int -> Bounds -> (Map.Map Int [(Int, Int)], [Int])
posMaptoGMap posm bounds@(bx, by) =
  let indices = [(x, y) | x <- [0 .. bx - 1], y <- [0 .. by - 1]]
   in (bgm bounds posm Map.empty indices, map (\(k, _) -> transformCoord bounds k) (filter ((== 0) . snd) (Map.assocs posm)))
  where
    bgm b pm gm (i : is) =
      let e = fromJust (Map.lookup i pm)
          n = neighboursFromMap b pm i e
       in bgm b pm (Map.insertWith (++) (transformCoord b i) n gm) is
    bgm _ _ gm [] = gm

dfs2 :: Map.Map Int [(Int, Int)] -> Set.Set Int -> Int -> Int -> [(Int, Int)] -> Int
dfs2 _ _ _ acc [] = acc
dfs2 g seen goal_level acc ((node, level):ns)
    | Set.member node seen = dfs2 g seen goal_level acc ns
    | goal_level == level = dfs2 g nseen goal_level (acc + 1) ns
    | otherwise = dfs2 g nseen goal_level acc (newNodes ++ ns)
        where nseen = Set.insert node seen
              newNodes = fromJust (Map.lookup node g)

dfs :: Map.Map Int [(Int, Int)] -> Set.Set Int -> Int -> (Int, Int) -> Int
dfs g seen goal_level (node, level)
  | Set.member node seen = 0
  | level == goal_level = 1
  | otherwise = sum (map (dfs g (Set.insert node seen) goal_level) n_step)
  where
    n_step = fromJust (Map.lookup node g)

d10p1 :: String -> Int
d10p1 s =
  let pm = inputToMap s
      l = filter (not . null) (lines s)
      bounds = (length (head l), length l)
      (gm, zeros) = posMaptoGMap pm bounds
   in sum (map (curry (\x -> dfs2 gm Set.empty 9 0 [x]) 0) zeros)


d10p2 :: String -> Int
d10p2 s =
  let pm = inputToMap s
      l = filter (not . null) (lines s)
      bounds = (length (head l), length l)
      (gm, zeros) = posMaptoGMap pm bounds
   in sum (map (curry (dfs gm Set.empty 9) 0) zeros)

example :: String
example =
  "0123\n\
  \1234\n\
  \8765\n\
  \9876\n"

example2 :: String
example2 =
    "89010123\n\
\78121874\n\
\87430965\n\
\96549874\n\
\45678903\n\
\32019012\n\
\01329801\n\
\10456732\n"