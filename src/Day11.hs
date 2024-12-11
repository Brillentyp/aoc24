{-# LANGUAGE TupleSections #-}

module Day11 (d11p1, d11p2) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map

parseInput :: String -> [Int]
parseInput = map read . filter (not . null) . splitOn " "

-- Note that this returns 0 for 0 (fine for my usecase, because zeros are caught by one of the other rules anyway)
numDigits :: Int -> Int
numDigits i = go i 0
  where
    go num acc
      | num == 0 = acc
      | otherwise = go (div num 10) (acc + 1)

pow :: Int -> Int -> Int
pow _ 0 = 1
pow b e = b * pow b (e - 1)

splitNumber :: Int -> [Int]
splitNumber num =
  let sl = div (numDigits num) 2
      n = pow 10 sl
      l = div num n
   in [l, num - (l * n)]

rules :: Int -> [Int]
rules i
  | i == 0 = [1]
  | even (numDigits i) = splitNumber i
  | otherwise = [i * 2024]

applyN :: (a -> a) -> Int -> a -> a
applyN _ 0 x = x
applyN f n x = applyN f (n - 1) (f x)

d11p1 :: String -> Int
d11p1 s = let inp = parseInput s in length (applyN (concatMap rules) 25 inp)

-- try some sort dp, maybe with a map? Array would be cheap, 9 * 75 entries should be enough
preprocess :: Int -> Int -> Map.Map (Int, Int) Int
preprocess x y = let indicesToCalc = [(i, j) | j <- [0 .. x], i <- [0 .. y]] in populateMap Map.empty indicesToCalc

rulesWithMap :: Map.Map (Int, Int) Int -> [(Int, Int)] -> Int -> Int
rulesWithMap m (e@(i, l) : ls) acc
  | l == 0 = rulesWithMap m ls (acc + 1)
  | i == 0 = rulesWithMap m ((1, l - 1) : ls) acc
  | nd == 1 = case Map.lookup e m of
      Just x -> rulesWithMap m ls (acc + x)
      Nothing -> rulesWithMap m ((if i == 0 then (1, l - 1) else (i * 2024, l - 1)) : ls) acc
  | even nd = rulesWithMap m ((head (splitNumber i), l - 1) : (splitNumber i !! 1, l - 1) : ls) acc
  | otherwise = rulesWithMap m ((i * 2024, l - 1) : ls) acc
  where
    nd = numDigits i
rulesWithMap _ [] acc = acc

populateMap :: Map.Map (Int, Int) Int -> [(Int, Int)] -> Map.Map (Int, Int) Int
populateMap m ((i, level) : ls) = populateMap (Map.insert (i, level) (rulesWithMap m [(i, level)] 0) m) ls
populateMap m [] = m

-- the parameters for preprocess are a bit random but they seem to work well
d11p2 :: String -> Int
d11p2 s =
  let inp = parseInput s
      pm = preprocess 60 100
   in rulesWithMap pm (map (,75) inp) 0
