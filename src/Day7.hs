{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day7(d7p1, d7p2) where

import Data.List.Split (splitOn)

parseInput :: String -> [(Int, [Int])]
parseInput = readParts . splitParts . filter (not . null) . lines

parseList :: [Char] -> [Int]
parseList = map read . filter (not . null) . splitOn " "

splitParts :: [[Char]] -> [([Char], [Char])]
splitParts = map ((\l -> (head l, concat (tail l))) . splitOn ":")

readParts :: [(String, String)] -> [(Int, [Int])]
readParts = map (\(l, r) -> (read l, parseList r))

try1 :: (Ord t, Num t) => [t] -> t -> t -> Bool
try1 (x : xs) current goal
  | current > goal = False
  | current == goal = True
  | otherwise = try1 xs (max current 1 * x) goal || try1 xs (current + x) goal
try1 [] current goal = current == goal

d7p1 :: String -> Int
d7p1 = sum . map fst . filter (\(goal, nums) -> try1 nums 0 goal) . parseInput

numDigits :: (Num a, Integral t) => t -> a
numDigits a = if a <= 9 then 1 else 1 + numDigits (div a 10)

pow10 :: Int -> Int
pow10 0 = 1
pow10 e = 10 * pow10 (e-1)

concatinate :: Int -> Int -> Int
concatinate a b = pow10 (numDigits b) * a + b


try2 :: [Int] -> Int -> Int -> Bool
try2 (x : xs) current goal
  | current > goal = False
  | otherwise = try2 xs (max current 1 * x) goal || try2 xs (current + x) goal || try2 xs (if current == 0 then x else concatinate current x) goal
try2 [] current goal = current == goal

d7p2 :: String -> Int
d7p2 = sum . map fst . filter (\(goal, nums) -> try2 nums 0 goal) . parseInput

example :: String
example =
  "190: 10 19\n\
  \3267: 81 40 27\n\
  \83: 17 5\n\
  \156: 15 6\n\
  \7290: 6 8 6 15\n\
  \161011: 16 10 13\n\
  \192: 17 8 14\n\
  \21037: 9 7 18 13\n\
  \292: 11 6 16 20"