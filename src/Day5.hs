{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day5(d5p1) where

import qualified Data.IntMap as Map
import Data.List.Split (splitOn)
import Prelude hiding (lookup)

parseOrdering :: [[Char]] -> Map.IntMap [Int]
parseOrdering l = Map.fromListWith (++) (map (\x -> (read (takeWhile (/= '|') x), [read (tail (dropWhile (/= '|') x))])) l)

parseLists :: [[Char]] -> [[Int]]
parseLists = map (map read) . filter (not . null) . map (splitOn ",")

checkForNumber :: (Foldable t1, Foldable t2, Eq a) => Map.IntMap (t1 a) -> Int -> t2 a -> Bool
checkForNumber m el l =
  let maybe_after_list = Map.lookup el m
   in case maybe_after_list of
        Just after_list -> all (`notElem` l) after_list
        Nothing -> True

checkList :: (Foldable t) => [Int] -> Map.IntMap (t Int) -> Bool
checkList list before_map = cl list before_map []
  where
    cl (l : ls) m prefix = checkForNumber m l prefix && cl ls m (l : prefix)
    cl [] _ _ = True

middle :: [a] -> a
middle l = l !! max 0 (div (length l) 2)

d5p1 :: String -> Int
d5p1 inp =
  let input = filter (not . null) (lines inp)
      m = parseOrdering (takeWhile (elem '|') input)
      lists = parseLists (dropWhile (elem '|') input)
   in sum (map middle (filter (`checkList` m) lists))


example :: String
example = "47|53\n97|13\n97|61\n97|47\n75|29\n61|13\n75|53\n29|13\n97|29\n53|29\n61|53\n97|53\n61|29\n47|13\n75|47\n97|75\n47|61\n75|61\n47|29\n75|13\n53|13\n\n75,47,61,53,29\n97,61,53,29,13\n75,29,13\n75,97,47,61,53\n61,13,29\n97,13,75,29,47\n"