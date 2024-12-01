module Day1 (d1p1, d1p2)
where

import Data.List.Split
import Data.List (sort)

parseIput :: String -> [(Int, Int)]
parseIput = map (myToTuple . filter (not . null) . splitOn " ") . filter (not . null) . lines


myToTuple :: (Read a, Read b) => [String] -> (a, b)
myToTuple (x:y:_) = (read x, read y)
myToTuple _ = error "oops"

d1p1 :: String -> Int
d1p1 inp = let  i = parseIput inp
                (l,r) = unzip i in
    sum (map abs ( zipWith subtract (sort l) (sort r)))

d1p2 :: String -> Int
d1p2 inp = let  i = parseIput inp
                (l,r) = unzip i in
                    sum (map (\x -> x * length (filter (==x) r)) l)
