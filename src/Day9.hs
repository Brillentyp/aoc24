module Day9 (d9p1, d9p2) where

import Data.Maybe (fromJust, isJust)

parseInput :: String -> ([Maybe Int], Int)
parseInput s = (toMaybe 0 s, numBlocks s)

numBlocks :: (Num a, Read a) => [Char] -> a
numBlocks b = sum (map (\x -> read [x]) (takeEverySecond b))

-- hope this is not to slow
toMaybe :: (Num t) => t -> String -> [Maybe t]
toMaybe i (x : y : xs) = replicate (read [x]) (Just i) ++ replicate (read [y]) Nothing ++ toMaybe (i + 1) xs
toMaybe i [x] = replicate (read [x]) (Just i)
toMaybe _ _ = []

takeEverySecond :: [a] -> [a]
takeEverySecond (x : _ : xs) = x : takeEverySecond xs
takeEverySecond [x] = [x]
takeEverySecond [] = []

createCompactedList :: [Maybe Int] -> Int -> [Int]
createCompactedList mi nb = let rml = reverse (filter isJust mi) in take nb (s1 mi rml)
  where
    s1 (m : ms) rm@(r : rs) =
      case m of
        Just x -> x : s1 ms rm
        Nothing -> fromJust r : s1 ms rs
    s1 [] _ = []
    s1 _ [] = []

d9p1 :: String -> Int
d9p1 s =
  let (lm, nb) = parseInput s
      compacted = createCompactedList lm nb
   in (sum . zipWith (*) [0 ..]) compacted

-- Block index length, Free length
data Extent = Block Int Int | Free Int
  deriving (Eq, Show)

isBlock :: Extent -> Bool
isBlock (Block _ _) = True
isBlock (Free _) = False

toExtents :: String -> Int -> [Extent]
toExtents (x : y : xs) i = Block i (read [x]) : Free (read [y]) : toExtents xs (i + 1)
toExtents [x] i = [Block i (read [x])]
toExtents [] _ = []

insertIntoFree :: [Extent] -> Extent -> [Extent]
insertIntoFree _ (Free _) = error "inserting Free extents is not supported"
-- insertIntoFree (ce : cs) e = if ce == e then ce : cs else ce : insertIntoFree cs e -- No block may be inserted after itself
insertIntoFree (ce : cs) e@(Block _ bl) =
  if e == ce
    then ce : cs -- we may not move blocks to the right
    else case ce of
      Block _ _ -> ce : insertIntoFree cs e
      Free fl -> if fl < bl then ce : insertIntoFree cs e else if fl == bl then e : deleteBlock e cs else e : Free (fl - bl) : deleteBlock e cs
insertIntoFree _ _ = error "help"

deleteBlock :: Extent -> [Extent] -> [Extent]
deleteBlock d (e@(Block _ l) : es) = if d == e then Free l : es else e : deleteBlock d es
deleteBlock d (e@(Free _) : es) = e : deleteBlock d es
deleteBlock _ [] = []

compactExtents :: [Extent] -> [Extent]
compactExtents extents =
  let reversedExtents = reverse (filter isBlock extents)
   in cE extents reversedExtents
  where
    cE e (r : re) = cE (insertIntoFree e r) re
    cE e [] = e

extentChecksum :: [Extent] -> Int
extentChecksum extents = sum (zipWith (*) [0 ..] (ec extents))
  where
    ec (e : es) = case e of
      (Free l) -> replicate l 0 ++ ec es
      Block i l -> replicate l i ++ ec es
    ec [] = []

d9p2 :: String -> Int
d9p2 s =
  let el = toExtents s 0
   in extentChecksum (compactExtents el)