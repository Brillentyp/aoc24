module Day4 (d4p1, d4p2, example) where

createDiagonalListsLR :: [[a]] -> [[a]]
createDiagonalListsLR l = [[(l !! y) !! x | (x, y) <- zip [i .. len - 1] [j .. len - 1]] | i <- [0 .. len - 1], j <- [0 .. len - 1], i == 0 || j == 0]
  where
    len = length l -- input is quadratic

createDiagonalListsRL :: [[a]] -> [[a]]
createDiagonalListsRL l = [[(l !! y) !! x | (x, y) <- zip (downTo i 0) [j .. len - 1]] | i <- [0 .. len - 1], j <- [0 .. len - 1], i == (len - 1) || j == 0]
  where
    len = length l -- input is quadratic

downTo :: (Ord t, Num t) => t -> t -> [t]
downTo from to = if from < to then [] else from : downTo (from - 1) to

createVerticalLists :: [[a]] -> [[a]]
createVerticalLists l = [[(l !! y) !! x | y <- [0 .. len - 1]] | x <- [0 .. len - 1]]
  where
    len = length l

countXmas :: (Num t) => [Char] -> t -> t
countXmas l@('X' : 'M' : 'A' : 'S' : _) count = countXmas (tail l) (count + 1)
countXmas l@('S' : 'A' : 'M' : 'X' : _) count = countXmas (tail l) (count + 1)
countXmas (_ : xs) count = countXmas xs count
countXmas [] c = c

d4p1 :: String -> Int
d4p1 inp =
  let l = filter (not . null) (lines inp)
   in sum (map (`countXmas` 0) l) + sum (map (`countXmas` 0) (createDiagonalListsLR l)) + sum (map (`countXmas` 0) (createDiagonalListsRL l)) + sum (map (`countXmas` 0) (createVerticalLists l))

-- linesxcolumns
axbWindows :: Int -> Int -> [[a]] -> [[[[a]]]]
axbWindows a b list = [[map (take b . drop j) (take a (drop i list))] | i <- [0 .. len - a], j <- [0 .. len - b]]
  where
    len = length list

checkXMas :: [[Char]] -> Int
checkXMas [['M', _, 'M'], [_, 'A', _], ['S', _, 'S']] = 1
checkXMas [['M', _, 'S'], [_, 'A', _], ['M', _, 'S']] = 1
checkXMas [['S', _, 'M'], [_, 'A', _], ['S', _, 'M']] = 1
checkXMas [['S', _, 'S'], [_, 'A', _], ['M', _, 'M']] = 1
checkXMas _ = 0

d4p2 :: String -> Int
d4p2 = sum . map checkXMas . concat . axbWindows 3 3 . filter (not . null) . lines

example :: String
example = "MMMSXXMASM\nMSAMXMSMSA\nAMXSXMAAMM\nMSAMASMSMX\nXMASAMXAMM\nXXAMMXXAMA\nSMSMSASXSS\nSAXAMASAAA\nMAMMMXMMMM\nMXMXAXMASX\n"