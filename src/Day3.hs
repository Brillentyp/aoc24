module Day3 (d3p1, d3p2) where

import Data.Char (isDigit)

checkValidNumberTo :: Char -> [Char] -> Bool
checkValidNumberTo delim s =
  let tillDelim = takeWhile (/= delim) s
      l = length tillDelim
   in l <= 3 && l >= 1 && all isDigit tillDelim

checkParenthContent :: [Char] -> Bool
checkParenthContent s = checkValidNumberTo ',' s && checkValidNumberTo ')' (tail (dropWhile isDigit s))

parseParenthContent :: [Char] -> (Int, Int)
parseParenthContent s = (read (takeWhile (/= ',') s), read (takeWhile isDigit (tail (dropWhile isDigit s))))

sol :: [Char] -> [(Int, Int)]
sol ('m' : 'u' : 'l' : '(' : xs)
  | checkParenthContent xs = parseParenthContent xs : sol xs
  | otherwise = sol xs
sol (_ : xs) = sol xs
sol [] = []

d3p1 :: [Char] -> Int
d3p1 = sum . map (uncurry (*)) . sol

sol2 :: [Char] -> Bool -> [(Int, Int)]
sol2 ('d' : 'o' : 'n' : '\'' : 't' : '(' : ')' : xs) _ = sol2 xs False
sol2 ('d' : 'o' : '(' : ')' : xs) _ = sol2 xs True
sol2 ('m' : 'u' : 'l' : '(' : xs) enabled
  | checkParenthContent xs && enabled = parseParenthContent xs : sol2 xs enabled
  | otherwise = sol2 xs enabled
sol2 (_ : xs) enabled = sol2 xs enabled
sol2 [] _ = []

d3p2 :: [Char] -> Int
d3p2 = sum . map (uncurry (*)) . flip sol2 True