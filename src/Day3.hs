module Day3 (d3p1) where

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