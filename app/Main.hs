{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Main (main) where

import Data.Char (isDigit)
import Lib
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then error "Missing argument"
    else putStrLn ("solution for " ++ head args)
  let arg = head args
  s <- getInput arg
  let res = getSolutionAlgo arg s
  print res

-- not a good way to do this, especially without verification on arg
getInput :: [Char] -> IO String
getInput arg = readFile ("inputs/" ++ takeWhile (/= 'p') arg ++ ".txt")

getSolutionAlgo :: [Char] -> String -> Int
getSolutionAlgo arg =
  let d = takeWhile isDigit (tail arg)
      p = tail (dropWhile (/= 'p') arg)
   in case (d, p) of
        ("1", "1") -> d1p1
        ("1", "2") -> d1p2
        ("2", "1") -> d2p1
        ("2", "2") -> d2p2
        ("3", "1") -> d3p1
        ("3", "2") -> d3p2
        ("4", "1") -> d4p1
        ("4", "2") -> d4p2
        ("5", "1") -> d5p1
        ("5", "2") -> d5p2
        ("6", "1") -> d6p1
        ("6", "2") -> d6p2
        ("7", "1") -> d7p1
        ("7", "2") -> d7p2
        ("8", "1") -> d8p1
        ("8", "2") -> d8p2
        ("9", "1") -> d9p1
        ("9", "2") -> d9p2
        ("10", "1") -> d10p1
        ("10", "2") -> d10p2
        ("11", "1") -> d11p1
        ("11", "2") -> d11p2
        _ -> error (arg ++ " " ++ d ++ " " ++ p ++ " is not implemented yet")