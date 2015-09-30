module OC.Utils where

import Data.List

charAfter :: Char -> String -> Char
charAfter c = head . strAfter c

strBetween :: Char -> Char -> String -> String
strBetween c1 c2 s = case (elemIndex c1 s, elemIndex c2 s) of
                      (_, Nothing) -> ""
                      (Nothing, _) -> ""
                      (Just _, Just _) -> strBefore c2 $ strAfter c1 s

strBefore :: Char -> String -> String
strBefore c = takeWhile (/= c)

strAfter :: Char -> String -> String
strAfter c = tail . dropWhile (/= c)

splitOn :: Char -> String -> [String]
splitOn char str
  | elem char str = (strBefore char str) : (splitOn char $ strAfter char str)
  | otherwise     = [str]
