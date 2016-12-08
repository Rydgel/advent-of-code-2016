module Shared.String
    ( substring
    ) where

substring :: String -> String -> Bool
substring _ [] = False
substring xs ys
  | prefix xs ys = True
  | substring xs (tail ys) = True
  | otherwise = False

prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys
