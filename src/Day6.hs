module Day6
    ( day6
    , day6'
    ) where

{- Day 6: Signals and Noise -}

import Control.Arrow ((&&&))
import Data.List (group, sort, transpose)

count :: String -> [(Int, Char)]
count = sort . map (length &&& head) . group . sort

day6 :: IO ()
day6 = do
  input <- readFile "resources/day6.txt"
  let response = map (snd . last . count) . transpose . lines $ input
  print response

{- Part Two -}

day6' :: IO ()
day6' = do
  input <- readFile "resources/day6.txt"
  let response = map (snd . head . count) . transpose . lines $ input
  print response
