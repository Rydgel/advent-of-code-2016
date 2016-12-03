module Day2
    ( day2
    , day2'
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Either
import qualified Data.Text            as T

{- Day 2: Bathroom Security -}

pad :: [String]
pad = [ ['1', '2', '3']
      , ['4', '5', '6']
      , ['7', '8', '9']
      ]

data Direction = UpD | DownD | LeftD | RightD
  deriving (Show)

parseDirections :: Parser [Direction]
parseDirections = many1 parseDirection

parseDirection :: Parser Direction
parseDirection = char 'U' *> pure UpD
             <|> char 'D' *> pure DownD
             <|> char 'L' *> pure LeftD
             <|> char 'R' *> pure RightD

instructions :: Direction -> (Int, Int)
instructions UpD    = ( 0, -1)
instructions DownD  = ( 0,  1)
instructions LeftD  = (-1,  0)
instructions RightD = ( 1,  0)

getDigit :: [String] -> (Int, Int) -> String
getDigit p (x, y) = [(p !! y) !! x]

chooseDigit :: [Direction] -> (Int, Int)
chooseDigit = foldl getNextPos (1, 1)
  where
    getNextPos :: (Int, Int) -> Direction -> (Int, Int)
    getNextPos (x, y) dir =
      let (nX, nY) = instructions dir
          newX     = max 0 (min (x + nX) 2)
          newY     = max 0 (min (y + nY) 2)
      in  (newX, newY)

day2 :: IO ()
day2 = do
  parseS <- T.lines . T.pack <$> readFile "resources/day2.txt"
  let directions = rights $ map (parseOnly parseDirections) parseS
      answer = foldl (\z d -> z ++ getDigit pad (chooseDigit d)) "" directions
  print answer

{- Part Two -}

pad2 :: [String]
pad2 = [ [' ', ' ', '1', ' ', ' ']
       , [' ', '2', '3', '4', ' ']
       , ['5', '6', '7', '8', '9']
       , [' ', 'A', 'B', 'C', ' ']
       , [' ', ' ', 'D', ' ', ' ']
       ]

chooseDigit' :: [Direction] -> (Int, Int)
chooseDigit' = foldl getNextPos (0, 2)
  where
    getNextPos :: (Int, Int) -> Direction -> (Int, Int)
    getNextPos (x, y) dir =
      let (nX, nY) = instructions dir
          newX     = max 0 (min (x + nX) 4)
          newY     = max 0 (min (y + nY) 4)
      in  case getDigit pad2 (newX, newY) of
        " " -> (x, y)
        _   -> (newX, newY)

day2' :: IO ()
day2' = do
  parseS <- T.lines . T.pack <$> readFile "resources/day2.txt"
  let directions = rights $ map (parseOnly parseDirections) parseS
      answer = foldl (\z d -> z ++ getDigit pad2 (chooseDigit' d)) "" directions
  print answer
