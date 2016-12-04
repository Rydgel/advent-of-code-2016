module Day3
    ( day3
    , day3'
    ) where

import           Data.Attoparsec.Text
import           Data.Either
import qualified Data.Text            as T

type Triangle = (Int, Int, Int)

parseTriangle :: Parser Triangle
parseTriangle = do
  a <- skipMany space *> decimal
  b <- skipMany space *> decimal
  c <- skipMany space *> decimal
  return (a, b, c)

isValid :: Triangle -> Bool
isValid (a, b, c) = a + b > c && a + c > b && b + c > a

day3 :: IO ()
day3 = do
  parseS <- T.lines . T.pack <$> readFile "resources/day3.txt"
  let triangles = rights $ map (parseOnly parseTriangle) parseS
  print $ length $ filter isValid triangles

{- Part Two -}

byCols :: [Triangle] -> [Triangle]
byCols ((a, b, c):(d, e, f):(g, h, i):rest) = (a, d, g) : (b, e, h) : (c, f, i) : byCols rest
byCols _ = []

day3' :: IO ()
day3' = do
  parseS <- T.lines . T.pack <$> readFile "resources/day3.txt"
  let triangles = rights $ map (parseOnly parseTriangle) parseS
  print $ length $ filter isValid (byCols triangles)
