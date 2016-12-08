{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day8
    ( day8
    ) where

import           Control.Applicative
import           Data.Array.Repa      ((:.) (..), Z (..))
import qualified Data.Array.Repa      as R
import qualified Data.Array.Repa.Eval as RE
import           Data.Attoparsec.Text hiding (take)
import           Data.Either
import qualified Data.Text            as T

{- Day 8: Two-Factor Authentication -}

data RotationType = Row | Column deriving (Show)
data Coord = X Int | Y Int deriving (Show)
data Command = Rect (Int,Int) | Rotation RotationType Coord Int deriving (Show)

{- Parsing stuff -}

parseRotationType :: Parser RotationType
parseRotationType =
      string "row" *> pure Row
  <|> string "column" *> pure Column

parseCoord :: Parser Coord
parseCoord = parseX <|> parseY
  where
    parseX = X <$> (string "x=" *> decimal)
    parseY = Y <$> (string "y=" *> decimal)

parseRectangle :: Parser Command
parseRectangle = do
  _ <- string "rect "
  a <- decimal
  _ <- char 'x'
  b <- decimal
  return $ Rect (a,b)

parseRotation :: Parser Command
parseRotation = do
  _ <- string "rotate "
  rotType <- parseRotationType
  _ <- space
  coord <- parseCoord
  _ <- string " by "
  i <- decimal
  return $ Rotation rotType coord i

parseCommand :: Parser Command
parseCommand = parseRectangle <|> parseRotation

{- Screen -}

type Cell = Int
type Grid = R.Array R.U R.DIM2 Cell

-- | size: The screen is 50 pixels wide and 6 pixels tall
initGrid :: Grid
initGrid = RE.fromList (Z :. 50 :. 6) $ replicate (6 * 50) 0

drawRect :: Grid -> Int -> Int -> Grid
drawRect grid width height =
  R.computeS $ R.fromFunction (Z :. 50 :. 6) sp
  where
    sp sh@(Z :. x :. y)
      | x < width && y < height = 1
      | otherwise = grid R.! sh

rotCol :: Grid -> Int -> Int -> Grid
rotCol grid col a =
  R.computeS $ R.fromFunction (Z :. 50 :. 6) sp
  where
    sp (Z :. x :. y) =
      let y' = if x == col then (y - a) `mod` 6 else y
      in  grid R.! (Z :. x :. y')

rotRow :: Grid -> Int -> Int -> Grid
rotRow grid row a =
  R.computeS $ R.fromFunction (Z :. 50 :. 6) sp
  where
    sp (Z :. x :. y) =
      let x' = if y == row then (x - a) `mod` 50 else x
      in  grid R.! (Z :. x' :. y)

step :: Grid -> Command -> Grid
step grid (Rect (width,height)) = drawRect grid width height
step grid (Rotation Row (Y row) i) = rotRow grid row i
step grid (Rotation Column (X col) i) = rotCol grid col i
step grid _ = grid

countLightOn :: Grid -> Int
countLightOn = R.sumAllS

day8 :: IO ()
day8 = do
  input <- T.lines . T.pack <$> readFile "resources/day8.txt"
  let results = rights $ map (parseOnly parseCommand) input
  let finalGrid = foldl step initGrid results
  print $ countLightOn finalGrid
  putStrLn ""
  drawGrid finalGrid

{- Part Two -}

drawGrid :: Grid -> IO ()
drawGrid grid = do
  let matrix = [ [grid R.! (Z :. x :. y) | x <- [0..49]] | y <- [0..5] ]
  mapM_ (putStrLn . map toChar) matrix
  where
    toChar 1 = '#'
    toChar _ = ' '
