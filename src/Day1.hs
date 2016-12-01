module Day1
    ( day1
    , day1'
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import qualified Data.List            as DL
import qualified Data.Text            as T

{- Day 1: No Time for a Taxicab -}

data CardinalPoint = North | Est | South | West deriving (Eq, Show)
data Position = Position (Int, Int) CardinalPoint deriving (Show)
data Direction = R | L | S deriving (Eq, Show)
data Move = Move Direction Int deriving (Show)

instance Eq Position where
  (Position coord1 _) == (Position coord2 _) =
    coord1 == coord2

instance Ord Position where
  compare (Position (x1,y1) _) (Position (x2,y2) _) =
    (abs x1 + abs y1) `compare` (abs x2 + abs y2)

parseMoves :: Parser [Move]
parseMoves = oneMove `sepBy` char ','
  where oneMove = Move <$> parseDirection <*> decimal
        parseDirection = skipSpace *> (char 'L' *> pure L <|> char 'R' *> pure R)

initState :: Position
initState = Position (0, 0) North

turnRight :: CardinalPoint -> CardinalPoint
turnRight North = Est
turnRight Est = South
turnRight South = West
turnRight West = North

changeFacing :: Direction -> CardinalPoint -> CardinalPoint
changeFacing L = turnRight . turnRight . turnRight
changeFacing R = turnRight
changeFacing S = id

move :: Position -> Move -> Position
move (Position (x, y) cardp) (Move direction amount) =
  let newFacing = changeFacing direction cardp
  in case newFacing of
    North -> Position (x, y + amount) newFacing
    Est -> Position (x + amount, y) newFacing
    South -> Position (x, y - amount) newFacing
    West -> Position (x - amount, y) newFacing

day1 :: IO ()
day1 = do
  parseS <- T.pack <$> readFile "resources/day1.txt"
  let (Right moves) = parseOnly parseMoves parseS
      (Position (x, y) _) = foldl move initState moves
  print $ abs x + abs y

{- Part Two -}

incrementalMoves :: [Move] -> [Move]
incrementalMoves = concatMap (\(Move d i) -> Move d 1 : replicate (i-1) (Move S 1))

groupPositionsByVisits :: [Move] -> [[Position]]
groupPositionsByVisits = DL.group . DL.sort . scanl move initState . incrementalMoves

day1' :: IO ()
day1' = do
  parseS <- T.pack <$> readFile "resources/day1.txt"
  let (Right moves) = parseOnly parseMoves parseS
  let visitesTwice = filter ((==2) . length) $ groupPositionsByVisits moves
  let Position (x, y) _ : _ = head visitesTwice
  print $ abs x + abs y
