module Day4
    ( day4
    , day4'
    ) where

import           Control.Applicative
import           Control.Arrow
import           Data.Attoparsec.Text hiding (take)
import           Data.Either
import           Data.List            hiding (takeWhile)
import qualified Data.Text            as T
import           Shared.ParserTools

{- Day 4: Security Through Obscurity -}

data Room = Room
  { encryptedName :: String
  , sectorId      :: Int
  , checksum      :: String
  } deriving (Show)

parseRoom :: Parser Room
parseRoom = Room . concat
  <$> many1 letter `sepEndBy1` char '-'
  <*> decimal
  <*> (char '[' *> many1 (letter <|> digit) <* char ']')

roomIsValid :: Room -> Bool
roomIsValid (Room en _ ch) = nCommonChars 5 (filter (/= '-') en) == ch
  where
    nCommonChars n = take n . map snd . sort . map (negate . length &&& head) . group . sort

day4 :: IO ()
day4 = do
  parseS <- T.lines . T.pack <$> readFile "resources/day4.txt"
  let rooms = rights $ map (parseOnly parseRoom) parseS
  print $ map (parseOnly parseRoom) parseS
  print $ sum . map sectorId . filter roomIsValid $ rooms

{- Part Two -}

rotate :: Int -> Char -> Char
rotate 0 c   = c
rotate _ '-' = ' '
rotate n 'z' = rotate (n-1) 'a'
rotate n  c  = rotate (n-1) (succ c)

isNorthPole :: Room -> Bool
isNorthPole (Room en si _) = "north" `isInfixOf` map (rotate si) en

day4' :: IO ()
day4' = do
  parseS <- T.lines . T.pack <$> readFile "resources/day4.txt"
  let rooms = rights $ map (parseOnly parseRoom) parseS
  print $ sectorId . head . filter isNorthPole . filter roomIsValid $ rooms
