{-# LANGUAGE OverloadedStrings #-}

module Day5
    ( day5
    , day5'
    ) where

import           Control.Lens
import           Control.Monad
import           Crypto.Hash.MD5
import qualified Data.ByteString        as B
import           Data.ByteString.Base16
import           Data.Char
import           Data.Maybe
import           Shared.ByteString

{- Day 5: How About a Nice Game of Chess? -}

puzzleInput :: B.ByteString
puzzleInput = "reyedfim"

packASCII :: String -> B.ByteString
packASCII = B.pack . map (fromIntegral . fromEnum)

numbers :: B.ByteString -> [B.ByteString]
numbers prefix =
  [ prefix `B.append` packASCII (show i)
  | i <- [(0 :: Int)..] ]

hashes :: B.ByteString -> [B.ByteString]
hashes prefix = encode . hash <$> numbers prefix

isGoodHash :: B.ByteString -> Bool
isGoodHash bs = "00000" == B.take 5 bs

day5 :: IO ()
day5 = print $ concatMap (bsToStr . (5 !!•)) . take 8 . filter isGoodHash . hashes $ puzzleInput

{- Part Two -}

initAnswer :: [Maybe Char]
initAnswer = replicate 8 Nothing

answerFoundDigit :: [Maybe Char] -> Char -> Char -> [Maybe Char]
answerFoundDigit ans index d
  | not (isDigit index) = ans
  | read [index] > 7 = ans
  | otherwise =
    case ans !! read [index] of
      Nothing -> ans & element (read [index]) .~ Just d
      _ -> ans

day5' :: IO ()
day5' =
  foldM_ computeF initAnswer $ filter isGoodHash . hashes $ puzzleInput
  where
    computeF :: [Maybe Char] -> B.ByteString -> IO [Maybe Char]
    computeF answers hash = do
      let index = head $ bsToStr (5 !!• hash)
      let value = head $ bsToStr (6 !!• hash)
      let newAnswers = answerFoundDigit answers index value
      print $ catMaybes newAnswers
      return newAnswers
