{-# LANGUAGE OverloadedStrings #-}

module Day5
    ( day5
    ) where

import           Crypto.Hash.MD5
import qualified Data.ByteString as B
import           Data.ByteString.Base16
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

day5' :: IO ()
day5' = undefined
