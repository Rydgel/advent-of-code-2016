{-# LANGUAGE OverloadedStrings #-}

module Day7
    ( day7
    , day7'
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Either
import           Data.List
import qualified Data.Text            as T
import           Shared.String

{- Day 7: Internet Protocol Version 7 -}

type IPV7 = (String, String)
data IPInfo = HN String | SN String deriving (Show)

isAbba :: String -> Bool
isAbba = any (\(a,b,c,d) -> a == d && b == c && a /= b) . abbas
  where
    abbas = zip4 <*> drop 1 <*> drop 2 <*> drop 3

parseIP :: Parser IPV7
parseIP = concatIPInfos <$> many1 (parseHn <|> parseSn)
  where
    parseHn = HN <$> (char '[' *> many1 letter <* char ']')
    parseSn = SN <$> many1 letter

concatIPInfos :: [IPInfo] -> IPV7
concatIPInfos = foldl parseI ([], [])
  where
    parseI (z1,z2) (HN s) = (z1, z2 ++ " " ++ s)
    parseI (z1,z2) (SN s) = (z1 ++ " " ++ s, z2)

day7 :: IO ()
day7 = do
  input <- T.lines . T.pack <$> readFile "resources/day7.txt"
  let results = rights $ map (parseOnly parseIP) input
  print $ length $ filter (True ==) $ (\(sn,hn) -> isAbba sn && not (isAbba hn)) <$> results

{- Part Two -}

day7' :: IO ()
day7' = do
  input <- T.lines . T.pack <$> readFile "resources/day7.txt"
  let results = rights $ map (parseOnly parseIP) input
  print $ length $ filter (True ==) $ (\(sn,hn) -> (any (checkHn hn) (zipSn sn))) <$> results
  where
    zipSn = zip3 <*> drop 1 <*> drop 2
    checkHn h (a,b,c) = a == c && a /= b && substring [b, a, b] h
