{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

module Shared.ByteString where

import           Data.ByteString
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (w2c)
import           Data.Char

infixr 5 :<

pattern b :< bs <- (uncons -> Just (b, bs))
pattern Empty   <- (uncons -> Nothing)

infixr 5 :•
pattern b :• bs <- (w2c -> b) :< bs

(!!•) :: Int -> B.ByteString -> B.ByteString
(!!•) i bs = B.take 1 $ B.drop i bs

bsToStr :: B.ByteString -> String
bsToStr = fmap (chr . fromEnum) . B.unpack
