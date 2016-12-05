module Shared.ParserTools
    ( sepEndBy
    , sepEndBy1
    ) where

import           Control.Applicative
import           Data.Attoparsec.Text

sepEndBy :: Alternative f => f a -> f b -> f [a]
sepEndBy p sep = sepBy p sep <* optional sep

sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 p sep = (:) <$> p <*> ((sep *> sepEndBy p sep) <|> pure [])
