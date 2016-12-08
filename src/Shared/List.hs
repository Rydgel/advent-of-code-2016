module Shared.List
    ( countF
    ) where


import Data.List

countF :: (a -> Bool) -> [a] -> Int
countF pred = length . filter pred
