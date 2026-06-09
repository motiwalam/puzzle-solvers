module Util.UnorderedPair where

import Data.Function (on)

data UPair a = UPair a a
    deriving (Show)

canonical :: Ord a => UPair a -> (a, a)
canonical (UPair a b)
    | a <= b    = (a, b)
    | otherwise = (b, a)

instance Eq a => Eq (UPair a) where
    UPair x1 y1 == UPair x2 y2
        = (x1 == x2 && y1 == y2) || (x1 == y2 && y1 == x2)

instance Ord a => Ord (UPair a) where
    compare = compare `on` canonical
