module Util where

countSat :: (a -> Bool) -> [a] -> Int
countSat p = length . filter p
