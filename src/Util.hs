module Util where

countSat :: (a -> Bool) -> [a] -> Int
countSat p = length . filter p

iterN :: Int -> (a -> a) -> a -> a
iterN n f x = iterate f x !! n
