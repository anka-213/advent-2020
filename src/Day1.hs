module Day1 where

import Data.List (tails, sort)
import Data.Bifunctor (Bifunctor(second))
-- import Relude.List (sort)
-- import Relude (Bifunctor(second))

parseDay1 :: String -> [Int]
parseDay1 = map read . lines

day1aNaive :: [Int] -> Int
day1aNaive l = head [a*b | a <- l, b <- l, a + b == 2020]

day1a :: [Int] -> Int
day1a l = head [a*b | (a: bs) <- tails l, b <- bs, a + b == 2020]

day1a' :: [Int] -> Int
day1a' xs = uncurry (*) . second (2020-) $ findEqPair as bs
  where
    as = sort xs
    bs = sort (map (2020 -) xs)

-- Takes two sorted lists
findEqPair :: [Int] -> [Int] -> (Int, Int)
findEqPair (a:_) (b:_) | a == b = (a,b)
findEqPair (a:as) (b:bs) | a < b = findEqPair as (b:bs)
findEqPair (a:as) (b:bs) | a > b = findEqPair (a:as) bs
findEqPair _ _ = error "No solution"


day1b :: [Int] -> Int
day1b l = head [a*b*c | (a:bs) <- tails l, (b:cs) <- tails bs, c <- cs, a + b + c == 2020]

findNSums :: (Eq a, Num a) => a -> Int -> [a] -> [[a]]
findNSums _ n _  | n < 0 = error $ "findNSums: Invalid number of sums: " ++ show n
findNSums 0 0 _ = pure [] -- We're done
findNSums _ 0 _ = mempty  -- We reached the end, but k /= 0
-- findNSums k n as | k < 0 = mempty -- Assuming all elements >= 0
findNSums k n as = do
    (a : bs) <- tails as
    (a:) <$> findNSums (k - a) (n - 1) bs

prodNSums :: (Num b, Eq b) => b -> Int -> [b] -> [b]
prodNSums k n xs = product <$> findNSums k n xs