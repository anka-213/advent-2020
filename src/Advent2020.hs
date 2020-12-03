{- |
Copyright: (c) 2020 Andreas Källberg
SPDX-License-Identifier: MIT
Maintainer: Andreas Källberg <anka.213@gmail.com>

My Haskell solutions to Advent of Code 2020
-}

module Advent2020
  ( module Advent2020
  , module Day1
  , module Day2
  , module Day3
  )
  where

import Day1
import Day2
import Day3

someFunc :: IO ()
someFunc = putStrLn ("someFunc" :: String)

-- $> l <- map read . lines <$> readFile "test/cases/Day1.input" :: IO [Int]

-- $> day1a l
