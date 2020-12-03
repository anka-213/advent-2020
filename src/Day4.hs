-- {-# LANGUAGE TupleSections #-}
-- {-# LANGUAGE NamedFieldPuns #-}
module Day4 where

-- import Prelude hiding (head)
-- import Relude.Extra (inverseMap)
-- import Data.Maybe (listToMaybe, fromMaybe)
-- import Control.Monad ((<=<))
-- import Data.List (unfoldr)

import Util
import AdventDay


type Day4Data = [()]

day4 :: Day Day4Data
day4 = Day
    { dayNr = 4
    , parser = parseDay4
    , part1 = undefined
    , part2 = undefined
    }


parseDay4 :: String -> Day4Data
parseDay4 = fmap parseLine . lines

parseLine :: String -> ()
parseLine _ = ()


-- >>>  getSampleInput day4

-- >>> foo <$> getSampleInput day3

-- >>> runDay day4 Part1 Sample

-- >>> runDay day4 Part1 Real

-- * Part 2

-- >>> runDay day4 Part2 Sample

-- >>> runDay day4 Part2 Real


--