{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day3 where

import Prelude hiding (head)
import Relude.Extra (inverseMap)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad ((<=<))
import Util
import Data.List (unfoldr)
import AdventDay


newtype Grid = Grid {unGrid :: [[Cell]]}

instance Show Grid where
  show = unlines . map ((++" …") . take 20 . concatMap show) . unGrid

data Cell = Empty | Tree
  deriving (Bounded, Enum)

instance Show Cell where
    show = (:[]) . displayCell

data Slope = Slope {right :: Int, down :: Int}
  deriving Show

data Direction = Horiz | Vert
  deriving Show

type Change = Grid -> Grid

mapGrid :: ([[Cell]] -> [[Cell]]) -> Change
mapGrid f = Grid . f . unGrid

day3 :: Day Grid
day3 = Day
    { dayNr = 3
    , parser = expandGrid . parseDay3
    , part1 = treesOnDefSlope
    , part2 = undefined
    }


displayCell :: Cell -> Char
displayCell Empty = '.'
displayCell Tree  = '#'

parseCell :: Char -> Cell
parseCell = fromMaybe (error "Unexpected char") . inverseMap displayCell

parseDay3 :: String -> Grid
parseDay3 = Grid . (map . map) parseCell . lines

expandGrid :: Grid -> Grid
expandGrid = mapGrid $ map cycle

stepRight :: Grid -> Grid
stepRight = mapGrid $ map tail

stepDown :: Grid -> Grid
stepDown = mapGrid $ drop 1

stepDir :: Direction -> Grid -> Grid
stepDir Horiz = stepRight
stepDir Vert = stepDown

iterN :: Int -> (a -> a) -> a -> a
iterN n f x = iterate f x !! n

moveDir :: Int -> Direction -> Change
moveDir n = iterN n . stepDir

moveSlope :: Slope -> Change
moveSlope Slope{down, right} = moveDir down Vert . moveDir right Horiz

here :: Grid -> Maybe Cell
here = listToMaybe <=< listToMaybe . unGrid

countTrees :: [Cell] -> Int
countTrees = countSat isTree

isTree :: Cell -> Bool
isTree Empty = False
isTree Tree  = True

followSlope :: Slope -> Grid -> [Cell]
followSlope slope = unfoldr slopeStep
  where
    slopeStep :: Grid -> Maybe (Cell, Grid)
    slopeStep g = (,moveSlope slope g) <$> here g

defaultSlope :: Slope
defaultSlope = Slope {right = 3, down = 1}

countTreesOnSlope :: Slope -> Grid -> Int
countTreesOnSlope slope = countTrees . followSlope slope

treesOnDefSlope :: Grid -> Int
treesOnDefSlope = countTreesOnSlope defaultSlope

-- >>>  getSampleInput day3
-- ..##.........##..... …
-- #...#...#..#...#...# …
-- .#....#..#..#....#.. …
-- ..#.#...#.#..#.#...# …
-- .#...##..#..#...##.. …
-- ..#.##.......#.##... …
-- .#.#.#....#.#.#.#... …
-- .#........#.#....... …
-- #.##...#...#.##...#. …
-- #...##....##...##... …
-- .#..#...#.#.#..#...# …

-- >>> followSlope defaultSlope <$> getSampleInput day3
-- [.,.,#,.,#,#,.,#,#,#,#]
-- [.,.,#,.,#,#,.,#,#,#,#]

-- >>> runDay day3 Part1 Sample
-- 7

-- >>> runDay day3 Part1 Real
-- 189

-- * Part 2

otherSlopes :: [Slope]
otherSlopes =
  [ Slope 1 1
  , Slope 3 1
  , Slope 5 1
  , Slope 7 1
  , Slope 1 2
  ]

treesOnAllSlopes :: Grid -> [Int]
treesOnAllSlopes grid = fmap (`countTreesOnSlope` grid) otherSlopes

prodSlopes :: Grid -> Int
prodSlopes = product . treesOnAllSlopes

-- >>> treesOnAllSlopes <$> getInput Sample day3
-- [2,7,3,4,2]

-- >>> prodSlopes <$> getInput Sample day3
-- 336

-- >>> treesOnAllSlopes <$> getInput Real day3
-- [74,189,65,63,30]

-- >>> prodSlopes <$> getInput Real day3
-- 1718180100


--