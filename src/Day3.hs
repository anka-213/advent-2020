{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
module Day3 where

import Prelude hiding (head)
import Relude.Extra (inverseMap)
import Data.Maybe (listToMaybe, fromMaybe)
import Control.Monad ((<=<))
import Util
import Data.List (unfoldr)


type Grid = [[Cell]]

data Cell = Empty | Tree
  deriving (Bounded, Enum)

instance Show Cell where
    show = (:[]) . displayCell

data Slope = Slope {right :: Int, down :: Int}
  deriving Show

data Direction = Horiz | Vert
  deriving Show

type Change = Grid -> Grid

displayCell :: Cell -> Char
displayCell Empty = '.'
displayCell Tree  = '#'

parseCell :: Char -> Cell
parseCell = fromMaybe (error "Unexpected char") . inverseMap displayCell

parseDay3 :: String -> Grid
parseDay3 = (map . map) parseCell . lines

expandGrid :: Grid -> Grid
expandGrid = map cycle

stepRight :: Grid -> Grid
stepRight = map tail

stepDown :: Grid -> Grid
stepDown = drop 1

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
here = listToMaybe <=< listToMaybe

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
