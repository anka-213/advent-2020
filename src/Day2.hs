{-# LANGUAGE NamedFieldPuns #-}
module Day2 where

import Text.ParserCombinators.ReadP
import Util
import AdventDay

day2 :: Day [PPPair]
day2 = Day
    { dayNr = 2
    , parser = parseDay2
    , part1 = countValid
    , part2 = countValidP2
    }

data Range = Range {from :: Int, to :: Int}
  deriving Show

inRange :: Range -> Int -> Bool
inRange Range{from,to} n = from <= n && n <= to

data Policy = Policy {range :: Range, currentChar :: Char}
  deriving Show

type Password = String

countChar :: Char -> Password -> Int
countChar c = length . filter (==c)

type PPPair = (Policy, Password)

readp :: Read a => ReadP a
readp = readS_to_P reads

num :: ReadP Int
num = readp

parse :: Show a => ReadP a -> String -> a
parse p s = case readP_to_S p s of
    [(a,"")] -> a
    err -> error $ "Couldn't parse line " ++ show s ++ " result: " ++ show err

pRange :: ReadP Range
pRange = Range <$> num <* string "-" <*> num

policy :: ReadP Policy
policy = Policy <$> pRange <* string " " <*> get

passwd :: ReadP Password
passwd = many get

parseLine :: String -> PPPair
parseLine = parse $ (,) <$> policy <* string ": " <*> passwd <* eof

parseDay2 :: String -> [PPPair]
parseDay2 = map parseLine . lines

checkPasswd :: Policy -> Password -> Bool
checkPasswd Policy{range, currentChar} pass =
    inRange range $ countChar currentChar pass

countValid :: [PPPair] -> Int
countValid = countSat (uncurry checkPasswd)

-- >>> countValid <$> getSampleInput day2
-- 2

-- >>> countValid <$> getMyInput day2
-- 515

-- * Part 2

getEdges :: Range -> [a] -> [a]
-- getEdges Range{from,to} xs = map fst . filter (\(_,n) -> n == from || n == to) $ zip xs [1..]
getEdges Range{from,to} xs = [x | (x,n) <- zip xs [1..], n == from || n == to]

checkPasswdPart2 :: Policy -> Password -> Bool
checkPasswdPart2 Policy{range, currentChar} pass =
    (==1) . countChar currentChar $ getEdges range pass
    -- inRange range $ countChar currentChar pass

countValidP2 :: [PPPair] -> Int
countValidP2 = countSat (uncurry checkPasswdPart2)


day2part2 :: String -> Int
day2part2 = countValidP2 . parseDay2

-- >>> countValidP2 <$> getSampleInput day2
-- 1

-- >>> countValidP2 <$> getMyInput day2
-- 711
