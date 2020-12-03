{-# LANGUAGE NamedFieldPuns #-}
module Day2 where

import Text.ParserCombinators.ReadP

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

parseFile :: String -> [PPPair]
parseFile = map parseLine . lines

readInputFile :: FilePath -> IO [PPPair]
readInputFile = fmap parseFile . readFile

getMyInput :: IO [PPPair]
getMyInput = readInputFile "day2/input.txt"

getSampleInput :: IO [PPPair]
getSampleInput = readInputFile "test/cases/day2.input"

checkPasswd :: Policy -> Password -> Bool
checkPasswd Policy{range, currentChar} pass =
    inRange range $ countChar currentChar pass

countSat :: (a -> Bool) -> [a] -> Int
countSat p = length . filter p

countValid :: [PPPair] -> Int
countValid = countSat (uncurry checkPasswd)

-- >>> countValid <$> getSampleInput
-- 2

-- >>> countValid <$> getMyInput
-- 515
