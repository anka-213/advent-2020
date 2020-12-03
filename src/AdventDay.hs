{-# LANGUAGE NamedFieldPuns #-}
module AdventDay where

type Output = Int

data Day input = Day
    { dayNr :: Int
    , parser :: String -> input
    , part1 :: input -> Output
    , part2 :: input -> Output
    }

data TestType = Sample | Real

testLocation :: TestType -> Day a -> FilePath
testLocation Sample Day{dayNr} = "test/cases/Day" ++ show dayNr ++ ".input"
testLocation Real   Day{dayNr} = "day" ++ show dayNr ++ "/input.txt"

data Part = Part1 | Part2

runPart :: Part -> Day a -> a -> Output
runPart Part1 = part1
runPart Part2 = part2

getRawInput :: TestType -> Day a -> IO String
getRawInput tt = readFile . testLocation tt

parseFile :: Day a -> FilePath -> IO a
parseFile Day{parser} = fmap parser . readFile

getInput :: TestType -> Day a -> IO a
getInput tt = parseFile <*> testLocation tt

getSampleInput :: Day a -> IO a
getSampleInput = getInput Sample

getMyInput :: Day a -> IO a
getMyInput = getInput Real

runDay :: Day a -> Part -> TestType -> IO Output
runDay day part tt = runPart part day <$> getInput tt day