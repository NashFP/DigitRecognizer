import System.Environment (getArgs)
import Data.List
import Data.Ord (compare)

-- Digit Recognizer written during NashFP meeting on 1-26-2016
-- Compile with: ghc -O2 digitrec.hs
-- Run with: digitrec testsamplefile trainingsamplefile
-- Example run: digitrec ../test-sample.csv ../training-sample.csv

replaceComma ',' = ' '
replaceComma c = c

-- Parses a comma-separated string of numbers into a list of ints
-- Haskell words function only splits on space so this function changes
-- commas to spaces before calling words. Then it maps the read function
-- over the list of strings returned by words.
parseLine :: String -> [Int]
parseLine s =
    map read $ words (map replaceComma s)

-- Reads the file content, uses lines to split it into a list of strings,
-- then maps parseLine over the list of lines
loadData filename = do
    filedata <- readFile filename
    return (map parseLine (tail (lines filedata)))

-- Compares two lists of pixels by summing the squares of the differences
-- Returns a pair containing the test sample digit and its score
compareSamples :: [Int] -> [Int] -> (Int,Int)
compareSamples trainingSample testSample =
  (head testSample,
    sum $ zipWith comparePixels (tail testSample) (tail trainingSample))
      where
        comparePixels p1 p2 = sqr (p1 - p2)
        sqr x = x * x

-- Finds the closes match by looking for the one with the minimum score
recognizeSample :: [Int] -> [[Int]] -> (Int,Int)
recognizeSample trainingSample testSamples =
  minimumBy compareScores (map (compareSamples trainingSample) testSamples)
    where
      compareScores (d1,s1) (d2,s2) = compare s1 s2 

-- Returns true if the recognizer recognized the correct digit
sampleMatches :: [[Int]] -> [Int] -> Bool
sampleMatches testSamples trainingSample =
    head trainingSample == fst (recognizeSample trainingSample testSamples)

main = do
  -- Get the command line argumentsw
  argv <- getArgs
  let [testfile,trainingfile] = take 2 argv

  -- Load the data
  testSample <- loadData testfile
  trainingSample <- loadData trainingfile

  -- Count the number of matches
  let numMatched = length $ filter (sampleMatches trainingSample) testSample

  let successRate = (fromIntegral numMatched) / (fromIntegral $ length testSample)
  putStrLn $ show successRate
