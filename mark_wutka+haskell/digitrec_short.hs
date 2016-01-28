import System.Environment (getArgs)
import Data.List
import Data.Array.Base
import Data.Array.Unboxed
import Data.Ord (compare)

-- Short-circuiting Array-version of digit recognizer
-- This program builds on the array version of the digit recognizer
-- by swapping out the built-in functions for tail-recursive iterative
-- functions that allow the scoring process to stop the moment the score
-- becomes greater than the current best score.
-- On my system, my original list-based version takes about 85 seconds
-- The array version takes about 48 seconds
-- The short-circuiting array version takes about 19 seconds

-- Compile with: ghc -O2 digitrec_short.hs
-- Run with: digitrec_short testsamplefile trainingsamplefile
-- Example run: digitrec_short ../test-sample.csv ../training-sample.csv

replaceComma ',' = ' '
replaceComma c = c

-- Parses a comma-separated string of numbers into an array of ints
-- Haskell words function only splits on space so this function changes
-- commas to spaces before calling words. Then it maps the read function
-- over the list of strings returned by words, then creates an array from that
parseLine :: String -> UArray Int Int
parseLine s =
    listArray (0, (length lineData) - 1) lineData
       where
         lineData = (map read $ words (map replaceComma s))

-- Reads the file content, uses lines to split it into a list of strings,
-- then maps parseLine over the list of lines
loadData filename = do
    filedata <- readFile filename
    return (map parseLine (tail (lines filedata)))

-- compareSamples tries to iterate recursively instead of using built-in functions and has a short-circuit
-- to stop iterating if the curr score is already greater than the best score
compareSamples' :: Int -> Int -> Int -> Int -> UArray Int Int -> UArray Int Int -> (Int,Int)
compareSamples' i end sum minSum testSample trainingSample =
    if (i == end) || (sum > minSum) then
        (trainingSample ! 0, sum)
    else
        compareSamples' (i+1) end (sum + sqr ((unsafeAt trainingSample i) - (unsafeAt testSample i))) minSum testSample trainingSample
            where
              sqr x = x * x

compareSamples testSample trainingSample currMin =
    compareSamples' 1 ((snd (bounds trainingSample)) + 1) 0 currMin testSample trainingSample

-- Finds the closest match by looking for the one with the minimum score, using an iterative process
-- to support short-circuiting
recognizeSample' best _ [] = best
recognizeSample' (currBestDigit,currBestScore) testSample (trainingSample : trainingSamples) =
    if currScore < currBestScore then
        recognizeSample' (currDigit,currScore) testSample trainingSamples
    else
        recognizeSample' (currBestDigit,currBestScore) testSample trainingSamples
            where
              (currDigit, currScore) = compareSamples testSample trainingSample currBestScore

-- Finds the closest match, seeding the initial digit and best score with -1,999999999
recognizeSample testSample trainingSamples = recognizeSample' (-1,999999999) testSample trainingSamples
      
-- Returns true if the recognizer recognized the correct digit
sampleMatches :: [UArray Int Int] -> UArray Int Int -> Bool
sampleMatches trainingSamples testSample =
    testSample ! 0 == fst (recognizeSample testSample trainingSamples)

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
