import System.Environment (getArgs)
import Data.List
import Data.Array.Unboxed
import Data.Ord (compare)

-- Array-version of digit recognizer
-- In theory it should be much faster to process the samples as arrays
-- because they are stored in a contiguous block of memory and modern
-- processors are optimized to pre-fetch contiguous locations.
-- In practice it only runs twice as fast as the list version

-- Compile with: ghc -O2 digitrec_array.hs
-- Run with: digitrec_array testsamplefile trainingsamplefile
-- Example run: digitrec_array ../test-sample.csv ../training-sample.csv

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

-- Compares two lists of pixels by summing the squares of the differences
-- Returns a pair containing the test sample digit and its score
compareSamples :: UArray Int Int -> UArray Int Int -> (Int,Int)
compareSamples trainingSample testSample =
  (testSample ! 0,
    sum $ map comparePixels (tail (indices trainingSample)))
      where
        comparePixels i = sqr((trainingSample ! i) - (testSample ! i))
        sqr x = x * x

-- Finds the closes match by looking for the one with the minimum score
recognizeSample :: UArray Int Int -> [UArray Int Int] -> (Int,Int)
recognizeSample trainingSample testSamples =
  minimumBy compareScores (map (compareSamples trainingSample) testSamples)
    where
      compareScores (d1,s1) (d2,s2) = compare s1 s2

-- Returns true if the recognizer recognized the correct digit
sampleMatches :: [UArray Int Int] -> UArray Int Int -> Bool
sampleMatches testSamples trainingSample =
    trainingSample ! 0 == fst (recognizeSample trainingSample testSamples)

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
