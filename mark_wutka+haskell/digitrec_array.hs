import System.Environment (getArgs)
import Data.List
import Data.Array.Base
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
compareSamples testSample trainingSample =
  (trainingSample ! 0,
    sum $ map comparePixels (tail (indices trainingSample)))
      where
        comparePixels i = sqr((unsafeAt trainingSample i) - (unsafeAt testSample i))
        sqr x = x * x

-- compareSamples2 tries to iterate recursively instead of using built-in functions
compareSamples2' :: Int -> Int -> Int -> UArray Int Int -> UArray Int Int -> (Int,Int)
compareSamples2' i end sum testSample trainingSample =
    if i == end then
        (trainingSample ! 0, sum)
    else
        compareSamples2' (i+1) end (sum + sqr ((unsafeAt trainingSample i) - (unsafeAt testSample i))) testSample trainingSample
            where
              sqr x = x * x

compareSamples2 testSample trainingSample =
    compareSamples2' 1 ((snd (bounds trainingSample)) + 1) 0 testSample trainingSample

-- Finds the closes match by looking for the one with the minimum score
recognizeSample :: UArray Int Int -> [UArray Int Int] -> (Int,Int)
recognizeSample testSample trainingSamples =
  minimumBy compareScores (map (compareSamples testSample) trainingSamples)
    where
      compareScores (d1,s1) (d2,s2) = compare s1 s2

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
