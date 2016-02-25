import System.Environment (getArgs)
import Data.List
import Data.Ord (compare)
import qualified Data.Vector.Unboxed as Vector

-- Vector-version of digit recognizer
-- Needs vector package (cabal install vector)
-- Compile with: ghc -O2 digitrec_vector.hs
-- Run with: digitrec_vector testsamplefile trainingsamplefile
-- Example run: digitrec_vector ../test-sample.csv ../training-sample.csv

replaceComma ',' = ' '
replaceComma c = c

-- Parses a comma-separated string of numbers into an array of ints
-- Haskell words function only splits on space so this function changes
-- commas to spaces before calling words. Then it maps the read function
-- over the list of strings returned by words, then creates an array from that
parseLine :: String -> (Int, Vector.Vector Int)
parseLine s =
    (head lineData, Vector.fromList (tail lineData))
       where
         lineData = (map read $ words (map replaceComma s))

-- Reads the file content, uses lines to split it into a list of strings,
-- then maps parseLine over the list of lines
loadData filename = do
    filedata <- readFile filename
    return (map parseLine (tail (lines filedata)))

-- Compares two lists of pixels by summing the squares of the differences
-- Returns a pair containing the test sample digit and its score
compareSamples :: (Int, Vector.Vector Int) -> (Int, Vector.Vector Int) -> (Int,Int)
compareSamples (testCh,testSample) (trainingCh,trainingSample) =
  (trainingCh,
    Vector.ifoldl' sumPixels 0 testSample)
      where
        sumPixels s i p1 = s + sqr (p1 - (trainingSample `Vector.unsafeIndex` i))
        sqr x = x * x

-- Finds the closes match by looking for the one with the minimum score
recognizeSample :: (Int, Vector.Vector Int) -> [(Int, Vector.Vector Int)] -> (Int,Int)
recognizeSample testSample trainingSamples =
  minimumBy compareScores (map (compareSamples testSample) trainingSamples)
    where
      compareScores (d1,s1) (d2,s2) = compare s1 s2

-- Returns true if the recognizer recognized the correct digit
sampleMatches :: [(Int, Vector.Vector Int)] -> (Int, Vector.Vector Int) -> Bool
sampleMatches trainingSamples (testCh, testSample) =
    testCh == fst (recognizeSample (testCh, testSample) trainingSamples)

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
