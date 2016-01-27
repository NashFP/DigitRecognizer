import Data.List
import Data.Array
import Debug.Trace

replaceComma ',' = ' '
replaceComma c = c

parseLine :: String -> Array Int Int
parseLine s =
    listArray (0, (length lineData) - 1) lineData
       where
         lineData = (map read $ words (map replaceComma s))

loadData filename = do
    filedata <- readFile filename
    return (map parseLine (tail (lines filedata)))

compareSamples :: Array Int Int -> Array Int Int -> Int    
compareSamples trainingSample testSample =
  sum $ map comparePixels (tail (indices trainingSample))
      where
        comparePixels i = sqr((trainingSample ! i) - (testSample ! i))
        sqr x = x * x

recognizeSample :: Array Int Int -> [Array Int Int] -> (Int,Int)
recognizeSample trainingSample testSamples =
  foldl' scoreSample (-1,9999999999) testSamples
      where
        scoreSample (bestDigit,bestScore) sample = 
            if newScore sample < bestScore then (sample ! 0, newScore sample) else (bestDigit,bestScore)
        newScore sample = compareSamples trainingSample sample

sampleMatches :: [Array Int Int] -> Array Int Int -> Bool
sampleMatches testSamples trainingSample =
    trainingSample ! 0 == fst (recognizeSample trainingSample testSamples)

main = do
  testSample <- loadData "../test-sample.csv"
  trainingSample <- loadData "../training-sample.csv"
--  let result = recognizeSample trainingSample testSample
  let numMatched = length $ filter (sampleMatches trainingSample) testSample
  let successRate = (fromIntegral numMatched) / (fromIntegral $ length testSample)
  putStrLn $ show numMatched
  putStrLn $ show (length testSample)
  putStrLn $ show successRate
