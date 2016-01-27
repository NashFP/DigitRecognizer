import Data.List

replaceComma ',' = ' '
replaceComma c = c

parseLine :: String -> [Int]
parseLine s =
    map read $ words (map replaceComma s)

loadData filename = do
    filedata <- readFile filename
    return (map parseLine (tail (lines filedata)))

compareSamples :: [Int] -> [Int] -> Int    
compareSamples trainingSample testSample =
  sum $ zipWith comparePixels (tail testSample) (tail trainingSample)
      where
        comparePixels p1 p2 = sqr (p1 - p2)
        sqr x = x * x

recognizeSample :: [Int] -> [[Int]] -> (Int,Int)
recognizeSample trainingSample testSamples =
  foldl' scoreSample (-1,9999999999) testSamples
      where
        scoreSample (bestDigit,bestScore) sample = if newScore sample < bestScore then (head sample, newScore sample) else (bestDigit,bestScore)
        newScore sample = compareSamples trainingSample sample

sampleMatches :: [[Int]] -> [Int] -> Bool
sampleMatches testSamples trainingSample =
    head trainingSample == fst (recognizeSample trainingSample testSamples)

main = do
  testSample <- loadData "../test-sample.csv"
  trainingSample <- loadData "../training-sample.csv"
--  let result = recognizeSample trainingSample testSample
  let numMatched = length $ filter (sampleMatches trainingSample) testSample
  let successRate = (fromIntegral numMatched) / (fromIntegral $ length testSample)
  putStrLn $ show numMatched
  putStrLn $ show (length testSample)
  putStrLn $ show successRate
