import System.IO
import Data.Text(split)
import Data.List

data Observation = Observation { label :: Int, pixels :: [Int] }
type CsvData = [Observation]

-- I copied this function off StackOverflow
splitBy delimiter = foldr f [[]]
  where f c l@(x:xs) | c == delimiter = []:l
                     | otherwise = (c:x):xs

parseLine :: String -> Observation
parseLine line =
  let (head:tail) = map read (splitBy ',' line)
  in Observation { label = head, pixels = tail }

readCsvFile :: String -> IO CsvData
readCsvFile filename = do
  chars <- readFile filename
  let csvLines = lines chars
  return (map parseLine (drop 1 csvLines))

intDist a b = abs (a - b)

distance :: [Int] -> Observation -> Int
distance testPixels obs =
  sum (zipWith intDist testPixels (pixels obs))

minByKey :: Ord k => (v -> k) -> [v] -> v
minByKey keyfn values =
  let pairs = map (\v -> (keyfn v, v)) values
      cmpByKey (k1, v1) (k2, v2) = compare k1 k2
      (kmin, vmin) = Data.List.minimumBy cmpByKey pairs
  in vmin

predict :: CsvData -> [Int] -> Int
predict trainingData testPixels =
  label (minByKey (distance testPixels) (reverse trainingData))

-- Return true if our prediction matches the observed label
passesTest :: CsvData -> Observation -> Bool
passesTest trainingData obs =
  predict trainingData (pixels obs) == label obs

computeScore :: CsvData -> CsvData -> Float
computeScore trainingData testData =
  let successes = filter (passesTest trainingData) testData
  in 100.0 * fromIntegral (length successes) / fromIntegral (length testData)

main = do
  trainingData <- readCsvFile "../training-sample.csv"
  testData <- readCsvFile "../test-sample.csv"
  let score = computeScore trainingData testData
  putStrLn (show score)
